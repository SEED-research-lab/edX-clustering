
#ChangeLog
# 2017.04.13    Added GUI user selection of Clickstream file
#               Retained student_id for output file (it's needed for identifying gendered subsets)

##########Reading files, converting to dataframe object, eliminating irrelevant columns#####

#User selection of the CLICKSTREAM data file to process
print("Select the SQL clickstream data file. It should end with 'courseware_studentmodule-prod-analytics.sql'")
filenameClickstream <- file.choose()

#read in the clickstream data and extract needed columns
dataClickstream <- readr::read_tsv(filenameClickstream)
dataClickstream <- dataClickstream[names(dataClickstream) %in% c("module_id","student_id","created")]

#read in the ordered module information
module_markers <- readr::read_csv("module_order_file.csv")
module_markers <- as.data.frame(module_markers)

##Ordering both dataClickstream and module_marker object by module_id field
dataClickstream=dataClickstream[order(dataClickstream$module_id,decreasing=F),]
module_markers=module_markers[order(module_markers$module_id,decreasing=F),]

##Eliminating module_id from dataClickstream if that id does not appear in module_marker
dataClickstream=subset(dataClickstream,dataClickstream$module_id %in% module_markers$module_id)

############################################################################################

##########Mapping module_id in every row of dataClickstream to order_integer of the module#########

change_list=c()
change_list=c(change_list,0)
for(i in unique(module_markers$module_id))
{
  temp_df=subset(dataClickstream,dataClickstream$module_id==i)
  change_list=c(change_list,nrow(temp_df))
}
for(i in 2:length(change_list))
{
  change_list[i]=change_list[i]+change_list[i-1]
}

marker_list=c()
for(i in 1:(length(change_list)-1))
{
  se=rep(module_markers$module_no[i],as.integer(1+(change_list[i+1])-(change_list[i]+1)))
  marker_list=c(marker_list,se)
}
dataClickstream<-cbind(dataClickstream,marker_list)

############################################################################################

##########Converting time to POSIXct format and adding time column to dataClickstream##############

time=as.POSIXct(dataClickstream$created,format="%m/%d/%Y %H:%M")
dataClickstream<-cbind(dataClickstream,time)
##Keeping only releveant columns
dataClickstream<-dataClickstream[names(dataClickstream) %in% c("student_id","time","marker_list")]
##Sorting dataClickstream in order of student_id
dataClickstream<-dataClickstream[order(dataClickstream$student_id,decreasing=F),]

############################################################################################

##########Converting student_id to integers from 1 to total_number_registered###############

u_id=c()
counter=1
for(i in sort(unique(dataClickstream$student_id),decreasing=F))
{
  temp_df=subset(dataClickstream,dataClickstream$student_id==i)
  se=rep(counter,nrow(temp_df))
  u_id=c(u_id,se)
  counter=counter+1
}
dataClickstream<-cbind(dataClickstream,u_id)
return(dataClickstream)


############################################################################################


##########Retaining relevant columns, renaming columns and writing to csv file##############

dataClickstream<-dataClickstream[names(dataClickstream) %in% c("student_id","marker_list","u_id","time")]
names(dataClickstream)<-c("orig_student_id","module_number","time","temp_student_id")
write.csv(dataClickstream,"preprocessed_data.csv")
rm(list=ls())
