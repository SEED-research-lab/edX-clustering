

#ChangeLog
# 2017.04.13    initial creation


#######################################
rm(list=ls())

# #User selection of the CLICKSTREAM data file to process
# print("Select the SQL clickstream data file. It should end with 'courseware_studentmodule-prod-analytics.sql'")
# filenameClickstream <- file.choose()
# 
# #read in the clickstream data and extract needed columns
# dataClickstream <- readr::read_tsv(filenameClickstream)
# dataClickstream <- dataClickstream[names(dataClickstream) %in% c("module_id","orig_student_id","created")]

dataClickstream <- readr::read_csv("preprocessed_data.csv")
# dataClickstreamMale <- readr::read_csv("preprocessed_data_males.csv")
# dataClickstreamFemale <- readr::read_csv("preprocessed_data_females.csv")

#User selection of the USER PROFILE data file to process
print("Select the SQL USER PROFILE data file.")
filenameUserProfile <- file.choose()

#read in the user profile data 
dataUserProfile <- readr::read_tsv(filenameUserProfile)
dataUserProfile <- dataUserProfile[names(dataUserProfile) %in% c("id", "user_id", "gender", "mailing_address", "year_of_birth", "level_of_education", "country")]

#find the subset for self-identified gendered profiles
males <- subset(dataUserProfile, dataUserProfile$gender == "m")
females <- subset(dataUserProfile, dataUserProfile$gender == "f")

##############################################

############## seperate the Clickstream data into gendered subsets

#create dataframes to save the gendered clickstream data to
dataClickstreamMale <- data.frame()
dataClickstreamFemale <- data.frame()

#create 2 lists for each gender (1) all known student IDs and (2) empty list to save users without clickstream data (never accessed the course)
maleID_List <- unique(males$user_id)
noAccessMales <- c()
femaleID_List <- unique(females$user_id)
noAccessFemales <- c()

#create progress status variables for male processing loop
iCount <- 0 #loop counter for completion updates
start <-  proc.time() #save the time (to compute ellapsed time of loop)
library(progress)
pb <- progress_bar$new(
  format = "  processing [:bar] :percent eta: :eta",
  total = 100, clear = FALSE, width= 120)

#build up a dataframe with all rows of each male user's clickstream data
print("Processing male user's clickstream data.")
pb$tick(0)
for(ID in maleID_List)
{
  if(nrow(subset(dataClickstream, dataClickstream$orig_student_id == ID)) > 0) #ensure that the ID exists in the clickstream data
  {
    dataClickstreamMale <- rbind(dataClickstreamMale, subset(dataClickstream, dataClickstream$orig_student_id == ID))
  }
  else #save to a list of males with no clickstream data (never accessed the course)
  {
    noAccessMales <- c(noAccessMales, ID)
  }
  
  #update the user on the percentage complete and elapsed time after every n users processed
  #Sys.sleep(1 / 100)
  iCount <- iCount + 1
  #the following will run for every 1% complete
  if(iCount%%as.integer((length(maleID_List))/100) == 0)
  {
    pb$tick()
    # print((iCount/(length(maleID_List)))*100)
    # #calculate the percentage and format it only 1 decimal place, save as a string
    # pctComplete <- sprintf("%.1f", iCount/(length(maleID_List))*100, digits = 2)
    # #print the status update
    # print(paste0("Males, ", pctComplete,"% complete", collapse = ""))
    # print(proc.time() - start)
  }
}
print(proc.time() - start)


#reset progress status variables for female processing loop
iCount <- 0 #loop counter for completion updates
start <-  proc.time() #save the time (to compute ellapsed time of loop)
pb <- progress_bar$new(
  format = "  processing [:bar] :percent eta: :eta",
  total = 100, clear = FALSE, width= 120)

#build up a dataframe with all rows of each female user's clickstream data
print("Processing female user's clickstream data.")
pb$tick(0)
for(ID in femaleID_List)
{
  if(nrow(subset(dataClickstream, dataClickstream$orig_student_id == ID)) > 0) #ensure that the ID exists in the clickstream data
  {
    dataClickstreamFemale <- rbind(dataClickstreamFemale, subset(dataClickstream, dataClickstream$orig_student_id == ID))
  }
  else #save to a list of females with no clickstream data (never accessed the course)
  {
    noAccessFemales <- c(noAccessFemales, ID)
  }
  
  #update the user on the percentage complete and elapsed time after every n users processed
  #Sys.sleep(1 / 100)
  iCount <- iCount + 1
  #the following will run for every 1% complete
  if(iCount%%as.integer((length(femaleID_List))/100) == 0)
  {
    pb$tick()
    # #calculate the percentage and format it only 1 decimal place, save as a string
    # pctComplete <- sprintf("%.1f", iCount/(length(maleID_List))*100, digits = 2)
    # #print the status update
    # print(paste0("Males, ", pctComplete,"% complete", collapse = ""))
    # print(proc.time() - start)
  }
}
print(proc.time() - start)

##########Converting student_id to integers from 1 to total_number_registered###############

ConvertStudentID <- function(dataClickstreamTemp)
{
  #remove prior temp_student_id column
  dataClickstreamTemp <- dataClickstreamTemp[names(dataClickstreamTemp) %in% c("orig_student_id","module_number","time")]
  temp_student_id=c()
  counter=1
  for(id in sort(unique(dataClickstreamTemp$orig_student_id),decreasing=F))
  {
    #find each instance of user id in the clickstream, save to temp data frame
    temp_df=subset(dataClickstreamTemp,dataClickstreamTemp$orig_student_id==id)  
    #create list with length to match the number of rows in temp_df
    seqen=rep(counter,nrow(temp_df))
    #append each list for each loop through the IDs
    temp_student_id=c(temp_student_id,seqen)
    #counter contains the temp student ID that is being stored in temp_student_id
    counter=counter+1
  }
  #add a new column to Clickstream data fram with the temp_student_id
  dataClickstreamTemp<-cbind(dataClickstreamTemp,temp_student_id)
}

#call temp_student_id funcion for each gender.  This sequential column is needed for the clustering.R
dataClickstreamFemale <- ConvertStudentID(dataClickstreamFemale)
dataClickstreamMale <- ConvertStudentID(dataClickstreamMale)



############################################################################################



################ saving files and printing final printouts

#save gendered clickstream data
write.csv(dataClickstreamMale,"preprocessed_data_males.csv")
write.csv(dataClickstreamFemale,"preprocessed_data_females.csv")

#print and save gendered no access data
print(paste0("Percentage of females with no access data: ", sprintf("%.1f", length(noAccessFemales)/nrow(females) * 100, "%", collapse = "")))
print(paste0("Percentage of males with no access data: ", sprintf("%.1f", length(noAccessMales)/nrow(males) * 100, "%", collapse = "")))

names(noAccessMales) <- c("Count","orig_student_id")
names(noAccessFemales) <- c("Count","orig_student_id")

write.csv(noAccessMales,"noAccess_males_UIDs.csv")
write.csv(noAccessFemales,"noAccess_females_UIDs.csv")

#clear enviornment variables
#rm(list=ls())