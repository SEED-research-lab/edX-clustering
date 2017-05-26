#Author: Doipayan Roy
#Institution: Purdue University
#Project: EdX data pipeline for clustering analytics
#Last modified: 9th May, 2017
#Version: 1

#preprocessed_data.csv is the output of preprocessing.R
#preproceesed_data.csv is read as a dataFrame object
#Fields of interest in clickstream data are: student_id, module_number and time

#User data is read as the dataFrame object user_data from user_profile.sql file
#Fields of interest in user_data are: user_id and gender

#Use the gender information in user_profile to create subsets of clickstream data for male and female students

#Reading the preprocessed data file, converting to dataFrame object
preprocessed_data<-read.csv("preprocessed_data.csv")
preprocessed_data<-as.data.frame(preprocessed_data)
#Retaining only relevant clickstream columns
preprocessed_data<-preprocessed_data[names(preprocessed_data) %in% c("student_id","module_number","time")]

#Reading user_profile and retaining student_id and gender columns
user_data<-readr::read_tsv("user_profile.sql")
user_data<-user_data[names(user_data) %in% c("user_id","gender")]
#Reatining only students that have provided their gender information ('m' or 'f')
user_data<-subset(user_data,user_data$gender %in% c("m","f"))
#List of IDs for male students and female students
maleID<-unique(subset(user_data,user_data$gender=="m")$user_id)
femaleID<-unique(subset(user_data,user_data$gender=="f")$user_id)

#Seperate dataFrames for storing clickstream records of males and females
maleClickstreamData<-data.frame()
femaleClickstreamData<-data.frame()
#Seperate lists (males and females) for storing IDs that appear in user_profile but not in clickstream file
malesNoAccess<-c()
femalesNoAccess<-c()

print(paste("Extracting clickstream for male learners (",length(maleID)," learners )..."),quote=F)
counter=0
for(ID in maleID)
{
  #Extract the clickstream rows that have student_id==ID for male students
  temp_df=subset(preprocessed_data,preprocessed_data$student_id==ID)
  #Condition for checking if ID appears in clickstream file
  if(nrow(temp_df)>0)
  {
    #Store all clickstream events of student_id==ID in dataFrame
    maleClickstreamData<-rbind(maleClickstreamData,temp_df)
  }
  else
  {
    #Store ID if it does not appear in clickstream file
    malesNoAccess<-c(malesNoAccess,ID)
  }
  counter=counter+1
  if(counter%%100==0)
  {
    print(counter)
  }
}

print(paste("Done! Extracting clickstream for female learners (",length(femaleID)," learners )..."),quote=F)
counter=0
for(ID in femaleID)
{
  #Extract the clickstream rows that have student_id==ID for female students
  temp_df=subset(preprocessed_data,preprocessed_data$student_id==ID)
  #Condition for checking if ID appears in clickstream file
  if(nrow(temp_df)>0)
  {
    #Store all clickstream events of student_id==ID in dataFrame
    femaleClickstreamData<-rbind(femaleClickstreamData,temp_df)
  }
  else
  {
    #Store ID if it does not appear in clickstream file
    femalesNoAccess<-c(femalesNoAccess,ID)
  }
  counter=counter+1
  if(counter%%100==0)
  {
    print(counter)
  }
}

print("Done! Creating temporary user id for male learners...",quote=F)
#Order clickstream records for males in increasing order of student_id and generate temporary student ids starting from 1
maleClickstreamData<-maleClickstreamData[order(maleClickstreamData$student_id,decreasing=F),]
males_u_id=c()
counter=1
for(ID in sort(unique(maleClickstreamData$student_id),decreasing=F))
{
  temp_df=subset(maleClickstreamData,maleClickstreamData$student_id==ID)
  se=rep(counter,nrow(temp_df))
  males_u_id=c(males_u_id,se)
  counter=counter+1
}
#Add column containing temporary student ids to male clickstream dataFrame
maleClickstreamData<-cbind(maleClickstreamData,males_u_id)
#Retain only relevant columns
maleClickstreamData<-maleClickstreamData[names(maleClickstreamData) %in% c("student_id","module_number","time","males_u_id")]
#Rename columns appropriately
names(maleClickstreamData)[4]<-"temp_student_id"

print("Done! Creating temporary user id for female learners...",quote=F)
#Order clickstream records for females in increasing order of student_id and generate temporary student ids starting from 1
femaleClickstreamData<-femaleClickstreamData[order(femaleClickstreamData$student_id,decreasing=F),]
females_u_id=c()
counter=1
for(ID in sort(unique(femaleClickstreamData$student_id),decreasing=F))
{
  temp_df=subset(femaleClickstreamData,femaleClickstreamData$student_id==ID)
  se=rep(counter,nrow(temp_df))
  females_u_id=c(females_u_id,se)
  counter=counter+1
}
#Add column containing temporary student ids to female clickstream dataFrame
femaleClickstreamData<-cbind(femaleClickstreamData,females_u_id)
#Retain only relevant columns
femaleClickstreamData<-femaleClickstreamData[names(femaleClickstreamData) %in% c("student_id","module_number","time","females_u_id")]
#Rename columns appropriately
names(femaleClickstreamData)[4]<-"temp_student_id"

print("Done! Writing clickstream data to files...",quote=F)
#Write male and female clickstreams to files
write.csv(maleClickstreamData,"preprocessed_data_males.csv")
write.csv(femaleClickstreamData,"preprocessed_data_females.csv")
rm(list=ls())