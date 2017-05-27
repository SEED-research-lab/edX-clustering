## ===================================================== ##
# Title:        Gendered subset extraction, optional additional preprocessing #####
#
#
# Author(s):    Taylor Williams, Doipayan Roy
# Institution:  Purdue University
# 
# Project:      EdX data pipeline for clustering analytics
# 
# Description:  Fields of interest in clickstream data are: student_id, module_number and time
#				User data is read as the dataFrame object user_data from user_profile.sql file
#				Fields of interest in user_data are: user_id and gender
#				Use the gender information in user_profile to create subsets of clickstream data for male and female students
# 
# Package dependancies: readr, [beepr]
#
# Changelog:
#     2017.04.13    initial creation
#     2017.05.05    adding in subdirectory functions  
#     2017.05.08.   Code cleaning, header update
#                   Audio notification for user input and script completion
#     2017.05.09.   Added filename check of user provided files (with override option)
#     2017.05.11.   Extracted possible functions to external files
#     2017.05.18.   Removed dependancies on beepr (for compatibility with RStudio server)
#     2017.05.25.   Integrated Doipayan's madifications up through 2017.05.09 
#                   Added timer to track script execution time
#                   Commented dependancies on "progress" package. 
#                   Added in more basic (package independant) progress indicator
## ===================================================== ##



######### Clean the environment ########## 
rm(list=ls())   


######### Internal functions ########## 
#Function: Interactively select working directory (OS independant, but not available for RStudio Server)
InteractiveSetWD <- function() {
  cat("IMPORTANT: Select your working directory. If a folder choice window doesn't appear, look for it behind your current window.")
  setwd('~')
  #tcltk package provides an OS independant way to select a folder
  library(tcltk)
  #setting the arguments (see package documentation for details)
  .tcl.objv  <- .Tcl.args.objv('-initialdir "~" -title "Choose a working directory"')
  # open a folder selection window (defaults to 'My Documents').  Sometimes this opens in the background.
  dir <- tclvalue(tkchooseDirectory()) 
  setwd(dir)
  
  return() 
}


#Function: Check to see if the current working directory contains an expected file.  
# If not then prompt user to select the correct directory
WorkingDirectoryCheck <- function(expectedFile) {
  #set directory variables
  curDir <- getwd()
  
  
  if(file.exists(file.path(curDir, expectedFile))){
    #if file does exists in the current WD, exit the funtion returning TRUE
    return(TRUE)
  } else{
    #if the file does not exist in the current WD, return FALSE
    return(FALSE)
  }
}


# end of functions
## *************************************** #####
# begin script setup


######### Check for correct working directory ########## 
#check the current working direcotry, inform user if incorrect and stop running script
if(!WorkingDirectoryCheck(expectedFile = "1_extractModules.R")){
  cat("The current working directory is NOT CORRECT.  
      Please set it to the directory containing the R scripts before reruning script.\n")
  
  #have user set the working directory
  # beepr::beep(sound = 10)   #notify user to provide input
  # InteractiveSetWD()
  
  break
}


######### External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")



# end of script setup
## *************************************** #####
# beginning of script functionality


#start a timer to track how long the script takes to execute
scriptStart <-  proc.time() #save the time (to compute ellapsed time of loop)




######### Reading files, converting to dataframe object, identify users with gender data #####

#read in the preprocessed clickstream data
preprocessedDataFilePath <- FileExistCheck(subDir = "2_PreprocessingOutput", filename = "preprocessed_data.csv")
#exit script if file not found, otherwise continue
ifelse(test = (preprocessedDataFilePath == FALSE), yes = return(), no = "")
dataClickstream <- readr::read_csv(preprocessedDataFilePath)

#User selection of the USER PROFILE data file to process (with sanatized user input)
repeat{
  cat("\n*****Select the SQL USER PROFILE data file.*****\n  (It should end with 'auth_userprofile-prod-analytics.sql')")
  #beepr::beep(sound = 10)   #notify user to provide input
  filenameUserProfile <- file.choose()
  
  filenameCheckResult <- ExpectedFileCheck(selectedFilename = filenameUserProfile, 
                                           expectedFileEnding = "auth_userprofile-prod-analytics.sql")
  
  if(filenameCheckResult == "matched"){
    #filename matched expected string, continue with script
    break
  }else if(filenameCheckResult == "overridden"){
    #continue script with the previously selected file
    break
  }else if(filenameCheckResult == "reselect"){
    #repeat file selection loop
  }
}

#Retaining only relevant clickstream columns
dataClickstream <- dataClickstream[names(dataClickstream) %in% c("student_id","module_number","time")]

#read in the user profile data 
dataUserProfile <- readr::read_tsv(filenameUserProfile)
dataUserProfile <- dataUserProfile[names(dataUserProfile) %in% c("id", "user_id", "gender", "mailing_address", "year_of_birth", "level_of_education", "country")]

#find the gendered subsets
maleSubset <- subset(dataUserProfile, dataUserProfile$gender == "m")
femaleSubset <- subset(dataUserProfile, dataUserProfile$gender == "f")



######### Seperate the Clickstream data into gendered subsets ###############

#create empty dataframes where we will save the gendered clickstream data
dataClickstreamMale <- data.frame()
dataClickstreamFemale <- data.frame()

#create 2 lists for each gender 
#(1) all known unique student IDs for each gender and 
#(2) empty list to save users without clickstream data (never accessed the course)
maleID_List <- unique(maleSubset$user_id)
noAccessMales <- c()
femaleID_List <- unique(femaleSubset$user_id)
noAccessFemales <- c()

#create progress status variables for male processing loop
iCount <- 0 #loop counter for completion updates
pct <- 0  #percentage complete tracker
start <-  proc.time() #save the time (to compute ellapsed time of loop)
# #initialize the consule progress bar
# library(progress)
# pb <- progress_bar$new(
#   format = "  processing [:bar] :percent eta: :eta",
#   total = 100, clear = FALSE, width= 120)

#build up a dataframe with all rows of each male user's clickstream data
cat("\nExtracting clickstream for male learners (",length(maleID_List)," learners )...\n")

# pb$tick(0)  #start the progress bar
for(ID in maleID_List)
{
  if(nrow(subset(dataClickstream, dataClickstream$student_id == ID)) > 0){ #ensure that the ID exists in the clickstream data
    dataClickstreamMale <- rbind(dataClickstreamMale, subset(dataClickstream, dataClickstream$student_id == ID))
  }
  else{ #save to a list of males with no clickstream data (never accessed the course)
    noAccessMales <- c(noAccessMales, ID)
  }
  
  #update the progress for every 1% complete
  iCount <- iCount + 1  
  if(iCount%%as.integer((length(maleID_List))/100) == 0){
    pct <- pct + 1
    cat("\r", pct, "% complete", sep = "")
    # pb$tick()
  }
}
cat("\nDone!\n")
#print the amount of time the previous loop required
print(proc.time() - start)


#reset progress status variables for female processing loop
iCount <- 0 #loop counter for completion updates
pct <- 0  #percentage complete tracker
start <-  proc.time() #save the time (to compute ellapsed time of loop)
# pb <- progress_bar$new(
#   format = "  processing [:bar] :percent eta: :eta",
#   total = 100, clear = FALSE, width= 120)

#build up a dataframe with all rows of each female user's clickstream data
cat("\nExtracting clickstream for female learners (",length(femaleID_List)," learners )...\n\n")

# pb$tick(0)
for(ID in femaleID_List)
{
  if(nrow(subset(dataClickstream, dataClickstream$student_id == ID)) > 0){ #ensure that the ID exists in the clickstream data
    dataClickstreamFemale <- rbind(dataClickstreamFemale, subset(dataClickstream, dataClickstream$student_id == ID))
  }
  else{ #save to a list of females with no clickstream data (never accessed the course)
    noAccessFemales <- c(noAccessFemales, ID)
  }
  
  #update the progress for every 1% complete
  iCount <- iCount + 1  
  if(iCount%%as.integer((length(femaleID_List))/100) == 0){
    pct <- pct + 1
    cat("\r", pct, "% complete", sep = "")
    # pb$tick()
  }
}
cat("\nDone!\n")
#print the amount of time the previous loop required
print(proc.time() - start)


######### Converting student_id to sequential integers from 1 to total_number_registered ###############

ConvertStudentID <- function(dataClickstreamTemp)
{
  #Order clickstream records for males in increasing order of student_id and generate temporary student ids starting from 1
  dataClickstreamTemp <- dataClickstreamTemp[order(dataClickstreamTemp$student_id, decreasing=F),]
  
  #remove prior temp_student_id column
  dataClickstreamTemp <- dataClickstreamTemp[names(dataClickstreamTemp) %in% c("student_id","module_number","time")]
  
  temp_student_id <- c()
  counter <- 1
  for(id in sort(unique(dataClickstreamTemp$student_id),decreasing=F))
  {
    #find each instance of user id in the clickstream, save to temp data frame
    temp_df <- subset(dataClickstreamTemp,dataClickstreamTemp$student_id==id)  
    #create list with length to match the number of rows in temp_df
    seqen <- rep(counter,nrow(temp_df))
    #append each list for each loop through the IDs
    temp_student_id <- c(temp_student_id,seqen)
    #counter contains the temp student ID that is being stored in temp_student_id
    counter <- counter+1
  }
  #add a new column to Clickstream data frame with the temp_student_id
  dataClickstreamTemp<-cbind(dataClickstreamTemp,temp_student_id)
  
  return(dataClickstreamTemp)
}

#call temp_student_id funcion for each gender.  This sequential id column is needed for 3_Clustering.R
cat("\nCreating temporary user id for male learners...")
dataClickstreamFemale <- ConvertStudentID(dataClickstreamFemale)

cat("\nCreating temporary user id for female learners...")
dataClickstreamMale   <- ConvertStudentID(dataClickstreamMale)





######### saving files and printing final printouts  #####

######### Write data to files ###############
subDirPath <- DirCheckCreate(subDir = "2_PreprocessingOutput")

#save gendered clickstream data 
cat("\nSaving CSV files.\n\n")
write.csv(x = dataClickstreamFemale, file = file.path(subDirPath, "preprocessed_data_females.csv", fsep = "/"))
write.csv(x = dataClickstreamMale,   file = file.path(subDirPath, "preprocessed_data_males.csv",   fsep = "/"))


#print and save gendered no access data
print(paste0("Percentage of females with no access data: ", sprintf("%.1f", length(noAccessFemales)/nrow(femaleSubset) * 100, "%", collapse = "")))
print(paste0("Percentage of males with no access data: ", sprintf("%.1f", length(noAccessMales)/nrow(maleSubset) * 100, "%", collapse = "")))

names(noAccessFemales) <- c("Count","student_id")
names(noAccessMales) <- c("Count","student_id")

write.csv(x = noAccessFemales, file = file.path(subDirPath, "noAccess_females_UIDs.csv", fsep = "/"))
write.csv(x = noAccessMales,   file = file.path(subDirPath, "noAccess_males_UIDs.csv",   fsep = "/"))



######### Notify user and Clear the environment  #############
# beepr::beep(sound = 10)   #notify user script is complete
# Sys.sleep(time = 0.1)     #pause 1/10 sec
# beepr::beep(sound = 10)
# Sys.sleep(time = 0.1)
# beepr::beep(sound = 10)

#print the amount of time the script required
cat("\n\n\nScript processing time details (in sec):\n")
print(proc.time() - scriptStart)

rm(list=ls())   #Clear environment variables

