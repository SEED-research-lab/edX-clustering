## ===================================================== ##
# Title:        Gendered subset extraction, optional additional preprocessing #####
# Project:      edX data pipeline for course user clustering analytics
#               https://tzwilliams.github.io/edX-clustering/
# 
# Copyright 2017 Krishna Madhavan
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#
#
#
# Authors:      Krishna Madhavan, Kerrie Douglas, Doipayan Roy, and Taylor Williams
# Affiliation:  Purdue University
#  
# Description:  Fields of interest in clickstream data are: student_id, module_number, and time
#				        User data is read as the dataFrame object user_data from user_profile.sql file
#			        	Fields of interest in user_data are: user_id and gender
#			        	Use the gender information in user_profile to create subsets of clickstream data 
#			        	  for male and female students
# 
# File input stack: 
#       {org}-{course}-{date}-auth_userprofile-prod-analytics.sql (source: edX)
#       preprocessed_data.csv (source: pipeline script 2_Preprocessing.R)
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
#     2017.05.18.   Removed dependencies on beepr (for compatibility with RStudio server)
#     2017.05.25.   Integrated Doipayan's modifications up through 2017.05.09 
#                   Added timer to track script execution time
#                   Commented dependencies on "progress" package. 
#                   Added in more basic (package independent) progress indicator
#     2017.07.14.   Minor code updates; added copyright information
#     2017.08.06.   Update to comments; spell check
#     2018.10.12.   Fixed bug that failed on inporting the user profile data 
#                     if bad characters were included in file
#                   Update to file selection (semi-automated)
## ===================================================== ##



######### Clean the environment ########## 
varsToRetain <- c("varsToRetain", "data_moduleAccess", "data_courseStructure", 
                  "dataUserProfile", "filenamePrefix", "dataFolderPath", "courseName")
rm(list=setdiff(ls(), varsToRetain))

######### Internal functions ########## 
#Function: Interactively select working directory (OS independent, but not available for RStudio Server)
InteractiveSetWD <- function() {
  cat("IMPORTANT: Select your working directory. If a folder choice window doesn't appear, look for it behind your current window.")
  setwd('~')
  #tcltk package provides an OS independent way to select a folder
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
  
  if(file.exists(file.path(getwd(), expectedFile))){
    #if file does exists in the current WD, exit the function returning TRUE
    return(TRUE)
  } else{
    #check for likely locations, set wd automatically if found and return TRUE
    if(file.exists(file.path(getwd(), "analytics", expectedFile))){
      setwd(file.path(getwd(), "analytics"))
      return(TRUE)
    } else if(file.exists(file.path(dirname(getwd()), expectedFile))){
      setwd(dirname(getwd()))   #set wd to parent directory of current wd
      return(TRUE)
    } else{
      #return FALSE if the file does not exist in the current WD (or other obvious locations)
      return(FALSE)
    }
  }
}


# end of functions
## *************************************** #####
# begin script setup

require(readr)

######### Check for correct working directory ########## 
#check for correct expected working directory, inform user if incorrect and stop running script
current.dir <- getwd()
thisFile = "2b_genderedSubsets.R"
expectedFile = file.path(thisFile)

if(!WorkingDirectoryCheck(expectedFile)){
  message("\nThe current working directory is NOT CORRECT.
          It is currently set to '", current.dir, "'
          Please set it to the directory containing the '", thisFile, 
          "' file and rerun this script.\n")
  
  #stop running current script
  break
}




######### Load required libraries ##########
require("data.table")
require("tcltk")

######### External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")



# end of script setup
## *************************************** #####
# beginning of script functionality


#start a timer to track how long the script takes to execute
scriptStart <-  proc.time() #save the time (to compute elapsed time of script)

## Check for pre-defined starting directory and course prefix ####
if(!exists("filenamePrefix")) filenamePrefix <- NULL
if(!exists("dataFolderPath")) dataFolderPath <- NULL
if(!exists("courseName")) courseName <- NULL


######### Reading files, converting to dataframe object, identify users with gender data #####

#read in the preprocessed clickstream data
if(exists("data_moduleAccess")){
  data_preprocessed <- data_moduleAccess
  rm(data_moduleAccess)
}else{
  DirCheckCreate(subDir = courseName)
  subDirPath <- DirCheckCreate(subDir = file.path(courseName,"2_PreprocessingOutput"))
  preprocessedDataFilePath <- FileExistCheck_workingDir(subDir = subDirPath, 
                                                        fullPathPassed = T,
                                                        filename = "preprocessed_data.csv")
  #exit script if file not found, otherwise continue
  ifelse(test = (preprocessedDataFilePath == FALSE), yes = return(), no = "")
  data_preprocessed <- readr::read_csv(preprocessedDataFilePath)
}

#Locate the USER PROFILE data file to process (with sanatized user input)
if(!exists("dataUserProfile")){
  filenameUserProfile <- 
    SelectFile(prompt = "*****Select the SQL USER PROFILE data file.*****  (It should end with 'auth_userprofile-prod-analytics.sql')", 
               defaultFilename = "auth_userprofile-prod-analytics.sql",
               filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix), 
                                       yes = filenamePrefix, no = ""), 
               fileTypeMatrix = matrix(c("SQL", ".sql"), 1, 2, byrow = TRUE),
               dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath), 
                                       yes = dataFolderPath, no = ""))
  
  #import data files
  dataUserProfile <- fread(filenameUserProfile, 
                           select = c("id", "user_id", "gender",
                                      "year_of_birth", "level_of_education", "country"),
                           quote = "")
  
}


#Retaining only relevant clickstream columns
data_preprocessed <- data_preprocessed[names(data_preprocessed) %in% 
                                         c("student_id","module_number","time")]

#read in the user profile data 
# dataUserProfile <- dataUserProfile[names(dataUserProfile) %in% c("id", "user_id", "gender", "mailing_address", "year_of_birth", "level_of_education", "country")]

#find the gendered subsets
maleSubset <- subset(dataUserProfile, dataUserProfile$gender == "m")
femaleSubset <- subset(dataUserProfile, dataUserProfile$gender == "f")



######### Separate the Clickstream data into gendered subsets ###############

#create empty dataframes where we will save the gendered clickstream data
data_preprocessedMale <- data.frame()
data_preprocessedFemale <- data.frame()

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
start <-  proc.time() #save the time (to compute elapsed time of loop)
# #initialize the console progress bar
# library(progress)
# pb <- progress_bar$new(
#   format = "  processing [:bar] :percent eta: :eta",
#   total = 100, clear = FALSE, width= 120)

#build up a dataframe with all rows of each male user's clickstream data
cat("\nExtracting clickstream for male learners (",length(maleID_List),"learners )...\n")

# pb$tick(0)  #start the progress bar
for(ID in maleID_List)
{
  #ensure that the ID exists in the clickstream data
  if(nrow(subset(data_preprocessed, data_preprocessed$student_id == ID)) > 0){ 
    data_preprocessedMale <- rbind(data_preprocessedMale, 
                                   subset(data_preprocessed, data_preprocessed$student_id == ID))
  }
  else{ #save to a list of males with no clickstream data (never accessed the course)
    noAccessMales <- c(noAccessMales, ID)
  }
  
  #update the progress for every 1% complete
  iCount <- iCount + 1  
  if(iCount%%as.integer((length(maleID_List))/100) == 0){
    pct <- pct + 1
    cat("\rMales ", pct, "% complete", sep = "")
    # pb$tick()
  }
}
cat("\nDone!\n")
#print the amount of time the previous loop required
cat("\n\n\nMale learner subset processing time details (in sec):\n")
print(proc.time() - start)


#reset progress status variables for female processing loop
iCount <- 0 #loop counter for completion updates
pct <- 0  #percentage complete tracker
start <-  proc.time() #save the time (to compute elapsed time of loop)
# pb <- progress_bar$new(
#   format = "  processing [:bar] :percent eta: :eta",
#   total = 100, clear = FALSE, width= 120)

#build up a dataframe with all rows of each female user's clickstream data
cat("\nExtracting clickstream for female learners (",length(femaleID_List),"learners )...\n")

# pb$tick(0)
for(ID in femaleID_List)
{
  #ensure that the ID exists in the clickstream data
  if(nrow(subset(data_preprocessed, data_preprocessed$student_id == ID)) > 0){ 
    data_preprocessedFemale <- rbind(data_preprocessedFemale, 
                                     subset(data_preprocessed, data_preprocessed$student_id == ID))
  }
  else{ #save to a list of females with no clickstream data (never accessed the course)
    noAccessFemales <- c(noAccessFemales, ID)
  }
  
  #update the progress for every 1% complete
  iCount <- iCount + 1  
  if(iCount%%as.integer((length(femaleID_List))/100) == 0){
    pct <- pct + 1
    cat("\rFemales ", pct, "% complete", sep = "")
    # pb$tick()
  }
}
cat("\nDone!\n")
#print the amount of time the previous loop required
cat("\n\n\nFemale learner subset processing time details (in sec):\n")
print(proc.time() - start)


######### Converting student_id to sequential integers from 1 to total_number_registered ###############

ConvertStudentID <- function(data_preprocessedTemp)
{
  #Order clickstream records for males in increasing order of student_id and 
  # generate temporary student ids starting from 1
  data_preprocessedTemp <- data_preprocessedTemp[order(data_preprocessedTemp$student_id, 
                                                   decreasing=F),]
  
  #remove prior temp_student_id column
  data_preprocessedTemp <- data_preprocessedTemp[names(data_preprocessedTemp) %in% 
                                               c("student_id","module_number","time")]
  
  temp_student_id <- c()
  counter <- 1
  for(id in sort(unique(data_preprocessedTemp$student_id),decreasing=F))
  {
    #find each instance of user id in the clickstream, save to temp data frame
    temp_df <- subset(data_preprocessedTemp,data_preprocessedTemp$student_id==id)  
    #create list with length to match the number of rows in temp_df
    seqen <- rep(counter,nrow(temp_df))
    #append each list for each loop through the IDs
    temp_student_id <- c(temp_student_id,seqen)
    #counter contains the temp student ID that is being stored in temp_student_id
    counter <- counter+1
  }
  #add a new column to Clickstream data frame with the temp_student_id
  data_preprocessedTemp<-cbind(data_preprocessedTemp,temp_student_id)
  
  return(data_preprocessedTemp)
}

#call temp_student_id funcion for each gender.  This sequential id column is needed 
# for 3_Clustering.R
cat("\nCreating temporary user id for male learners...")
data_preprocessedFemale <- ConvertStudentID(data_preprocessedFemale)

cat("\nCreating temporary user id for female learners...")
data_preprocessedMale   <- ConvertStudentID(data_preprocessedMale)





######### saving files and printing final printouts  #####

######### Write data to files ###############
DirCheckCreate(subDir = courseName)
subDirPath <- DirCheckCreate(subDir = file.path(courseName, "1_extractModulesOutput"))

#save gendered clickstream data 
cat("\nSaving CSV files.\n\n")
write.csv(x = data_preprocessedFemale, file = file.path(subDirPath, 
                                                      "preprocessed_data_females.csv", 
                                                      fsep = "/"))
write.csv(x = data_preprocessedMale,   file = file.path(subDirPath, 
                                                      "preprocessed_data_males.csv",   
                                                      fsep = "/"))


#print and save gendered no access data
noAccessFemalesPct <- length(noAccessFemales)/nrow(femaleSubset) * 100
noAccessFemalesPct <- sprintf("%.1f", noAccessFemalesPct, "%", collapse = "")
noAccessMalesPct <- length(noAccessMales)/nrow(maleSubset) * 100
noAccessMalesPct <- sprintf("%.1f", noAccessMalesPct, "%", collapse = "")

cat(paste0("\nPercentage of registered female learners who never accessed the course: ", 
             noAccessFemalesPct, "%"))
cat(paste0("\nPercentage of registered male learners who never accessed the course: ", 
             noAccessMalesPct, "%"))

names(noAccessFemales) <- c("Count","student_id")
names(noAccessMales) <- c("Count","student_id")

write.csv(x = noAccessFemales, file = file.path(subDirPath, 
                                                paste0("noAccess_females_UIDs_", 
                                                       noAccessFemalesPct, " pct of female regs.csv"),
                                                fsep = "/"))
write.csv(x = noAccessMales,   file = file.path(subDirPath, 
                                                paste0("noAccess_males_UIDs_", 
                                                       noAccessMalesPct, " pct of male regs.csv"),
                                                fsep = "/"))



######### Notify user and Clear the environment  #############
#print the amount of time the script required
cat("\n\n\nScript (2b_genderedSubsets.R) processing time details (in sec):\n")
print(proc.time() - scriptStart)

#Clear environment variables
rm(list=setdiff(ls(), varsToRetain))
