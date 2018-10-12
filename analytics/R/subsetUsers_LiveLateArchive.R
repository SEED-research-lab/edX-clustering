## ===================================================== ##
# Title:        Subset users based on first interaction event dates ####
# Project:      edX data pipeline for course user clustering analytics
#               https://tzwilliams.github.io/edX-clustering/
#
# Copyright 2018 Krishna Madhavan
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
# Authors:      Krishna Madhavan, Kerrie Douglas, and Taylor Williams
# Affiliation:  Purdue University
#
# Description: Build separate user lists (and save to CSV) based on when each 
#               leaner first interacted with the course.  Lists for LIVE, LATE, and
#               ARCHIVED learners are created.  Date thresholds for when each group
#               begins can be modified within the script. 
#
#
# File input stack (R: required; O: optional):
#     R:  preprocessed_data.csv         (source: pipeline script 2_Preprocessing.R)
#
# File outputs:
#       CSV data: 3 files, each with the first recorded interaction for 
#                   each user in the course
#       
# Package dependencies: lubridate, plyr, beepr
#
# Changelog:
#     2018.04.22. Initial code
#
# Feature wish list:  (*: planned but not complete)
#                   *
## ===================================================== ##


## _Clean the environment ####
rm(list=ls()) 

######### Load required libraries ##########
require(lubridate)  #working with dates
require(plyr)       #data wrangling
require(beepr)     #user notifications




######### External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")
source("R/DisplayPercentComplete.R")


######### Internal functions ########## 
#Function: Check to see if the current working directory contains an expected file.  
# If not then prompt user to select the correct directory
WorkingDirectoryCheck <- function(expectedFilePath) {
  
  if(file.exists(file.path(getwd(), expectedFilePath))){
    #if file does exists in the current WD, exit the function returning TRUE
    return(TRUE)
  } else{
    #check for likely locations, set wd automatically if found and return TRUE
    if(file.exists(file.path(getwd(), "analytics", expectedFilePath))){
      setwd(file.path(getwd(), "analytics"))
      return(TRUE)
    } else if(file.exists(file.path(dirname(getwd()), expectedFilePath))){
      setwd(dirname(getwd()))   #set wd to parent directory of current wd
      return(TRUE)
    } else{
      #return FALSE if the file does not exist in the current WD (or other obvious locations)
      return(FALSE)
    }
  }
}


######### Setup ########## 

## _Check for correct working directory ########## 
#check for correct expected working directory, inform user if incorrect and stop running script
current.dir <- getwd()
expectedFile = "PipelineInitiator.R"
expectedFilePath = file.path(expectedFile)

if(!WorkingDirectoryCheck(expectedFilePath)){
  message("\nThe current working directory is NOT CORRECT.
          It is currently set to '", current.dir, "'
          Please set it to the directory containing the '", expectedFile, 
          "' file and rerun this script.\n")
  
  #stop running current script
  break
}

## _Get and create working directory variables ########## 
#set working directory to correct subdirectory
orig.dir <- getwd()
analyticsPath <- file.path(orig.dir, "analytics")
setwd(analyticsPath)







# end of script setup
## *************************************** #####
# beginning of script functionality


## Main ####
## 

#set date thresholds for course start, late enrollers, and archive learners
  #get dates from user 
  #TODO(sanitize/verify input)
  inputDate1_courseStart <- readline(prompt="Enter course START date (YYYY-MM-DD): ")
  # inputdate2_lateStart <- readline(prompt="Enter course START date (YYYY-MM-DD): ")
  inputdate3_courseEnd <- readline(prompt="Enter course END date (YYYY-MM-DD): ")

  #build dates
  date1_liveStart <- ymd_hms(paste0(inputDate1_courseStart, " 00:00:00 UTC")) #course start date
  date2_lateStart <- date1_liveStart+ddays(15) #late date is 15 days after the course start (as found in Douglas, K., Aggarwal, H., Williams, T. V., Fan, Y., & Bermel, P. (2018). Comparison of live, late and archived mode learner behavior in an advanced engineering MOOC. In Frontiers in Education Conference. San Jose, CA, USA.)
  date3_archiveStart <- ymd_hms(paste0(inputdate3_courseEnd, " 00:00:00 UTC"))+ddays(1) #day after course end date

  
  
#read in the clickstream data
  #check for preprocessed datafile existence
  preprocessedDataFilePath <- FileExistCheck_workingDir(subDir = "2_PreprocessingOutput",
                                             filename = "preprocessed_data.csv")
  #exit script if file not found, otherwise continue
  ifelse(preprocessedDataFilePath == FALSE, yes = return(), no = "")
  #read in data from the appropriate learner (sub)set
  data0_complete <- readr::read_csv(preprocessedDataFilePath)
  
  
  # beepr::beep(sound = 10)   #notify user to provide input
  # message("Please select the appropriate 'preprocessed_data.csv' file 
  #         (found in the '2_PreprocessingOutput' folder).")
  # filename <- file.choose()
  # data0_complete <- readr::read_csv(filename)


#save list of unique UIDs in data
UIDs_all <- unique(data0_complete$student_id)


#subset data into the enrollment groups (based on first interaction event)
data0_preCourseUsers <- data.frame()
data1_liveUsers <- data.frame()
data2_lateUsers <- data.frame()
data3_archiveUsers <- data.frame()

for (i in UIDs_all) {
  #find first recorded event for each UID
  curUserData <- subset(x = data0_complete, 
                        subset = data0_complete$student_id == i)
  #arrange events in ascending order and only keep first event
  curUserData <- arrange(curUserData, time)[1,]
  
  #save user to proper enrollment group based on the threshold dates
  if(curUserData$time[1] >= date3_archiveStart){
    #save user in the archive group if first interaction is after course closes
    data1_liveUsers <- rbind(data1_liveUsers, curUserData)
  }else if(curUserData$time[1] >= date2_lateStart){
    #save user in the late group if first interaction is after the late date
    data2_lateUsers <- rbind(data2_lateUsers, curUserData)
  }else if(curUserData$time[1] >= date1_liveStart){
    #save user in the live group if first interaction is after the course start
    data3_archiveUsers <- rbind(data3_archiveUsers, curUserData)
  }else{
    #save user in the list of users who aren't students if first 
    #   recorded event predated the course start date
    data0_preCourseUsers <- rbind(data0_preCourseUsers, curUserData)
  }
  
  #| print completion progress to console   ####
    #durring first iteration, create progress status variables for main processing loop
    if(i==UIDs_all[1])
    {
      iCount <- 0 #loop counter for completion updates
      pct <- 0  #percentage complete tracker
    }
  
    #print function
    updateVars <- DisplayPercentComplete(dataFrame = UIDs_all, iCount, pct, displayText = "Sorting learners. Working through ID list: ")
  
    #update status variables (for next iteration)
    iCount <- updateVars$iCount
    pct <- updateVars$pct
  
    #print update
    cat(updateVars$toPrint)
}


######### Write data to files ###############
#call function to check for the existance of the subdirectory; create it if it doesn't exist
subDirPath <- DirCheckCreate(subDir = "2_PreprocessingOutput")

#write a CSV file for the next step in processing. 
cat("\nSaving CSV file.")
write.csv(file = file.path(subDirPath, "userList0_pre-courseUsers.csv", fsep = "/"), 
          x = data0_preCourseUsers)
write.csv(file = file.path(subDirPath, "userList1_live.csv", fsep = "/"), 
          x = data1_liveUsers)
write.csv(file = file.path(subDirPath, "userList2_late.csv", fsep = "/"), 
          x = data2_lateUsers)
write.csv(file = file.path(subDirPath, "userList3_archive.csv", fsep = "/"), 
          x = data3_archiveUsers)



beepr::beep(sound = 10)   #notify user of script completion
