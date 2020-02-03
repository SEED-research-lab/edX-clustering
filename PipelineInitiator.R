## ===================================================== ##
# Title:        Pipeline Initiator ####
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
# Description:  Initiates the clustering pipeline, sequentially calling the pipeline scripts
#               and providing continuity between them.
#               
# File input stack: 
#       {org}-{course}-{date}-course_structure-prod-analytics.json        (source: edX)
#       {org}-{course}-{date}-courseware_studentmodule-prod-analytics.sql (source: edX)
#       {org}-{course}-{date}-auth_userprofile-prod-analytics.sql         (source: edX)
#       
# Package dependencies: cluster, e1071, tidyverse, readxl, jsonlite, readr, tcltk, beepr, data.table
#
# Changelog:
#     2017.07.14.   Initial version
#     2017.08.06.   Update to comments; spell check
#     2018.11.04.   Automation updates
#     
#
# Feature wish list:  (*: planned but not complete)
#                   * remove unnecessary packages from the PackRat library
#                   * allow user to give a timeframe of events to include
## ===================================================== ##




## _Clean the environment ####
rm(list=ls()) 


######### Internal functions ########## 
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


######### Setup ########## 

## _Check for correct working directory ########## 
#check for correct expected working directory, inform user if incorrect and stop running script
current.dir <- getwd()
thisFile = "PipelineInitiator.R"
expectedFile = file.path(thisFile)

if(!WorkingDirectoryCheck(expectedFile)){
  message("\nThe current working directory is NOT CORRECT.
          It is currently set to '", current.dir, "'
          Please set it to the directory containing the '", thisFile, 
          "' file and rerun this script.\n")
  
  #stop running current script
  break
}

## _Get and create working directory variables ########## 
#set working directory to correct subdirectory
orig.dir <- getwd()
analyticsPath <- file.path(orig.dir, "analytics")
setwd(analyticsPath)

## _Load required libraries ##########
require("stringr")

## _External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")


######### Main ########## 

#start a timer to track how long the pipeline takes to execute
start <-  proc.time() #save the time (to report the pipeline's running time at the end of the script)

## Check for pre-defined starting directory and course prefix ####
if(!exists("filenamePrefix")) filenamePrefix <- NULL
if(!exists("dataFolderPath")) dataFolderPath <- NULL
if(!exists("courseName")) courseName <- NULL


## get data file locations from user ####
  #get JSON
  #Locate the JSON course structure data file to process (with sanitized user input)
  filenameJSON <- 
    SelectFile(prompt = "*****Select the JSON COURSE STRUCTURE file.*****  (It should end with 'course_structure-prod-analytics.json')", 
               defaultFilename = "course_structure-prod-analytics.json", 
               filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix), 
                                       yes = filenamePrefix, no = ""), 
               fileTypeMatrix = matrix(c("JSON", ".json"), 1, 2, byrow = TRUE),
               dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath), 
                                       yes = dataFolderPath, no = ""))

  
  #extract course prefix; extract the folder path
  filenamePrefix <- str_extract(string = basename(filenameJSON), 
                                pattern = ".*(?=course_structure-prod-analytics.json$)")
  courseName <- str_extract(string = filenamePrefix, 
                            pattern = "^[^-]*-[^-]*(?=-)")
  dataFolderPath <- dirname(filenameJSON)
  
  
  #try to automatically get the other files (ask for them if fails)
  #Locate the clickstream data file to process (with sanitized user input)
  filename_moduleAccess <- 
    SelectFile(prompt = "*****Select the SQL CLICKSTREAM data file.*****  (It should end with 'courseware_studentmodule-prod-analytics.sql')", 
               defaultFilename = "courseware_studentmodule-prod-analytics.sql",
               filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix), 
                                       yes = filenamePrefix, no = ""), 
               fileTypeMatrix = matrix(c("SQL", ".sql"), 1, 2, byrow = TRUE),
               dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath), 
                                       yes = dataFolderPath, no = ""))
  
  
  #Locate the USER PROFILE data file to process (with sanatized user input)
  filenameUserProfile <- 
    SelectFile(prompt = "*****Select the SQL USER PROFILE data file.*****  (It should end with 'auth_userprofile-prod-analytics.sql')", 
               defaultFilename = "auth_userprofile-prod-analytics.sql",
               filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix), 
                                       yes = filenamePrefix, no = ""), 
               fileTypeMatrix = matrix(c("SQL", ".sql"), 1, 2, byrow = TRUE),
               dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath), 
                                       yes = dataFolderPath, no = ""))
  
  
  
  #import data files ####
  data_courseStructure <- jsonlite::fromJSON(filenameJSON)
  data_moduleAccess <- readr::read_tsv(filename_moduleAccess)
  dataUserProfile <- 
    data.table::fread(filenameUserProfile, 
                      select = c("id", "user_id", "gender",
                                 "year_of_birth", "level_of_education", "country"),
                      quote = "")
  

DirCheckCreate(subDir = courseName)


#ask user if they want to pre-specify the clustering technique and number of clusters
repeat{
  beepr::beep(sound = 10)   #notify user to provide input
  pre_specify <- readline(prompt="Would you like to PRE-SPECIFY the clustering technique, population, and number of clusters? (Y/N): 
                          (CAUTION: not recommended if ideal number of clusters for data is not yet known)");

    if(pre_specify == "y" || pre_specify == "Y"){  
      pre_specify <- TRUE
      
      ######### User providing dataset details #####
      beepr::beep(sound = 10)   #notify user to provide input
      cat("\nEnter a description of this datasest (to be included on graphs).
    (suggested format: [Data source, e.g., edX], [Course number, e.g., nano515x], [Data date, e.g., Data from 2016.11.18])")
      dataSetDescription <- readline(prompt="Description: ");
      
      
      
      ## get clustering technique
      repeat{
        clusterTypeSelection <- readline(prompt="\nEnter '1' or {nothing} for K-means clustering, 
      '2' for c-means (fuzzy) clustering: ");

        #exit loop and continue script if input valid
        if(clusterTypeSelection == 1  | 
           clusterTypeSelection == "" |   
           clusterTypeSelection == 2  ){
          break
        }
        beepr::beep(sound = 10)   #notify user to provide input
      }   #repeat if none of the conditions were met (i.e., user input was invalid)
      
      
      ##Get user input for number of clusters
      repeat{
        K <- readline("\nEnter the desired number of clusters (maximum 10): ");
        K <- as.integer(K);
        
        ifelse(!is.na(K) & (K > 0) & (K <= 10), 
               yes = break, 
               no = print("Please enter a valid number.", quote=FALSE))
      }
      
      
      
      ## get population
      repeat{
        userSubsetSelection <- readline(prompt="\n Who to cluster?: 
Enter '1' or {nothing} for all learners,  
      '2' or 'f' for female learners,
      '3' or 'm' for male learners,
      '4' live learners,
      '5' late learners,
      '6' archive learners,
      '7' or 'c' custom ID list");

        
        # set if the user subgroups should be calculated based on selection
        if(userSubsetSelection == 4 | 
           userSubsetSelection == 5 |
           userSubsetSelection == 6){
          inputLLA <- '1' #set to find live, late, and archive groups,  
          
        }else{
          inputLLA <- '2' #don't find live, late, and archive groups,  
        }

        #exit loop and continue script if input valid
        if(userSubsetSelection == 1 | userSubsetSelection == "" |
           userSubsetSelection == 2 | userSubsetSelection == 'f' | userSubsetSelection == 'F' |
           userSubsetSelection == 3 | userSubsetSelection == 'm' | userSubsetSelection == 'M' |
           userSubsetSelection == 4 | 
           userSubsetSelection == 5 |
           userSubsetSelection == 6 | 
           userSubsetSelection == 7 | userSubsetSelection == 'c'){
          break
        }
        beepr::beep(sound = 10)   #notify user to provide input
      }   #repeat if none of the conditions were met (i.e., user input was invalid)
      
      #save selections to file to be recalled in 3_Clustering.R
      save(list = c("dataSetDescription", "clusterTypeSelection", "K", "userSubsetSelection", "inputLLA"), 
           file = "initiator_userPreselectionValues.RData")
      
      break
      
    }else if(pre_specify == "n" || pre_specify == "N"){  
    pre_specify <- FALSE
    
    dataSetDescription   <- NULL
    clusterTypeSelection <- NULL
    K                    <- NULL
    userSubsetSelection  <- NULL
    inputLLA             <- NULL
    
    save(list = c("dataSetDescription", "clusterTypeSelection", "K", "userSubsetSelection", "inputLLA"), 
         file = "initiator_userPreselectionValues.RData")
    
    break
  }
  else{
    message("Please enter either 'Y' or 'N'.\n")
  }
  
} # repeat if invalid input provided





#source (run) the pipeline script files in sequence   ####
source("1_extractModules.R")
source("2_Preprocessing.R")
source("2b_genderedSubsets.R")
source("3_Clustering.R")
message("\n**** Cluster graph created! ****\n")


#ask user if additional cluster charts are desired, if so run '3_Clustering.R' again
repeat{
  beepr::beep(sound = 10)   #notify user to provide input
  continueClustering <- readline(prompt="Would you like to create another cluster graph from this data? (Y/N): ");
  
  # delete the pre-selection data file
  if (file.exists("initiator_userPreselectionValues.RData")) file.remove("initiator_userPreselectionValues.RData")
  
  #if user selected to create an additional cluster graph
  if(continueClustering == "y" || continueClustering == "Y"){  
    source("3_Clustering.R")
    message("\n**** Cluster graph created! ****\n")
  }
  else if(continueClustering == "n" || continueClustering == "N"){  
    break
  }
  else{
    message("Please enter either 'Y' or 'N'.\n")
  }
  
  #repeat unless if user indicated to end
}

# TW:bug: this variable appears to be deleted with the rm(list=ls()) commands within the 
#         pipeline.  Find a more robust way to save this directory and the start time.
  #return working directory to where it began when the script was called
  # setwd(orig.dir)
  
  #print the amount of time the script required
  # cat("\n\n\nPipeline processing runtime details (in sec):\n")
  # print(proc.time() - start)

#Indicate pipeline completion
message("\n**** Clustering pipeline complete! ****\n")

#Clear environment variables
rm(list=ls())   

