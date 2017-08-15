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
# Package dependencies: [none]
#
# Changelog:
#     2017.07.14.   Initial version
#     2017.08.06.   Update to comments; spell check
#     
#
# Feature wish list:  (*: planned but not complete)
#                   * have user provide paths to the edX data files here (once)
#                   * pass selected data file paths between sourced scripts
#                   * remove unnecessary packages from the PackRat library
## ===================================================== ##




## _Clean the environment ####
rm(list=ls()) 


######### Internal functions ########## 
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

## _External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")


######### Main ########## 

#start a timer to track how long the pipeline takes to execute
start <-  proc.time() #save the time (to report the pipeline's running time at the end of the script)

#source (run) the pipeline script files in sequence
source("1_extractModules.R")
source("2_Preprocessing.R")
source("2b_genderedSubsets.R")
source("3_Clustering.R")
message("\n**** Cluster graph created! ****\n")


#ask user if additional cluster charts are desired, if so run '3_Clustering.R' again
repeat{
  #beepr::beep(sound = 10)   #notify user to provide input
  continueClustering <- readline(prompt="Would you like to create another cluster graph from this data? (Y/N): ");
  
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


#return working directory to where it began when the script was called
# TW:bug: this variable appears to be deleted with the rm(list=ls()) commands within the 
#         pipeline.  Find a more robust way to save this directory.
# setwd(orig.dir)


#print the amount of time the script required
cat("\n\n\nPipeline processing runtime details (in sec):\n")
print(proc.time() - start)

#Indicate pipeline completion
message("\n**** Clustering pipeline complete! ****\n")

#Clear environment variables
rm(list=ls())   

