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
# Authors:      Krishna Madhavan (1), Kerrie Douglas (2), 
#               Doipayan Roy (2), and Taylor Williams (2)
# Affiliations: (1) Microsoft, (2) Purdue University
# 
# Description:  Initiates the clustering pipeline, sequentally calling the pipeline scripts
#               and providing continuity between them.
# 
# Package dependancies: [none]
#
# Changelog:
#     2017.07.14.   Initial version
#     
#
# Feature wishlist:  (*: planned but not complete)
#                   *pass selected data filepaths between sourced scripts
#                   *remove unnecessisary packages from the PackRat library
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
#source the pipeline script files
source("1_extractModules.R")
source("2_Preprocessing.R")
source("2b_genderedSubsets.R")
source("3_Clustering.R")

#return working directory to where it began when the script was called
setwd(orig.dir)

#Indicate pipeline completion
message("\n**** Clustering pipeline complete! ****\n")
