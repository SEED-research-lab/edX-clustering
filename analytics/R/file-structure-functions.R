## ===================================================== ##
# Title:        Functions for file-structure needs in edX clustering pipeline ####
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
# Authors:      Krishna Madhavan (1), Kerrie Douglas (2), 
#               Doipayan Roy (2), and Taylor Williams (2)
# Affiliations: (1) Microsoft, (2) Purdue University
# 
# Description:  Functions to check for existence of directories and files
# 
# Package dependancies: [beepr]
#
# Changelog:
#     2017.05.11.   Initial function extraction from pipeline
#     2017.05.11.   Minor updates to console output
#     2017.07.14.   Minor code updates; added copyright information
## ===================================================== ##


#Function: Check for existance of subdirectory, create if it doesn't exist.
DirCheckCreate <- function(subDir) {
  #set directory variables
  mainDir <- getwd()
  
  #check for/create subdirectory
  if(!dir.exists(file.path(mainDir, subDir))){
    cat(paste0("The directory '", subDir, "' does not exist in '", mainDir, "' -- creating directory"))
    dir.create(file.path(mainDir, subDir))
    subDirPath <- file.path(mainDir, subDir)
  }else{
    cat(paste0("The directory '", subDir, "' exists in '", mainDir, "' -- continuing script"))
    subDirPath <- file.path(mainDir, subDir)
  }
  return(subDirPath)
}




#Function: Check to see if the selected filename contains the expected text. 
#   Allow user to continue with provided file if desired
ExpectedFileCheck <- function(selectedFilename, expectedFileEnding) {
  #check to see if the user selected file contains the expected character string
  
  #grep is a regular expression string search.  It looks for the pattern within x.  The index positions in x are returned
  # where the pattern is found.  Here x only has a single entry, so if grep() returns 1 then the pattern matches.
  matchResult <- grepl(pattern = expectedFileEnding, x = selectedFilename)
  if(grepl(pattern = expectedFileEnding, x = selectedFilename)){
    #the pattern is found, inform user and exit the function
    cat("\n\nThe filename you provided MATCHES the expected text.")
    return("matched")
    # break
  }else {
    #otherwise, inform the user that the strings don't match and provide option to override (continue with provided file)
    cat("\n\nERROR: The filename you provided DOESN'T MATCH the expected text.",
        "\n       filename provided: ", selectedFilename,
        "\n       expected in filename: ", expectedFileEnding)
    
    repeat{
      #give the user the option to continue with the selected file
      #beepr::beep(sound = 10)   #notify user to provide input
      overrideChoice <- readline(prompt="Enter 1 to use currently selected file, 2 to select a new file: ")
      
      if(overrideChoice == 1){
        return("overridden")
      }else if(overrideChoice == 2){
        return("reselect")
      }else{
        cat("Invalid entry.\n")
      }
    }
  }
}



#Function: Check for existance of file passed in
FileExistCheck <- function(subDir, filename) {
  
  #set parameters for file location
  mainDir <- getwd()
  
  #store the file path
  filePath <- file.path(mainDir, subDir, filename, fsep = "/")
  
  #check for existance of CSV module order file
  if(file.exists(filePath)){
    cat(paste(filename, "found -- continuing"))
    return(filePath)
  }else{
    cat(paste("ERROR: ", filename, "not found -- exiting script"))
    rm(list=ls()) ## Clear the environment
    return(FALSE)  #retun signal to exit script if file not found
  }
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