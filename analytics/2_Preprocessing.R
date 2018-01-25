## ===================================================== ##
# Title:        Main Preprocessing of Clickstream Data ####
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
# Description:  Clickstream data is read as the dataFrame object named raw_data
#               Fields of interest in clickstream data are: student_id, module_id and created
#               Module data is read as the dataFrame object named module_markers
#               module_markers contain an integer id for each module, 
#                 which reflects the sequence in which modules occur in the course
# 
# 
# File input stack: 
#       {org}-{course}-{date}-courseware_studentmodule-prod-analytics.sql (source: edX)
#       module_order_file.csv (source: pipeline script 1_extractModules.R)
# 
# 
# Package dependencies: readr, [tcltk, beepr]
#
# Changelog:
#     2017.04.13.   Added GUI user selection of Clickstream file
#                   Retained student_id for output file (it's needed for identifying gendered subsets)
#     2017.05.02.   Input files read from subdirectory
#                   Created output files placed into a separate subdirectory
#     2017.05.03.   Put subdirectory and file checking code into functions
#     2017.05.08.   Code cleaning, header update
#                   Audio notification for user input and script completion
#     2017.05.09.   Added filename check of user provided files (with override option)
#     2017.05.11.   Extracted possible functions to external files
#     2017.05.18.   Removed dependencies on beepr (for compatibility with RStudio server)
#     2017.05.25.   Integrated Doipayan's modifications up through 2017.05.09 
#                   Added timer to track script execution time
#     2017.07.14.   Minor code updates; added copyright information
#     2017.08.06.   Update to comments; spell check
## ===================================================== ##



######### Clean the environment ########## 
rm(list=ls())   


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


######### Check for correct working directory ########## 
#check for correct expected working directory, inform user if incorrect and stop running script
current.dir <- getwd()
thisFile = "2_Preprocessing.R"
expectedFile = file.path(thisFile)

if(!WorkingDirectoryCheck(expectedFile)){
  message("\nThe current working directory is NOT CORRECT.
          It is currently set to '", current.dir, "'
          Please set it to the directory containing the '", thisFile, 
          "' file and rerun this script.\n")
  
  #stop running current script
  break
}


######### External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")




# end of script setup
## *************************************** #####
# beginning of script functionality


#start a timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute elapsed time of script)



######### Reading files, converting to dataframe object, eliminating irrelevant columns#####

#Locate the clickstream data file to process (with sanitized user input)
repeat{
  prompt <- "*****Select the SQL CLICKSTREAM data file.*****  (It should end with 'courseware_studentmodule-prod-analytics.sql')"
  cat("\n", prompt)
  #beepr::beep(sound = 10)   #notify user to provide input
  
  # filenameClickstream <- file.choose() #commented out, but may still be needed if working in RStudio server environment
  filenameClickstream <- tcltk::tk_choose.files(caption = prompt, 
                                                default = "courseware_studentmodule-prod-analytics.sql",
                                                filter = matrix(c("SQL", ".sql"), 1, 2, byrow = TRUE),
                                                multi = FALSE)

  filenameCheckResult <- ExpectedFileCheck(selectedFilename = filenameClickstream, 
                                           expectedFileEnding = "courseware_studentmodule-prod-analytics.sql")
  
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

#read in the clickstream data and extract needed columns
dataClickstream <- readr::read_tsv(filenameClickstream)
dataClickstream <- dataClickstream[names(dataClickstream) %in% c("module_id","student_id","created")]

#read in the ordered module information
moduleOrderFilePath <- FileExistCheck(subDir = "1_extractModulesOutput", filename = "module_order_file.csv")
#exit script if file not found, otherwise continue
ifelse(test = moduleOrderFilePath == FALSE, yes = return(), no = "")

#read in CSV data and convert to data frame
module_markers <- readr::read_csv(moduleOrderFilePath)
module_markers <- as.data.frame(module_markers)

##Ordering both dataClickstream and module_marker object by module_id field
dataClickstream <- dataClickstream[order(dataClickstream$module_id,decreasing=F),]
module_markers <- module_markers[order(module_markers$module_id,decreasing=F),]

##Eliminating module_id from dataClickstream if that id does not appear in module_marker
dataClickstream <- subset(dataClickstream,dataClickstream$module_id %in% module_markers$module_id)


## ===================================================== ##



######### Mapping module_id in every clickstream event, its integer reflects the module ordering

#Clickstream and module marker dataframe have been ordered in increasing order of module_id
#Change_list documents the row numbers in ordered clickstream file at which module_id changes
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

#marker_list is a column containing integer id of module in each clickstream event
marker_list=c()
for(i in 1:(length(change_list)-1))
{
  se=rep(module_markers$module_no[i],as.integer(1+(change_list[i+1])-(change_list[i]+1)))
  marker_list=c(marker_list,se)
}

#Add marker_list column to clickstream file
dataClickstream<-cbind(dataClickstream,marker_list)

## ===================================================== ##

######### Converting time to POSIXct format and adding time column to dataClickstream##############

time=as.POSIXct(dataClickstream$created,format="%m/%d/%Y %H:%M")
dataClickstream<-cbind(dataClickstream,time)

##Keeping only releveant columns
dataClickstream<-dataClickstream[names(dataClickstream) %in% c("student_id","time","marker_list")]

## ===================================================== ##

######### Converting student_id to integers from 1 to total_number_registered###############

##Sorting dataClickstream in order of student_id
dataClickstream<-dataClickstream[order(dataClickstream$student_id,decreasing=F),]

#u_id is a column containing student_id of each clickstream event mapped to an integer
#Each student is assigned a unique integer
u_id=c()
counter=1
for(i in sort(unique(dataClickstream$student_id),decreasing=F))
{
  temp_df=subset(dataClickstream,dataClickstream$student_id==i)
  
  se=rep(counter,nrow(temp_df))
  
  u_id=c(u_id,se)
  
  counter=counter+1
}

#Adding u_id column to dataClickstream
dataClickstream<-cbind(dataClickstream,u_id)


## ===================================================== ##




######### Retaining relevant columns, renaming columns and writing to CSV file##############

dataClickstream<-dataClickstream[names(dataClickstream) %in% c("student_id","marker_list","u_id","time")]
names(dataClickstream)<-c("student_id","module_number","time","temp_student_id")


######### Write data to files ###############
#call function to check for the existance of the subdirectory; create it if it doesn't exist
subDirPath <- DirCheckCreate(subDir = "2_PreprocessingOutput")

#write a CSV file for the next step in processing. 
cat("\nSaving CSV file.")
write.csv(file = file.path(subDirPath, "preprocessed_data.csv", fsep = "/"), x = dataClickstream)


######### Notify user and Clear the environment  #############
# beepr::beep(sound = 10)   #notify user script is complete
# Sys.sleep(time = 0.1)     #pause 1/10 sec
# beepr::beep(sound = 10)
# Sys.sleep(time = 0.1)
# beepr::beep(sound = 10)

#print the amount of time the script required
cat("\n\n\nScript (2_Preprocessing.R) processing time details (in sec):\n")
print(proc.time() - start)

rm(list=ls())   #Clear environment variables


