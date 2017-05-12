## ===================================================== ##
# Title:        Main Preprocessing of Clickstream Data ####
#
#
# Author(s):    Doipayan Roy, Taylor Williams
# Institution:  Purdue University
# 
# Project:      
# 
# Description:  []
# 
# Package dependancies: readr, tcltk, beepr
#
# Changelog:
#     2017.04.13.   Added GUI user selection of Clickstream file
#                   Retained student_id for output file (it's needed for identifying gendered subsets)
#     2017.05.02.   Input files read from subdirectory
#                   Created output files placed into a seperate subdirectory
#     2017.05.03.   Put subdirectory and file checking code into functions
#     2017.05.08.   Code cleaning, header update
#                   Audio notification for user input and script completion
#     2017.05.09.   Added filename check of user provided files (with override option)
#     2017.05.11.   Extracted possible functions to external files
## ===================================================== ##



######### Clean the environment ########## 
rm(list=ls())   


######### Internal functions ########## 
#Function: Interactively select working directory (OS independant)
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
WorkingDirectoryCheck <- function() {
  #set directory variables
  curDir <- getwd()
  #set a filename expected to exist in the working directory
  expectedFile <- "1_extractModules.R"
  
  if(file.exists(file.path(curDir, expectedFile))){
    #if file does exists in the current WD, exit the funtion returning TRUE
    return(TRUE)
  } else{
    #if the file does not exist in the current WD, return FALSE
    return(FALSE)
  }
}



######### Check for correct working directory ########## 
#continue checking the current working direcotry and prompting user for the correct directory 
# while the workingDirectoryCheck returns false
while(!WorkingDirectoryCheck()){
  cat("The current working directory is NOT CORRECT.  Please set it to the directory containing the R scripts.\n")
  
  #have user set the working directory
  beepr::beep(sound = 10)   #notify user to provide input
  InteractiveSetWD()
}


######### External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")




# end of script setup
## *************************************** #####
# beginning of script functionality




######### Reading files, converting to dataframe object, eliminating irrelevant columns#####
  
#Locate the clickstream data file to process (with sanatized user input)
repeat{
  cat("*****Select the SQL CLICKSTREAM data file.*****\n  (It should end with 'courseware_studentmodule-prod-analytics.sql')")
  beepr::beep(sound = 10)   #notify user to provide input
  filenameClickstream <- file.choose()
  
  filenameCheckResult <- ExpectedFileCheck(selectedFilename = filenameClickstream, expectedFileEnding = "courseware_studentmodule-prod-analytics.sql")
  
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

																		

######### Mapping module_id in every row of dataClickstream to order_integer of the module#########

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

marker_list=c()
for(i in 1:(length(change_list)-1))
{
  se=rep(module_markers$module_no[i],as.integer(1+(change_list[i+1])-(change_list[i]+1)))
  marker_list=c(marker_list,se)
}
dataClickstream<-cbind(dataClickstream,marker_list)

## ===================================================== ##

######### Converting time to POSIXct format and adding time column to dataClickstream##############

time=as.POSIXct(dataClickstream$created,format="%m/%d/%Y %H:%M")
dataClickstream<-cbind(dataClickstream,time)
##Keeping only releveant columns
dataClickstream<-dataClickstream[names(dataClickstream) %in% c("student_id","time","marker_list")]
##Sorting dataClickstream in order of student_id
dataClickstream<-dataClickstream[order(dataClickstream$student_id,decreasing=F),]

## ===================================================== ##

######### Converting student_id to integers from 1 to total_number_registered###############

u_id=c()
counter=1
for(i in sort(unique(dataClickstream$student_id),decreasing=F))
{
  temp_df=subset(dataClickstream,dataClickstream$student_id==i)
																   
  se=rep(counter,nrow(temp_df))
												   
  u_id=c(u_id,se)
																				 
  counter=counter+1
}
dataClickstream<-cbind(dataClickstream,u_id)
#return(dataClickstream)    #TW (2017.05.02) I think this line should be deleted


## ===================================================== ##

															


######### Retaining relevant columns, renaming columns and writing to CSV file##############

dataClickstream<-dataClickstream[names(dataClickstream) %in% c("student_id","marker_list","u_id","time")]
names(dataClickstream)<-c("orig_student_id","module_number","time","temp_student_id")


######### Write data to files ###############
  ## TW (2017.05.03): I'm trying to get this working from an external function. The following line is attempting this.
    # if(!exists("DirCheckCreate", mode="function")) source(file.path(getwd(), "analytics", "fun_DirCheckCreate.R", fsep = "/"))
#call function to check for the existance of the subdirectory; create it if it doesn't exist
subDirPath <- DirCheckCreate(subDir = "2_PreprocessingOutput")

#write a CSV file for the next step in processing. 
write.csv(file = file.path(subDirPath, "preprocessed_data.csv", fsep = "/"), x = dataClickstream)

######### Notify user and Clear the environment  #############
beepr::beep(sound = 10)   #notify user script is complete
Sys.sleep(time = 0.1)     #pause 1/10 sec
beepr::beep(sound = 10)
Sys.sleep(time = 0.1)
beepr::beep(sound = 10)

rm(list=ls())   #Clear environment variables

							