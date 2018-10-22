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
#     2018.10.12.   Update to file selection (semi-automated)
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
start <-  proc.time() #save the time (to compute elapsed time of script)


## Check for pre-defined starting directory and course prefix ####
if(!exists("filenamePrefix")) filenamePrefix <- NULL
if(!exists("dataFolderPath")) dataFolderPath <- NULL
if(!exists("courseName")) courseName <- NULL


######### Reading files, converting to dataframe object, eliminating irrelevant columns#####
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

#Locate the clickstream data file to process (with sanitized user input)
if(!exists("data_moduleAccess")){
  filenameClickstream <- 
    SelectFile(prompt = "*****Select the SQL CLICKSTREAM data file.*****  (It should end with 'courseware_studentmodule-prod-analytics.sql')", 
               defaultFilename = "courseware_studentmodule-prod-analytics.sql",
               filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix), 
                                       yes = filenamePrefix, no = ""), 
               fileTypeMatrix = matrix(c("SQL", ".sql"), 1, 2, byrow = TRUE),
               dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath), 
                                       yes = dataFolderPath, no = ""))
  
  
  #read in the clickstream data 
  data_moduleAccess <- readr::read_tsv(file = filenameClickstream)
}

# extract needed columns
data_moduleAccess <- data_moduleAccess[names(data_moduleAccess) %in% 
                                         c("module_id","student_id","created")]

#read in the ordered module information
DirCheckCreate(subDir = courseName)
subDirPath <- DirCheckCreate(subDir = file.path(courseName, "1_extractModulesOutput"))
moduleOrderFilePath <- FileExistCheck_workingDir(subDir = subDirPath, 
                                                 fullPathPassed = T,
                                                 filename = "module_order_file.csv")
#exit script if file not found, otherwise continue
ifelse(test = moduleOrderFilePath == FALSE, yes = return(), no = "")

#read in CSV data and convert to data frame
module_markers <- readr::read_csv(moduleOrderFilePath)
module_markers <- as.data.frame(module_markers)

##Ordering both data_moduleAccess and module_marker object by module_id field
data_moduleAccess <- data_moduleAccess[order(data_moduleAccess$module_id,decreasing=F),]
module_markers <- module_markers[order(module_markers$module_id,decreasing=F),]

##Eliminating module_id from data_moduleAccess if that id does not appear in module_marker
data_moduleAccess <- subset(data_moduleAccess,data_moduleAccess$module_id %in% module_markers$module_id)


## ===================================================== ##



######### Mapping module_id in every clickstream event, its integer reflects the module ordering

#Clickstream and module marker dataframe have been ordered in increasing order of module_id
#Change_list documents the row numbers in ordered clickstream file at which module_id changes
change_list=c()
change_list=c(change_list,0)
for(i in unique(module_markers$module_id))
{
  temp_df=subset(data_moduleAccess,data_moduleAccess$module_id==i)
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
data_moduleAccess<-cbind(data_moduleAccess,marker_list)

## ===================================================== ##

######### Converting time to POSIXct format and adding time column to data_moduleAccess##############

time=as.POSIXct(data_moduleAccess$created,format="%m/%d/%Y %H:%M")
data_moduleAccess<-cbind(data_moduleAccess,time)

##Keeping only releveant columns
data_moduleAccess<-data_moduleAccess[names(data_moduleAccess) %in% 
                                       c("student_id","time","marker_list")]

## ===================================================== ##

######### Converting student_id to integers from 1 to total_number_registered###############

##Sorting data_moduleAccess in order of student_id
data_moduleAccess<-data_moduleAccess[order(data_moduleAccess$student_id,decreasing=F),]

#u_id is a column containing student_id of each clickstream event mapped to an integer
#Each student is assigned a unique integer
#create progress status variables for male processing loop
iCount <- 1 #loop counter for completion updates
pct <- 0  #percentage complete tracker
u_id=c()

for(i in sort(unique(data_moduleAccess$student_id),decreasing=F))
{
  temp_df=subset(data_moduleAccess,data_moduleAccess$student_id==i)
  
  se=rep(iCount,nrow(temp_df))
  
  u_id=c(u_id,se)
  
  
  #update the progress for every 1% complete
  iCount <- iCount + 1  
  if(iCount%%as.integer((length(unique(data_moduleAccess$student_id)))/100) == 0){
    pct <- pct + 1
    cat("\rAssigning students unique value: ", pct, "% complete", sep = "")
  }
}
cat("\nDone!\n")

#Adding u_id column to data_moduleAccess
data_moduleAccess<-cbind(data_moduleAccess, u_id)


## ===================================================== ##




######### Retaining relevant columns ##############

data_moduleAccess<-data_moduleAccess[names(data_moduleAccess) %in% 
                                       c("student_id","marker_list","u_id","time")]
names(data_moduleAccess)<-c("student_id","module_number","time","temp_student_id")


######### No access users ID list ##############
#create 2 lists 
#(1) all known unique student IDs  and 
#(2) empty list to save users without clickstream data (never accessed the course)
ID_List <- unique(dataUserProfile$user_id)
noAccess <- c()


#create progress status variables for male processing loop
iCount <- 0 #loop counter for completion updates
pct <- 0  #percentage complete tracker
start <-  proc.time() #save the time (to compute elapsed time of loop)

# look for registered user who never accessed
for(ID in ID_List){
  #save to a list of  with no clickstream data (never accessed the course)
  if(nrow(subset(data_moduleAccess, data_moduleAccess$student_id == ID)) == 0){ 
    noAccess <- c(noAccess, ID)
  }
  
  #update the progress for every 1% complete
  iCount <- iCount + 1  
  if(iCount%%as.integer((length(ID_List))/100) == 0){
    pct <- pct + 1
    cat("\rLooking for registered users who have no access events: ", pct, "% complete", sep = "")
  }
}

cat("\nDone!\n")
#print the amount of time the previous loop required
cat("\n\n\nNo access learner processing time details (in sec):\n")
print(proc.time() - start)



######### Write data to files ###############
#call function to check for the existance of the subdirectory; create it if it doesn't exist
DirCheckCreate(subDir = courseName)
subDirPath <- DirCheckCreate(subDir = file.path(courseName, "2_PreprocessingOutput"))

#write a CSV file for the next step in processing. 
cat("\nSaving CSV file.\n")
write.csv(file = file.path(subDirPath, "preprocessed_data.csv", fsep = "/"), 
          x = data_moduleAccess)



#print and save no access data
noAccessPct <- length(noAccess)/length(ID_List) * 100
noAccessPct <- sprintf("%.1f", noAccessPct, "%", collapse = "")

cat(paste0("Percentage of registered learners who never accessed the course: ", 
             noAccessPct, "%"))
names(noAccess) <- c("Count","student_id")

write.csv(x = noAccess, file = file.path(subDirPath, 
                                         paste0("noAccess_all_UIDs_", 
                                                noAccessPct, " pct of regs.csv"), 
                                         fsep = "/"))



######### Notify user and Clean the environment  #############
#print the amount of time the script required
cat("\n\n\nScript (2_Preprocessing.R) processing time details (in sec):\n")
print(proc.time() - start)

#Clean environment variables
rm(list=setdiff(ls(), varsToRetain))

