## ===================================================== ##
# Title:        Extraction tool for edX MOOC module sequence ####
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
# Description:  For their course sequencing, edX provides a jumbled (yet structured) JSON file. 
#               As provided it is not in a useful form for our analyzing user engagement with the course.  
#               This algorithm creates an ordered csv file with 
#               (1) edX's cryptic UID: Module_ID. 
#               (2) the provided human-readable text description of the module.  
#               (3) an ordinal listing of the children of each module--that is 
#                   a "0" for the course module, 
#                   a "1" for chapter level modules, 
#                   a "2" for the chapter's children, 
#                   a "3" for the chapter's children's children, and so on.
#               (4) a sequential numbering of the modules.
# 
# File input stack: 
#       {org}-{course}-{date}-course_structure-prod-analytics.json        (source: edX)
#       {org}-{course}-{date}-courseware_studentmodule-prod-analytics.sql (source: edX)
# 
# 
# Package dependencies: jsonlite, readr, tcltk
#
# Changelog:
#     2017.03.10.   Initial distributed version
#     2017.03.10.   Minor updates including header comments
#                     Distributed internally to MOOC research group through Dropbox
#     2017.03.10.   Transitioned code identifying "course" as the highest level 
#                     (rather than the previously used "chapter"). This was done to (1) clean up 
#                     the code logic and (2) to take advantage of the chapters being properly 
#                     ordered in the "course" module.
#                   Modified output csv filename to contain the original JSON filename. 
#     2017.03.22.   Minor updates to improve clarity
#     2017.03.30.   Removed all modules from courseHierarchy that were never clicked by ANY user
#     2017.04.05.   Automated generation of module_order_file.csv to feed directly into Preprossing.r. 
#                             Saving deleted modules into a separate csv file for examination
#     2017.04.11.   Changed file selection to a GUI file browser. 
#                             Direct read in from sql clickstream file eliminates the external conversion from SQL to CSV
#     2017.04.20.   Fixed csv export bug if a module title contains a comma 
#                             Added section headings
#     2017.05.02.   Created output files placed into a separate subdirectory
#     2017.05.03.   Put subdirectory and file checking code into functions
#     2017.05.08.   Code cleaning, header update
#                   Audio notification for user input and script completion
#     2017.05.09.   Added filename check of user provided files (with override option)
#     2017.05.11.   Extracted possible functions to external files
#     2017.05.18.   Removed dependencies on beepr (for compatibility with RStudio server)
#     2017.05.25.   Added timer to track script execution time
#     2017.07.14.   Minor code updates; added copyright information
#     2017.08.06.   Update to comments; spell check
#     2018.10.12.   Update to file selection (semi-automated)
## ===================================================== ##


######### Clean the environment ########## 
rm(list=setdiff(ls(), c("data_moduleAccess", "data_courseStructure", "dataUserProfile")))

######### Internal functions ########## 
#Function: Recursive child search to build the course hierarchy
ChildSearch <- function(data, courseHierarchy, curModuleIndex, hierarchicalLvl) {
  #store the number of children the current module has
  numChildren <- length(data[[curModuleIndex]][["children"]])
  
  #go one level deeper if the current Module has children
  if(numChildren > 0){
    #locate the next child and see if it has any children itself
    for(j in 1:numChildren){
      curChildIndex <- match(data[[curModuleIndex]][["children"]][j], moduleNames)
      
      ###store the data (ID, display name, lvl)
      #concatenate matching module names into the course hierarchy (using rbind)
      courseHierarchy <- rbind(courseHierarchy,
                               c(names(data[curChildIndex]),
                                 data[[curChildIndex]][["metadata"]]["display_name"], hierarchicalLvl))
      courseHierarchy <- ChildSearch(data, courseHierarchy, curChildIndex, hierarchicalLvl+1)
    } 
  }
  
  return(courseHierarchy)
}


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
thisFile = "1_extractModules.R"
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


######### Import course structure JSON file data #####
#Locate the JSON course structure data file to process (with sanitized user input)
if(!exists("data_courseStructure")){
  filenameJSON <- 
    SelectFile(prompt = "*****Select the JSON COURSE STRUCTURE file.*****  (It should end with 'course_structure-prod-analytics.json')", 
               defaultFilename = "course_structure-prod-analytics.json", 
               filenamePrefix = ifelse(exists("filenamePrefix") & !is.null(filenamePrefix), 
                                       yes = filenamePrefix, no = ""), 
               fileTypeMatrix = matrix(c("JSON", ".json"), 1, 2, byrow = TRUE),
               dataFolderPath = ifelse(exists("dataFolderPath") & !is.null(dataFolderPath), 
                                       yes = dataFolderPath, no = ""))
  
  #import the JSON data file
  data_courseStructure <- jsonlite::fromJSON(filenameJSON)
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



######### Identify course level module #############

#copy the module names into a separate variable
moduleNames <- names(data_courseStructure)

#create variable to track which level the module should be sorted at (top, child, grandchild, etc)
hierarchicalLvl <- 0
#build empty matrix to store hierarchy for entire course
courseHierarchy <- matrix(nrow = 0, ncol = 3, byrow = FALSE)


#Check for category type "course" for each module in the file; 
#   save matching module into the courseHierarchy data structure
for(i in 1:length(moduleNames)){
  if(data_courseStructure[[moduleNames[i]]]["category"] == "course"){
    courseModule <- moduleNames[i]
    courseHierarchy <- c(moduleNames[i], 
                         data_courseStructure[[moduleNames[i]]][["metadata"]]["display_name"], 
                         hierarchicalLvl)
  }
}




######### Initiate recursive search ###############

#initialize variables and initiate recursive searching for children
#set the current hierarchy level
hierarchicalLvl <- 0  #course level
#conduct the recursive child search for each of the chapter level modules. Concatenate the results onto courseHierarchy
courseHierarchy <- ChildSearch(data_courseStructure, 
                               courseHierarchy, 
                               courseModule, 
                               hierarchicalLvl+1) 



######### Remove unaccessed modules ###############

#convert to a data frame so we can use the "$" notation
courseHierarchy <- as.data.frame(courseHierarchy)
names(courseHierarchy) <- c("module_id","module_title","module_hierarchy_level")




#create an empty data frame to store the modules not clicked on by anyone
DeletedModules <- data.frame()
#create an empty data frame to store the modules only clicked on by a few people
typicallyUnusedModules <- data.frame()


moduleList <- unique(courseHierarchy$module_id)
for(module in moduleList)
{
  #delete module row if 
    # no events are found in the clickstream data that match the current module
    # or
    # module type is "vertical"
  if((nrow(subset(data_moduleAccess, data_moduleAccess$module_id == module)) == 0) |
     (grepl(pattern = "vertical", x = module) == TRUE))
  {
    DeletedModules <- rbind(DeletedModules, 
                            subset(courseHierarchy,courseHierarchy$module_id == module))
    courseHierarchy <- courseHierarchy[courseHierarchy$module_id != module,]
  }
  
  #create a log of modules that were accessed by fewer than 0.1% of learners
  accessMin <- round(0.001 * length(unique(data_moduleAccess$student_id)))
  if(nrow(subset(data_moduleAccess, 
                 data_moduleAccess$module_id == module)) < accessMin)
  {
    typicallyUnusedModules <- 
      rbind(typicallyUnusedModules, 
            subset(courseHierarchy,courseHierarchy$module_id == module))
  }
  
}



## Add column for a sequential module number

#create vector with sequential numbers to add as a column in courseHierarchy
numModules <- nrow(courseHierarchy)
module_no <- 0:(numModules - 1)
module_no <- as.data.frame(module_no)
#add it to the dataframe
courseHierarchy <- cbind(courseHierarchy,module_no)


######### Write data to files ###############
## trying to get this working from an external function
# if(!exists("DirCheckCreate", mode="function")) source(file.path(getwd(), "analytics", "fun_DirCheckCreate.R", fsep = "/"))
#call function to check for the existance of the subdirectory; create it if it doesn't exist
source("R/file-structure-functions.R")
subDirPath <- DirCheckCreate(subDir = "1_extractModulesOutput")



#identify and save the column number index for the module titles 
#   (To be used later in forcing quotes around the title strings.  Needed in case a module title contains a comma.)
modTitleColIndex <- grep("module_title", colnames(courseHierarchy))

#converting data frames to matricies to be able to write to CSV (write.csv() will not accept a data frame)
courseHierarchy <- as.matrix(courseHierarchy)
DeletedModules <- as.matrix(DeletedModules)
typicallyUnusedModules <- as.matrix(typicallyUnusedModules)


#write a CSV file for the next step in processing.  (Also write a CSV file of those modules which were removed and those that were not much used.)
#   quotes are forced around the module title names in case one contains a comma 
cat("\nSaving CSV file.")
write.csv(file = file.path(subDirPath, "module_order_file.csv", fsep = "/"), 
          x = courseHierarchy, quote = c(modTitleColIndex))
write.csv(file = file.path(subDirPath, "modules_deleted.csv", fsep = "/"),
          x = DeletedModules, quote = c(modTitleColIndex))
write.csv(file = file.path(subDirPath, 
                           paste0("modules_with_fewer_than_", accessMin, "_accesses.csv"), 
                           fsep = "/"),
          x = typicallyUnusedModules, quote = c(modTitleColIndex))

#save the course prefix
write.csv(file = file.path(subDirPath, paste0(filenamePrefix, ".csv"), fsep = "/"),
          x = filenamePrefix)


######### Notify user and Clear the environment  #############
# beepr::beep(sound = 10)   #notify user script is complete
# Sys.sleep(time = 0.1)     #pause 1/10 sec
# beepr::beep(sound = 10)
# Sys.sleep(time = 0.1)
# beepr::beep(sound = 10)

#print the amount of time the script required
cat("\n\n\nScript (1_extractModules.R) processing time details (in sec):\n")
print(proc.time() - start)

#Clear environment variables
rm(list=setdiff(ls(), c("data_moduleAccess", "data_courseStructure", "dataUserProfile",
                        "filenamePrefix", "dataFolderPath")))


