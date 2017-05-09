## ===================================================== ##
# Title:        Extraction tool for edX MOOC module sequence (from JSON data file) ####
#
#
# Author(s):    Taylor Williams
# Institution:  Purdue University
# 
# Description:  For their course sequencing, edX provides a jumbled (yet structured) JSON file. 
#               As provided it is not in a useful form for our analyzing user engagement with the course.  
#               This algorythm creates an ordered csv file with 
#               (1) edX's cryptic UID: Module_ID. 
#               (2) the provided human-readable text description of the module.  
#               (3) an ordinal listing of the children of each module--that is 
#                   a "0" for the course module, 
#                   a "1" for chapter level modules, 
#                   a "2" for the chapter's children, 
#                   a "3" for the chapter's children's children, and so on.
#               (4) a sequential numbering of the modules.
# 
# Package dependancies: jsonlite, readr, tcltk, beepr
#
# Changelog:
#     2017.03.10.   Initial distributed version
#     2017.03.10.   Minor updates including header comments
#                     Distributed internally to MOOC research group through Dropbox
#     2017.03.10.   Transitioned code identifying "course" as the highest level 
#                     (rather than the previously used "chapter"). This was done to (1) clean up 
#                     the code logic and (2) to take advanage of the chapters being properly 
#                     ordered in the "course" module.
#                   Modified output csv filename to contain the origional JSON filename. 
#     2017.03.22.   Minor updates to improve clarity
#     2017.03.30.   Removed all modules from courseHierarchy that were never clicked by ANY user
#     2017.04.05.   Automated generation of module_order_file.csv to feed directly into Preprossing.r. 
#                             Saving deleted modules into a seperate csv file for examinateion
#     2017.04.11.   Changed file selection to a GUI file browser. 
#                             Direct read in from sql clickstream file eliminates the external conversion from SQL to CSV
#     2017.04.20.   Fixed csv export bug if a module title contains a comma 
#                             Added section headings
#     2017.05.02.   Created output files placed into a seperate subdirectory
#     2017.05.03.   Put subdirectory and file checking code into functions
#     2017.05.08.   Code cleaning, header update
#                   Audio notification for user input and script completion
#     2017.05.09.   Added filename check of user provided files (with override option)
## ===================================================== ##


######### Functions ##########

#Function: Check for existance of subdirectory, create if it doesn't exist.
DirCheckCreate <- function(subDir) {
  #set directory variables
  mainDir <- getwd()
  
  #check for/create subdirectory
  if(!dir.exists(file.path(mainDir, subDir))){
    cat(paste(subDir, " does not exist in '", mainDir, "' -- creating"))
    dir.create(file.path(mainDir, subDir))
    subDirPath <- file.path(mainDir, subDir)
  }else{
    cat(paste(subDir, " exists in '", mainDir, "' -- continuing"))
    subDirPath <- file.path(mainDir, subDir)
  }
  return(subDirPath)
}


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

#Function: Recursive child search to build the course heirarchy
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
      beepr::beep(sound = 10)   #notify user to provide input
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


######### Check for correct working directory ########## 

#continue checking the current working direcotry and prompting user for the correct directory 
# while the workingDirectoryCheck returns false
while(!WorkingDirectoryCheck()){
  cat("The current working directory is NOT CORRECT.  Please set it to the directory containing the R scripts.\n")
  
  #have user set the working directory
  beepr::beep(sound = 10)   #notify user to provide input
  InteractiveSetWD()
}




######### Import course structure JSON file data #####

#Locate the JSON course structure data file to process (with sanatized user input)
repeat{
  cat("*****Select the JSON COURSE STRUCTURE file.*****\n  (It should end with 'course_structure-prod-analytics.json')")
  beepr::beep(sound = 10)   #notify user to provide input
  filenameJSON <- file.choose()
  
  filenameCheckResult <- ExpectedFileCheck(selectedFilename = filenameJSON, expectedFileEnding = "course_structure-prod-analytics.json")
  
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

#import the JSON data file
data <- jsonlite::fromJSON(filenameJSON)


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



######### Identify course level module #############

#copy the module names into a seperate variable
moduleNames <- names(data)

#create variable to track which level the module should be sorted at (top, child, grandchild, etc)
hierarchicalLvl <- 0
#build empty matrix to store hierarchy for entire course
courseHierarchy <- matrix(nrow = 0, ncol = 3, byrow = FALSE)


#Check for category type "course" for each module in the file; 
#   save matching module into the courseHierarchy data structure
for(i in 1:length(moduleNames)){
  if(data[[moduleNames[i]]]["category"] == "course"){
    courseModule <- moduleNames[i]
    courseHierarchy <- c(moduleNames[i], data[[moduleNames[i]]][["metadata"]]["display_name"], hierarchicalLvl)
  }
}


          

######### Initiate recursive search ###############

#initalize variables and initate recursive searching for children
  #set the current hierarchy level
  hierarchicalLvl <- 0  #course level
  #conduct the recursive child search for each of the chapter level modules. Concatenate the results onto courseHierarchy
  courseHierarchy <- ChildSearch(data, courseHierarchy, courseModule, hierarchicalLvl+1) 

  

######### Remove unaccessed modules ###############

#convert to a data frame so we can use the "$" notation
courseHierarchy <- as.data.frame(courseHierarchy)
names(courseHierarchy) <- c("module_id","module_title","module_hierarchy_level")



#read in the clickstream data 
raw_data <- readr::read_tsv(file = filenameClickstream)

#create an empty data frame to store the modules not clicked on by anyone
DeletedModules <- data.frame()


moduleList <- unique(courseHierarchy$module_id)
for(module in moduleList)
{
  #if no rows are found in the clickstream data that match the current module, then delete module row
  if(nrow(subset(raw_data, raw_data$module_id == module))==0)
  {
    DeletedModules <- rbind(DeletedModules, subset(courseHierarchy,courseHierarchy$module_id == module))
    courseHierarchy <- courseHierarchy[courseHierarchy$module_id != module,]
  }
}



## Add column for a sequential module number

#create vector with sequential numbers to add as a column in courseHierarchy
numModules <- nrow(courseHierarchy)
module_no <- 0:(numModules -1)
module_no <- as.data.frame(module_no)
#add it to the dataframe
courseHierarchy <- cbind(courseHierarchy,module_no)


######### Write data to files ###############
  ## trying to get this working from an external function
  # if(!exists("DirCheckCreate", mode="function")) source(file.path(getwd(), "analytics", "fun_DirCheckCreate.R", fsep = "/"))
#call function to check for the existance of the subdirectory; create it if it doesn't exist
subDirPath <- DirCheckCreate(subDir = "1_extractModulesOutput")



#identify and save the column number index for the module titles 
#   (To be used later in forcing quotes around the title strings.  Needed in case a module title contains a comma.)
modTitleColIndex <- grep("module_title", colnames(courseHierarchy))

#converting courseHierarchy to a matrix to be able to write to CSV (write.csv() will not accept a data frame)
courseHierarchy <- as.matrix(courseHierarchy)
DeletedModules <- as.matrix(DeletedModules)


#write a CSV file for the next step in processing.  (Also write a CSV file of those modules which were removed.)
#   quotes are forced around the module title names in case one contains a comma
write.csv(file = file.path(subDirPath, "module_order_file.csv", fsep = "/"), 
          x = courseHierarchy, quote = c(modTitleColIndex))
write.csv(file = file.path(subDirPath, "modules_deleted.csv", fsep = "/"),
          x = DeletedModules, quote = c(modTitleColIndex))


######### Notify user and Clear the environment  #############
beepr::beep(sound = 10)   #notify user script is complete
Sys.sleep(time = 0.1)     #pause 1/10 sec
beepr::beep(sound = 10)
Sys.sleep(time = 0.1)
beepr::beep(sound = 10)

rm(list=ls())   #Clear environment variables
