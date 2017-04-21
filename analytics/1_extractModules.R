##################################
# Title:        Extraction tool for edX MOOC module sequence (from JSON data file)
#
#
# Author(s):    Taylor Williams
# Institution:  Purdue University
# 
# Description:  For their course sequencing, edX provides a jumbled (yet structured) JSON file. 
#               As provided it is not in a useful form for our analyzing user engagement with the course.  
#               This algorythm creates an ordered csv file with 
#               (1) the cryptic edX Module_ID, 
#               (2) the provided human-readable text description of the module, and 
#               (3) an ordinal listing of the children of each module--that is 
#                   a "0" for the course module, 
#                   a "1" for chapter level modules, 
#                   a "2" for the chapter's children, 
#                   a "3" for the chapter's children's children, and so on.
# 
# Package dependancies: jsonlite, readr
#
# Changelog:
# rev2017a    10 Mar. 2017    Initial distributed version (emailed to Doipayan)
# rev2017b    10 Mar. 2017    Minor updates including header comments
#                             Distributed internally to MOOC research group through Dropbox
# rev2017c    10 Mar. 2017    Transitioned code identifying "course" as the highest level 
#                               (rather than the previously used "chapter"). This was done to (1) clean up 
#                               the code logic and (2) to take advanage of the chapters being properly 
#                               ordered in the "course" module.
#                             Modified output csv filename to contain the origional JSON filename. 
# rev2017d    22 Mar. 2017    Minor updates to improve clarity
# rev2017e    30 Mar. 2017    Removed all modules from courseHierarchy that were never clicked by ANY user
# rev2017f    05 Apr. 2017    Automated generation of module_order_file.csv to feed directly into Preprossing.r. 
#                             Saving deleted modules into a seperate csv file for examinateion
# rev2017g    11 Apr. 2017    Changed file selection to a GUI file browser. 
#                             Direct read in from sql clickstream file eliminates the external conversion from SQL to CSV
# rev2017h    20 Apr. 2017    Fixed csv export bug if a module title contains a comma 
#                             Added section headings
##################################

## Setup and data import##########
#IMPORTANT: Set your working directory
  
#Locate the JSON course structure data file you want processed
print("Select the JSON course structure file. It should end with 'course_structure-prod-analytics.json'")
filenameJSON <- file.choose()

#import the JSON data file
data <- jsonlite::fromJSON(filenameJSON)


#Locate the clickstream data file you want processed
print("Select the SQL clickstream data file. It should end with 'courseware_studentmodule-prod-analytics.sql'")
filenameClickstream <- file.choose()

## Identify course level module #############

#copy the module names into a seperate variable
moduleNames <- names(data)

#create variable to track which level the module should be sorted at (top, child, grandchild, etc)
hierarchicalLvl <- 0
#build empty matrix to store hierarchy for entire course
courseHierarchy <- matrix(, nrow = 0, ncol = 3, byrow = FALSE)


#Check for category type "course" for each module in the file; 
#   save matching module into the courseHierarchy data structure
for(i in 1:length(moduleNames)){
  if(data[[moduleNames[i]]]["category"] == "course"){
    courseModule <- moduleNames[i]
    courseHierarchy <- c(moduleNames[i], data[[moduleNames[i]]][["metadata"]]["display_name"], hierarchicalLvl)
  }
}



## FUNCTION: Recursive Child Search ############  
            
#recursive function to build the course heirarchy
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

## Initiate recursive search ###############


#initalize variables and initate recursive searching for children

#set the current hierarchy level
hierarchicalLvl <- 0  #course level
#conduct the recursive child search for each of the chapter level modules. Concatenate the results onto courseHierarchy
courseHierarchy <- ChildSearch(data, courseHierarchy, courseModule, hierarchicalLvl+1) 


## Remove unaccessed modules ###############

#convert to a data frame so we can use the "$" notation
courseHierarchy <- as.data.frame(courseHierarchy)
names(courseHierarchy) <- c("module_id","module_title","module_hierarchy_level")



#read in the clickstream data 
raw_data <- readr::read_tsv(filenameClickstream)


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


##Write data to files ###############

#identify and save the column number index for the module titles 
#   (To be used later in forcing quotes around the title strings.  Needed in case a module title contains a comma.)
modTitleColIndex <- grep("module_title", colnames(courseHierarchy))

#converting courseHierarchy to a matrix to be able to write to CSV (write.csv() will not accept a data frame)
courseHierarchy <- as.matrix(courseHierarchy)
DeletedModules <- as.matrix(DeletedModules)

#write a CSV file for the next step in processing.  (Also write CSV file of those modules which were removed.)
#   quotes are forced around the module title names in case one contains a comma
write.csv(file = "module_order_file.csv", x = courseHierarchy, quote = c(modTitleColIndex))
write.csv(file = "modules_deleted.csv", x = DeletedModules, quote = c(modTitleColIndex))


## Clear the environment  #############
rm(list=ls())
