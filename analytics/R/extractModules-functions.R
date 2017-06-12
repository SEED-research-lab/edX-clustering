## ===================================================== ##
# Title:        Functions for module ordering in edX clustering pipeline ####
#
#
# Author(s):    Taylor Williams
# Institution:  Purdue University
# 
# Project:      EdX data pipeline for clustering analytics
# 
# Description:  []
# 
# Package dependancies: [none]
#
# Changelog:
#     2017.05.11.    initial function extraction from 1_extractModules.R
## ===================================================== ##



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

