## ===================================================== ##
# Title:        Function: display percent complete to console ####
# Project:      edX data pipeline for course user clustering analytics
#               https://tzwilliams.github.io/edX-clustering/
#
# Copyright 2018 Taylor Williams
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
# Authors:      Taylor Williams
# Affiliation:  Purdue University
#
# Description: Look at the dataframe being processed and display percent 
#               complete message for user. To be called from within a lengthy 
#               processing loop.  The code to copy into the loop is included as
#               a comment at the beginning of the function below.
#
#
# File input stack (R: required; O: optional): none
#
# File outputs: none
#
# Package dependencies: none
#
# Changelog:
#     2018.04.22. Initial code
#
# Feature wish list:  (*: planned but not complete)
#                   *
## ===================================================== ##


DisplayPercentComplete <- function(dataFrame, iCount, pct, displayText = ""){ 
 
  ## Add the following code in the loop where this function is being called:
    #     #| print completion progress to console   ####
    # #durring first iteration, create progress status variables for main processing loop
    # if(i==1)
    # {
    #   iCount <- 0 #loop counter for completion updates
    #   pct <- 0  #percentage complete tracker
    # }
    # 
    # #print function
    # updateVars <- DisplayPercentComplete(dataFrame = [update], iCount, pct, displayText = "[UPDATE with what process is happening]: ")
    # 
    # #update status variables (for next iteration)
    # iCount <- updateVars$iCount
    # pct <- updateVars$pct
    # 
    # #print update
    # cat(updateVars$toPrint)
  #########
 
  
  
  #| print completion progress to console   ####
  if(length(dataFrame) > 1)
  {

      iCount <- iCount + 1  
      if(length(dataFrame) < 10 &
         iCount%%as.integer((length(dataFrame))/1) == 0 & 
         pct <= 100)
      {
        pct <- pct + 1
        ifelse(pct>100, yes = (pct <- 100), no = "")  #cap pct to 100
        toPrint <- paste0("\r", displayText, pct, "% complete")
      }else if(length(dataFrame) < 100 & 
         iCount%%as.integer((length(dataFrame))/10) == 0 & 
         pct <= 100)
      {
        pct <- pct + 10
        ifelse(pct>100, yes = (pct <- 100), no = "")  #cap pct to 100
        toPrint <- paste0("\r", displayText, pct, "% complete")
      }else if(length(dataFrame) >= 100 &
               iCount%%as.integer((length(dataFrame))/100) == 0 & 
               pct <= 100)
      {
        pct <- pct + 1
        ifelse(pct>100, yes = (pct <- 100), no = "")  #cap pct to 100
        toPrint <- paste0("\r", displayText, pct, "% complete")
      }else
      {
        toPrint <- paste0("\r", displayText)
      }
      
    # }
  }else{
    toPrint <- paste0("\r", "")
  }
  
  #return both variables in a list (to be extracted after the function call)
    #extract using two lines like the following:
    # var1 <- returnedList$var1
    # var2 <- returnedList$var2
  return(list("iCount" = iCount,"pct" = pct, "toPrint" = toPrint))
}
  