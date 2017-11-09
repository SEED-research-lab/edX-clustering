## ===================================================== ##
# Title:        Plotting clusters ####
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
# Description:  Plotting and saving the usage clusters 
# 
# Package dependancies: []
#
# Changelog:
#     2017.11.08.    initial function extraction from pipeline with minor updates
## ===================================================== ##

PlotClusters <- function(clusterTypeName, K, data_preprocessed, data_access, cluster_order, dataSetName, dataSetDescription) {
  ## **Plotting clusters ####
  ## Arguments
  ##    clusterTypeName : name of clustering technique (e.g., k-means or c-means)
  ##    data_preprocessed : 
  ##    data_access : 
  ## Return
  ##    none
      
  # SETTINGS
  # check number of users being plotted and adjust settings appropriately  
  #   "-": dash
  #   ".": point, but can't scale
  #   15 : filled square
  #   16 : filled circle
  #   18 : filled diamond
  # (prior versions of the plotting function used `pointType <- "."`` and `pointScalingFactor <- 1``)
  if(nrow(data_access) < 600) {
    pointType <- 18   # diamond helps fill the vertical space when the number of users is lower
    pointScalingFactor <- .3
  }else{
    #diamonds are very faint at this scale
    pointType <- 18
    pointScalingFactor <- .1
    
    # #dash are very faint at this scale,  also, dashs mislead the horizontal fill of the data (indicate more modules were interacted with than were)
    # pointType <- "-"  # dash works better for larger number of users since it is less likely to overlap vertically
    # pointScalingFactor <- .3  #.1 gives no overlap with 1600 users but is very faint
  }
  

  counter <- 1
  
  print("Plotting clusters...")
  x=1:length(unique(data_preprocessed$module_number))
  pdf.options(reset = TRUE)
  #set the pdf name (descriptive)
  pdf(paste0(dataSetDescription, ". ", dataSetName, ". ", clusterTypeName, " plot (", K, ").pdf"))
  #set the plot options (including descriptive subtitle)
  plot(x = 1,pch = ".",col = "white",
       xlim = c(0,length(unique(data_preprocessed$module_number))),
       ylim = c(0,max(data_access$temp_student_id)),
       xlab = "Module number", ylab = "Users",
       main = paste0("Users clustered by course module interaction (", dataSetName,")\n", 
                     clusterTypeName, " (", K, " clusters)\n",
                     dataSetDescription))
  par(new=T)
  for(k in cluster_order)
  {
    cat("\nPlotting cluster:", k)
    #Subset of students belonging to cluster_id=k
    temp <- subset(data_access,data_access$cluster_id==k)
    
    #for loop iterating over every clickstream event in subset obtained above
    for(j in 1:nrow(temp))
    {
      stud_id <- temp$temp_student_id[j]
      temp2 <- subset(data_preprocessed,data_preprocessed$temp_student_id==stud_id)
      access_list <- rep(NA,length(unique(data_preprocessed$module_number)))
      for(i in 1:length(access_list))
      {
        if(i %in% temp2$module_number)
        {
          access_list[i] <- i
        }
      }
      
      #plot each cluster (from 1 to a maximum of 10) using a different color
      ##TW:??: ask DR why the colors end up randomized.  I think it'd be better if they were consistent across graphs
      if(k==1)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="red", 
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==2)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="blue",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==3)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="black",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==4)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="green",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==5)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="darkgrey",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==6)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="pink",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==7)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="orange",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==8)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="brown",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==9)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="yellow",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==10)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),col="cyan",
             pch=pointType, cex = pointScalingFactor, 
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
    }
  }
  dev.off()
  cat("\nDone plotting!")
}