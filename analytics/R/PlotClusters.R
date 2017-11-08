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

PlotClusters <- function(clusterTypeName, K, data_preprocessed, data_access, access_list, cluster_order, dataSetName, dataSetDescription) {
  ## **Plotting clusters ####
  ## Arguments
  ##    clusterTypeName : name of clustering technique (e.g., k-means or c-means)
  ##    data_preprocessed : 
  ##    data_access : 
  ## Return
  ##    none

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
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="red",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==2)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="blue",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==3)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="black",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==4)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="green",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==5)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="yellow",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==6)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="pink",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==7)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="orange",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==8)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="brown",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==9)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="darkgrey",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter <- counter+1
      }
      else if(k==10)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="cyan",
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