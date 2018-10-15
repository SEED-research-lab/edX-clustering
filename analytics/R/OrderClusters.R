## ===================================================== ##
# Title:        Determine cluster order ####
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
# Description:  Ordering clusters in decreasing order of accesses, heaviest user cluster comes first
# 
# Package dependancies: []
#
# Changelog:
#     2017.11.08.    initial function extraction from pipeline with minor updates
## ===================================================== ##

OrderClusters <- function(data_access, K) {
  # Ordering clusters in decreasing order of accesses, heaviest user cluster comes first and so on
  #  mean_accesses is a list of the average number of accesses per user in each cluster
  #  cluster_order contains the cluster_id's ordered in increasing order of access activity
  # arguments: 
  #   data_access : 
  #   K : number of clusters
  # return:
  #   cluster_order : the cluster ordering from most engaged to least

  counter <- 1
  mean_accesses <- c()  # list of the average number of accesses per user in each cluster
  cluster_order <- c()  # cluster_id's ordered in increasing order of access activity
  
  for(i in 1:K)
  {
    temp <- subset(data_access,data_access$cluster_id==i)
    mean_accesses <- c(mean_accesses,mean(temp$number_accesses))
  }
  mean_accesses_sorted <- sort(mean_accesses,decreasing=F)
  
  
  for(i in 1:K)
  {
    cluster_order <- c(cluster_order,which(mean_accesses==mean_accesses_sorted[i]))
  }
  
  return(cluster_order)
}
