edX Course Usage clustering Pipeline
==============
_&lt;<https://github.com/tzwilliams/edX-clustering>&gt;_

## Environment Setup
1) At presant this pipeline is designed to work within the RStudio IDE (https://www.rstudio.com/).  You will need to either install R and RStudio or use an online version of RStudio.  An online version is available for free on nanoHUB.org (https://nanohub.org/resources/rstudio).

## Instructions for using the package
1)	Create a working folder.  
1)  Download the source code from gitHUB.  Extract it into the working directory created above. 
1)	You will need access to 3 edX datafiles.  You may move these into your working directory if you wish (although this is not necessisary): (UniversityX)-(course_name)-xxxxxx-courseware_studentmodule-prod-analytics.sql, (UniversityX)-(course_name)-xxxxxx-auth_userprofile-prod-analytics.sql, and (UniversityX)-(course_name)-xxxxxx-course_structure-prod-analytics.json
1)  Open the "edX-clustering.Rproj" file found within the root direcoty of the gitHUB download.  This should launch RStudio (if working in the desktop).  You will see RStudio informing you of packrat packages being installed--this install may take some time the first time you open the project.

1)	Within RStudio, set the working directory to the "analytics" directory within the directory you created above. This can be done by running the following command in the Console window of RStudio: `setwd(“/path_to_work_directory_analytics_folder”)`.  
1)	Run the 1_extractModules.R file. This can be done within the Console using the command `source("1_extractModules.R")`. You may also open the file and click on ‘source’ under the ‘Code’ tab.  Follow the directions on the console (you will be asked for the location to the file(s) above).  The output files will be in a newly created subdirectory matching the .R filename.
1)	Run the 2_Preprocessing.R file. Follow the same procedure as above.
1)	Run the 2b_genderedSubsets.R file. Follow the same procedure as above.
1)	Run the 3_Clustering.R file. Follow the same procedure as above. Choose the clustering technique and user group to cluster. Based on the elbow plot and/or gap statistics, choose the number  of clusters. The cluster plot will be generated using the clustering technique and number of clusters desired by the user.  The resulting cluster visulizations will be in the corresponding subdirectory within your working directory.

### Notes:
1)	Depending on the size of the clickstream file, number of modules and clustering algorithm chosen, generating the cluster plots from raw data can take anywhere from a few minutes to more than 20 minutes.
1)	The code is designed such that if there are modules missing in module_order_file, the clustering will ignore the missing modules. In other words, if, by error, the module_order_file does not have every module in the course, the missing modules will be not be featured in the clustering.
1)	There are plenty of internal parameters in the clustering process that has been set to static values in the code. Normally, the user will not have the freedom to change these values.  Choosing these parameters require a good understanding of the data and clustering techniques. Should the user want to change these values, it can be done by modifying relevant parts of the code.


## Copyright
 Copyright 2017 Krishna Madhavan
 
     Licensed under the Apache License, Version 2.0 (the "License");
     you may not use this file except in compliance with the License.
     You may obtain a copy of the License at
     
     http://www.apache.org/licenses/LICENSE-2.0
     
     Unless required by applicable law or agreed to in writing, software
     distributed under the License is distributed on an "AS IS" BASIS,
     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     See the License for the specific language governing permissions and
     limitations under the License.
