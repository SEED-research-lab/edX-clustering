edX Course Usage clustering Pipeline
==============
_&lt;<https://tzwilliams.github.io/edX-clustering/>&gt;_
_&lt;<https://github.com/tzwilliams/edX-clustering>&gt;_

This project is still being actively developed.  As is, the code is highly experimental and will need to be cleaned before production use.

## Environment Setup
1) At presant this pipeline is designed to work within the RStudio IDE (https://www.rstudio.com/).  You will need to either install R and RStudio or use an online version of RStudio.  Alternatively, an online version is available for free on nanoHUB.org (https://nanohub.org/resources/rstudio); however, the following instructions may need to be slightly modified if using the online version.

## Instructions for using the package
1)	Create a working folder.  
1)  Download the source code from gitHUB.  Extract it into the working directory created above. 
1)	You will need access to 3 edX datafiles.  You may move these into your working directory if you wish (although this is not necessisary): (UniversityX)-(course_name)-xxxxxx-courseware_studentmodule-prod-analytics.sql, (UniversityX)-(course_name)-xxxxxx-auth_userprofile-prod-analytics.sql, and (UniversityX)-(course_name)-xxxxxx-course_structure-prod-analytics.json
1)  Open the "edX-clustering.Rproj" file found within the root direcoty of the gitHUB download.  This should launch RStudio (if working in the desktop).  You will see RStudio informing you of packrat packages being installed--this install may take some time the first time you open the project.

1)  Execute the "PipelineInitator.R" file either by using the command `source("PipelineInitator.R")` or by opening the file in RStudio and clicking on ‘Source file...’ in the ‘Code’ menu.  Follow the directions on the console (you will be asked for the location to the data file(s).  Be aware some steps may require multiple minutes to complete.
1)  When asked in the Console, enter a description of the course and data (e.g., edX, naono515x, Data from 2015.11.15), select the clustering technique, and selete the user group to cluster.  The program will recommend a number of clusters based on the elbow plot and/or gap statistics; choose the number of clusters. The cluster plot will be generated using the clustering technique and number of clusters desired by the user.  The resulting cluster visulizations will be in the subdirectory '/analytics/3_ClusteringOutput' within your working directory.



### Notes:
1)	Depending on the size of the clickstream file, number of modules and clustering algorithm chosen, generating the cluster plots from raw data can take anywhere from a few minutes to more than 20 minutes.
1)	The code is designed such that if there are modules missing in module_order_file, the clustering will ignore the missing modules. In other words, if, by error, the module_order_file does not have every module in the course, the missing modules will be not be featured in the clustering.
1)  The code is designed such that a module will not be included in the clustering if it was never clicked on by any user.
1)	There are some internal parameters in the clustering process that have been set to static values in the code. Normally, the user will not have the freedom to change these values.  Choosing these parameters require a good understanding of the data and clustering techniques. Should the user want to change these values, it can be done by modifying relevant parts of the code.


## Copyright
 Copyright 2017 Krishna Madhavan, Kerrie Douglas, Doipayan Roy, and Taylor Williams
 
     Licensed under the Apache License, Version 2.0 (the "License");
     you may not use this file except in compliance with the License.
     You may obtain a copy of the License at
     
     http://www.apache.org/licenses/LICENSE-2.0
     
     Unless required by applicable law or agreed to in writing, software
     distributed under the License is distributed on an "AS IS" BASIS,
     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     See the License for the specific language governing permissions and
     limitations under the License.
