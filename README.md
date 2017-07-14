edX Course Usage Clustering Pipeline
==============
Authors: Krishna Madhavan (1), Kerrie Douglas (2), Doipayan Roy (2), and Taylor Williams (2)

Affiliations: (1) Microsoft, (2) Purdue University

_&lt;<https://tzwilliams.github.io/edX-clustering/>&gt;_
_&lt;<https://github.com/tzwilliams/edX-clustering>&gt;_

Please note that this project is still under active development.  As is, the code is highly experimental and will need to be cleaned before production use.

## Environment Setup
1) At present this pipeline is designed to work within the RStudio IDE (https://www.rstudio.com/).  You will need to either install R and RStudio or use an online version of RStudio.  Alternatively, an online version is available for free on nanoHUB.org (https://nanohub.org/resources/rstudio); however, the following instructions may need to be slightly modified if using the online version.

## Instructions for using the package
1)	Create a working folder.  

1)  Download the source code from GitHub as a zip file.  Extract the zip file into the working directory created above. 

1)	You will additionally need access to 3 edX datafiles.  You may move these into your working directory if you wish (although this is not necessary).  The generic filename structures of the files you need are: (UniversityX)-(course_name)-xxxxxx-courseware_studentmodule-prod-analytics.sql, (UniversityX)-(course_name)-xxxxxx-auth_userprofile-prod-analytics.sql, and (UniversityX)-(course_name)-xxxxxx-course_structure-prod-analytics.json

1)  Open the "edX-clustering.Rproj" file found within the root directory of the gitHUB download.  This should launch RStudio (if working in the desktop).  You will see RStudio informing you of packrat packages being installed--this install may take some time the first time you open the project.

1)  Execute the "PipelineInitator.R" file either by using the RStudio Console command `source("PipelineInitiator.R")` or by opening the file in RStudio and clicking on ‘Source file...’ in the ‘Code’ menu.  This R script will sequentially call the individual R scripts which constitute the pipeline.  Follow the directions on the console (you will be asked for the location of the data files--more than once for some files).  Be aware some processing steps may require multiple minutes to complete.

1)  When asked in the Console, enter a description of the course and data (e.g., edX, naono515x, Data from 2015.11.15), select the clustering technique, and select the user group to cluster.  The program will recommend one or more clusters based on the elbow plot and/or gap statistics; choose the number of clusters. The cluster plot will be generated using the clustering technique and number of clusters desired by the user.  The resulting cluster visualizations will be in the subdirectory '/analytics/3_ClusteringOutput' within your working directory.

1)  If the cluster chart was successfully created `**** Cluster graph created! ****` will display in the Console.

1)  The program will ask if you would like to create additional cluster graphs from this data (e.g., using a different cluster method, a different user subset, or a different number of clusters).  The cluster code (`3_Clustering.R`) will execute again if told 'Y', otherwise the pipeline will complete and exit.

1)  If the pipeline completes successfully, `**** Clustering pipeline complete! ****` will display in the Console.



### Notes:
1)  If you encounter an error before the script completes you will need to rerun the `PipelineInitiator.R' script after resetting your working directory to the directory containing the `PipelineInitiator.R' file using the `setwd("working_directory_path")` command (replace "working_directory_path" with the correct path within quotes).

1)  If you see an error like `Error in loadNamespace(name) : there is no package called ‘e1071’`, a package did not properly install.  To correct this type `install.packages("e1071")` in the Console (replacing "e1071"--quotes included--with the package indicated in the error message).

1)	Depending on the size of the clickstream file, number of modules and clustering algorithm chosen, generating the cluster plots from raw data can take anywhere from a few minutes to more than 20 minutes.

1)	The code is designed such that if there are modules missing in the module_order_file.csv file, the clustering will ignore the missing modules. In other words, if, by error, the module_order_file.csv does not have every module in the course, the missing modules will not be included in the clustering graph.

1) 	The code is designed such that a module will not be included in the clustering graph if it was never clicked on by any user.  Some courses have modules which cannot be interacted with by the users. 

1)	There are some internal parameters in the clustering process that have been set to static values in the code. Normally, the user will not have the freedom to change these values.  Choosing these parameters require a good understanding of the data and clustering techniques. Should the user want to change these values, it can be done by modifying relevant parts of the code.


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
