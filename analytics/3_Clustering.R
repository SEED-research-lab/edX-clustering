## ===================================================== ##
# Title:        Clustering users based on boolean course module interaction ####
#
#
# Author(s):    Doipayan Roy, Taylor Williams
# Institution:  Purdue University
# 
# Description:  []
# 
# Package dependancies: readr, cluster, e1071, tcltk, beepr
#
# Changelog:
#     2017.04.13    Added gendered clickstream user choices
#                   Changed gap plots to only go up to 10 clusters (previously was set to 20 clusters)
#     2017.05.03.   Input files read from subdirectory
#                   Created output files placed into a seperate subdirectory
#     2017.05.03.   Put subdirectory and file checking code into functions
#     2017.05.08.   Code cleaning, header update
#                   Audio notification for user input and script completion
#                   Plot output files set to PDF (had been EPS)
#                   Output filenames and plot subtitles are now descriptive 
#                    (using the user provided description)
#     2017.05.10.   Fuzzy code updated to match updates above (which had only been to the k-means code)
#                   Added valid user input check for clustering technique selection
#     2017.05.11.   Extracted possible functions to external files
## ===================================================== ##



######### Clean the environment ########## 
rm(list=ls())   


######### Internal functions ########## 
#Function: Interactively select working directory (OS independant, but not available for RStudio Server)
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
WorkingDirectoryCheck <- function(expectedFile) {
  #set directory variables
  curDir <- getwd()
  
  
  if(file.exists(file.path(curDir, expectedFile))){
    #if file does exists in the current WD, exit the funtion returning TRUE
    return(TRUE)
  } else{
    #if the file does not exist in the current WD, return FALSE
    return(FALSE)
  }
}


# end of functions
## *************************************** #####
# begin script setup


######### Check for correct working directory ########## 
#check the current working direcotry, inform user if incorrect and stop running script
if(!WorkingDirectoryCheck(expectedFile = "1_extractModules.R")){
  cat("The current working directory is NOT CORRECT.  
      Please set it to the directory containing the R scripts before reruning script.\n")
  
  #have user set the working directory
  # beepr::beep(sound = 10)   #notify user to provide input
  # InteractiveSetWD()
  
  break
}


######### External function sourcing ########## 
#load external functions
source("R/file-structure-functions.R")




# end of script setup
## *************************************** #####
# beginning of script functionality


#start a timer to track how long the script takes to execute
start <-  proc.time() #save the time (to compute ellapsed time of loop)



######### User providing dataset details #####
#beepr::beep(sound = 10)   #notify user to provide input
cat("\nEnter a description of this datasest (to be included on graphs).
    (suggested format: [Data source, e.g., edX], [Course number, e.g., nano515x], [Data date, e.g., Data from 2016.11.18])")
dataSetDescription <- readline(prompt="Description: ");



######### Choose clustering technique #######################################################
#Choose clustering method (repeating to sanitize user input)
repeat{
  #beepr::beep(sound = 10)   #notify user to provide input
  clusterTypeSelection <- readline(prompt="Enter 1 for K-means clustering, 2 for Fuzzy means clustering: ");
  
  if(clusterTypeSelection == 1 || clusterTypeSelection == 2){  #valid clustering method selected
    #exit loop and continue script
    break
  }
  
  #repeat if none of the conditions were met (i.e., user input was invalid)
}


######### User selection of data set to process #####
#Choose clickstream data(sub)set (repeating to sanitize user input)
repeat{
  #beepr::beep(sound = 10)   #notify user to provide input
  userSetSelection <- readline(prompt="Enter 1 for all users, 2 for female users, 3 for male users: ");

  if(userSetSelection == 1){  #dataset: all users
    #check for datafile existance
    preprocessedDataFilePath <- FileExistCheck(subDir = "2_PreprocessingOutput", filename = "preprocessed_data.csv")
    #exit script if file not found, otherwise continue
    ifelse(preprocessedDataFilePath == FALSE, yes = return(), no = "")
    dataSetName <- "all users"
    break
  }
  else if(userSetSelection == 2){  #dataset: female users
    #check for datafile existance
    preprocessedDataFilePath <- FileExistCheck(subDir = "2_PreprocessingOutput", filename = "preprocessed_data_females.csv")
    #exit script if file not found, otherwise continue
    ifelse(preprocessedDataFilePath == FALSE, yes = return(), no = "")
    dataSetName <- "females"
    break
  }
  else if(userSetSelection == 3){  #dataset: male users
    #check for datafile existance
    preprocessedDataFilePath <- FileExistCheck(subDir = "2_PreprocessingOutput", filename = "preprocessed_data_males.csv")
    #exit script if file not found, otherwise continue
    ifelse(preprocessedDataFilePath == FALSE, yes = return(), no = "")
    dataSetName <- "males"
    break
  }

  #repeat if none of the conditions were met (i.e., user input was invalid)
}

## Read data and retain needed columns
#read in data
data <- readr::read_csv(preprocessedDataFilePath)

####data<-as.data.frame(data)
##retain relevant columns
data<-data[names(data) %in% c("temp_student_id","module_number","time")]
##Ordering dataframe by student_id column
data<-data[order(data$temp_student_id,decreasing=F),]


##Save the current working directory.  Set the output directory to the working directory. 
##  (wd will be restored at the end of the script)
    
  #save current working directory
  initialWD_save <- getwd()
  
  ## TW (2017.05.03): I'm trying to get this working from an external function
  # if(!exists("DirCheckCreate", mode="function")) source(file.path(getwd(), "analytics", "fun_DirCheckCreate.R", fsep = "/"))
  #call function to check for the existance of the subdirectory; create it if it doesn't exist
  subDirPath <- DirCheckCreate(subDir = "3_ClusteringOutput")
  
  #set working directory for the remainder of the script
  setwd(subDirPath)
## ===================================================== ##


######### Calculating number of unique module accesses for each student and saving to file########

cat("\nCalculating number of access events for each learner...")
access_list=c()
for(i in 1:length(unique(data$temp_student_id)))
{
  temp=subset(data,data$temp_student_id==i)
  access_list=c(access_list,nrow(temp))
}
cat("\nDone! Saving to file...")
data_final=data.frame(1:length(unique(data$temp_student_id)),access_list)
names(data_final)<-c("temp_student_id","number_accesses")
write.csv(data_final,"access_data.csv")
## ===================================================== ##


######### If k-means chosen ######################################################

if(clusterTypeSelection==1)
{
  ##Set label
  clusterTypeName <- "k-means clustering"
  
  ##Read access data file and delete irrelevant columns
  data <- read.csv("access_data.csv",header=T)
  data <- as.data.frame(data)
  data <- data[names(data) %in% c("temp_student_id","number_accesses")]
  
  ## **Generate elbow plot from access data ####
  cat("\nGenerating elbow plot...")
  elbow_plot_values <- c()
  for(k in 1:20)
  {
    K_m <- kmeans(data$number_accesses,centers=k,iter.max=50,algorithm="Lloyd")
    elbow_plot_values <- c(elbow_plot_values,K_m$betweenss/K_m$totss)
  }
  #set the range of x values to include in the plot
  x=1:20
  pdf.options(reset = TRUE)
  #set the PDF name
  pdf(paste0(dataSetDescription, ". ", dataSetName, ". ", "elbow_plot.pdf"))
  #set plot options (including descriptive subtitle)
  plot(x = x, y = elbow_plot_values, col="blue", type="l",
       xlim=c(0,20), ylim=c(0,1.2), xlab="Number of clusters", 
       ylab="Between cluster sum of squares / Total sum of squares", 
       main = paste0("Elbow plot (", dataSetName,")\n", 
                     clusterTypeName, "\n",
                     dataSetDescription))
  dev.off()
  print("Done!")

  
  ## **Generate gap plot from access data ####
  cat("\nGenerating gap plot...")
  gap_statistic=cluster::clusGap(as.matrix(data$number_accesses),K.max=10,FUN=kmeans,verbose=FALSE)
  gap_values=(as.data.frame(gap_statistic$Tab))$gap
  #set the range of x values to include in plot
  x=1:10
  pdf.options(reset = TRUE)
  #set the PDF name
  #Simple name (to delete): pdf("gap_plot.pdf")
  pdf(paste0(dataSetDescription, ". ", dataSetName, ". ", "gap_plot.pdf"))
  #set plot options (including descriptive subtitle)
  plot(x = x, y = gap_values, col="black", xlim = c(0,10), ylim = c(0,1),
       xlab="Number of clusters", ylab="Gap", type="l",
       main = paste0("Gap plot (", dataSetName,")\n", 
                     clusterTypeName, "\n",
                     dataSetDescription))
  #close the PDF creation connection
  dev.off()
  print("Done!")

  ## **Make recommendations for cluster number ####
  ##Make recommendation for number of clusters based on elbow plot (change less than 2)
  for(i in 3:10)
  {
    percent_inc=(elbow_plot_values[i]-elbow_plot_values[i-1])/elbow_plot_values[i-1]
    if(percent_inc<0.02)
    {
      print(paste("Recommendation for number of clusters using elbow plot:",i-1))
      break
    }
  }
  
  ##Make recommendation for number of clusters based on gap statistics (first peak in plot)
  optimal_clusters <- c()
  counter <- 0
  for(i in 2:19)    #??????for DR??? should this be from 2:9 now?
  {
    if(gap_values[i-1]<gap_values[i] & gap_values[i+1]<gap_values[i])
    {
      optimal_clusters=c(optimal_clusters,i)
      counter=counter+1
    }
    if(counter==2)
    {
      print(paste("Recommendation for number of clusters using gap statistics:",
                  optimal_clusters[1],"or",optimal_clusters[2]))
      break
    }
  }

  ##User input for number of clusters
  #beepr::beep(sound = 10)   #notify user to provide input
  K <- readline("Enter the desired number of clusters (maximum 10): ");
  K <- as.integer(K);
  
  ## **Reading access file and performing K-means on access data using user's choice of number of clusters ####
  data_preprocessed <- readr::read_csv(preprocessedDataFilePath)
  data_preprocessed <- data_preprocessed[names(data_preprocessed) %in% c("temp_student_id","module_number")]
  data_access <- read.csv("access_data.csv",header=T)
  data_access <- data_access[names(data_access) %in% c("temp_student_id","number_accesses")]
  data_access <- data_access[order(data_access$temp_student_id,decreasing=F),]
  K_m <- kmeans(data_access$number_accesses,centers=K,iter.max=100,algorithm="Lloyd")
  cluster_id <- K_m$cluster
  data_access <- cbind(data_access,cluster_id)
  data_access <- data_access[order(data_access$number_accesses,decreasing=F),]
  
  ## **Ordering clusters ####
  counter=1
  mean_accesses=c()
  cluster_order=c()
  for(i in 1:K)
  {
    temp=subset(data_access,data_access$cluster_id==i)
    mean_accesses=c(mean_accesses,mean(temp$number_accesses))
  }
  mean_accesses_sorted=sort(mean_accesses,decreasing=F)
  for(i in 1:K)
  {
    cluster_order=c(cluster_order,which(mean_accesses==mean_accesses_sorted[i]))
  }
  
  ## **Plotting clusters ####
  print("Plotting clusters...")
  x=1:length(unique(data_preprocessed$module_number))
  pdf.options(reset = TRUE)
  #set the pdf name (descriptive)
  pdf(paste0(dataSetDescription, ". ", dataSetName, ". ", "k-means_cluster_plot (", K, ").pdf"))
  #set the plot options (including descriptive subtitle)
  plot(x = 1,pch = ".",col="white",
       xlim = c(0,length(unique(data_preprocessed$module_number))),
       ylim = c(0,max(data_access$temp_student_id)),
       xlab = "Module number", ylab = "Users",
       main = paste0("Users clustered by courseware access (", dataSetName,")\n", 
                     clusterTypeName, " (", K, " clusters)\n",
                     dataSetDescription))
  par(new=T)
  for(k in cluster_order)
  {
    temp=subset(data_access,data_access$cluster_id==k)
    for(j in 1:nrow(temp))
    {
      stud_id=temp$temp_student_id[j]
      temp2=subset(data_preprocessed,data_preprocessed$temp_student_id==stud_id)
      access_list=rep(NA,length(unique(data_preprocessed$module_number)))
      for(i in 1:length(access_list))
      {
        if(i %in% temp2$module_number)
        {
          access_list[i]=i
        }
      }
      if(k==1)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="red",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==2)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="blue",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==3)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="black",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==4)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="green",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==5)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="yellow",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==6)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="pink",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==7)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="orange",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==8)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="brown",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==9)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="darkgrey",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==10)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="cyan",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
    }
  }
  dev.off()
  print("Done!")

} else if(clusterTypeSelection==2)
{

######### If fuzzy means chosen #####################################################

  ##Set label
  clusterTypeName <- "Fuzzy clustering"
  
  ##Read access data file and delete irrelevant columns
  data <- read.csv("access_data.csv",header=T)
  data <- as.data.frame(data)
  data <- data[names(data) %in% c("temp_student_id","number_accesses")]
    
  ## **Generate gap plot from access data ####
  cat("\nGenerating gap plot...")
  gap_statistic=cluster::clusGap(as.matrix(data$number_accesses),K.max=10,FUN=kmeans,verbose=FALSE)   #??for DR??? should kmeans be being used here?  We're in the fuzzy clustering section
  gap_values=(as.data.frame(gap_statistic$Tab))$gap
  #set the range of x values to include in the plot
  x=1:10
  #set the PDF name
  pdf(paste0(dataSetDescription, ". ", dataSetName, ". ", "gap_plot.pdf"))
  #set plot options (including descriptive subtitle)
  plot(x = x,y = gap_values,col="black",xlim=c(0,10),ylim=c(0,1),
       xlab="Number of clusters",ylab="Gap",type="l",
       main = paste0("Gap plot (", dataSetName,")\n", 
                     clusterTypeName, "\n",
                     dataSetDescription))
  #close the PDF creation connection
  dev.off()
  print("Done!")
  
  ## **Make recommendation for number of clusters based on gap statistics (first peak in plot) ####
  optimal_clusters=c()
  counter=0
  for(i in 2:9)
  {
    if(gap_values[i-1]<gap_values[i] & gap_values[i+1]<gap_values[i])
    {
      optimal_clusters=c(optimal_clusters,i)
      counter=counter+1
    }
    if(counter==2)
    {
      print(paste("Recommendation for number of clusters using gap statistics:",optimal_clusters[1],"or",optimal_clusters[2]))
      break
    }
  }

  ##User input for number of clusters
  #beepr::beep(sound = 10)   #notify user to provide input
  K<-readline("Enter the desired number of clusters (maximum 10): ");
  K<-as.integer(K);
  
  ## **Reading access file and performing K-means on access data using user's choice of number of clusters ####
  data_preprocessed <- readr::read_csv(preprocessedDataFilePath)
  data_preprocessed <- data_preprocessed[names(data_preprocessed) %in% c("temp_student_id","module_number")]
  data_access <- read.csv("access_data.csv",header=T)
  data_access <- data_access[names(data_access) %in% c("temp_student_id","number_accesses")]
  data_access <- data_access[order(data_access$temp_student_id,decreasing=F),]
  C_m <- e1071::cmeans(as.matrix(data_access$number_accesses),centers=K)
  cluster_id <- C_m$cluster
  data_access <- cbind(data_access,cluster_id)
  data_access <- data_access[order(data_access$number_accesses,decreasing=F),]
    
  ## **Ordering clusters ####
  counter=1
  mean_accesses=c()
  cluster_order=c()
  for(i in 1:K)
  {
    temp=subset(data_access,data_access$cluster_id==i)
    mean_accesses=c(mean_accesses,mean(temp$number_accesses))
  }
  mean_accesses_sorted=sort(mean_accesses,decreasing=F)
  for(i in 1:K)
  {
    cluster_order=c(cluster_order,which(mean_accesses==mean_accesses_sorted[i]))
  }
  
  ## **Plotting clusters ####
  print("Plotting clusters...")
  x=1:length(unique(data_preprocessed$module_number))
  pdf.options(reset = TRUE)
  #set the pdf name (descriptive)
  pdf(paste0(dataSetDescription, ". ", dataSetName, ". ", "fuzzy_cluster_plot (", K, ").pdf"))
  #set the plot options (including descriptive subtitle)
  plot(1,pch=".",col="white",xlim=c(0,length(unique(data_preprocessed$module_number))),
       ylim=c(0,max(data_access$temp_student_id)),xlab="Module number",ylab="Users",
       main = paste0("Users clustered by courseware access (", dataSetName,")\n", 
                     clusterTypeName, " (", K, " clusters)\n",
                     dataSetDescription))
  par(new=T)
  for(k in cluster_order)
  {
    temp=subset(data_access,data_access$cluster_id==k)
    for(j in 1:nrow(temp))
    {
      stud_id=temp$temp_student_id[j]
      temp2=subset(data_preprocessed,data_preprocessed$temp_student_id==stud_id)
      access_list=rep(NA,length(unique(data_preprocessed$module_number)))
      for(i in 1:length(access_list))
      {
        if(i %in% temp2$module_number)
        {
          access_list[i]=i
        }
      }
      if(k==1)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="red",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==2)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="blue",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==3)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="black",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==4)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="green",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==5)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="yellow",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==6)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="pink",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==7)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="orange",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==8)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="brown",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==9)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="darkgrey",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==10)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="cyan",
             xlim=c(0,length(unique(data_preprocessed$module_number))),
             ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
    }
  }
  dev.off()
  print("Done!")
} else
{
  print("Invalid choice! Please enter 1 or 2...")
}



## Restore the working directory from when the script began
setwd(initialWD_save)



######### Notify user and Clear the environment  #############
# beepr::beep(sound = 10)   #notify user script is complete
# Sys.sleep(time = 0.1)     #pause 1/10 sec
# beepr::beep(sound = 10)
# Sys.sleep(time = 0.1)
# beepr::beep(sound = 10)

#print the amount of time the script required
cat("\n\n\nScript processing time details (in sec):\n")
print(proc.time() - start)

rm(list=ls())   #Clear environment variables

