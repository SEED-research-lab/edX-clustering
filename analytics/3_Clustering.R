
#ChangeLog
# 2017.04.13    Added gendered clickstream user choices
#               Changed gap plots to only go up to 10 clusters (previously was set to 20 clusters)


##########Reading preprocessed file, converting to dataframe object, eliminating irrelevant columns############################################################################################################################

##Choose clickstream data(sub)set (repeating to sanitize user input)
repeat{
  n<-readline(prompt="Enter 1 for all users, 2 for female users, 3 for male users: ");
  
  if(n == 1){
    data<-readr::read_csv("preprocessed_data.csv")
    break
  }
  else if(n == 2){
    data<-readr::read_csv("preprocessed_data_females.csv")
    break
  } 
  else if(n == 3){
    data<-readr::read_csv("preprocessed_data_males.csv")
    break
  }

  #repeat if none of the conditions were met (i.e., user input was invalid)
}

####data<-as.data.frame(data)
##retain relevant columns
data<-data[names(data) %in% c("temp_student_id","module_number","time")]
##Ordering dataframe by student_id column
data<-data[order(data$temp_student_id,decreasing=F),]

###############################################################################################################################################################################################################################

##########Calculating number of unique module accesses for each student and saving to file#####################################################################################################################################

print("Calculating number of access events for each learner...")
access_list=c()
for(i in 1:length(unique(data$temp_student_id)))
{
  temp=subset(data,data$temp_student_id==i)
  access_list=c(access_list,nrow(temp))
}
print("Done! Saving to file...")
data_final=data.frame(1:length(unique(data$temp_student_id)),access_list)
names(data_final)<-c("temp_student_id","number_accesses")
write.csv(data_final,"access_data.csv")
rm(list=ls())

###############################################################################################################################################################################################################################

##########Choose clustering technique##########################################################################################################################################################################################

n<-readline(prompt="Enter 1 for K-means clustering, 2 for Fuzzy means clustering: ");
n<-as.integer(n);

###############################################################################################################################################################################################################################

##########If choice is K means#################################################################################################################################################################################################

if(n==1)
{
  ##Read access data file and delete irrelevant columns
  data<-read.csv("access_data.csv",header=T)
  data<-as.data.frame(data)
  data<-data[names(data) %in% c("temp_student_id","number_accesses")]
  
  ##Generate elbow plot from access data
  print("Generating elbow plot...")
  elbow_plot_values=c()
  for(k in 1:20)
  {
    K_m=kmeans(data$number_accesses,centers=k,iter.max=50,algorithm="Lloyd")
    elbow_plot_values=c(elbow_plot_values,K_m$betweenss/K_m$totss)
  }
  x=1:20
  setEPS()
  postscript("elbow_plot.eps")
  plot(x,elbow_plot_values,col="blue",xlim=c(0,20),ylim=c(0,1.2),xlab="Number of clusters",ylab="Between cluster sum of squares / Total sum of squares",main="Elbow plot",type="l")
  dev.off()
  print("Done!")
  
  ##Generate gap plot from access data
  print("Generating gap plot...")
  gap_statistic=cluster::clusGap(as.matrix(data$number_accesses),K.max=10,FUN=kmeans,verbose=FALSE)
  gap_values=(as.data.frame(gap_statistic$Tab))$gap
  x=1:10
  setEPS()
  postscript("gap_plot.eps")
  plot(x,gap_values,col="black",xlim=c(0,10),ylim=c(0,0.5),xlab="Number of clusters",ylab="Gap",main="Gap plot",type="l")
  dev.off()
  print("Done!")
  
  ##Make recommendation for number of clusters based on elbow plot (change less than 2%)
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
  optimal_clusters=c()
  counter=0
  for(i in 2:19)
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
  rm(list=ls())
  
  ##User input for number of clusters
  K<-readline("Enter the desired number of clusters (maximum 10): ");
  K<-as.integer(K);
  
  ##Reading access file and performing K-means on access data using user's choice of number of clusters
  data_preprocessed<-read.csv("preprocessed_data.csv",header=T)
  data_preprocessed<-data_preprocessed[names(data_preprocessed) %in% c("temp_student_id","module_number")]
  data_access<-read.csv("access_data.csv",header=T)
  data_access<-data_access[names(data_access) %in% c("temp_student_id","number_accesses")]
  data_access=data_access[order(data_access$temp_student_id,decreasing=F),]
  K_m<-kmeans(data_access$number_accesses,centers=K,iter.max=100,algorithm="Lloyd")
  cluster_id=K_m$cluster
  data_access=cbind(data_access,cluster_id)
  data_access<-data_access[order(data_access$number_accesses,decreasing=F),]
  
  ##Ordering clusters
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
  
  ##Plotting clusters
  print("Plotting clusters...")
  x=1:length(unique(data_preprocessed$module_number))
  setEPS()
  postscript("Cluster_plot_kmeans.eps")
  plot(1,pch=".",col="white",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab="Module number",ylab="Users",main="Users clustered by courseware access")
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
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="red",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==2)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="blue",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==3)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="black",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==4)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="green",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==5)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="yellow",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==6)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="pink",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==7)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="orange",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==8)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="brown",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==9)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="darkgrey",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==10)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="cyan",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
    }
  }
  dev.off()
  print("Done!")
  rm(list=ls())
  
} else if(n==2)
{

##########If choice is fuzzy means#############################################################################################################################################################################################

  ##Read access data file and delete irrelevant columns
  data<-read.csv("access_data.csv",header=T)
  data<-as.data.frame(data)
  data<-data[names(data) %in% c("temp_student_id","number_accesses")]
    
  ##Generate gap plot from access data
  print("Generating gap plot...")
  gap_statistic=cluster::clusGap(as.matrix(data$number_accesses),K.max=10,FUN=kmeans,verbose=FALSE)
  gap_values=(as.data.frame(gap_statistic$Tab))$gap
  x=1:10
  setEPS()
  postscript("gap_plot.eps")
  plot(x,gap_values,col="black",xlim=c(0,10),ylim=c(0,0.5),xlab="Number of clusters",ylab="Gap",main="Gap plot",type="l")
  dev.off()
  print("Done!")
  
  ##Make recommendation for number of clusters based on gap statistics (first peak in plot)
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
  rm(list=ls())
  
  ##User input for number of clusters
  K<-readline("Enter the desired number of clusters (maximum 10): ");
  K<-as.integer(K);
  
  ##Reading access file and performing K-means on access data using user's choice of number of clusters
  data_preprocessed<-read.csv("preprocessed_data.csv",header=T)
  data_preprocessed<-data_preprocessed[names(data_preprocessed) %in% c("temp_student_id","module_number")]
  data_access<-read.csv("access_data.csv",header=T)
  data_access<-data_access[names(data_access) %in% c("temp_student_id","number_accesses")]
  data_access=data_access[order(data_access$temp_student_id,decreasing=F),]
  C_m<-e1071::cmeans(as.matrix(data_access$number_accesses),centers=K)
  cluster_id=C_m$cluster
  data_access=cbind(data_access,cluster_id)
  data_access<-data_access[order(data_access$number_accesses,decreasing=F),]
    
  ##Ordering clusters
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
  
  ##Plotting clusters
  print("Plotting clusters...")
  x=1:length(unique(data_preprocessed$module_number))
  setEPS()
  postscript("Cluster_plot_fuzzy.eps")
  plot(1,pch=".",col="white",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab="Module number",ylab="Users",main="Users clustered by courseware access")
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
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="red",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==2)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="blue",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==3)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="black",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==4)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="green",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==5)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="yellow",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==6)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="pink",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==7)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="orange",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==8)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="brown",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==9)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="darkgrey",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
      else if(k==10)
      {
        plot(x=access_list,y=rep(counter,length(access_list)),pch=".",col="cyan",xlim=c(0,length(unique(data_preprocessed$module_number))),ylim=c(0,max(data_access$temp_student_id)),xlab=" ",ylab=" ",axes=F)
        par(new=T)
        counter=counter+1
      }
    }
  }
  dev.off()
  print("Done!")
  rm(list=ls())
} else
{
  print("Invalid choice! Please enter 1 or 2...")
}