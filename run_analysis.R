run_analysis<-
{
   ##Read the test data X gives the measures for each variable
  x<-read.table("./test/X_test.txt")
  ##Read the test data Y gives a numberic value 1:6 for each activity  
  y<-read.table("./test/Y_test.txt")
  
  ## Convert each of the numbers 1:6 to it description
  attach(y)
  y$V1[V1 == 1] <- "Walking"
  y$V1[V1 == 2] <- "Walkingupstairs"
  y$V1[V1 == 3] <- "Walkingdownstairs"
  y$V1[V1 == 4] <- "sitting"
  y$V1[V1 == 5] <- "standing"
  y$V1[V1 == 6] <- "laying"
  detach(y)
  ##Read the subjects 1:30 match to the activities  
  s<-read.table("./test/subject_test.txt")
  
  ##Merge each of the tables into a single table using cbind
  
  test_data1<-cbind(y,x)
  test_data<-cbind(s,test_data1)
  
  ##repeat the process to create a single data frame for the training data   
  
  ##Read the test data X gives the measures for each variable  
  x<-read.table("./train/X_train.txt")
  ##Read the test data Y gives a numberic value 1:6 for each activity  
  y<-read.table("./train/Y_train.txt")
  
  ## Convert each of the numbers 1:6 to it description
  attach(y)
  y$V1[V1 == 1] <- "Walking"
  y$V1[V1 == 2] <- "Walkingupstairs"
  y$V1[V1 == 3] <- "Walkingdownstairs"
  y$V1[V1 == 4] <- "sitting"
  y$V1[V1 == 5] <- "standing"
  y$V1[V1 == 6] <- "laying"
  detach(y)
  ##Read the subjects 1:30 match to the activities    
  s<-read.table("./train/subject_train.txt")
  
  ##Merge each of the tables into a single table using cbind
  train_data1<-cbind(y,x)
  train_data<-cbind(s,train_data1)
  
  ##Merge the training and test data into a single data frame
  
  full_data<-rbind(test_data,train_data)
  
  ##Change the headings for each column to the name of the appropriate feature
  
  names<-read.table("features.txt")
  newnames<-as.character(names$V2)
  
  ##To complete the data frame the first two columns need to be included Subject and Activity  
  sub_act<-c("subject","activity")
  variables<-c(sub_act,newnames)
  
  ##Complete the change for column names  
  names(full_data)<-variables
  names(full_data)
  
  ##Select out only the features which refer to mean and standard deviation  
  get<-grep("mean|std",names(full_data))
  
  ## create a Data Frame which only contains the mean and standard deviation of features
  get1<-c(1:2,get)
  get_data<-full_data[,get1]
  
  ##Discovered that above also contains a feature meanFreq, these are to be removed   
  
  mf<-grep("meanFreq",names(get_data))
  mean.std<-get_data[,-mf]
  
  ##Final step, establish the mean of each variable for each activity and each subject using the plyr package, this needs to installed and loaded
  ##call the tidy data "project_tidy_data
  library(plyr)
  project_tidy_data<-ddply(mean.std, .(subject,activity), numcolwise(mean))
  ##save the Data Frame to the project folder in a csv format
  write.csv(project_tidy_data,"project_tidy_data.csv")
  print("File Name: project_tidy_data")
  print(project_tidy_data)
}
