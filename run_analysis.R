  # clean workspace
  rm(list=ls());
  # Set working directory to read in files
  setwd("C:/Users/praxk_000/Desktop/Git/Get Data/UCI HAR Dataset");
  
  # 1. Merge training and testing data to create the dataset
  # Read in the training data
  features<-read.table('features.txt', header=FALSE);
  activity_labels<-read.table('activity_labels.txt', header=FALSE);
  subjecttrain<-read.table('./train/subject_train.txt', header=FALSE);
  xtrain<-read.table('./train/X_train.txt', header=FALSE);
  ytrain<-read.table('./train/Y_train.txt', header=FALSE);
  # Name the columns of the datasets
  colnames(activity_labels)<-c('activityID', 'activityType');
  colnames(subjecttrain)<-"subjectID";
  colnames(xtrain)<-features[,2];
  colnames(ytrain)<-"activityID";
  # Create training data
  trainingdata<-cbind(ytrain,subjecttrain,xtrain);
  #  Read in the testing data
  subjecttest<-read.table('./test/subject_test.txt', header=FALSE);
  xtest<-read.table('./test/X_test.txt', header=FALSE);
  ytest<-read.table('./test/Y_test.txt', header=FALSE);
  #Assign column names
  colnames(subjecttest)<-"subjectID";
  colnames(xtest)<-features[,2];
  colnames(ytest)<-"activityID";
  # Createa testing data
  testingdata<-cbind(ytest,subjecttest,xtest);
  # Create final data set
  finaldata<-rbind(trainingdata,testingdata);
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  colNames<-colnames(finaldata);
  subsetvector<-(grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
  finaldata<-finaldata[subsetvector==TRUE];
  
  # 3. Use descriptive activity names to name the activities in the data set
  finaldata<-merge(finaldata, activity_labels, by="activityID", all.x=TRUE);
  colNames<-colnames(finaldata);
  
  # 4. Appropriately labels the data set with descriptive variable names.
  for( i in 1:length(colNames))
  {
    colNames[i] = gsub("-mean()","Mean",colNames[i])
    colNames[i] = gsub("-std$", "StdDev", colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
  };
  
  colnames(finaldata)<-colNames;
  
  # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  finaldata_no_activityType  = finaldata[,names(finaldata) != 'activityType'];
  #newnames<-colNames[3:20];
  #activityID<- finaldata_no_activityType$activityID;
  #subjectID<- finaldata_no_activityType$subjectID;
  # Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
  #tidyData    = aggregate(finaldata_no_activityType[,newnames],by=list(activityID,subjectID),mean);
  tidyData    = aggregate(finaldata_no_activityType[,names(finaldata_no_activityType) != c('activityID','subjectID')],by=list(activityID=finaldata_no_activityType$activityID,subjectID = finaldata_no_activityType$subjectID),mean);
  # Merging the tidyData with activityType to include descriptive acitvity names
  tidyData    = merge(tidyData,activity_labels,by='activityID',all.x=TRUE);
  
  # Export the tidyData set 
  write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');