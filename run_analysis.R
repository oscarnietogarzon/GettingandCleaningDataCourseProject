#define Url
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#Download the zip file into the project

download.file(Url, 
destfile = "/Users/oscarnietogarzon/Desktop/GettingandCleaningDataCourseProject/data.zip")

#unzip data directory
unzip("/Users/oscarnietogarzon/Desktop/GettingandCleaningDataCourseProject/data.zip",
       exdir = "./")

#saves the initial part of the directory and creates other vectors to load the data
dir_data <- "./"
folder <- c("test", "train")

#load test and train data
z = 1
for (i in folder) {
  #saves the directory where the data is stored
  root <- paste0(dir_data, dir(dir_data)[4], "/", i)
  for (j in 1:4){
    if(j > 1)
    {
      assign(dir(root)[j], read.table(paste0(root, "/", dir(root)[j])))
    }
    j = j + 1
  }
  z = z + 1
}

#saves the names of each variable
ref_colum <- unlist(read.table(paste0(dir_data, dir(dir_data)[4],"/",dir(paste0(dir_data, dir(dir_data)[4]))[3])),
         use.names = F)[562:1122]

#create a single dataframe for each data set and merging it with the references names
for (i in 1:length(ref_colum)){
  
  if (i == 1)
    #create the initial data frames for each data set (test/train)
  {test <- data.frame(label = y_test.txt, subject = subject_test.txt, 
                      type = as.factor(rep("test", length(y_test.txt))))

  names(X_test.txt) <- ref_colum  #put the name of the variable
  
  train <- data.frame(label=y_train.txt, subject = subject_train.txt,
                      type = as.factor(rep("train", length(y_train.txt))))
  
  names(X_train.txt) <- ref_colum}  #put the name of the variable
  
  #extract only the measurements with mean or std at the end 
  #these measurements are the 
  if (grepl("mean\\()|std()", ref_colum[i]) == TRUE)
  {
    test <- cbind(test, X_test.txt[, i])
    train <- cbind(train, X_train.txt[, i])
  }
  
  #finally it puts all together
  if (i == length(ref_colum))
  {
    #load the activity names
    ref_act <- read.table(paste0( dir_data, dir(dir_data)[4],"/",dir(paste0(dir_data, dir(dir_data)[4]))[1]),
                          header = F)
    
    #puts the measurements column names 
    n <- grep("-mean\\()|std\\()", ref_colum, value = T)
    names(test) <- c("activity", "subject","type", n)
    names(train) <- c("activity", "subject","type",n)
    
    #merge the data sets
    agg_data <- rbind(test, train) 
    #add activity labels and renames the data set
    tidy_dataset <- agg_data
    tidy_dataset$activity <- as.factor(factor(tidy_dataset$activity, labels=ref_act[,2]))
  }
}

#average of each variable for each activity and each subject
library(dplyr)
library(reshape2)
#create the second data set creating the individual average 
for (i in 4:69)
{
  if (i == 4)
  {
    #creates the second data base with the average of the first variable
    data_set_2 <- dcast(tidy_dataset, activity + subject ~ names(tidy_dataset)[i], 
        value.var = names(tidy_dataset)[i], mean)
  } else {
    #merge the created data set with the average column of the next variable
    data_set_2 <- cbind(data_set_2, dcast(tidy_dataset, activity + subject ~ names(tidy_dataset)[i], 
                                          value.var = names(tidy_dataset)[i], mean)[,3] )
    if (i == 69)
    {
    #assign the names of the variables
      colnames(data_set_2)[3:68] <- n
    }
  }
}

#saves the second data set into a csv file
write.csv(data_set_2, file = "./second_tidy_data_set.csv")
#saves the second data set
write.table(data_set_2, file = "./second_tidy_data_set.csv", row.names = F)

