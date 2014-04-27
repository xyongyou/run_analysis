path <- getwd()

## 
## find the columns of -mean() -std()
f6 <- paste(path,'/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt',sep = "")
features <- read.table(f6,quote = "\"")


trans <- data.frame()
trans_name <- data.frame()
j <- 0
for (i in 1:length(features$V1)){
  x <- unlist(strsplit(as.character(features$V2[i]),'-'))
  if (identical(x[2],'mean()') ||  identical(x[2],'std()')){
    trans[i,1] <- T
    j <- j+1
    trans_name[j,1] <- features$V2[i]
    
  }
  else
    trans[i,1] <- F
    
}
trans <- t(trans)
trans_name <- t(trans_name)


## 
if (F){
  f0 <- paste(path,"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",sep = "")
  X_train <- read.table(f0,quote="\"")
  
  f1 <- paste(path,'/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt',sep = "")
  y_train <- read.table(f1,quote="\"")
  
  f2 <- paste(path,'/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt',sep = "")
  subject_train <- read.table(f2,quote="\"")
  
  f3 <- paste(path,'/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt',sep = "")
  X_test <- read.table(f3,quote="\"")
  
  f4 <- paste(path,'/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt',sep = "")
  y_test <- read.table(f4,quote="\"")
  
  f5 <- paste(path,'/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt',sep = "")
  subject_test <- read.table(f5,quote="\"")
  
  
  for (i in 1:nrow(y_train)){
    if (y_train$V1[i] == 1){
      y_train$V1[i] <- "WALKING"
    }
    else if (y_train$V1[i] == 2){
      y_train$V1[i] <- "WALKING_UPSTAIRS"
    }
    else if (y_train$V1[i] == 3){
      y_train$V1[i] <- "WALKING_DOWNSTAIRS"
    }
    else if (y_train$V1[i] == 4){
      y_train$V1[i] <- "SITTING"
    }
    else if (y_train$V1[i] == 5){
      y_train$V1[i] <- "STANDING"
    }
    else if (y_train$V1[i] == 6){
      y_train$V1[i] <- "LAYING"
    }
    
        
  }
  
  
  for (i in 1:nrow(y_test)){
    if (y_test$V1[i] == 1){
      y_test$V1[i] <- "WALKING"
    }
    else if (y_test$V1[i] == 2){
      y_test$V1[i] <- "WALKING_UPSTAIRS"
    }
    else if (y_test$V1[i] == 3){
      y_test$V1[i] <- "WALKING_DOWNSTAIRS"
    }
    else if (y_test$V1[i] == 4){
      y_test$V1[i] <- "SITTING"
    }
    else if (y_test$V1[i] == 5){
      y_test$V1[i] <- "STANDING"
    }
    else if (y_test$V1[i] == 6){
      y_test$V1[i] <- "LAYING"
    }
    
    
  }
  
  
  train <- data.frame()
  test <- data.frame()
  train <- cbind(subject_train,y_train,X_train[,trans])
  test <- cbind(subject_test,y_test,X_test[,trans])  
  merge_data <- rbind(train,test)
  
  names(merge_data)[1] <- 'subject'
  names(merge_data)[2] <- 'activity'
  end <- ncol(trans_name)+2
  names(merge_data)[3:end] <- trans_name
  
}

## write.table
write.table(merge_data,file = "tidy_data.txt",sep = " ",row.names = F)


## calculate the average of each variable
data1 <- merge_data
data1[,2] = NULL
res1 <- sapply(split(data1,data1$subject),mean)



data2 <- merge_data
data2[,1] = NULL
res2 <- sapply(split(data2,data2$subject),mean)


