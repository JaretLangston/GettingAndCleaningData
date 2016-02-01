run_analysis <- function(){
        library(dplyr)
        #Set the working directory
        setwd("~/git/datasciencecoursera/GettingAndCleaningData/data")
        #read the activity labels
        activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
        #read the test set
        xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
        ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
        subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        #read the train set
        xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
        ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
        subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        #combine the test data
        test <- xtest[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)]
        test <- cbind(activity=ytest$V1,test)
        test <- cbind(subjectid=subjecttest$V1,test)
        #combine the train data
        train <- xtrain[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)]
        train <- cbind(activity=ytrain$V1,train)
        train <- cbind(subjectid=subjecttrain$V1,train)
        #combine test and train datasets
        duprows <- rownames(train) %in% rownames(test)
        combined <- rbind(train, test[!duprows,])
        #rename the mean and std columns
        names(combined)[names(combined)=="V1"] <- "time.BodyAcc.mean.lessX"
        names(combined)[names(combined)=="V2"] <- "time.BodyAcc.mean.lessY"
        names(combined)[names(combined)=="V3"] <- "time.BodyAcc.mean.lessZ"
        names(combined)[names(combined)=="V4"] <- "time.BodyAcc.std.lessX"
        names(combined)[names(combined)=="V5"] <- "time.BodyAcc.std.lessY"
        names(combined)[names(combined)=="V6"] <- "time.BodyAcc.std.lessZ"
        names(combined)[names(combined)=="V41"] <- "time.GravityAcc.mean.lessX"
        names(combined)[names(combined)=="V42"] <- "time.GravityAcc.mean.lessY"
        names(combined)[names(combined)=="V43"] <- "time.GravityAcc.mean.lessZ"
        names(combined)[names(combined)=="V44"] <- "time.GravityAcc.std.lessX"
        names(combined)[names(combined)=="V45"] <- "time.GravityAcc.std.lessY"
        names(combined)[names(combined)=="V46"] <- "time.GravityAcc.std.lessZ"
        names(combined)[names(combined)=="V81"] <- "time.BodyAccJerk.mean.lessX"
        names(combined)[names(combined)=="V82"] <- "time.BodyAccJerk.mean.lessY"
        names(combined)[names(combined)=="V83"] <- "time.BodyAccJerk.mean.lessZ"
        names(combined)[names(combined)=="V84"] <- "time.BodyAccJerk.std.lessX"
        names(combined)[names(combined)=="V85"] <- "time.BodyAccJerk.std.lessY"
        names(combined)[names(combined)=="V86"] <- "time.BodyAccJerk.std.lessZ"
        names(combined)[names(combined)=="V121"] <- "time.BodyGyro.mean.lessX"
        names(combined)[names(combined)=="V122"] <- "time.BodyGyro.mean.lessY"
        names(combined)[names(combined)=="V123"] <- "time.BodyGyro.mean.lessZ"
        names(combined)[names(combined)=="V124"] <- "time.BodyGyro.std.lessX"
        names(combined)[names(combined)=="V125"] <- "time.BodyGyro.std.lessY"
        names(combined)[names(combined)=="V126"] <- "time.BodyGyro.std.lessZ"
        names(combined)[names(combined)=="V161"] <- "time.BodyGyroJerk.mean.lessX"
        names(combined)[names(combined)=="V162"] <- "time.BodyGyroJerk.mean.lessY"
        names(combined)[names(combined)=="V163"] <- "time.BodyGyroJerk.mean.lessZ"
        names(combined)[names(combined)=="V164"] <- "time.BodyGyroJerk.std.lessX"
        names(combined)[names(combined)=="V165"] <- "time.BodyGyroJerk.std.lessY"
        names(combined)[names(combined)=="V166"] <- "time.BodyGyroJerk.std.lessZ"
        names(combined)[names(combined)=="V201"] <- "time.BodyAccMag.mean"
        names(combined)[names(combined)=="V202"] <- "time.BodyAccMag.std"
        names(combined)[names(combined)=="V214"] <- "time.GravityAccMag.mean"
        names(combined)[names(combined)=="V215"] <- "time.GravityAccMag.std"
        names(combined)[names(combined)=="V227"] <- "time.BodyAccJerkMag.mean"
        names(combined)[names(combined)=="V228"] <- "time.BodyAccJerkMag.std"
        names(combined)[names(combined)=="V240"] <- "time.BodyGyroMag.mean"
        names(combined)[names(combined)=="V241"] <- "time.BodyGyroMag.std"
        names(combined)[names(combined)=="V253"] <- "time.BodyGyroJerkMag.mean"
        names(combined)[names(combined)=="V254"] <- "time.BodyGyroJerkMag.std"
        names(combined)[names(combined)=="V266"] <- "freq.BodyAcc.mean.lessX"
        names(combined)[names(combined)=="V267"] <- "freq.BodyAcc.mean.lessY"
        names(combined)[names(combined)=="V268"] <- "freq.BodyAcc.mean.lessZ"
        names(combined)[names(combined)=="V269"] <- "freq.BodyAcc.std.lessX"
        names(combined)[names(combined)=="V270"] <- "freq.BodyAcc.std.lessY"
        names(combined)[names(combined)=="V271"] <- "freq.BodyAcc.std.lessZ"
        names(combined)[names(combined)=="V345"] <- "freq.BodyAccJerk.mean.lessX"
        names(combined)[names(combined)=="V346"] <- "freq.BodyAccJerk.mean.lessY"
        names(combined)[names(combined)=="V347"] <- "freq.BodyAccJerk.mean.lessZ"
        names(combined)[names(combined)=="V348"] <- "freq.BodyAccJerk.std.lessX"
        names(combined)[names(combined)=="V349"] <- "freq.BodyAccJerk.std.lessY"
        names(combined)[names(combined)=="V350"] <- "freq.BodyAccJerk.std.lessZ"
        names(combined)[names(combined)=="V424"] <- "freq.BodyGyro.mean.lessX"
        names(combined)[names(combined)=="V425"] <- "freq.BodyGyro.mean.lessY"
        names(combined)[names(combined)=="V426"] <- "freq.BodyGyro.mean.lessZ"
        names(combined)[names(combined)=="V427"] <- "freq.BodyGyro.std.lessX"
        names(combined)[names(combined)=="V428"] <- "freq.BodyGyro.std.lessY"
        names(combined)[names(combined)=="V429"] <- "freq.BodyGyro.std.lessZ"
        names(combined)[names(combined)=="V503"] <- "freq.BodyAccMag.mean"
        names(combined)[names(combined)=="V504"] <- "freq.BodyAccMag.std"
        names(combined)[names(combined)=="V516"] <- "freq.BodyBodyAccJerkMag.mean"
        names(combined)[names(combined)=="V517"] <- "freq.BodyBodyAccJerkMag.std"
        names(combined)[names(combined)=="V529"] <- "freq.BodyBodyGyroMag.mean"
        names(combined)[names(combined)=="V530"] <- "freq.BodyBodyGyroMag.std"
        names(combined)[names(combined)=="V542"] <- "freq.BodyBodyGyroJerkMag.mean"
        names(combined)[names(combined)=="V543"] <- "freq.BodyBodyGyroJerkMag.std"
        #create lookup vector for activity names
        lookupActivity <- activitylabels[,"V2"]
        #update the activity names
        combined$activity <-lookupActivity[combined$activity] 
        #group by subject and activity and calculate the mean
        meanRslt <- combined %>% group_by(subjectid,activity) %>% summarize_each(funs(mean))
        #write out result files
        write.csv(combined,"./UCI HAR Dataset/combinedResults.csv")
        write.csv(meanRslt,"./UCI HAR Dataset/combinedResults_mean.csv")
       
        
}