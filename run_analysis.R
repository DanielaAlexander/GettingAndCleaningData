library(plyr)
library(dplyr)


##================================================================
## 1. Merge the training and the test sets to create one data set.
##================================================================

	## in the Train and Test folders - there are 3 files and a subfolder in each of these. 
	## I will ignore the subfolder, as the data in there is not relevant to our exercise

	## file X_train.txt is a fixed file format that contains the training data - 561 columns and 7352 rows
  ## file X_test.txt is a fixed file format that contains the training data - 561 columns and 2947 rows
xTrain<-read.fwf("./Train/X_train.txt", widths = rep(c(4), 561))
xTest<-read.fwf("./Test/X_test.txt", widths = rep(c(4), 561))

	##rbind them together - this is our new unified data set
mergedData<-rbind(xTrain, xTest)

##==========================================================================================
## 2. Extract only the measurements on the mean and standard deviation for each measurement. 
##==========================================================================================
	##read the features file - 2 columns, 561 rows
featFile<-read.csv("features.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)

	##identify the needed columns: 
	##search for mean, Mean, std or Std in all values of our featFile column 2
	##subset our data set to those columns
srch<-"[Mm]ean|[Ss]td"
msCols<-grep(srch, featFile[,2])
msMergedData<-mergedData[, msCols]


##==========================================================================================
## 3. Use descriptive activity names to name the activities in the dataset
##==========================================================================================
	##read the activity names
actNames<-read.csv("activity_labels.txt", header=FALSE, sep = " ", stringsAsFactors = FALSE)

	## files Y-train.txt and Y_test.txt contain the activities/labels for the X-train.txt and X_test.txt files 
yTrain<-read.csv("./Train/Y_train.txt", header=FALSE, stringsAsFactors = FALSE)
yTest<-read.csv("./Test/Y_test.txt", header=FALSE, stringsAsFactors = FALSE)

	##put together the numbered activities with their labels; use join to preserve the order
labeledYTrain<-join(yTrain, actNames, by = "V1")
labeledYTest<-join(yTest, actNames, by = "V1")

	##rbind the new Train and Test activities in this precise order
activ<-rbind(labeledYTrain, labeledYTest)

	##cbind them with the msMergedData data set
labeledData<-cbind(activ, msMergedData)

	##get rid of the first column, which only has the number for the activity
labeledData <- labeledData[, -1]

##==========================================================================================
## 4. Appropriately label the data set w/ descriptive variable name
##==========================================================================================

	## Rename the columns using their original names in the features.txt
srch<-"[Mm]ean|[Ss]td"
msCols<-grep(srch, featFile[,2])
dataColNames<-featFile[msCols, 2]

	## Add the activity as the first column
dataColNames <- append(dataColNames, "Activity", after = 0)

	##then rename all columns
colnames(labeledData) <- dataColNames


##==========================================================================================
## 5. from data created at step 4 create a second, independent, tidy dataset
##    with the average for each activity and each subject
##==========================================================================================

	## first, get the subjects matched with the data

	## files subject_train.txt and subject_test.txt containing the subjects for which the measurements were taken
subTrain<-read.csv("./Train/subject_train.txt", header=FALSE, stringsAsFactors = FALSE)
subTest<-read.csv("./Test/subject_test.txt", header=FALSE, stringsAsFactors = FALSE)

	##rbind them in the order Train, Test as we did all the other ones, so they match the data and the activities
subjects<-rbind(subTrain, subTest)
col(subjects)<- "Subject"

	##create a new data frame, joining them together
subjData<-cbind(subjects, labeledData)

	##tidy the data to get precisely 1 subject X 1 activity combination with their mean as value
	##my tidy data set has 180 rows (30 subjects x  6 activities)  and 86 columns
tidyData<- ddply(subjData, .(Subject, Activity), numcolwise(mean))


##=================================================================================
## write the dataset in a txt file created with write.table() using row.name=FALSE
##=================================================================================
write.table(tidyData, "tidy_data_DA.txt", row.name=FALSE)
