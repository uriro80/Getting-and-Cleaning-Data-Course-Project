# Getting-and-Cleaning-Data-Course-Project
Final Project for the course

# load the relevant libraries to work with.
library(rlang)
library(dplyr) 
library(tidyr)
library(stringr)
library(data.table)
## download zipped file to my machine and load it to r
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(URL,temp)
# opening a zipped file on local machine
zipdf <- unzip(temp, list = TRUE)
fileInd <- grep(".test.+.txt$|.train.+.txt$", zipdf$Name) # Find file indices in zipdf$Name for data extraction
ListNames <- grep(".test.+.txt$|.train.+.txt$", zipdf$Name, value = T) # Find file names in zipdf$Name for data extraction
# Extract feature names
filename_path <- zipdf$Name[2]; feature_names <- read.table(unz(temp, filename_path))
# Extract activity names
filename_path <- zipdf$Name[1]; activities <- read.table(unz(temp, filename_path))
## Loop through the files to load the data into R.
#Pre allocate vectors
DF_List_test <- vector(mode = "list", length = length(fileInd)/2) #pre allocate a list vector to receive the data.
VarName_test <- vector(mode = "character", length = length(fileInd)/2) #pre allocate a character vector to receive the names of the files
DF_List_trail <- vector(mode = "list", length = length(fileInd)/2) #pre allocate a list vector to receive the data.
VarName_trail <- vector(mode = "character", length = length(fileInd)/2) #pre allocate a character vector to receive the names of the files
# Loop through the files to extract the data.
c = 0
for (iReadData in 1:length(fileInd)){
        c = c+1
        filename_path <- zipdf$Name[fileInd[iReadData]] # extract file path for the specific files
        Ind_slash = gregexpr("./.", filename_path)[[1]] # Find the indices of the "/" in the file name.
        VarName <- substr(filename_path, Ind_slash[length(Ind_slash)]+2, nchar(filename_path)-4) # Use slash index to find the file name
        if(iReadData <= length(fileInd)/2){
                DF_List_test[[c]] <- read.table(unz(temp, filename_path)) # extract the test data from the file to the list
                VarName_test[c] <- VarName
        }else{
                DF_List_trail[[c-length(fileInd)/2]] <- read.table(unz(temp, filename_path)) # extract the trail data from the file to the list
                VarName_trail[c-length(fileInd)/2] <- VarName
        }
}
unlink(temp) # disconnect the connection with the zip file
## 1. Merges the training and the test sets to create one data set.

# Form unique names for the variables in a data frame where x is the list of the file names you want to fix.
AddNum2Header <- function(x, Rep.Vec = as.character(1:128)){
        VarName <- substr(x, 1, nchar(x)-4)
                Headers <- paste(rep(VarName,128), Rep.Vec, sep = "_")
}
Headers_tmp <- lapply(VarName_test[1:9], AddNum2Header) #use lapply to apply AddNum2Header() for all relevant columns 
Headers <- c(unlist(Headers_tmp), "participants", feature_names$V2, "activity", "set") #form the full column names vector

# make DF_List_test/trail to a data frame using bind() and add a "set" column (training or testing) using mutate() function.
# names() are changed to allow for the use of mutate() function and provide meaningful names to the variables.
DF_test <- do.call(cbind, DF_List_test)
names(DF_test) <- as.character(c(1:1715))
DF_test <- mutate(DF_test, set = "test")
DF_trail <- do.call(cbind, DF_List_trail)
names(DF_trail) <- as.character(c(1:1715))
DF_trail <- mutate(DF_trail, set = "train")
DF_FullData <- rbind(DF_test, DF_trail)
names(DF_FullData) <- Headers

## 2. Extract only the measurements on the mean and standard deviation for each measurement into df2. 
df2 <- DF_FullData %>% select(c(matches("Participants"), contains("mean()")|contains("std()"), matches("activity"), matches("set")))

## 3. Uses descriptive activity names to name the activities in the data set
# change the activity numbers in the data frame by the activity names according to the key. 
# Replace String with another String using stringr::str_replace() function
df2$activity <- str_replace(df2$activity, as.character(df2$activity), activities[df2$activity,2])

## 4. Appropriately labels the data set with descriptive variable names.
# At this point df2 is a data frame with descriptive variable names and activity names.

## 5. From the data set in step 4, create a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
# group the data by participants and activity which are the variables to average but also by set in order not to loose this information.
df.means <- df2 %>% 
          group_by(participants, activity, set) %>% 
          summarise_at(vars(grep("X$|Y$|Z$", names(DF2), value = T)), mean)# Taking only the columns with a plane #because the magnitude could always be calculated from them.
tidydf <- df.means %>% pivot_longer(!c("participants", "activity", "set"), names_to = "measurement_plane", values_to = "values") %>% #gather all relevant values from header names into rows
         separate_wider_delim(cols = "measurement_plane", delim = "()-" ,names = c("measurement", "plane")) %>% #split 2 values that are in the same column to different columns.
        separate_wider_delim(cols = "measurement", delim = "-" ,names = c("measurement", "descriptive")) %>% # split the data in measurement to mean and std. 
        pivot_wider(names_from = descriptive, values_from = values) # write the values separately for mean and std values.
 
 ## Tidy data output      
tidydf
# A tibble: 4,320 × 7
# Groups:   participants, activity [180]
   participants activity set   measurement   plane     mean    std
          <int> <chr>    <chr> <chr>        <chr>    <dbl>  <dbl>
 1            1 LAYING   train tBodyAcc     X      0.222   -0.928
 2            1 LAYING   train tBodyAcc     Y     -0.0405  -0.837
 3            1 LAYING   train tBodyAcc     Z     -0.113   -0.826
 4            1 LAYING   train tGravityAcc  X     -0.249   -0.897
 5            1 LAYING   train tGravityAcc  Y      0.706   -0.908
 6            1 LAYING   train tGravityAcc  Z      0.446   -0.852
 7            1 LAYING   train tBodyAccJerk X      0.0811  -0.958
 8            1 LAYING   train tBodyAccJerk Y      0.00384 -0.924
 9            1 LAYING   train tBodyAccJerk Z      0.0108  -0.955
10            1 LAYING   train tBodyGyro    X     -0.0166  -0.874
# … with 4,310 more rows
# ℹ Use `print(n = ...)` to see more rows

The data is tidy since every variable is in different column, every observation is in one row, and each type of observational unit is a table.