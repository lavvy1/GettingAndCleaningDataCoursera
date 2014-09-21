#peer_project.R
###############################################################
#
# script to create dataset smartphone_by_subjects_and_activity
#
###############################################################


#############################
# read in all needed datasets
#############################
test_x <-read.table(".\\X_test.txt")
train_x <- read.table(".\\X_train.txt")

test_y  <-read.table(".\\y_test.txt")
train_y <-read.table(".\\y_train.txt")

features <-read.table(".\\features.txt")
dim(features)

activity_labels <-read.table(".\\activity_labels.txt")

subject_test  <-read.table(".\\subject_test.txt")
subject_train <-read.table(".\\subject_train.txt")

#################################################################
# put the data sets 'x' into one large data set, using row merge.
#################################################################

train_test_x <- rbind(train_x, test_x)

################################################
# do similar row merge for supporting datasets
###############################################
train_test_y       <- rbind(train_y, test_y)
subject_train_test <- rbind(subject_train, subject_test)

#############################
# plyr package will be needed
#############################
download.packages("dplyr")
library(dplyr)

##########################################################
# add feature column names to 'x' dataset
# note use of [[ ]] to make features variable into a vec
##########################################################
  
colnames(train_test_x) <- features[["V2"]]


###############################################################################
# from dataset train_test_x, keep only mean and standard deviation variables
# use one step for each and merge using cbind
##############################################################################

train_test_x_m   <- select(train_test_x, contains("mean"))
train_test_x_sd  <- select(train_test_x, contains("std"))
train_test_x_msd <-  cbind(train_test_x_m, train_test_x_sd)

#######################################
# dataset now has 86 variables.
# check for no overlaps in row merge.
######################################
dim(train_test_x_m);dim(train_test_x_sd);dim(train_test_msd)

################################
# change subject column name
# merge subject with activity
################################

colnames(subject_train_test)[1] <- c("subject")
act_sub <- cbind(subject_train_test, train_test_y)

#################################################################
# add activity labels to activity coded variable = 1:6 in act_sub
# keep only labels and give name to col V2
################################################################
train_test_y_names <- merge (act_sub, activity_labels, by="V1")

train_test_y_names <- tbl_df(train_test_y_names)
train_test_y_names <- select(train_test_y_names, subject, V2)
colnames(train_test_y_names)[2] <- c("activity")

#################################################
# merge activity and subjects into 'x' data.frame
#################################################
train_test_y_names   <- tbl_df(train_test_y_names)
train_test_x_msd_act <- cbind (train_test_x_msd, train_test_y_names)

####################################################################################
# tidy names of cols: delete '-', '(' and ')', change 't' to 'time', 'f' to 'freq', 
# in appropriate location of name
# note use of 'wide' dataset as 'tidy' as discussed in the Discussion Forums
###################################################################################
names(train_test_x_msd_act) <- gsub("-",quote(""),names(train_test_x_msd_act))
names(train_test_x_msd_act) <- gsub("\\(", quote(""), names(train_test_x_msd_act))
names(train_test_x_msd_act) <- gsub("\\)", quote(""), names(train_test_x_msd_act))
names(train_test_x_msd_act) <- gsub(",", quote(""), names(train_test_x_msd_act))

for (i in  1:length(names(train_test_x_msd_act))){
  if (substr (names(train_test_x_msd_act)[i],1,1) == 't')
     { names(train_test_x_msd_act)[i] <- sub("t", "time", names(train_test_x_msd_act)[i])}
  if (substr (names(train_test_x_msd_act)[i],1,1) == 'f')
     { names(train_test_x_msd_act)[i] <- sub("f", "frequency", names(train_test_x_msd_act)[i])}
}

###########################################################################
# create tidy data set of average of each var for each activity and subject
# done by using the 'group_by' command followed by 'summarise_each' command
# write dataset to working directory
###########################################################################

by_tr_te_x <- group_by(train_test_x_msd_act, activity, subject)
smartphone_by_subject_and_activity  <- summarise_each(by_tr_te_x, funs(mean) )

write.table(smartphone_by_subject_and_activity, file =".//smartphone_by_subject_and_activity.txt", row.names=FALSE)


