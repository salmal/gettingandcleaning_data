runanalysis <- function() {
      # Read the data associated with the test subjects and create the required
      # df frame the mean and the standard deviations for the measurements on the X, Y, Z
      # Axis. Do the same for the training subjects data
      # once these 2 df are ready, row bind the data frames to merge
      # provide the labels to the data frame to be more descriptive
      # once this is done, subset this data frame for each subject and activity for
      # the mean readings of the x, y, Z axis. 
      # Now provide the labels. 
      # summarize this information for each activity by subject
      # write out the df
      # 
      # set the wd to the directory containg the test data. the other files are read wrt
      # this directory
      
      #load the required libraries
      library(dplyr)
      library(reshape2)
      # read the test data 
      df_te = read.table('x_test.txt')
      # read the features i.e. the column names of the observations
      # the features are the same for test and train data
      col_hd <- read.table("../features.txt")
      # attach the column names to the df
      colnames(df_te) <- col_hd[,2]
      # read the  test subject df
      te_sub <- read.table("subject_test.txt", col.names = 'Subject')
      # read the test activity df
      te_act <- read.table("y_test.txt", col.names = 'Activity_Id')
      # bind the readings, subject and activity readings 
      df_te <- cbind(te_sub,te_act,df_te)
      #read the activity labels, 
      #These are the same across the test and training data
      act_lab <- read.table("../activity_labels.txt", col.names = c('Activity_Id','Activity'))
      #
      # Now do the above for the training data.
      #
      # read the training data 
      df_tr = read.table('../train/x_train.txt')
      # attach the column names to the df
      colnames(df_tr) <- col_hd[,2]
      #
      # read the  training subject df
      #
      tr_sub <- read.table("../train/subject_train.txt", col.names = 'Subject')
      # read the training activity df
      tr_act <- read.table("../train/y_train.txt", col.names = 'Activity_Id')
      # bind the readings, subject and activity readings
      df_tr <- cbind(tr_sub,tr_act,df_tr)
      #
      # rowbind the two df df_tr and df_te to get the merged df
      #
      merge_df <- rbind(df_te, df_tr)
      #
      # remove the duplicated columns, also they are not columns of interest
      merge_df <- merge_df[,!duplicated(colnames(merge_df))]
      # make colnames compliant naming standards, so tbl_df commands work properly
      colnames(merge_df) <- gsub("[\\(\\)\\-]","_",names(merge_df))
      # merge the activity labels
      merge_df <- merge(merge_df, act_lab, by.x = 'Activity_Id', by.y = 'Activity_Id')
      #convert the df to a tbl_df
      merge_df <- tbl_df(merge_df)
      #
      #make a new df having only the correct mean and std columns
      merge_df <- select(merge_df, Subject, Activity, contains('mean__'), contains('std__'))
      #
      #melt the df to have the all the variable names by the columns of interest
      # i.e. by Subject and Activity
      m_melt <- melt(merge_df, id.vars = colnames(merge_df[1:2]), measure.vars = colnames(merge_df[3:68]) )
      #cast the melted df to create all the means, rounded the means to 6
      my_cast <- dcast(m_melt, Subject + Activity ~ variable, function(x) round(mean(x),6))
      #
      #write out the the data frame to the OS for submission
      write.table(my_cast, file = 'summaryofmeans.txt', row.names = FALSE)

}