###########################################
#all features
###########################################


combined_bucket = Reduce(function(x, y) merge(x, y, by="bblid", all.x=TRUE), list(Environment_bucket_trimmed,
                                                                                  Demographics_bucket,
                                                                                  Family_bucket,
                                                                                  Trauma_bucket,
                                                                                  Clinical_bucket_full,
                                                                                  Cognitive_bucket))





#make sure all columns have less than 10% NA
which(colSums(is.na(combined_bucket))>=90) #0   

#check empty rows
which(rowSums(is.na(combined_bucket))>=100) #36  99 905
sum(is.na(combined_bucket[36,])) #124
sum(is.na(combined_bucket[99,])) #131
sum(is.na(combined_bucket[905,]))#157

#NA 
number_NA = sum(is.na(combined_bucket))
cat("\nNumber of Na in Data: ", number_NA, "\n")
number_NA_col = colSums(is.na(combined_bucket))
data_dim = dim(combined_bucket)
number_NA/((data_dim[2]-1)*data_dim[1]) #0.0067
cat("% of NA: ", number_NA/((data_dim[2]-1)*data_dim[1]),"\n" )

combined_bucket


