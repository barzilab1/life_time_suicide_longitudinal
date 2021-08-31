
#read the CSVs
PHQ_Data = read_csv("Data/PHQ9_file_for_Elina_with_PNC_FH.csv")
PNC_Core_Data_clinical = read_csv("Data/PNC_Core_Data_clinical.csv")
PNC_Core_Data_cognitive = read_csv("Data/PNC_Core_Data_cognitive.csv")
PNC_Core_Data_cognitive_ALTERNATIVE = read_csv("Data/PNC_Core_Data_cognitive_ALTERNATIVE.csv")
PNC_Core_Data_demographics = read_csv("Data/PNC_Core_Data_demographics.csv")
PNC_Core_Data_environment = read_csv("Data/PNC_Core_Data_environment.csv")
GO1_Substance_Use = read_csv("Data/n9462_substance_go1_052215.csv")
GOASSESS_Timeline = read_csv("Data/n14508_GOASSESS_Timeline_noncorrected_20131021_redcap_pid251.csv")
GO1_grade_repeats = read_csv("Data/GO1_grade_repeats.csv")
SepDivorce = read_csv("Data/SepDivorce.csv")
Sips_Extras = read_csv("Data/sips_extras.csv")

################
# build buckets 
################

###Y_bucket
Y_bucket = PHQ_Data[,c("bblid","Current_Suicidal_Ideation","Lifetime_Suicide_Attempt" , "Depression_mod_above_at_phq", "PHQ9_Sum")]
summary(Y_bucket)
#remove empty rows
Y_bucket = Y_bucket[!is.na(Y_bucket$Current_Suicidal_Ideation),]
#change range to [0,1]
Y_bucket[,c("Current_Suicidal_Ideation","Lifetime_Suicide_Attempt")] = Y_bucket[,c("Current_Suicidal_Ideation","Lifetime_Suicide_Attempt")] -1

###EDU
Edu_info = merge(GOASSESS_Timeline[,1:2],GO1_grade_repeats)
#get bblid
temp = do.call('rbind', strsplit(as.character(Edu_info$redcapid),'_',fixed=TRUE))
Edu_info$bblid = temp[,1]
Edu_info$suffix = temp[,2]
Edu_info = merge(Y_bucket[,"bblid", drop = F],Edu_info)

#combine rows with the same bblid
Edu_info_combined = aggregate(Edu_info[,c("tml007","dem107","dem108","dem109")], list(bblid=Edu_info$bblid), function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
})

#find contradictions between parent and child answer
double_value_indexes = which(lapply(Edu_info_combined$tml007, length) > 1 )
#take the parent answer
for ( i in double_value_indexes){
  Edu_info_combined$tml007[i] = Edu_info$tml007[Edu_info$bblid == Edu_info_combined$bblid[i] & Edu_info$suffix == "MI"]
}
Edu_info_combined$tml007 = unlist(Edu_info_combined$tml007)

###age indicator for age >= 11
PNC_Core_Data = PNC_Core_Data_demographics[,c("bblid","ageAtClinicalAssess1")]
PNC_Core_Data$above11 = ifelse(PNC_Core_Data_demographics$ageAtClinicalAssess1 >= 132,1,0)

###Family_bucket
Family_bucket = PHQ_Data[!is.na(PHQ_Data$Current_Suicidal_Ideation),
                         c("bblid","Ran_psychosis_FH","Ran_depression_FH","Ran_bipolar_or_lithium_FH","Ran_Sui_attempt_or_death_FH","Ran_substance_FH")] 
Family_bucket = merge(Family_bucket, PNC_Core_Data_demographics[,c(1,9:11)])
Family_bucket = merge(Family_bucket, SepDivorce)
Family_bucket = source(paste(getwd(),"longitudinal/Family.R", sep = "/"))$value


###Environment_bucket
Environment_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_environment[,c(1:16)])
Environment_bucket_trimmed = source(paste(getwd(),"longitudinal/Environmental.R", sep = "/"))$value

###Trauma_bucket
Trauma_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_environment[,-c(2:16)])  
Trauma_bucket = source(paste(getwd(),"longitudinal/Trauma.R", sep = "/"))$value
#age above 11- check if child or parent did the assesment 
# Trauma_bucket = merge(Trauma_bucket, PNC_Core_Data[,c("bblid","above11")])

###Demographics_bucket
Demographics_bucket = merge(PHQ_Data[,c("bblid", "goassessPhqDurMonths")], PNC_Core_Data_demographics[,c(1,2,4:8)])
Demographics_bucket = merge(Y_bucket[,c("bblid")], Demographics_bucket)
Demographics_bucket = merge(Demographics_bucket, Edu_info_combined[,c("bblid","tml007")])
Demographics_bucket = source(paste(getwd(),"longitudinal/Demographics.R", sep = "/"))$value

###Clinical_bucket
Clinical_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_clinical)
Clinical_bucket = merge(Sips_Extras, Clinical_bucket)
#add if the child skipped a class 
Clinical_bucket = merge(Clinical_bucket, Edu_info_combined[,c(1,3)])
#age above 11- check if child or parent did the assesment 
# Clinical_bucket = merge(Clinical_bucket, PNC_Core_Data[,c("bblid","above11")])
Substance_bucket = merge(Y_bucket[,c("bblid")], GO1_Substance_Use[,c(1,76:89)])
Clinical_bucket_full = source(paste(getwd(),"longitudinal/Clinical.R", sep = "/"))$value

###Cognitive_bucket
Cognitive_raw_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive_ALTERNATIVE)
Cognitive_bucket = merge(Y_bucket[,c("bblid")], PNC_Core_Data_cognitive)
Cognitive_bucket = source(paste(getwd(),"longitudinal/Cognitive.R", sep = "/"))$value


if(!with_sui2){
  Clinical_bucket_full = Clinical_bucket_full[,! names(Clinical_bucket_full) %in% c("sui002")]
}

#get features names from each bucket
buckets_features_names = list(
  family       = colnames(Family_bucket)[-1],
  environment  = colnames(Environment_bucket)[-1],
  trauma       = colnames(Trauma_bucket)[-1],
  demographics = colnames(Demographics_bucket)[-1],
  clinical     = colnames(Clinical_bucket)[-1],
  cognitive    = colnames(Cognitive_bucket)[-1]
)

buckets_features_names[["all"]] =  as.vector(unlist(buckets_features_names))
number_features = length(buckets_features_names$all)
print(paste0("Number of features in dataset: ", number_features))


