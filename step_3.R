#################################
#' step 3 
#' check auc for top features
#################################


features_list <- read_csv("output/features_list.csv")
features_list = as.list(features_list)

# add demographics_features to all lists
for (i in names(features_list)){
  features_list[[i]] = union(na.omit(features_list[[i]]) , buckets_features_names$demographics)
}


auc_results = run_rf_ridge(x,y, features_list)
saveRDS(auc_results, file = "output/step_3_auc.rds")