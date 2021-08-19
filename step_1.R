#################################
#' step 1 
#' get auc for each bucket
#################################


#' (1) run algo and save all auc 
auc_results = run_rf_ridge(x,y, buckets_features_names)
saveRDS(auc_results, file = "output/step_1_auc.rds")
ridge_auc = auc_results$ridge_auc
rf_auc = auc_results$rf_auc

#' (2) check significance 
cat("\nridge")
cat("\n\nclinical")
check_significance(ridge_auc$clinical, as.data.frame(ridge_auc)[,c("demographics","family","cognitive","environment","trauma")],"ridge")
cat("\ncombined")
check_significance(ridge_auc$all, as.data.frame(ridge_auc)[,c("demographics","family","cognitive","environment","trauma","clinical")],"ridge")

cat("\nRF")
cat("\n\nclinical")
check_significance(rf_auc$clinical, as.data.frame(rf_auc)[,c("demographics","family","cognitive","environment","trauma")], "RF")
cat("\ncombined")
check_significance(rf_auc$all,  as.data.frame(rf_auc)[,c("demographics","family","cognitive","environment","trauma","clinical")], "RF")
