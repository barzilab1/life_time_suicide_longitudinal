#################################
#' step 2 
#' select top features
#################################

# (1) run the 3 algo on combine bucket
res = run_lasso_stir_rf(x,y)
saveRDS(res, file = "output/step_2_res.rds")

res_lasso = res$lasso
res_Relieff = res$stir
res_rf = res$rf



# (2) get top features from 3 algos and create mean-rank list
ranked_features = rank_features(res_lasso,res_Relieff,res_rf)

features_list = list()

features_list$lasso   =  ranked_features$feature[ranked_features$rank_lasso   > (193 - res_lasso$number_selected)]
features_list$relieff =  ranked_features$feature[ranked_features$rank_Relieff > (193 - res_Relieff$number_selected)]

print(paste0("\nNumber chosen lasso: ", length(features_list$lasso)))
print(paste0("\nNumber chosen relieff: ", length(features_list$relieff)))


rf_bigest_score = find_biggest_gap(res_rf$features[,1])
features_list$rf = ranked_features$feature[ranked_features$score_rf >= rf_bigest_score]

lasso_index = length(features_list$lasso)
relieff_index = length(features_list$relieff)
rf_index = length(features_list$rf)

# (3) get subset from mean rank
max_length = max(lasso_index,relieff_index, rf_index)
max_length = (round(max_length/5)*5) 
max_length = max(35,max_length)
mean_rank_features = ranked_features[order(ranked_features[["mean_rank"]], decreasing = TRUE),c("feature"),drop = F]
for (i in seq(5,max_length,5)){
  
  col_name = paste0("mean_rank_",i )
  features_list[[col_name]] = mean_rank_features$feature[1:i]
  
}
  
temp = sapply(features_list, '[', seq(max(lengths(features_list))))
write.csv(temp,"output/features_list.csv",row.names = F, na = "")





ind = 1
#print 
df = matrix(nrow = (lasso_index+relieff_index+rf_index+max_length), ncol = 3, dimnames = list(c(),c("model","feature","bucket")))
df[ind:lasso_index,"model"] = paste0("Lasso\n(n=",lasso_index,")")
df[ind:lasso_index, c("feature")] = features_list$lasso

ind = lasso_index
df[(ind +1):(ind+relieff_index),"model"] = paste0("Relieff\n(n=",relieff_index,")")
df[(ind +1):(ind+relieff_index), c("feature")] = features_list$relieff

ind = ind + relieff_index
df[(ind +1):(ind+rf_index),"model"] = paste0("Random Forest\n(n=",rf_index,")")
df[(ind +1):(ind+rf_index), c("feature")] = features_list$rf

ind = ind + rf_index
df[(ind +1):(ind+max_length),"model"] = paste0("Mean Rank\n(n=",max_length,")")
df[(ind +1):(ind+max_length), c("feature")] = features_list[[paste0("mean_rank_",max_length)]]


df = as.data.frame(df)
df$bucket = ifelse( df$feature %in% buckets_features_names$demographics,  "Demographics", 
            ifelse( df$feature %in% buckets_features_names$environment,  "Neighborhood", 
            ifelse( df$feature %in% buckets_features_names$family,  "Family", 
            ifelse( df$feature %in% buckets_features_names$trauma,  "Trauma", 
            ifelse( df$feature %in% buckets_features_names$cognitive,  "Neurocognitive", "Clinical")))) )
           
            
  



write.csv(df,"output/graph_data.csv",row.names = F)



levels_df = unique(df$model)

df$model <- factor(df$model, levels = levels_df)

ggplot(data = df , mapping = aes(x = model, fill = bucket)) +  
  geom_bar( width = 0.7, position="fill") +
  labs( y = "Percent of features in subset" , fill = "", x ="") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#F5B7B1","#ABEBC6","#ABB0EB","#F5D9B1", "#E6ABEB","#B1F5B7"))+
  theme(axis.text=element_text(size=10, colour = "black"), 
        legend.text=element_text(size=10)) 
  


