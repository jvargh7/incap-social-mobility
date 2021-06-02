library(MplusAutomation)

final_model <- readModels(paste0(path_mplus_working,"/lca/tertiles w covariates/tertiles w covariates_4.out"))

final_model$class_counts$classificationProbs.mostLikely %>% 
  data.frame() %>% 
  rename_all(~class_labels) %>% 
  mutate(C = factor(row.names(.),levels=c(1:4),
                    labels=class_labels)) %>% 
  dplyr::select(C,'Stable Low','Stable High','Downwardly Mobile','Upwardly Mobile') %>% 
  .[match(c('Stable Low','Stable High','Downwardly Mobile','Upwardly Mobile'),.$C),] %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/stable4.csv"))
