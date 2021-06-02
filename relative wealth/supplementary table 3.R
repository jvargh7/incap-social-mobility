# SETUP ------------
library(MplusAutomation)


# Run Tertiles ---------

lca_tertiles <- readModels(paste0(path_mplus_working,"/lca/tertiles"))

# Run Tertiles with Covariates ---------
lca_twc <- readModels(paste0(path_mplus_working,"/lca/tertiles w covariates"))

# # Run Tertiles with Covariates with Cluster ---------
# lca_twcm <- readModels(paste0(path_mplus_working,"/lca/tertiles w covariates mom"))


summaries <- bind_rows(imap_dfr(lca_tertiles,.f=function(x,name){data.frame(model = name,
                                                                            x$summaries,
                                                                            class_counts = x$class_counts$mostLikely$count %>% paste0(.,collapse=","))}),
                       imap_dfr(lca_twc,.f=function(x,name){data.frame(model = name,
                                                                       x$summaries,
                                                                       class_counts = x$class_counts$mostLikely$count %>% paste0(.,collapse=","))})
                       # imap_dfr(lca_twcm,.f=function(x,name){data.frame(model = name,
                       #                                                 x$summaries)}),
                       
)  %>% 
  dplyr::select(Title,Filename,Observations,AIC,BIC,aBIC,AICC,Entropy,
                T11_VLMR_2xLLDiff,T11_VLMR_PValue,
                BLRT_2xLLDiff,BLRT_PValue,class_counts
  ) %>% 
  mutate(C = str_extract(Filename,"([0-9]\\.out)") %>% str_replace(.,"\\.out","") %>% as.numeric(),
         type = str_extract(Filename,"_([a-z\\s])+_") %>% str_replace_all(.,"_","")) %>% 
  group_by(type) %>% 
  mutate(BIC_rank = rank(BIC),
         BLRT_better = case_when(BLRT_PValue < 0.05 ~ 0,
                                 TRUE ~ 1)*1.5,
         VLMR_better = case_when(T11_VLMR_PValue < 0.05 ~ 0,
                                 TRUE ~ 1)*1.5,
         Entropy_rank = rank(-Entropy)
  ) %>% 
  ungroup() %>% 
  mutate(overall = rowSums(.[,c("BIC_rank","BLRT_better","VLMR_better","Entropy_rank")])) %>% 
  arrange(overall)

summaries %>% 
  dplyr::select(Title,Filename,C,class_counts,BIC,aBIC,BLRT_PValue,Entropy) %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/stable3.csv"))
