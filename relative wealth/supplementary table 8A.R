# stable8a ------------

stable8a <- clean_glm_result(glm_c_all,link = "zinf glmmTMB log") %>% 
  dplyr::select(iv,type,RR) %>% 
  mutate(iv = str_replace(iv,"C_fac",""))
stable8a %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/supp table 8A.csv"),row.names = FALSE)

# stable8b --------------
stable8a_alt <- clean_glm_result(glm_c_all_alt,link = "zinf glmmTMB log") %>% 
  dplyr::select(iv,type,RR) %>% 
  mutate(iv = str_replace(iv,"C_SH",""))
stable8a_alt %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/supp table 8A alt.csv"),row.names = FALSE)