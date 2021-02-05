
glm_c <-  glmmTMB(as.formula(paste0("adsrq ~ C + SEX + (1|d_id_unim)")),
                  ziformula = ~1+SEX,
                  family="poisson",
                  data = merged_df)
clean_glm_result(glm_c,link = "zinf glmmTMB log") %>% 
  dplyr::select(iv,type,RR)


glm_c_alt <-  glmmTMB(as.formula(paste0("adsrq ~ C_SH + SEX + (1|d_id_unim)")),
                      ziformula = ~1+SEX,
                      family="poisson",
                      data = merged_df)

clean_glm_result(glm_c_alt,link = "zinf glmmTMB log") %>% 
  dplyr::select(iv,type,RR)