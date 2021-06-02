
source(paste0("absolute wealth/aw02_multiple imputation with auxiliary.R"))
source(paste0("functions/gmethods_functions.R"))
source(paste0("functions/display_results.R"))
source(paste0("absolute wealth/aw04_ipw for mi.R"))
source(paste0("relative wealth/rw04_ipaw for alive.R"))


formula_srq = "adsrq ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural + (1|d_id_unim)"

# i = 1

models_srq_f <- list()
models_srq_m <- list()
models_srq_c <- list()

for (i in 1:mi_iter){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs,i)$moscho_sib
    )
  
  glm_f <-  glmmTMB(as.formula(formula_srq),
                    ziformula = ~1,
                    family="poisson",
                    data = model_df %>% 
                      dplyr::filter(chsex=="female"))
  
  glm_m <-  glmmTMB(as.formula(formula_srq),
                    ziformula = ~1,
                    family="poisson",
                    data = model_df %>% 
                      dplyr::filter(chsex=="male"))
  
  glm_c <-  glmmTMB(as.formula(paste0(formula_srq,"+ chsex")),
                    ziformula = ~1+chsex,
                    family="poisson",
                    data = model_df)
  
  
  models_srq_f[[i]] <- glm_f
  models_srq_m[[i]] <- glm_m
  models_srq_c[[i]] <- glm_c
}

stable8b <- bind_rows(clean_mi_conditionalregression(models_srq_f,link = "zinf glmmTMB log") %>% 
                        mutate(sex = "Female"),
                      clean_mi_conditionalregression(models_srq_m,link = "zinf glmmTMB log") %>% 
                        mutate(sex = "Male"),
                      clean_mi_conditionalregression(models_srq_c,link = "zinf glmmTMB log") %>% 
                        mutate(sex = "Combined")
) %>% 
  dplyr::select(sex,iv,RR) %>% 
  rename(Coefficient = RR) 

stable8b %>% 
  display_results()

write.csv(stable8b,paste0(path_dissertation,"/aim 2/working/incap/supp table8b.csv"))