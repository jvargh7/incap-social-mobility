library(compareGroups)
library(glmmTMB)

source(paste0("absolute wealth/aw02_multiple imputation with auxiliary.R"))
source(paste0("functions/gmethods_functions.R"))
source(paste0("functions/display_results.R"))
source(paste0("absolute wealth/aw04_ipw for mi.R"))
source(paste0("relative wealth/rw04_ipaw for alive.R"))

rhs_formula_o = "~ pcall6775_1 + pcall1987_1 + pcall2002_1 + pcall1618_1 + moscho_sib + byear + sex + gtatole*exposure1000 + gtadeduyr1618"


# BMI ------------


formula_bmi = "adbmi ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"

# i = 1

models_bmi_o <- list()
models_bmi_c <- list()

for (i in 1:mi_iter){
  
  cat("\n Iteration ",i)
  
  model_df = analysis7_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs,i)$moscho_sib
           
    ) %>% 
    mutate(
      na_bmi = case_when(is.na(adbmi) ~ 1,
                         TRUE ~ 0)) %>% 
    left_join(alive_df %>% 
                dplyr::select(id_uni,c_alive2016),
              by = "id_uni")
  
  
  c_bmi_6 = censoring_weights(c_formula = paste0("na_bmi", rhs_formula_o),
                              df = complete(miaux_dfs,i) %>% 
                                mutate(na_bmi = model_df$na_bmi),
                              type = "glm")
  
  
  model_df$c_bmi_6 = c_bmi_6
  model_df <- model_df %>% 
    dplyr::filter(!is.na(adbmi))
  
  
  glm_o <- geepack::geeglm(as.formula(paste0(formula_bmi,"+ chsex")),
                           family = gaussian(),
                           # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                           # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                           # control=glmControl(optimizer ="nloptwrap2"),
                           data = model_df,
                           id = d_id_unim,
                           weights = c_bmi_6,
                           corstr = "unstructured")
  
  glm_c <- geepack::geeglm(as.formula(paste0(formula_bmi,"+ chsex")),
                           family = gaussian(),
                           # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                           # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                           # control=glmControl(optimizer ="nloptwrap2"),
                           data = model_df,
                           id = d_id_unim,
                           weights = c_bmi_6*c_alive2016,
                           corstr = "unstructured")
  
  models_bmi_c[[i]] <- glm_c
  models_bmi_o[[i]] <- glm_o
  
  bind_rows(
    clean_glm_result(glm_c,link = "geeglm identity") %>% 
      mutate(sex = "Combined")
  ) %>% 
    dplyr::select(sex,iv,Coefficient) %>% 
    display_results(.)
  
  cat("##### pagebreak")
  
  
}

bind_rows(
  clean_mi_conditionalregression(models_bmi_o,link = "lmer identity") %>% 
    mutate(sex = "IPW Outcome"),
  clean_mi_conditionalregression(models_bmi_c,link = "lmer identity") %>% 
    mutate(sex = "IPW Alive * Outcome")
) %>% 
  dplyr::select(sex,iv,Coefficient) %>% 
  display_results(.) 

# SRQ-20 ------------

formula_srq = "adsrq ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"

models_srq_o <- list()
models_srq_c <- list()

for (i in 1:mi_iter){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis7_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs,i)$moscho_sib
    ) %>% 
    mutate(
      na_srq = case_when(is.na(adsrq) ~ 1,
                         TRUE ~ 0)) %>% 
    left_join(alive_df %>% 
                dplyr::select(id_uni,c_alive2018),
              by = "id_uni")
  
  
  
  c_srq_6 = censoring_weights(c_formula = paste0("na_srq", rhs_formula_o),
                              df = complete(miaux_dfs,i) %>% 
                                mutate(na_srq = model_df$na_srq),
                              type = "glm")
  
  
  glm_o <- geepack::geeglm(as.formula(paste0(formula_srq,"+ chsex")),
                           family = gaussian(),
                           data = model_df,
                           id = d_id_unim,
                           weights = c_srq_6,
                           corstr = "unstructured")
  glm_c <- geepack::geeglm(as.formula(paste0(formula_srq,"+ chsex")),
                           family = gaussian(),
                           data = model_df,
                           id = d_id_unim,
                           weights = c_srq_6*c_alive2018,
                           corstr = "unstructured")
  
  
  models_srq_c[[i]] <- glm_c
  models_srq_o[[i]] <- glm_o
}

bind_rows(
  clean_mi_conditionalregression(models_srq_o,link = "geeglm identity") %>% 
    mutate(sex = "IPW Outcome"),
  clean_mi_conditionalregression(models_srq_c,link = "geeglm identity") %>% 
    mutate(sex = "IPW Alive*Outcome")
) %>% 
  dplyr::select(sex,iv,Coefficient) %>% 
  display_results()

# RAVENS -------------

formula_ravens = "adravenstotscore ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"

# i = 1

models_ravens_o <- list()
models_ravens_c <- list()

for (i in 1:mi_iter){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis7_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs,i)$moscho_sib
    ) %>% 
    mutate(
      na_ravens = case_when(is.na(adravenstotscore) ~ 1,
                            TRUE ~ 0)) %>% 
    left_join(alive_df %>% 
                dplyr::select(id_uni,c_alive2018),
              by = "id_uni")
  
  
  c_ravens_6 = censoring_weights(c_formula = paste0("na_ravens", rhs_formula_o),
                                 df = complete(miaux_dfs,i) %>% 
                                   mutate(na_ravens = model_df$na_ravens),
                                 type = "glm")
  
  
  glm_o <- geepack::geeglm(as.formula(paste0(formula_ravens,"+ chsex")),
                           family = gaussian(),
                           data = model_df,
                           id = d_id_unim,
                           weights = c_ravens_6,
                           corstr = "unstructured")
  glm_c <- geepack::geeglm(as.formula(paste0(formula_ravens,"+ chsex")),
                           family = gaussian(),
                           data = model_df,
                           id = d_id_unim,
                           weights = c_ravens_6*c_alive2018,
                           corstr = "unstructured")
  
  
  models_ravens_c[[i]] <- glm_c
  models_ravens_o[[i]] <- glm_o
}

bind_rows(
  clean_mi_conditionalregression(models_ravens_o,link = "lmer identity") %>% 
    mutate(sex = "IPW Outcome"),
  clean_mi_conditionalregression(models_ravens_c,link = "lmer identity") %>% 
    mutate(sex = "IPW Alive*Outcome")
) %>% 
  dplyr::select(sex,iv,Coefficient) %>% 
  display_results()
