library(compareGroups)
library(glmmTMB)

source(paste0("absolute wealth/aw02_multiple imputation with auxiliary.R"))
source(paste0("functions/gmethods_functions.R"))
source(paste0("functions/display_results.R"))

# BMI ------------

formula_bmi = "adbmi ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"

# i = 1

models_bmi_f <- list()
models_bmi_m <- list()
models_bmi_c <- list()

for (i in 1:mi_iter){
  
  cat("\n Iteration ",i)
  
  model_df = analysis7_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs,i)$moscho_sib
           
    )
  
  glm_f <- lm(as.formula(formula_bmi),
              family = gaussian(),
              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
              # control=glmControl(optimizer ="nloptwrap2"),
              data = model_df %>% 
                dplyr::filter(chsex=="female"))
  
  glm_m <- lm(as.formula(formula_bmi),
              family = gaussian(),
              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
              # control=glmControl(optimizer ="nloptwrap2"),
              data = model_df %>% 
                dplyr::filter(chsex=="male"))
  
  glm_c <- lm(as.formula(paste0(formula_bmi,"+ chsex")),
              family = gaussian(),
              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
              # control=glmControl(optimizer ="nloptwrap2"),
              data = model_df)
  
  models_bmi_f[[i]] <- glm_f
  models_bmi_m[[i]] <- glm_m
  models_bmi_c[[i]] <- glm_c
  
  bind_rows(clean_glm_result(glm_f,link = "lmer identity") %>% 
              mutate(sex = "Female"),
            clean_glm_result(glm_m,link = "lmer identity") %>% 
              mutate(sex = "Male"),
            clean_glm_result(glm_c,link = "lmer identity") %>% 
              mutate(sex = "Combined")
  ) %>% 
    dplyr::select(sex,iv,Coefficient) %>% 
    display_results(.)
  
  cat("##### pagebreak")
  
  
}

bind_rows(clean_mi_conditionalregression(models_bmi_f,link = "lmer identity") %>% 
            mutate(sex = "Female"),
          clean_mi_conditionalregression(models_bmi_m,link = "lmer identity") %>% 
            mutate(sex = "Male"),
          clean_mi_conditionalregression(models_bmi_c,link = "lmer identity") %>% 
            mutate(sex = "Combined")
) %>% 
  dplyr::select(sex,iv,Coefficient) %>% 
  display_results(.) 

# SRQ-20 ------------

formula_srq = "adsrq ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"


models_srq_f <- list()
models_srq_m <- list()
models_srq_c <- list()

for (i in 1:mi_iter){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis7_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull()
    )
  
  glm_f <- lm(as.formula(formula_srq),
              family = gaussian(),
              data = model_df %>% 
                dplyr::filter(chsex=="female"))
  
  glm_m <- lm(as.formula(formula_srq),
              family = gaussian(),
              data = model_df %>% 
                dplyr::filter(chsex=="male"))
  
  glm_c <- lm(as.formula(paste0(formula_srq,"+ chsex")),
              family = gaussian(),
              data = model_df)
  
  models_srq_f[[i]] <- glm_f
  models_srq_m[[i]] <- glm_m
  models_srq_c[[i]] <- glm_c
}

bind_rows(clean_mi_conditionalregression(models_srq_f,link = "lmer identity") %>% 
            mutate(sex = "Female"),
          clean_mi_conditionalregression(models_srq_m,link = "lmer identity") %>% 
            mutate(sex = "Male"),
          clean_mi_conditionalregression(models_srq_c,link = "lmer identity") %>% 
            mutate(sex = "Combined")
) %>% 
  dplyr::select(sex,iv,Coefficient) %>% 
  display_results()


# RAVENS ---------

formula_ravens = "adravenstotscore ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"

# i = 1

models_ravens_f <- list()
models_ravens_m <- list()
models_ravens_c <- list()

for (i in 1:mi_iter){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis7_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs,i)$moscho_sib
    )
  
  glm_f <- lm(as.formula(formula_ravens),
              family = gaussian(),
              data = model_df %>% 
                dplyr::filter(chsex=="female"))
  
  glm_m <- lm(as.formula(formula_ravens),
              family = gaussian(),
              data = model_df %>% 
                dplyr::filter(chsex=="male"))
  
  glm_c <- lm(as.formula(paste0(formula_ravens,"+ chsex")),
              family = gaussian(),
              data = model_df)
  
  
  models_ravens_f[[i]] <- glm_f
  models_ravens_m[[i]] <- glm_m
  models_ravens_c[[i]] <- glm_c
}

bind_rows(clean_mi_conditionalregression(models_ravens_f,link = "lmer identity") %>% 
            mutate(sex = "Female"),
          clean_mi_conditionalregression(models_ravens_m,link = "lmer identity") %>% 
            mutate(sex = "Male"),
          clean_mi_conditionalregression(models_ravens_c,link = "lmer identity") %>% 
            mutate(sex = "Combined")
) %>% 
  dplyr::select(sex,iv,Coefficient) %>% 
  display_results()

# HEIGHT -------------

formula_ht = "adht ~ pcall6775_1 + cs_1987 + cs_2002 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear"

# i = 1

models_ht_f <- list()
models_ht_m <- list()
models_ht_c <- list()

for (i in 1:mi_iter){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs,i)$moscho_sib
    )
  
  glm_f <- lm(as.formula(formula_ht),
              family = gaussian(),
              data = model_df %>% 
                dplyr::filter(chsex=="female"))
  
  glm_m <- lm(as.formula(formula_ht),
              family = gaussian(),
              data = model_df %>% 
                dplyr::filter(chsex=="male"))
  
  glm_c <- lm(as.formula(paste0(formula_ht,"+ chsex")),
              family = gaussian(),
              data = model_df)
  
  
  models_ht_f[[i]] <- glm_f
  models_ht_m[[i]] <- glm_m
  models_ht_c[[i]] <- glm_c
}

bind_rows(clean_mi_conditionalregression(models_ht_f,link = "lmer identity") %>% 
            mutate(sex = "Female"),
          clean_mi_conditionalregression(models_ht_m,link = "lmer identity") %>% 
            mutate(sex = "Male"),
          clean_mi_conditionalregression(models_ht_c,link = "lmer identity") %>% 
            mutate(sex = "Combined")
) %>% 
  dplyr::select(sex,iv,Coefficient) %>% 
  display_results()


# Table 4 output ------------

table4 <- bind_rows(clean_mi_conditionalregression(models_bmi_c,link = "lmer identity") %>% 
                      mutate(sex = "Combined",outcome = "BMI",
                             nobs = nobs(models_bmi_c[[1]])),
                    clean_mi_conditionalregression(models_srq_c,link = "lmer identity") %>% 
                      mutate(sex = "Combined",outcome ="SRQ20",
                             nobs = nobs(models_srq_c[[1]])),
                    clean_mi_conditionalregression(models_ravens_c,link = "lmer identity") %>% 
                      mutate(sex = "Combined",outcome = "RAVENS",
                             nobs = nobs(models_ravens_c[[1]]))) %>% 
  dplyr::select(sex,iv,outcome, Coefficient) %>% 
  dplyr::filter(iv %in% c("pcall6775_1","cs_1987","cs_2002","cs_1618")) %>% 
  mutate(iv = factor(iv,levels=c("pcall6775_1","cs_1987","cs_2002","cs_1618"),
                     ordered = TRUE,
                     labels=c("1, S1969-77",
                              "2, CS1987",
                              "3, CS2002",
                              "4, CS2015-18"
                     )
  )) %>%
  arrange(sex,iv) %>% 
  
  pivot_wider(names_from = c("sex","outcome"),values_from = "Coefficient")


write.csv(table4,paste0(path_dissertation,"/aim 2/working/incap/table 4.csv"),row.names = FALSE)