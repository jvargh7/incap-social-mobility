library(compareGroups)
library(glmmTMB)

display_results <- function(results_df){
  
  results_df %>% 
    mutate(iv = case_when(iv == "gtatole" ~ "ATOLE (PERIOD = NONE)",
                          iv == "byear" ~ "BIRTH YEAR - 1962",
                          iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL (FRESCO)",
                          iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL (FRESCO)",
                          iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                          iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                          
                          iv == "exposure1000" ~ "PERIOD = FULL (FRESCO)",
                          iv %in% c("gtatole:exposure1000","atole1000") ~ "ATOLE x FULL",
                          
                          iv == "pcall6775_1" ~ "CHILD SOCIAL CLASS (z-scores)",
                          iv == "cs_1987" ~ "CONDITIONAL Z-SCORE 1987",
                          iv == "cs_2002" ~ "CONDITIONAL Z-SCORE 2002",
                          iv == "cs_1618" ~ "CONDITIONAL Z-SCORE 2015-18",
                          
                          
                          iv == "gtvillageAT_CO" ~ "VILLAGE = CONACASTE (AT)",
                          iv == "gtvillageAT_SJ" ~ "VILLAGE = SAN JUAN (AT)",
                          iv == "gtvillageFR_SD" ~ "VILLAGE = SANTO DOMINGO (FR)",
                          iv == "gtvillageFR_ES" ~ "VILLAGE = ESPIRITU SANTO (FR)",
                          
                          iv == "chsexfemale" ~ "Sex = FEMALE",
                          
                          iv == "scale(moht_stataimp)" ~ "MATERNAL HEIGHT (STATA IMPUTED, relative z-scores)",
                          iv == "moscho_stataimp" ~ "MATERNAL SCHOOLING (STATA IMPUTED)",
                          iv == "moscho_sib" ~ "MATERNAL SCHOOLING",
                          
                          iv == "score_change_ge300yes" ~ "(HOLMS RAHE STRESS >= 300)",
                          iv %in% c("scale(adeduyr)","scale(gtadeduyr1618)") ~ "COMPLETED YEARS OF SCHOOLING (relative z-scores)",
                          iv %in% c("gtadeduyr1618","adeduyr") ~ "COMPLETED YEARS OF SCHOOLING",
                          iv == "gtadwealthindex2018" ~ "ADULT SOCIAL CLASS (z-scores)",
                          TRUE ~ NA_character_
    )
    ) %>% 
    pivot_wider(names_from="sex",values_from = "Coefficient") %>% 
    knitr::kable(format="markdown") %>% 
    print(.)
  
}

# BMI ------------

formula_bmi = "adbmi ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear"

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

formula_srq = "adsrq ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear"


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

formula_ravens = "adravenstotscore ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear"

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