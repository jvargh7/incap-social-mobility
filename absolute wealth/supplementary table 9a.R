analysis_df <- readRDS(paste0(path_dissertation,"/aim 2/working/incap/cs_analysis_df.RDS")) %>% dplyr::filter(gtchbyear < 71)
miaux_dfs67 <- readRDS(paste0(path_dissertation,"/aim 2/working/incap/miaux_dfs67.RDS"))
source(paste0(path_replication_repo,"/absolute wealth/aw06_conditional wealth for pre 1971 born.R"))

source(paste0(path_mobility_repo,"/functions/display_results.R"))


# BMI ------------

formula_bmi = "adbmi ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"

# i = 1

models_bmi_f <- list()
models_bmi_m <- list()
models_bmi_c <- list()

for (i in 1:miaux_dfs67$m){
  
  cat("\n Iteration ",i)
  
  model_df = analysis_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs67,i)$moscho_sib
           
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

# SRQ-20 -------------

formula_srq = "adsrq ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"


models_srq_f <- list()
models_srq_m <- list()
models_srq_c <- list()

for (i in 1:miaux_dfs67$m){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs67,i)$moscho_sib
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


# RAVENS -------------

formula_ravens = "adravenstotscore ~ pcall6775_1 + cs_1987 + cs_2002 + cs_1618 + moscho_sib + gtadeduyr1618 + gtatole*exposure1000 + byear + rural"

# i = 1

models_ravens_f <- list()
models_ravens_m <- list()
models_ravens_c <- list()

for (i in 1:miaux_dfs67$m){
  
  # cat("\n Iteration ",i)
  
  model_df = analysis_df %>% 
    mutate(cs_1987 = cwealth1987_imp %>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_2002 = cwealth2002_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           cs_1618 = cwealth1618_imp%>% dplyr::select(paste0("V",i)) %>% pull(),
           moscho_sib = complete(miaux_dfs67,i)$moscho_sib
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


# SUPPLEMENTARY TABLE 9A ------------

stable9a <- bind_rows(clean_mi_conditionalregression(models_bmi_c,link = "lmer identity") %>% 
                        mutate(sex = "Combined",outcome = "BMI",
                               nobs = nobs(models_bmi_c[[1]])),
                      clean_mi_conditionalregression(models_srq_c,link = "lmer identity") %>% 
                        mutate(sex = "Combined",outcome ="SRQ20",
                               nobs = nobs(models_srq_c[[1]])),
                      clean_mi_conditionalregression(models_ravens_c,link = "lmer identity") %>% 
                        mutate(sex = "Combined",outcome = "RAVENS",
                               nobs = nobs(models_ravens_c[[1]]))) %>% 
  dplyr::filter(iv %in% c("pcall6775_1","cs_1987","cs_2002","cs_1618")) %>% 
  mutate(iv = factor(iv,levels=c("pcall6775_1","cs_1987","cs_2002","cs_1618"),
                     ordered = TRUE,
                     labels=c("1, S1969-77",
                              "2, CS1987",
                              "3, CS2002",
                              "4, CS2015-18"
                     )
  )) %>%
  arrange(sex,iv) 

stable9a %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/stable 9a complete.csv"),row.names = FALSE)

stable9a %>% 
  dplyr::select(sex,iv,outcome, Coefficient) %>% 
  
  pivot_wider(names_from = c("sex","outcome"),values_from = "Coefficient") %>% 
  
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/stable 9a.csv"),row.names = FALSE)