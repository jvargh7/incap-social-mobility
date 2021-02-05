ses_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses_masters.RDS"))

gates <- read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta"))
alive2018 <- gates %>% 
  dplyr::filter(edo_vida2018 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()
alive2016 <- gates %>% 
  dplyr::filter(edo_vida2015 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()

source(paste0(path_incap_repo,"/structural/classify_urban_rural.R"))
source(paste0(path_incap_repo,"/structural/early_life.R"))


outcome_vars <- c( "adbmi","adht","adhtc3","gtadfatkg2016","adwc",
                   "adsbp","addbp",
                   
                   "adglucose","adhdl","gtadldlc2016","adtgl",
                   
                   
                   
                   
                   
                   "adravenstotscore",
                   "addccsnihcomputedscore","adexecutivefunctionfs", "adflankernihcomputedscore",
                   "adlsrawscore", "adpcrawscore",
                   
                   "adhappy","adsrq","adlifesat","gtadhappytot2018","gtadcortisol2016"
                   
)

analysis7_df <- readRDS(paste0(path_cohorts_data_for_analysis,"/cohorts_data_for_analysis.RDS")) %>%
  dplyr::filter(site == "guatemala") %>% 
  # dplyr::filter(!is.na(gtadladdercommunity2018)|!is.na(gtadladdereconomic2018)) %>% 
  dplyr::mutate(id_uni = pin - 20000000) %>% 
  dplyr::select(id_uni,
                one_of(outcome_vars),
                one_of("gtadeduyr1618","adschooling","ademployment","adrelstat",
                       "gtadladdercommunity2018",
                       "gtadladdereconomic2018")
  ) %>% 
  left_join(incap_early_life,
            by = c("id_uni")) %>% 
  left_join(ses_masters,
            by = "id_uni") %>% 
  mutate(gtvillage = factor(comun,levels=c(3,6,8,14),labels=c("FR_ES","AT_CO","FR_SD","AT_SJ"))) %>% 
  
  mutate(ht = case_when(!is.na(adht) ~ adht,
                        !is.na(adhtc3) ~ adhtc3,
                        TRUE ~ NA_real_)) %>% 
  
  mutate(fmi = gtadfatkg2016/(adht/100)^2,
         ffmi = adbmi-fmi) %>% 
  
  mutate(n_outcomes = apply(.[,outcome_vars],1,function(x) sum(!is.na(x))) %>% as.numeric(.)) %>% 
  
  mutate(missing1987 = case_when(is.na(pcall1987_1) & !is.na(pcall6775_1) ~ 1,
                                 is.na(pcall6775_1) ~ NA_real_,
                                 TRUE ~ 0) , 
         # missing1996 = case_when(is.na(pcall1996_1) & !is.na(pcall6775_1) ~ 1,
         #                         is.na(pcall6775_1) ~ NA_real_,
         #                         TRUE ~ 0),
         missing2002 = case_when(is.na(pcall2002_1) & !is.na(pcall6775_1) ~ 1,
                                 is.na(pcall6775_1) ~ NA_real_,
                                 TRUE ~ 0),
         missing2016 = case_when(is.na(pcall2016_1) & !is.na(pcall6775_1) ~ 1,
                                 is.na(pcall6775_1) ~ NA_real_,
                                 TRUE ~ 0),
         missing2018 = case_when(is.na(pcall2018_1) & !is.na(pcall6775_1) ~ 1,
                                 is.na(pcall6775_1) ~ NA_real_,
                                 TRUE ~ 0),
         gtchatoleexposurestatus = factor(gtchatoleexposurestatus,levels=c("none","partial","full")),
         gtchatoleexposurestatusnumeric = case_when(gtchatoleexposurestatus == "none" ~ 0,
                                                    gtchatoleexposurestatus == "partial" ~ 1,
                                                    gtchatoleexposurestatus == "full" ~ 2,
                                                    TRUE ~ NA_real_
         ),
         gtchatoleexposureyn = case_when(gtchatoleexposurestatus == "none" ~ 0,
                                         gtchatoleexposurestatus %in% c("full","partial") ~ 1,
                                         TRUE ~ NA_real_),
         exposure1000 = case_when(gtchatoleexposurestatus %in% c("none","partial") ~ 0,
                                  gtchatoleexposurestatus %in% c("full") ~ 1,
                                  TRUE ~ NA_real_),
         atole1000 = case_when(gtchatoleexposurestatus %in% c("none","partial") ~ 0,
                               gtchatoleexposurestatus %in% c("full") & gtatole == 1 ~ 1,
                               gtchatoleexposurestatus %in% c("full") & gtatole == 0 ~ 0,
                               TRUE ~ NA_real_),
         
         tert6775 = cut(pcall6775_1,
                        breaks = quantile(pcall6775_1,
                                          probs=c(0,0.33,0.67,1),
                                          na.rm=TRUE),
                        include.lowest = TRUE,
                        
                        labels = c("low","mid","high")),
         byear = gtchbyear - 62,
         byeargt70 = case_when(gtchbyear > 70 ~ 1,
                               TRUE ~ 0),
         marst = case_when(momarst == "" ~ NA_character_,
                           TRUE ~ momarst),
         
         employmentyn = case_when(ademployment %in% c("formal") ~ 1,
                                  ademployment %in% c("unemployed","informal") ~ 0,
                                  TRUE ~ NA_real_),
         employment_fiu = case_when(ademployment %in% c("formal","informal","unemployed") ~ ademployment,
                                    TRUE ~ NA_character_),
         
         employment_formal = case_when(ademployment %in% c("formal") ~ 1,
                                       ademployment %in% c("unemployed","informal") ~ 0,
                                       TRUE ~ NA_real_),
         employment_informal = case_when(ademployment %in% c("informal") ~ 1,
                                         ademployment %in% c("unemployed","formal") ~ 0,
                                         TRUE ~ NA_real_)
         
  ) %>% 
  mutate(pcall1618_1 = case_when(!is.na(pcall2016_1) ~ pcall2016_1,
                                 is.na(pcall2016_1) & !is.na(pcall2018_1) ~ pcall2018_1,
                                 TRUE ~ NA_real_)) %>% 
  mutate_at(vars(starts_with("missing")), function(x) factor(x,levels=c(0,1),labels=c("Available","Missing"))) %>% 
  
  dplyr::filter(id_uni %in% unique(c(alive2016,alive2018))) %>% 
  
  mutate(missing_status = case_when(
    missing1987 == "Available" & missing2002 == "Available" ~ 1,
    missing1987 == "Available" & missing2002 == "Missing" ~ 2,
    missing1987 == "Missing" & missing2002 == "Available" ~ 3,
    missing1987 == "Missing" & missing2002 == "Missing" ~ 4,
    TRUE ~ NA_real_
  )) %>% 
  mutate(missing_status = factor(missing_status,levels=c(1:4),labels=c("Both","1987 only","2002 only","Neither"))) %>% 
  
  # Merge with urban rural classification and structural
  left_join(ur %>% 
              dplyr::select(iduni,urbano_rural) %>% 
              mutate(urbano_rural = factor(urbano_rural,levels=c(0:2),labels=c("urban","rural","other")),
                     rural = case_when(urbano_rural == "urban" ~ 0,
                                       urbano_rural == "rural" ~ 1,
                                       TRUE ~ NA_real_)
              ),
            by=c("id_uni"="iduni")) %>% 
  ## The below line removes everyone who isn't having an outcome
  # dplyr::filter(n_outcomes >0) %>%
  dplyr::filter(urbano_rural %in% c("urban","rural")|is.na(urbano_rural)) %>% 
  
  dplyr::filter(!is.na(gtadeduyr1618))


attr(analysis7_df$gtvillage,"label") <- "Village Fixed Effect"
attr(analysis7_df$fmi,"label") <- "Fat Mass Index"
attr(analysis7_df$ffmi,"label") <- "Fat Free Mass Index"