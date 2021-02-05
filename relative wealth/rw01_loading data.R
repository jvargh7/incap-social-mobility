
# CROSS-SECTIONAL WEALTH --------------
ses_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses_cs.RDS"))
gates <- read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta"))

alive2018 <- gates %>% 
  dplyr::filter(edo_vida2018 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()

path_mplus_working <- "C:/Cloud/OneDrive - Emory University/MOOCs/Workshops/20190724_Latent Variable Models/working/"


# ANALYSIS_GMETHODS DF ------------
# analysis_lca_dat.RDS comes from:
# rw02_analysis_gmethods_df.R
# rw03_analysis_lca_dat.R

analysis_gmethods <- readRDS(paste0(path_mplus_working,"/lca/analysis_lca_dat.RDS"))

# FINAL LCA MODEL ------------
library(MplusAutomation)
path_mplus_working <- "C:/Cloud/OneDrive - Emory University/MOOCs/Workshops/20190724_Latent Variable Models/working/"
expit = function(mu,se){
  out = case_when(!is.na(se) ~ paste0(exp(mu) %>% round(.,2)," (",
                                      exp(mu + qnorm(0.025)*se) %>% round(.,2),", ",
                                      exp(mu + qnorm(0.975)*se) %>% round(.,2),")"),
                  TRUE ~ "Ref (1.00)")
  return(out)
}

# col_names <- 


final_model <- readModels(paste0(path_mplus_working,"/lca/tertiles w covariates/tertiles w covariates_4.out"))


# MERGE LCA WITH ANALYSIS_GMETHODS DF ------------

merged_df <- analysis_gmethods %>% 
  dplyr::select(id_uni,adbmi,adsrq,adravenstotscore,
                adht,
                urbano_rural,
                gtchbyear,chsex,
                moscho_sib,gtadeduyr1618,
                moage_imputed, moscho_imputed,
                gtatole,
                d_id_unim,
                pcall6775_1,pcall1987_1,pcall2002_1,pcall2016_1,pcall2018_1,pcall1618_1,gtadwealthindex1618) %>% 
  left_join(final_model$savedata %>% 
              data.frame(),
            by=c("id_uni"="ID_UNI")) %>% 
  mutate(C = paste0("Class ",C)
  ) %>% 
  group_by(C) %>% 
  mutate(nonNA_bmi = sum(!is.na(adbmi)),
         nonNA_srq = sum(!is.na(adsrq)),
         nonNA_ravens = sum(!is.na(adravenstotscore))
  ) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(gtadeduyr1618)) %>% 
  mutate(SEX = factor(SEX,levels=,labels=c("Female","Male")))