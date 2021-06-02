
# CROSS-SECTIONAL WEALTH --------------
ses_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses_cs.RDS"))
gates <- read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta"))

alive2018 <- gates %>% 
  dplyr::filter(edo_vida2018 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()

path_mplus_working <- "H:/Dissertation/aim 2/working/incap"


# ANALYSIS_GMETHODS DF ------------
# analysis_lca_dat.RDS comes from:
# aw01_analysis_df.R
# rw03_analysis_lca_dat.R

analysis_lca_dat <- readRDS(paste0(path_mplus_working,"/lca/analysis_lca_dat.RDS"))

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

class_labels=c("Upwardly Mobile","Stable High","Stable Low","Downwardly Mobile")
class_levels=c(1:4)

merged_df <- analysis_lca_dat %>% 
  dplyr::select(id_uni,adbmi,adsrq,adravenstotscore,
                adht,
                rural,
                gtchbyear,chsex,
                moscho_sib,gtadeduyr1618,
                moage_imputed, moscho_imputed,
                gtatole,
                d_id_unim,
                pcall6775_1,pcall1987_1,pcall2002_1,pcall2016_1,pcall2018_1,pcall1618_1) %>% 
  left_join(final_model$savedata %>% 
              data.frame(),
            by=c("id_uni"="ID_UNI")) %>% 
  mutate(C = paste0("Class ",C)
  ) %>% 
  mutate(C_fac = factor(C,levels=paste0("Class ",class_levels),labels=class_labels)) %>% 
  mutate(C_fac = fct_relevel(C_fac,"Stable Low",after=0)) %>% 
  mutate(C_fac = fct_relevel(C_fac,"Stable High",after=1)) %>% 
  mutate(C_fac = fct_relevel(C_fac,"Downwardly Mobile",after=2)) %>%
  mutate(C_fac = fct_relevel(C_fac,"Upwardly Mobile",after=3)) %>%
  group_by(C) %>% 
  mutate(nonNA_bmi = sum(!is.na(adbmi)),
         nonNA_srq = sum(!is.na(adsrq)),
         nonNA_ravens = sum(!is.na(adravenstotscore))
  ) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(gtadeduyr1618)) %>% 
  mutate(SEX = factor(SEX,levels=,labels=c("Female","Male"))) %>% 
  mutate(C_SH = relevel(factor(C_fac),ref="Stable High"))