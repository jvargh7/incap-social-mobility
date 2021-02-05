# SETUP ------------
library(MplusAutomation)
path_mplus_working <- "C:/Cloud/OneDrive - Emory University/MOOCs/Workshops/20190724_Latent Variable Models/working/"


# APPROACH 1------------
analysis_lca_dat <- analysis_gmethods_df  %>% 
  dplyr::filter((missing2016 == "Available"|
                   missing2018 == "Available"))

analysis_lca_dat <- analysis_lca_dat %>% 
  
  dplyr::mutate(bin1 = cut(pccs6775_1,
                           breaks = quantile(pccs6775_1,
                                             probs=c(0,0.5,1),
                                             na.rm=TRUE),
                           include.lowest = TRUE,
                           labels = c("low","high")),
                bin3 = cut(pccs1987_1,
                           breaks = quantile(pccs1987_1,
                                             probs=c(0,0.5,1),
                                             na.rm=TRUE),
                           include.lowest = TRUE,
                           
                           labels = c("low","high")),
                bin5 = cut(pccs2002_1,
                           breaks = quantile(pccs2002_1,
                                             probs=c(0,0.5,1),
                                             na.rm=TRUE),
                           include.lowest = TRUE,
                           
                           labels = c("low","high")),
                
                tert1 = cut(pccs6775_1,
                            breaks = quantile(pccs6775_1,
                                              probs=c(0,0.33,0.67,1),
                                              na.rm=TRUE),
                            include.lowest = TRUE,
                            labels = c("low","med","high")),
                tert3 = cut(pccs1987_1,
                            breaks = quantile(pccs1987_1,
                                              probs=c(0,0.33,0.67,1),
                                              na.rm=TRUE),
                            include.lowest = TRUE,
                            
                            labels = c("low","med","high")),
                tert5 = cut(pccs2002_1,
                            breaks = quantile(pccs2002_1,
                                              probs=c(0,0.33,0.67,1),
                                              na.rm=TRUE),
                            include.lowest = TRUE,
                            
                            labels = c("low","med","high"))) %>% 
  group_by(urbano_rural) %>% 
  mutate(bin6 = cut(pccs1618_1,
                    breaks = quantile(pccs1618_1,
                                      probs=c(0,0.5,1),
                                      na.rm=TRUE),
                    include.lowest = TRUE,
                    labels = c("low","high")),
         tert6 = cut(pccs1618_1,
                     breaks = quantile(pccs1618_1,
                                       probs=c(0,0.33,0.67,1),
                                       na.rm=TRUE),
                     include.lowest = TRUE,
                     labels = c("low","med","high"))) %>% 
  ungroup() %>% 
  mutate_at(vars(starts_with("bin")),function(x) case_when(x == "low" ~ 1,
                                                           x == "high" ~ 2,
                                                           TRUE ~ NA_real_)) %>% 
  mutate_at(vars(starts_with("tert")),function(x) case_when(x == "low" ~ 1,
                                                            x == "med" ~ 2,
                                                            x == "high" ~ 3,
                                                            TRUE ~ NA_real_)) %>% 
  mutate(sex = case_when(chsex == "female" ~ 0,
                         chsex == "male" ~ 1,
                         TRUE ~ NA_real_),
         eduyrz = scale(gtadeduyr1618),
         eduyr = case_when(!is.na(gtadeduyr1618) ~ gtadeduyr1618 - median(gtadeduyr1618,na.rm = TRUE),
                           TRUE ~ NA_real_),
         moageimp = scale(moage_imputed),
         byearz = scale(byear))

bin_vars = paste0("bin",c(1,3,5,6))
tert_vars = paste0("tert",c(1,3,5,6))

saveRDS(analysis_lca_dat, paste0(path_mplus_working,"/lca/analysis_lca_dat.RDS"))

prepareMplusData(analysis_lca_dat %>% dplyr::select(id_uni,sex,
                                                    d_id_unim,moscho_imputed,moageimp,
                                                    gtatole,exposure1000,atole1000,
                                                    byear,
                                                    eduyr,
                                                    one_of(tert_vars)) %>% 
                   mutate_all(~as.numeric(.)),
                 paste0(path_mplus_working,"/lca/tertiles/analysis_lca_tert.dat"))

prepareMplusData(analysis_lca_dat %>% dplyr::select(id_uni,sex,
                                                    d_id_unim,moscho_imputed,moageimp,
                                                    gtatole,exposure1000,atole1000,
                                                    byear,
                                                    eduyr,
                                                    one_of(tert_vars)) %>% 
                   mutate_all(~as.numeric(.)),
                 paste0(path_mplus_working,"/lca/tertiles w covariates/analysis_lca_tert.dat"))


prepareMplusData(analysis_lca_dat %>% dplyr::select(id_uni,sex,
                                                    d_id_unim,moscho_imputed,moageimp,
                                                    gtatole,exposure1000,atole1000,
                                                    byearz,byear,
                                                    eduyrz,eduyr,
                                                    one_of(bin_vars)) %>% 
                   mutate_all(~as.numeric(.)),
                 paste0(path_mplus_working,"/lca/binary/analysis_lca_bin.dat"))



