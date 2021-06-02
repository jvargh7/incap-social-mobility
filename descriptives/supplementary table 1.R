gates <- haven::read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta"))
alive2018 <- gates %>% 
  dplyr::filter(edo_vida2018 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()
alive2016 <- gates %>% 
  dplyr::filter(edo_vida2015 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()

ses_cs <- readRDS(paste0(path_incap_ses_dfa,"/ses_masters.RDS")) %>% 
  arrange(id_uni)
source(paste0(path_incap_repo,"/structural/early_life.R"))

stable1_df <- incap_early_life %>% 
  left_join(ses_cs,
            by = "id_uni") %>% 
  mutate(participation2016 = case_when(!is.na(pcall2016_1) ~ "Participated",
                                       id_uni %in% alive2016 ~ "Did not participate",
                                       TRUE ~ "Died"),
         
         participation2018 = case_when(!is.na(pcall2018_1) ~ "Participated",
                                       id_uni %in% alive2018 ~ "Did not participate",
                                       TRUE ~ "Died"),
         
         exposure1000 = case_when(gtchatoleexposurestatus %in% c("none","partial") ~ 0,
                                  gtchatoleexposurestatus %in% c("full") ~ 1,
                                  TRUE ~ NA_real_),
         atole1000 = case_when(gtchatoleexposurestatus %in% c("none","partial") ~ 0,
                               gtchatoleexposurestatus %in% c("full") & gtatole == 1 ~ 1,
                               gtchatoleexposurestatus %in% c("full") & gtatole == 0 ~ 0,
                               TRUE ~ NA_real_)
  ) %>% 
  dplyr::select(id_uni,participation2016,participation2018,
                moscho_sib,moage,pcall6775_1,
                gtchbyear,chsex,gtatole,exposure1000,atole1000
  )

stable1a <- compareGroups(participation2016 ~ .-id_uni-participation2018,data=stable1_df,
                          method = c(2,1,1,2,3,1,1,1)) %>% 
  createTable(.,digits=2,show.all=TRUE,show.n = TRUE,sd.type = 2,q.type = c(2,2))

export2xls(stable1a,file=paste0(path_dissertation,"/aim 2/working/incap/stable1a.xlsx"))

stable1b <- compareGroups(participation2018 ~ .-id_uni-participation2016,data=stable1_df,
                          method = c(2,1,1,2,3,1,1,1)) %>% 
  createTable(.,digits=2,show.all=TRUE,show.n = TRUE,sd.type = 2,q.type = c(2,2))

export2xls(stable1b,file=paste0(path_dissertation,"/aim 2/working/incap/stable1b.xlsx"))
