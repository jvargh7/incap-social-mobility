path_census <- paste0(path_incap_ses_box,"/BDGT_oriente/Census")



popdemogind <- read_dta(paste0(path_incap_ses_box,
                               "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                               "popdemogind.dta"))
popdemoind67 <- read_dta(paste0(path_incap_ses_box,
                                "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                                "popdemogind67.dta"))

years = c(1967,1975,1987,2002,2016,2018)



source(paste0(path_incap_repo,"/ses/ses01_pc04.R"))
source(paste0(path_incap_repo,"/ses/ses02_pc16.R"))
source(paste0(path_incap_repo,"/ses/ses03_pc18.R"))
source(paste0(path_incap_repo,"/ses/ses04_pcall update.R"))
# source(paste0(path_incap_repo,"/ses/ses05_pcadf.R"))

source(paste0(path_incap_repo,"/structural/classify_urban_rural.R"))

items <- c(
  "radio",
  "tocad",
  "coser",
  "frig",
  "tv",
  "bike",
  "moto",
  "auto",
  
  "video",
  "equipo",
  "compu",
  "telef",
  "molino",
  "washingmachine",
  "plancha",
  
  "cellphone",
  "ipod",
  "cable",
  "inter",
  "directtv",
  "micro",
  "licua",
  "cerdos",
  "aves",
  
  "sitio90",
  "tenen_7",
  "cuartv07",
  "piso_high",
  "techo_high",
  "pared_high",
  "cocin_34",
  "poele_high",
  "letrin_yes",
  "luz",
  "abasag_high",
  "desag_high",
  "garbage_5")


pca_cs_df <- data.frame(id_uni = numeric(),
                        census = numeric(),
                        comuni = numeric(),
                        familia = numeric())

ses_pca_obj = data.frame(
  year = numeric(),
  type = character(),
  item = character(),
  PC1 = numeric()
)

ses_pca_imp = data.frame(
  year = numeric(),
  row = character(),
  type = character(),
  importance = numeric()
)


for (year in years){
  
  
  # ! (2016, 2018) ------------------
  if(!year %in% c(2016,2018) ){
    temp_pca_df = pcall %>% 
      dplyr::filter(census == year) %>% 
      dplyr::select(id_uni,census,comuni,familia, one_of(items))
    
    # Impute all NA to 0 - will get removed in temp_pca()
    temp_pca_df[is.na(temp_pca_df)] <- 0
    
    temp_pca_obj = temp_pca_df %>% 
      dplyr::select(-id_uni,-census,-comuni,-familia) %>% 
      temp_pca(.,scale_term)
    
    # POST HOC CORRECTION for non 2016, non 2018 ----------
    
    
    temp_pca_df[,paste0("pcall",c("","_2","_3"))] <- temp_pca_obj$x[,1:3]*ifelse(year %in% c(1975,1987,2002),-1,1)
    
    
    
    pca_cs_df <- bind_rows(pca_cs_df,
                           temp_pca_df)
    
    ses_pca_obj = bind_rows(ses_pca_obj,
                            temp_pca_obj$rotation %>% 
                              data.frame() %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                              mutate(year = year,
                                     type = "Cross-sectional") %>% 
                              dplyr::select(year, type,row,PC1) %>% 
                              rename(item = row))
    
    ses_pca_imp = bind_rows(ses_pca_imp,
                            summary(temp_pca_obj)$importance[2,] %>% 
                              data.frame(.) %>% 
                              rename_all(~paste0("importance",.)) %>% 
                              rename(importance = importance.) %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate(year = year, type = "Cross-sectional") %>% 
                              dplyr::select(year,type,row,importance)
    )
    
    rm(temp_pca_df)
  }
  # 2016 ------------------
  if(year==2016){
    temp_pca_df_u = pcall %>% 
      dplyr::filter(census == year) %>% 
      left_join(ur %>% 
                  dplyr::select(iduni,urbano_rural2015),
                by = c("id_uni" = "iduni")) %>% 
      dplyr::filter(urbano_rural2015 == 0) %>% 
      dplyr::select(id_uni,census,comuni,familia, one_of(items))
    
    # Impute all NA to 0 - will get removed in temp_pca()
    temp_pca_df_u[is.na(temp_pca_df_u)] <- 0
    
    temp_pca_obj_u = temp_pca_df_u %>% 
      dplyr::select(-id_uni,-census,-comuni,-familia,) %>% 
      temp_pca(.,scale_term)
    
    
    temp_pca_df_r = pcall %>% 
      dplyr::filter(census == year) %>% 
      left_join(ur %>% 
                  dplyr::select(iduni,urbano_rural2015),
                by = c("id_uni" = "iduni")) %>% 
      dplyr::filter(urbano_rural2015 == 1) %>% 
      dplyr::select(id_uni,census,comuni,familia, one_of(items))
    
    temp_pca_df_r[is.na(temp_pca_df_r)] <- 0
    
    temp_pca_obj_r = temp_pca_df_r %>% 
      dplyr::select(-id_uni,-census,-comuni,-familia,) %>% 
      temp_pca(.,scale_term)
    
    # POST HOC CORRECTION for 2016 ----------
    
    temp_pca_df_r[,paste0("pcall",c("","_2","_3"))] <- temp_pca_obj_r$x[,1:3]*-1
    temp_pca_df_u[,paste0("pcall",c("","_2","_3"))] <- temp_pca_obj_u$x[,1:3]
    
    
    pca_cs_df <- bind_rows(pca_cs_df,
                           temp_pca_df_u,
                           temp_pca_df_r)
    
    ses_pca_obj = bind_rows(ses_pca_obj,
                            
                            temp_pca_obj_r$rotation %>% 
                              data.frame() %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                              mutate(year = year,
                                     type = "Rural") %>% 
                              dplyr::select(year, type,row,PC1) %>% 
                              rename(item = row),
                            
                            temp_pca_obj_u$rotation %>% 
                              data.frame() %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                              mutate(year = year,
                                     type = "Urban") %>% 
                              dplyr::select(year, type,row,PC1) %>% 
                              rename(item = row))
    
    ses_pca_imp = bind_rows(ses_pca_imp,
                            summary(temp_pca_obj_r)$importance[2,] %>% 
                              data.frame(.) %>% 
                              rename_all(~paste0("importance",.)) %>% 
                              rename(importance = importance.) %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate(year = year, type = "Rural") %>% 
                              dplyr::select(year, type, row,importance),
                            summary(temp_pca_obj_u)$importance[2,] %>% 
                              data.frame(.) %>% 
                              rename_all(~paste0("importance",.)) %>% 
                              rename(importance = importance.) %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate(year = year, type = "Urban") %>% 
                              dplyr::select(year, type,row,importance)
    )
    
    rm(temp_pca_df_r,temp_pca_df_u)
  }
  
  # 2018 ------------------
  if(year==2018){
    temp_pca_df_u = pcall %>% 
      dplyr::filter(census == year) %>% 
      left_join(ur %>% 
                  dplyr::select(iduni,urbano_rural2018),
                by = c("id_uni" = "iduni")) %>% 
      dplyr::filter(urbano_rural2018 == 0) %>% 
      dplyr::select(id_uni,census,comuni,familia, one_of(items))
    
    temp_pca_df_u[is.na(temp_pca_df_u)] <- 0
    
    temp_pca_obj_u = temp_pca_df_u %>% 
      dplyr::select(-id_uni,-census,-comuni,-familia,) %>% 
      temp_pca(.,scale_term)
    
    
    temp_pca_df_r = pcall %>% 
      dplyr::filter(census == year) %>% 
      left_join(ur %>% 
                  dplyr::select(iduni,urbano_rural2018),
                by = c("id_uni" = "iduni")) %>% 
      dplyr::filter(urbano_rural2018 == 1) %>% 
      dplyr::select(id_uni,census,comuni,familia, one_of(items))
    
    temp_pca_df_r[is.na(temp_pca_df_r)] <- 0
    
    temp_pca_obj_r = temp_pca_df_r %>% 
      dplyr::select(-id_uni,-census,-comuni,-familia,) %>% 
      temp_pca(.,scale_term)
    
    temp_pca_df_u[,paste0("pcall",c("","_2","_3"))] <- temp_pca_obj_u$x[,1:3]
    temp_pca_df_r[,paste0("pcall",c("","_2","_3"))] <- temp_pca_obj_r$x[,1:3]
    
    pca_cs_df <- bind_rows(pca_cs_df,
                           temp_pca_df_u,
                           temp_pca_df_r)
    
    rm(temp_pca_df_r,temp_pca_df_u)
    
    ses_pca_obj = bind_rows(ses_pca_obj,
                            
                            temp_pca_obj_r$rotation %>% 
                              data.frame() %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                              mutate(year = year,
                                     type = "Rural") %>% 
                              dplyr::select(year, type,row,PC1) %>% 
                              rename(item = row),
                            
                            temp_pca_obj_u$rotation %>% 
                              data.frame() %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                              mutate(year = year,
                                     type = "Urban") %>% 
                              dplyr::select(year, type,row,PC1) %>% 
                              rename(item = row))
    
    ses_pca_imp = bind_rows(ses_pca_imp,
                            summary(temp_pca_obj_r)$importance[2,] %>% 
                              data.frame(.) %>% 
                              rename_all(~paste0("importance",.)) %>% 
                              rename(importance = importance.) %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate(year = year, type = "Rural") %>% 
                              dplyr::select(year, type,row,importance),
                            summary(temp_pca_obj_u)$importance[2,] %>% 
                              data.frame(.) %>% 
                              rename_all(~paste0("importance",.)) %>% 
                              rename(importance = importance.) %>% 
                              mutate(row = rownames(.)) %>% 
                              mutate(year = year, type = "Urban") %>% 
                              dplyr::select(year, type,row,importance)
    )
    
  }
  
  
}

pcall <- pca_cs_df

source(paste0(path_incap_repo,"/ses/ses06_matching for 1967-1975.R"))
source(paste0(path_incap_repo,"/ses/ses07_pc6775.R"))
source(paste0(path_incap_repo,"/ses/ses08_pc87.R"))
# source(paste0(path_incap_repo,"/ses/ses09_pc96.R"))
source(paste0(path_incap_repo,"/ses/ses10_pc02.R"))


ses_cs <- pc6775 %>% 
  dplyr::rename(pcall6775_1 = pcall6775,
                pcall1967_1 = pcall67,
                pcall1967_2 = pcall67_2,
                pcall1967_3 = pcall67_3,
                
                pcall1975_1 = pcall75,
                pcall1975_2 = pcall75_2,
                pcall1975_3 = pcall75_3
  ) %>% 
  left_join(pc87 %>% 
              dplyr::select(id_uni,pcall:pcall_3) %>% 
              dplyr::rename(pcall1987_1 = pcall,
                            pcall1987_2 = pcall_2,
                            pcall1987_3 = pcall_3),
            by = "id_uni") %>% 
  
  # left_join(pc96 %>% 
  #             dplyr::select(id_uni,pcall:pcall_3) %>% 
  #             dplyr::rename(pcall1996_1 = pcall,
  #                           pcall1996_2 = pcall_2,
  #                           pcall1996_3 = pcall_3),
  #           by = "id_uni") %>% 
  
  left_join(pc02 %>%
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2002_1 = pcall,
                            pcall2002_2 = pcall_2,
                            pcall2002_3 = pcall_3),
            by = "id_uni") %>%
  
  left_join(meta_master %>% 
              left_join(pcall %>% 
                          dplyr::filter(census == 2016),
                        by = c("id_selected"="id_uni")) %>%
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2016_1 = pcall,
                            pcall2016_2 = pcall_2,
                            pcall2016_3 = pcall_3),
            by = "id_uni") %>% 
  
  left_join(gates_master %>% 
              left_join(pcall %>% 
                          dplyr::filter(census == 2018),
                        by = c("id_selected"="id_uni")) %>% 
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2018_1 = pcall,
                            pcall2018_2 = pcall_2,
                            pcall2018_3 = pcall_3),
            by = "id_uni")


ses_cs %>% 
  dplyr::select(-comuni_67,-comuni_75,
                -familia_67,-familia_75,
                -srcses_67,-srcses_75,
                -fechan,-master) %>% 
  
  compareGroups(comun~.-id_uni,data=.) %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)  

# save -----
saveRDS(ses_pca_obj, paste0(path_incap_ses_dfa,"/ses_cs_pca_obj.RDS"))
saveRDS(ses_pca_imp, paste0(path_incap_ses_dfa,"/ses_cs_pca_imp.RDS"))
saveRDS(pca_cs_df, paste0(path_incap_ses_dfa,"/pca_cs_df.RDS"))


saveRDS(ses_cs, paste0(path_incap_ses_dfa,"/ses_cs.RDS"))
write_dta(ses_cs, paste0(path_incap_ses_dfa,"/ses_cs.dta"),version=12)
write_csv(ses_cs, paste0(path_incap_ses_dfa,"/ses_cs.csv"))


ses_cs <- readRDS(paste0(path_incap_ses_dfa,"/ses_cs.RDS"))
