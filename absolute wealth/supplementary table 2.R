var2_levels = c("radio","tocad","coser",
                "frig","tv","bike",
                "moto","auto","sitio90",
                "tenen_7","cuartv07",
                "piso_high","techo_high",
                "pared_high","cocin_34",
                "poele_high","letrin_yes",
                "luz","abasag_high",
                
                "molino","plancha","cerdos",
                "aves","video","equipo","compu",
                "telef","cable","micro","licua",
                "washingmachine","cellphone","ipod",
                "inter","directtv","garbage_5","desag_high"
)
var2_labels = c("Radio","Record Player","Sewing Machine",
                "Refrigerator","Television","Bicycle",
                "Motorcycle","Automobile","Owns land",
                "Owns house","Rooms per member",
                "High quality floor","High quality roof",
                "High quality walls","Separate kitchen",
                "Formal cooking medium","Sanitary installation",
                "Electricity","Improved water source",
                
                "Hand grinder","Electric Iron","Pigs","Poultry",
                "VCR","Music system","Computer",
                "Telephone","Cable TV","Microwave","Blender",
                "Washing machine","Cellphone","Ipod",
                "Internet","Direct TV/Netflix","Public garbage system","Improved sewage system"
                
)

# Refer 
stable2 <- readRDS(paste0(path_incap_ses_dfa,"/ses_cs_pca_obj.RDS")) %>% 
  mutate(item = factor(item,levels=var2_levels,labels=var2_labels)) %>% 
  pivot_wider(names_from=c("year","type"),values_from="PC1")

write.csv(stable2,paste0(path_dissertation,"/aim 2/working/incap/stable2.csv"))

ses_pca_imp <- readRDS(paste0(path_incap_ses_dfa,"/ses_cs_pca_imp.RDS")) %>% 
  dplyr::filter(row == "PC1")
