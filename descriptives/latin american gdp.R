gdp_per_cap2010 <- read.csv(paste0(path_dissertation,"/aim 2/working/incap/world development indicators/Data_Extract_From_World_Development_Indicators/fa09820f-a8fa-4374-ba95-a52ba9afba60_Data.csv")) %>%
  dplyr::filter(Series.Code == "NY.GDP.PCAP.KD")


# https://www.britannica.com/topic/list-of-countries-in-Latin-America-2061416
latam_gpc <- gdp_per_cap2010 %>% 
  dplyr::filter(Country.Name %in% c("Belize","Costa Rica","El Salvador",
                                    "Guatemala","Honduras","Mexico","Nicaragua",
                                    "Panama","Argentina","Bolivia","Brazil","Chile",
                                    "Colombia","Ecuador",
                                    # "French Guiana",
                                    "Guyana",
                                    "Paraguay","Peru","Suriname","Uruguay","Venezuela, RB",
                                    "Cuba","Dominican Republic","Haiti")) %>% 
  rename_at(vars(starts_with("X")),~str_replace(.,"X[0-9]{4}[\\.]{2}","")) %>% 
  dplyr::select(Country.Name,YR1971.,YR2014.) %>% 
  mutate_at(vars(starts_with("YR")),~as.numeric(levels(.))[.]) %>% 
  # https://www.investopedia.com/terms/c/cagr.asp#:~:text=Formula%20and%20Calculation%20of%20CAGR,-C%20A%20G&text=To%20calculate%20the%20CAGR%20of,one%20from%20the%20subsequent%20result.
  mutate(cagr = (YR2014./YR1971.)^(1/42) -1)

summary(latam_gpc$cagr)


gdp_per_cap2010 <- read.csv(paste0(path_dissertation,"/aim 2/working/incap/world development indicators/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_2252313/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_2252313.csv"),skip = 4)

latam_gpc <- gdp_per_cap2010 %>% 
  dplyr::filter(Country.Name %in% c("Belize","Costa Rica","El Salvador",
                                    "Guatemala","Honduras","Mexico","Nicaragua",
                                    "Panama","Argentina","Bolivia","Brazil","Chile",
                                    "Colombia","Ecuador",
                                    # "French Guiana",
                                    "Guyana",
                                    "Paraguay","Peru","Suriname","Uruguay","Venezuela, RB",
                                    "Cuba","Dominican Republic","Haiti")) %>% 
  rename_at(vars(starts_with("X")),~str_replace(.,"X[0-9]{4}[\\.]{2}","")) %>% 
  dplyr::select(Country.Name,X1965,X2014) %>% 
  # https://www.investopedia.com/terms/c/cagr.asp#:~:text=Formula%20and%20Calculation%20of%20CAGR,-C%20A%20G&text=To%20calculate%20the%20CAGR%20of,one%20from%20the%20subsequent%20result.
  mutate(cagr = (X2014/X1965)^(1/48) -1)
