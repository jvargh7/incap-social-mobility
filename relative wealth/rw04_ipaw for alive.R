gates <- read_dta(paste0(path_gtml_redcap_data,"/Gates_data_2019-07-12.dta"))
alive2018 <- gates %>% 
  dplyr::filter(edo_vida2018 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()
alive2016 <- gates %>% 
  dplyr::filter(edo_vida2015 %in% c(1,4,6)) %>% 
  dplyr::select(iduni) %>% 
  pull()

source(paste0(path_incap_repo,"/structural/early_life.R"))

ses_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses_masters.RDS")) %>% 
  arrange(id_uni)


alive_df <- incap_early_life %>% 
  left_join(ses_masters %>% 
              dplyr::select(id_uni,pcall6775_1),
            by = "id_uni") %>% 
  mutate(alive2018 = case_when(id_uni %in% alive2018 ~ 1,
                               TRUE ~ 0),
         alive2016 = case_when(id_uni %in% alive2016 ~ 1,
                               TRUE ~ 0)) %>% 
  mutate(exposure1000 = case_when(gtchatoleexposurestatus %in% c("partial","none") ~ 0,
                                  TRUE ~ 1))

rhs_formula_a = "~ chsex + moscho_imputed + moage_imputed + gtatole*exposure1000 + gtchbyear + pcall6775_1"
alive_df$c_alive2018 = censoring_weights(c_formula = paste0("alive2018", rhs_formula_a),
                                         df = alive_df,
                                         type = "glm")

alive_df$c_alive2016 = censoring_weights(c_formula = paste0("alive2016", rhs_formula_a),
                                         df = alive_df,
                                         type = "glm")