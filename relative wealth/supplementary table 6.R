
# Relative to Stable Low -------------
ipw_df <- merged_df %>% 
  left_join(alive_df %>% 
              dplyr::select(id_uni,c_alive2016,c_alive2018),
            by = "id_uni") %>% 
  mutate(
    na_ht = case_when(is.na(adht) ~ 1,
                      TRUE ~ 0),
    na_bmi = case_when(is.na(adbmi) ~ 1,
                       TRUE ~ 0),
    na_ravens = case_when(is.na(adravenstotscore) ~ 1,
                          TRUE ~ 0),
    na_srq = case_when(is.na(adsrq) ~ 1,
                       TRUE ~ 0))

rhs_formula_o = "~ pcall6775_1 + pcall1618_1 + gtadeduyr1618 + chsex + moscho_imputed + gtatole*EXPOSURE + gtchbyear"

c_ht_6 = censoring_weights(c_formula = paste0("na_ht", rhs_formula_o),
                           df = ipw_df,
                           type = "glm")

c_bmi_6 = censoring_weights(c_formula = paste0("na_bmi", rhs_formula_o),
                            df = ipw_df,
                            type = "glm")
c_ravens_6 = censoring_weights(c_formula = paste0("na_ravens", rhs_formula_o),
                               df = ipw_df,
                               type = "glm")
c_srq_6 = censoring_weights(c_formula = paste0("na_srq", rhs_formula_o),
                            df = ipw_df,
                            type = "glm")

gee_ht <- geepack::geeglm(adht~ C + SEX, 
                          data = ipw_df,
                          id = d_id_unim,
                          weights = c_ht_6*c_alive2016,
                          corstr = "unstructured")
gee_bmi <- geepack::geeglm(adbmi~ C + SEX, 
                           data = ipw_df,
                           id = d_id_unim,
                           weights = c_bmi_6*c_alive2016,
                           corstr = "unstructured")

gee_srq <- geepack::geeglm(adsrq~ C + SEX, 
                           data = ipw_df,
                           id = d_id_unim,
                           weights = c_srq_6*c_alive2018,
                           corstr = "unstructured")

gee_ravens <- geepack::geeglm(adravenstotscore~ C + SEX, 
                              data = ipw_df,
                              id = d_id_unim,
                              weights = c_ravens_6*c_alive2018,
                              corstr = "unstructured")

bind_rows(clean_glm_result(gee_ht,"geeglm identity") %>% 
            mutate(outcome = "HEIGHT"),
          clean_glm_result(gee_bmi,"geeglm identity") %>% 
            mutate(outcome = "BMI"),
          clean_glm_result(gee_srq,"geeglm identity") %>% 
            mutate(outcome = "SRQ"),
          clean_glm_result(gee_ravens,"geeglm identity") %>% 
            mutate(outcome = "RAVENS")) %>% 
  dplyr::select(outcome,iv,Coefficient) %>% 
  pivot_wider(names_from = "outcome",values_from="Coefficient") %>% 
  knitr::kable()

# Relative to Stable High ------------

gee_ht_alt <- geepack::geeglm(adht~ C_SH + SEX, 
                              data = ipw_df,
                              id = d_id_unim,
                              weights = c_ht_6*c_alive2016,
                              corstr = "unstructured")
gee_bmi_alt <- geepack::geeglm(adbmi~ C_SH + SEX, 
                               data = ipw_df,
                               id = d_id_unim,
                               weights = c_bmi_6*c_alive2016,
                               corstr = "unstructured")

gee_srq_alt <- geepack::geeglm(adsrq~ C_SH + SEX, 
                               data = ipw_df,
                               id = d_id_unim,
                               weights = c_srq_6*c_alive2018,
                               corstr = "unstructured")

gee_ravens_alt <- geepack::geeglm(adravenstotscore~ C_SH + SEX, 
                                  data = ipw_df,
                                  id = d_id_unim,
                                  weights = c_ravens_6*c_alive2018,
                                  corstr = "unstructured")

bind_rows(clean_glm_result(gee_ht_alt,"geeglm identity") %>% 
            mutate(outcome = "HEIGHT"),
          clean_glm_result(gee_bmi_alt,"geeglm identity") %>% 
            mutate(outcome = "BMI"),
          clean_glm_result(gee_srq_alt,"geeglm identity") %>% 
            mutate(outcome = "SRQ"),
          clean_glm_result(gee_ravens_alt,"geeglm identity") %>% 
            mutate(outcome = "RAVENS")) %>% 
  dplyr::select(outcome,iv,Coefficient) %>% 
  pivot_wider(names_from = "outcome",values_from="Coefficient") %>% 
  knitr::kable()

