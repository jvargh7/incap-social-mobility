source(paste0(path_mobility_repo,"/aw01_analysis_df.R"))


library(mice)
library(lme4)

mi_iter = 10


mi_null <- mice(analysis_df %>% 
                  mutate(sex = case_when(chsex == "female" ~ 0,
                                         TRUE ~ 1),
                         byear = gtchbyear - 62,
                         
                  ) %>% 
                  dplyr::select(one_of(c("pcall6775_1","pcall1987_1","pcall2002_1","pcall1618_1",
                                         "moscho_sib","sex","byear","gtadeduyr1618",
                                         "gtatole","exposure1000","rural",
                                         "d_id_unim"))),
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix



# method[2:3] <- "rf"

# miaux_dfs67 ---------------

miaux_dfs67 <- mice(analysis_df %>% 
                      dplyr::filter(gtchbyear < 71) %>% 
                      mutate(sex = case_when(chsex == "female" ~ 0,
                                             TRUE ~ 1),
                             byear = gtchbyear - 62
                      ) %>% 
                      dplyr::select(one_of(c("pcall6775_1","pcall1987_1","pcall2002_1","pcall1618_1",
                                             "moscho_sib","sex","byear","gtadeduyr1618",
                                             "gtatole","exposure1000","rural",
                                             "d_id_unim"))),
                    method = method,
                    pred = pred,
                    m=mi_iter,maxit=50,seed=500)

plot(miaux_dfs67)

saveRDS(miaux_dfs67,paste0(path_dissertation,"/aim 2/working/incap/miaux_dfs67.RDS"))


# miaux_dfs75 ---------------

miaux_dfs75 <- mice(analysis_df %>% 
                      dplyr::filter(gtchbyear >= 71) %>% 
                      mutate(sex = case_when(chsex == "female" ~ 0,
                                             TRUE ~ 1),
                             byear = gtchbyear - 62
                      ) %>% 
                      dplyr::select(one_of(c("pcall6775_1","pcall1987_1","pcall2002_1","pcall1618_1",
                                             "moscho_sib","sex","byear","gtadeduyr1618",
                                             "gtatole","exposure1000","rural",
                                             "d_id_unim"))),
                    method = method,
                    pred = pred,
                    m=mi_iter,maxit=50,seed=500)

plot(miaux_dfs75)

saveRDS(miaux_dfs75,paste0(path_dissertation,"/aim 2/working/incap/miaux_dfs75.RDS"))
