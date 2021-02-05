

library(mice)
library(lme4)

mi_iter = 10

source(paste0("functions/residuals_mi.R"))
source(paste0("functions/rsquared_mi.R"))

mi_null <- mice(analysis7_df %>% 
                  mutate(sex = case_when(chsex == "female" ~ 0,
                                         TRUE ~ 1),
                         byear = gtchbyear - 62,
                         
                  ) %>% 
                  dplyr::select(one_of(c("pcall6775_1","pcall1987_1","pcall2002_1","pcall1618_1",
                                         "moscho_sib","sex","byear","gtadeduyr1618",
                                         "gtatole","exposure1000",
                                         "d_id_unim"))),
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c(1:4),c(5:11)] <- 0
pred[c(5:11),c(1:4)] <- 0

# method[2:3] <- "rf"

mi_dfs <- mice(analysis7_df %>% 
                 mutate(sex = case_when(chsex == "female" ~ 0,
                                        TRUE ~ 1),
                        byear = gtchbyear - 62
                 ) %>% 
                 dplyr::select(one_of(c("pcall6775_1","pcall1987_1","pcall2002_1","pcall1618_1",
                                        "moscho_sib","sex","byear","gtadeduyr1618",
                                        "gtatole","exposure1000",
                                        "d_id_unim"))),
               method = method,
               pred = pred,
               m=mi_iter,maxit=50,seed=500)

plot(mi_dfs)



# AUXILIARY NOT IN CONDITIONALS --------------
# 1987 ------

# not adjusting for gtadeduyr1618
model_cwealth1987 <- '
pcall1987_1 ~ pcall6775_1
'
result_cwealth1987 <- with(mi_dfs,lm(as.formula(model_cwealth1987)))

cwealth1987_imp <- residuals_mi(result_cwealth1987,method="identity")

# 2002 ------
model_cwealth2002 <- '
pcall2002_1 ~ pcall6775_1 + pcall1987_1
'
result_cwealth2002 <- with(mi_dfs,lm(as.formula(model_cwealth2002)))

cwealth2002_imp <- residuals_mi(result_cwealth2002,method="identity")

# 2015-18 ------
model_cwealth1618 <- '
pcall1618_1 ~ pcall6775_1 + pcall1987_1 + pcall2002_1
'
result_cwealth1618 <- with(mi_dfs,lm(as.formula(model_cwealth1618)))

cwealth1618_imp <- residuals_mi(result_cwealth1618,method="identity")