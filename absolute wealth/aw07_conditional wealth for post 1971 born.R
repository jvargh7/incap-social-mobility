
source(paste0(path_replication_repo,"/package/residuals_mi.R"))
source(paste0(path_replication_repo,"/package/rsquared_mi.R"))

# AUXILIARY NOT IN CONDITIONALS --------------
# 1987 ------

# not adjusting for gtadeduyr1618
# pcall1987_1 ~ pcall6775_1 + moscho_sib + byear + sex + gtatole
model_cwealth1987 <- '
pcall1987_1 ~ pcall6775_1
'
result_cwealth1987 <- with(miaux_dfs75,lm(as.formula(model_cwealth1987)))

cwealth1987_imp <- residuals_mi(result_cwealth1987,method="identity")

# 2002 ------
# pcall2002_1 ~ pcall6775_1 + pcall1987_1 + moscho_sib + byear + sex + gtatole + gtadeduyr1618
model_cwealth2002 <- '
pcall2002_1 ~ pcall6775_1 + pcall1987_1
'
result_cwealth2002 <- with(miaux_dfs75,lm(as.formula(model_cwealth2002)))

cwealth2002_imp <- residuals_mi(result_cwealth2002,method="identity")

# 2015-18 ------
# pcall1618_1 ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_sib + byear + sex + gtatole + gtadeduyr1618
model_cwealth1618 <- '
pcall1618_1 ~ pcall6775_1 + pcall1987_1 + pcall2002_1
'
result_cwealth1618 <- with(miaux_dfs75,lm(as.formula(model_cwealth1618)))

cwealth1618_imp <- residuals_mi(result_cwealth1618,method="identity")