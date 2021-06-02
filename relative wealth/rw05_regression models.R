
# Adjusted for sex ----------
m_bmi <- lm(adbmi ~ C_fac + SEX,data=merged_df)
m_ht <- lm(adht ~ C_fac + SEX,data=merged_df)
m_srq <- lm(adsrq ~ C_fac + SEX ,data=merged_df)
m_ravens <- lm(adravenstotscore~ C_fac + SEX,data=merged_df)

m_bmi_alt <- lm(adbmi ~ C_SH + SEX,data=merged_df)
m_ht_alt <- lm(adht ~ C_SH + SEX,data=merged_df)
m_srq_alt <- lm(adsrq ~ C_SH + SEX ,data=merged_df)
m_ravens_alt <- lm(adravenstotscore~ C_SH + SEX,data=merged_df)

glm_c <-  glmmTMB(as.formula(paste0("adsrq ~ C_fac + SEX + (1|d_id_unim)")),
                  ziformula = ~1+SEX,
                  family="poisson",
                  data = merged_df)
glm_c_alt <-  glmmTMB(as.formula(paste0("adsrq ~ C_SH + SEX + (1|d_id_unim)")),
                      ziformula = ~1+SEX,
                      family="poisson",
                      data = merged_df)

# Adjusted for sex and eduyr ----------
m_bmi_edu <- lm(adbmi ~ C_fac + SEX + EDUYR,data=merged_df)
m_ht_edu <- lm(adht ~ C_fac + SEX + EDUYR,data=merged_df)
m_srq_edu <- lm(adsrq ~ C_fac + SEX + EDUYR,data=merged_df)
m_ravens_edu <- lm(adravenstotscore~ C_fac + SEX + EDUYR,data=merged_df)



# Stratify by sex ---------

# m_bmi2f <- lm(adbmi ~ C_fac,data=merged_df %>% dplyr::filter(SEX=="Female"))
# m_bmi2m <- lm(adbmi ~ C_fac,data=merged_df %>% dplyr::filter(SEX=="Male"))
# m_ht2f <- lm(adht ~ C_fac,data=merged_df %>% dplyr::filter(SEX=="Female"))
# m_ht2m <- lm(adht ~ C_fac,data=merged_df %>% dplyr::filter(SEX=="Male"))
# m_srq2f <- lm(adsrq ~ C_fac ,data=merged_df %>% dplyr::filter(SEX=="Female"))
# m_srq2m <- lm(adsrq ~ C_fac ,data=merged_df %>% dplyr::filter(SEX=="Male"))
# m_ravens2f <- lm(adravenstotscore~ C_fac,data=merged_df %>% dplyr::filter(SEX=="Female"))
# m_ravens2m <- lm(adravenstotscore~ C_fac,data=merged_df %>% dplyr::filter(SEX=="Male"))


# Sex interaction only ------------
m_ht2 <- lm(adht ~ C_fac*SEX,data=merged_df)
m_bmi2 <- lm(adbmi ~ C_fac*SEX,data=merged_df)
m_ravens2 <- lm(adravenstotscore~ C_fac*SEX,data=merged_df)
m_srq2 <- lm(adsrq ~ C_fac*SEX ,data=merged_df)

# Sex interaction + adjusted for eduyr ------------
m_bmi_edu2 <- lm(adbmi ~ C_fac*SEX + EDUYR,data=merged_df)
m_ht_edu2 <- lm(adht ~ C_fac*SEX + EDUYR,data=merged_df)
m_srq_edu2 <- lm(adsrq ~ C_fac*SEX + EDUYR,data=merged_df)
m_ravens_edu2 <- lm(adravenstotscore~ C_fac*SEX + EDUYR,data=merged_df)


# All LCA covariates --------
m_ht_all <- lm(adht ~ C_fac + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_bmi_all <- lm(adbmi ~ C_fac + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_ravens_all <- lm(adravenstotscore~ C_fac + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_srq_all <- lm(adsrq ~ C_fac + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100 ,data=merged_df)

m_ht_all_alt <- lm(adht ~ C_SH + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_bmi_all_alt <- lm(adbmi ~ C_SH + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_ravens_all_alt <- lm(adravenstotscore~ C_SH + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_srq_all_alt <- lm(adsrq ~ C_SH + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100 ,data=merged_df)

glm_c_all <-  glmmTMB(as.formula(paste0("adsrq ~ C_fac + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100 + (1|d_id_unim)")),
                      ziformula = ~1+SEX,
                      family="poisson",
                      data = merged_df)
glm_c_all_alt <-  glmmTMB(as.formula(paste0("adsrq ~ C_SH + SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100 + (1|d_id_unim)")),
                          ziformula = ~1+SEX,
                          family="poisson",
                          data = merged_df)

# All LCA covariates + sex interaction ------------
m_bmi_all2 <- lm(adbmi ~ C_fac*SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_ht_all2 <- lm(adht ~ C_fac*SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_srq_all2 <- lm(adsrq ~ C_fac*SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
m_ravens_all2 <- lm(adravenstotscore~ C_fac*SEX + MOSCHO + GTATOLE + BYEAR + EDUYR + EXPOSURE + ATOLE100,data=merged_df)
