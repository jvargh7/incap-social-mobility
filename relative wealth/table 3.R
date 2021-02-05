
# Relative to Stable Low -------------

m_bmi <- lm(adbmi ~ C + SEX,data=merged_df)
m_ht <- lm(adht ~ C + SEX,data=merged_df)
m_srq <- lm(adsrq ~ C + SEX ,data=merged_df)
m_ravens <- lm(adravenstotscore~ C + SEX,data=merged_df)

# class_labels=c("Stable Low",
#          "Downwardly Mobile",
#          "Upwardly Mobile",
#          "Stable High")



bind_rows(tidy(m_ht) %>% 
            mutate(outcome="HEIGHT"),
          tidy(m_bmi) %>% 
            mutate(outcome="BMI"),
          tidy(m_srq) %>% 
            mutate(outcome = "SRQ-20"),
          tidy(m_ravens) %>% 
            mutate(outcome = "RAVENS")) %>% 
  mutate(term = str_replace(term,"CClass","Class")) %>% 
  dplyr::filter(term!="(Intercept)") %>% 
  mutate(ymin=estimate-1.96*std.error,
         ymax=estimate+1.96*std.error) %>%
  mutate(out = paste0(estimate %>% round(.,2)," (",
                      ymin %>% round(.,2),", ",
                      ymax %>% round(.,2),")")) %>% 
  dplyr::select(term,out,outcome) %>% 
  pivot_wider(names_from="term",values_from="out")


# Relative to Stable High --------

merged_df$C_SH = relevel(factor(merged_df$C),ref="Class 4")
# C = factor(C,labels=c("Stable Low",
#                                "Downwardly Mobile",
#                                "Upwardly Mobile",
#                                "Stable High"
#                                )
m_bmi_alt <- lm(adbmi ~ C_SH + SEX,data=merged_df)
m_ht_alt <- lm(adht ~ C_SH + SEX,data=merged_df)
m_srq_alt <- lm(adsrq ~ C_SH + SEX ,data=merged_df)
m_ravens_alt <- lm(adravenstotscore~ C_SH + SEX,data=merged_df)


bind_rows(tidy(m_ht_alt) %>% 
            mutate(outcome="HEIGHT"),
          tidy(m_bmi_alt) %>% 
            mutate(outcome="BMI"),
          tidy(m_srq_alt) %>% 
            mutate(outcome = "SRQ-20"),
          tidy(m_ravens_alt) %>% 
            mutate(outcome = "RAVENS")) %>% 
  mutate(term = str_replace(term,"CClass","Class")) %>% 
  dplyr::filter(term!="(Intercept)") %>% 
  mutate(ymin=estimate-1.96*std.error,
         ymax=estimate+1.96*std.error) %>%
  mutate(out = paste0(estimate %>% round(.,2)," (",
                      ymin %>% round(.,2),", ",
                      ymax %>% round(.,2),")")) %>% 
  dplyr::select(term,out,outcome) %>% 
  pivot_wider(names_from="term",values_from="out")