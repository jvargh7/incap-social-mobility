
# Ref = Stable Low ---------------
table3a <- bind_rows(tidy(m_ht_all) %>% 
                       mutate(outcome="HEIGHT"),
                     tidy(m_bmi_all) %>% 
                       mutate(outcome="BMI"),
                     tidy(m_srq_all) %>% 
                       mutate(outcome = "SRQ-20"),
                     tidy(m_ravens_all) %>% 
                       mutate(outcome = "RAVENS")) %>% 
  mutate(term = str_replace(term,"C_fac","")) %>% 
  dplyr::filter(term!="(Intercept)") %>% 
  mutate(ymin=estimate-1.96*std.error,
         ymax=estimate+1.96*std.error) %>%
  mutate(out = paste0(estimate %>% round(.,2)," (",
                      ymin %>% round(.,2),", ",
                      ymax %>% round(.,2),")")) %>% 
  dplyr::select(term,out,outcome) %>% 
  pivot_wider(names_from="outcome",values_from="out")

table3a %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/table 3a.csv"))

# Ref = Stable High --------
table3b <- bind_rows(tidy(m_ht_all_alt) %>% 
                       mutate(outcome="HEIGHT"),
                     tidy(m_bmi_all_alt) %>% 
                       mutate(outcome="BMI"),
                     tidy(m_srq_all_alt) %>% 
                       mutate(outcome = "SRQ-20"),
                     tidy(m_ravens_all_alt) %>% 
                       mutate(outcome = "RAVENS")) %>% 
  mutate(term = str_replace(term,"C_SH","")) %>% 
  dplyr::filter(term!="(Intercept)") %>% 
  mutate(ymin=estimate-1.96*std.error,
         ymax=estimate+1.96*std.error) %>%
  mutate(out = paste0(estimate %>% round(.,2)," (",
                      ymin %>% round(.,2),", ",
                      ymax %>% round(.,2),")")) %>% 
  dplyr::select(term,out,outcome) %>% 
  pivot_wider(names_from="outcome",values_from="out")

table3b %>% 
  write.csv(.,paste0(path_dissertation,"/aim 2/working/incap/table 3b.csv"))

table3b