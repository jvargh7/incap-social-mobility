library(broom)
# library(geepack)

png(paste0(path_dissertation,"/aim 2/figures/sex interaction.png"),width = 12, height = 8, units = "in",res=300)
m_bmi2f <- lm(adbmi ~ C,data=merged_df %>% dplyr::filter(SEX=="Female"))
m_bmi2m <- lm(adbmi ~ C,data=merged_df %>% dplyr::filter(SEX=="Male"))
m_ht2f <- lm(adht ~ C,data=merged_df %>% dplyr::filter(SEX=="Female"))
m_ht2m <- lm(adht ~ C,data=merged_df %>% dplyr::filter(SEX=="Male"))
m_srq2f <- lm(adsrq ~ C ,data=merged_df %>% dplyr::filter(SEX=="Female"))
m_srq2m <- lm(adsrq ~ C ,data=merged_df %>% dplyr::filter(SEX=="Male"))
m_ravens2f <- lm(adravenstotscore~ C,data=merged_df %>% dplyr::filter(SEX=="Female"))
m_ravens2m <- lm(adravenstotscore~ C,data=merged_df %>% dplyr::filter(SEX=="Male"))

bind_rows(tidy(m_ht2f) %>% 
            mutate(outcome="HEIGHT",
                   sex = "Female"),
          tidy(m_bmi2f) %>% 
            mutate(outcome="BMI",
                   sex = "Female"),
          tidy(m_srq2f) %>% 
            mutate(outcome = "SRQ-20",
                   sex = "Female"),
          tidy(m_ravens2f) %>% 
            mutate(outcome = "RAVENS",
                   sex = "Female"),
          tidy(m_ht2m) %>% 
            mutate(outcome="HEIGHT",
                   sex = "Male"),
          tidy(m_bmi2m) %>% 
            mutate(outcome="BMI",
                   sex = "Male"),
          tidy(m_srq2m) %>% 
            mutate(outcome = "SRQ-20",
                   sex = "Male"),
          tidy(m_ravens2m) %>% 
            mutate(outcome = "RAVENS",
                   sex = "Male"),
          tidy(m_ht) %>% 
            mutate(outcome="HEIGHT",
                   sex = "Pooled"),
          tidy(m_bmi) %>% 
            mutate(outcome="BMI",
                   sex = "Pooled"),
          tidy(m_srq) %>% 
            mutate(outcome = "SRQ-20",
                   sex = "Pooled"),
          tidy(m_ravens) %>% 
            mutate(outcome = "RAVENS",
                   sex = "Pooled")
          
) %>% 
  mutate(term = str_replace(term,"CClass","Class")) %>%
  # mutate(term = case_when(term == "Class 1" ~ "Stable Low",
  #                         term == "Class 2" ~ "Stable High",
  #                         term == "Class 3" ~ "Downwardly Mobile",
  #                         term == "Class 4" ~ "Upwardly Mobile")) %>% 
  bind_rows(data.frame(term = "Class 1",
                       estimate = 0,
                       sex = rep(c("Female","Male","Pooled"),each = 4),
                       outcome = rep(c("HEIGHT","BMI","SRQ-20","RAVENS"),times = 3)
  )) %>% 
  dplyr::filter(! term %in% c("(Intercept)","SEXMale")) %>% 
  
  mutate(term = factor(term,levels= paste0("Class ", c(1:4)), 
                       labels=c("Stable Low","Downwardly Mobile","Upwardly Mobile","Stable High"))) %>% 
  
  ggplot(data=.,aes(x=reorder(term,desc(term)),y=estimate)) +
  geom_pointrange(aes(ymin=estimate-1.96*std.error,ymax=estimate+1.96*std.error)) +
  geom_hline(yintercept=0,col="red",linetype=2) +
  # scale_x_discrete(limits = rev(levels(term))) +
  coord_flip() +
  theme_bw(base_size = 12) +
  theme(axis.text = element_text(size=14),
        strip.text = element_text(size=16),
        axis.title = element_text(size=16)) +
  xlab("") +
  ylab("Linear Regression Coefficient") +
  facet_grid(outcome ~sex)
dev.off()


# ANOVA -------
m_ht2 <- lm(adht ~ C*SEX,data=merged_df)
m_bmi2 <- lm(adbmi ~ C*SEX,data=merged_df)
m_srq2 <- lm(adsrq ~ C*SEX ,data=merged_df)
m_ravens2 <- lm(adravenstotscore~ C*SEX,data=merged_df)

anova(m_ht,m_ht2)
anova(m_bmi,m_bmi2)
anova(m_srq,m_srq2)
anova(m_ravens,m_ravens2)