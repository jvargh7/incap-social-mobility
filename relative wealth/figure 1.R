png(paste0(path_dissertation,"/aim 2/figures/mean wealth z by class.png"),res=300,width = 12,height=6,units = "in")
merged_df %>% 
  mutate_at(vars(one_of("pcall6775_1","pcall1987_1","pcall2002_1","gtadwealthindex1618")),~scale(.)) %>% 
  rename(w1 = pcall6775_1,
         w2 = pcall1987_1,
         w3 = pcall2002_1,
         w4 = gtadwealthindex1618) %>% 
  dplyr::filter(C!="Class NA") %>% 
  group_by(C) %>% 
  summarize_at(vars(one_of(paste0("w",c(1:4)))), ~mean(.,na.rm=TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols=starts_with("w"),names_to = "time",values_to = "mean") %>% 
  mutate(time = factor(time,levels=paste0("w",c(1:4)), labels=c("1967-75","1987","2002","2015-18")),
         # C = factor(C,labels=paste0("Class ",c(1:4)))
         C = factor(C,labels=c("Stable Low",
                               "Downwardly Mobile",
                               "Upwardly Mobile",
                               "Stable High"
         ))
  ) %>% 
  ggplot(data=.,aes(x=time,y=mean,col=C,group=C)) +
  geom_path(size=3) +
  geom_point(col="black",size=4) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  scale_color_discrete("") +
  ylab("Mean cross-sectional wealth z-score") +
  xlab("")
dev.off()