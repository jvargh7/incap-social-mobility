stable9a <- read.csv(paste0(path_dissertation,"/aim 2/working/incap/stable 9a complete.csv")) 
stable9b <- read.csv(paste0(path_dissertation,"/aim 2/working/incap/stable 9b complete.csv"))


comparison_stable9 <- left_join(stable9a %>% 
                                  dplyr::select(iv,outcome,theta_D,sqrt_T_D) %>% 
                                  rename(est67 = theta_D,
                                         se67 = sqrt_T_D),
                                stable9b %>% 
                                  dplyr::select(iv,outcome,theta_D,sqrt_T_D) %>% 
                                  rename(est75 = theta_D,
                                         se75 = sqrt_T_D),
                                by = c("iv","outcome")
) %>% 
  mutate(z_diff = (est75-est67)/sqrt(se67^2 + se75^2)) %>% 
  dplyr::select(iv,outcome,z_diff) %>% 
  pivot_wider(names_from="outcome",values_from="z_diff")
