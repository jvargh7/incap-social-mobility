merged_df %>% 
  dplyr::filter(!is.na(GTATOLE)) %>% 
  compareGroups(C ~ GTATOLE + EXPOSURE + ATOLE100 + MOSCHO +  gtchbyear + SEX + gtadeduyr1618 + urbano_rural + T1 + T2 + T3 + T4 + adbmi + adsrq + adravenstotscore + moscho_sib + adht,data=.,
                method = c(1,1,1,2,2,1,2,3,3,3,3,3,1,2,1,2,1),include.miss = TRUE) %>% 
  createTable(.,type = 1,show.n = TRUE,sd.type = 2,q.type = c(1,2),show.all = TRUE)