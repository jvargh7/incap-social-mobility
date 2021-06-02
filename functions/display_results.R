display_results <- function(results_df){
  
  results_df %>% 
    mutate(iv = case_when(iv == "gtatole" ~ "ATOLE (PERIOD = NONE)",
                          iv == "byear" ~ "BIRTH YEAR - 1962",
                          iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL (FRESCO)",
                          iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL (FRESCO)",
                          iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                          iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                          
                          iv == "exposure1000" ~ "PERIOD = FULL (FRESCO)",
                          iv %in% c("gtatole:exposure1000","atole1000") ~ "ATOLE x FULL",
                          
                          iv == "pcall6775_1" ~ "CHILD SOCIAL CLASS (z-scores)",
                          iv == "cs_1987" ~ "CONDITIONAL Z-SCORE 1987",
                          iv == "cs_2002" ~ "CONDITIONAL Z-SCORE 2002",
                          iv == "cs_1618" ~ "CONDITIONAL Z-SCORE 2015-18",
                          
                          
                          iv == "gtvillageAT_CO" ~ "VILLAGE = CONACASTE (AT)",
                          iv == "gtvillageAT_SJ" ~ "VILLAGE = SAN JUAN (AT)",
                          iv == "gtvillageFR_SD" ~ "VILLAGE = SANTO DOMINGO (FR)",
                          iv == "gtvillageFR_ES" ~ "VILLAGE = ESPIRITU SANTO (FR)",
                          
                          iv == "chsexfemale" ~ "Sex = FEMALE",
                          iv == "rural" ~ "RURAL (=1)",
                          iv == "scale(moht_stataimp)" ~ "MATERNAL HEIGHT (STATA IMPUTED, relative z-scores)",
                          iv == "moscho_stataimp" ~ "MATERNAL SCHOOLING (STATA IMPUTED)",
                          iv == "moscho_sib" ~ "MATERNAL SCHOOLING",
                          
                          iv == "score_change_ge300yes" ~ "(HOLMS RAHE STRESS >= 300)",
                          iv %in% c("scale(adeduyr)","scale(gtadeduyr1618)") ~ "COMPLETED YEARS OF SCHOOLING (relative z-scores)",
                          iv %in% c("gtadeduyr1618","adeduyr") ~ "COMPLETED YEARS OF SCHOOLING",
                          iv == "gtadwealthindex2018" ~ "ADULT SOCIAL CLASS (z-scores)",
                          TRUE ~ NA_character_
    )
    ) %>% 
    pivot_wider(names_from="sex",values_from = "Coefficient") %>% 
    knitr::kable(format="markdown") %>% 
    print(.)
  
}