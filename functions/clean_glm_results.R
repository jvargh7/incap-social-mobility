clean_glm_result <- function(glm_result,link="logit"){
  
  if(link == "glmer logit"){
    res_out <- glm_result %>% 
      broom::tidy(.) %>% 
      dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(OR = paste0(round(exp(estimate),2)," \t (",
                         round(exp(estimate - 1.96*std.error),2),", ",
                         round(exp(estimate + 1.96*std.error),2),")"),
             lci = exp(estimate - 1.96*std.error),
             uci = exp(estimate + 1.96*std.error)
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link == "glmer log"){
    res_out <- glm_result %>% 
      broom::tidy(.) %>% 
      dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(RR = paste0(round(exp(estimate),2)," \t (",
                         round(exp(estimate - 1.96*std.error),2),", ",
                         round(exp(estimate + 1.96*std.error),2),")"),
             lci = exp(estimate - 1.96*std.error),
             uci = exp(estimate + 1.96*std.error)
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link == "logit"){
    res_out <- glm_result %>% 
      broom::tidy(.,exponentiate=TRUE) %>% 
      dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(OR = paste0(round(estimate,2)," \t(",
                         round(estimate*exp(- 1.96*std.error),2),", ",
                         round(estimate*exp(+ 1.96*std.error),2),")"),
             lci = estimate*exp(- 1.96*std.error),
             uci = estimate*exp(+ 1.96*std.error)
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link == "multinom logit"){
    res_out <- glm_result %>% 
      broom::tidy(.,exponentiate=TRUE) %>% 
      dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(OR = paste0(round(estimate,2)," \t(",
                         round(estimate*exp(- 1.96*std.error),2),", ",
                         round(estimate*exp(+ 1.96*std.error),2),")"),
             lci = estimate*exp(- 1.96*std.error),
             uci = estimate*exp(+ 1.96*std.error)
             
      ) %>% 
      rename(iv = term) 
  }
  
  
  if(link == "log"){
    res_out <- glm_result %>% 
      broom::tidy(.,exponentiate=TRUE) %>% 
      dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(RR = paste0(round(estimate,2)," \t(",
                         round(estimate*exp(- 1.96*std.error),2),", ",
                         round(estimate*exp(+ 1.96*std.error),2),")"),
             lci = estimate*exp(- 1.96*std.error),
             uci = estimate*exp(+ 1.96*std.error)
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link %in% c("lm","lmer identity","geeglm identity")){
    res_out <- glm_result %>% 
      broom::tidy(.) %>% 
      dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(Coefficient = paste0(round(estimate,2)," \t(",
                                  round(estimate - 1.96*std.error,2),", ",
                                  round(estimate + 1.96*std.error,2),")"),
             lci = estimate - 1.96*std.error,
             uci = estimate + 1.96*std.error
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link == "zinf glmmTMB log"){
    res_out <- bind_rows(summary(glm_result)$coefficients$cond %>% 
                           data.frame() %>% 
                           mutate(term = row.names(.)) %>% 
                           mutate(type = "Conditional"),
                         summary(glm_result)$coefficients$zi %>% 
                           data.frame() %>% 
                           mutate(term = row.names(.)) %>% 
                           mutate(type = "Zero Inflation")
    ) %>% 
      dplyr::filter(term != "(Intercept)",!is.na(Std..Error)) %>% 
      mutate(RR = paste0(round(exp(Estimate),2)," \t(",
                         round(exp(Estimate - 1.96*Std..Error),2),", ",
                         round(exp(Estimate + 1.96*Std..Error),2),")"),
             lci = exp(Estimate - 1.96*Std..Error),
             uci = exp(Estimate + 1.96*Std..Error)
             
      ) %>% 
      rename(iv = term) 
  }
  
  
  return(res_out)
  
}



