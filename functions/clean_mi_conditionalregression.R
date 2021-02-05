source(paste0("functions/clean_glm_result.R"))


adjusted_ci = function(model_list,link="lmer identity"){
  D = length(model_list)
  
  if(link == "zinf glmmTMB log"){
    df = purrr::imap_dfr(model_list,
                         function(x,name){
                           bind_rows(summary(x)$coefficients$cond %>% 
                                       data.frame() %>% 
                                       mutate(term = row.names(.)) %>% 
                                       mutate(type = "Conditional"),
                                     summary(x)$coefficients$zi %>% 
                                       data.frame() %>% 
                                       mutate(term = row.names(.)) %>% 
                                       mutate(type = "Zero Inflation")) %>% 
                             mutate(index = name)
                         }
                         
    ) %>% 
      rename(estimate = Estimate,
             std.error = Std..Error)
  }
  
  if(link %in% c("lm","lmer identity","glmer logit","glmer log","geeglm identity")){
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           x %>% 
                             broom::tidy(exponentiate=FALSE) %>%  
                             mutate(index = name)
                         })
    
  }
  
  if(link %in% c("parametric G","doubly robust G")){
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           x %>% 
                             mutate(index = name)
                         })
    
  }
  
  
  df = df %>% 
    dplyr::filter(!is.na(std.error)) %>% 
    mutate(W_d = std.error^2) %>% 
    group_by(term) %>% 
    mutate(B_D = var(estimate)) %>% 
    dplyr::summarize(B_D = mean(B_D),
                     W_D = mean(W_d),
                     theta_D = mean(estimate)
    ) %>% 
    ungroup() %>% 
    
    mutate(T_D = W_D + (1 + 1/D)*B_D,
           gamma_D = (1 + 1/D)*(B_D/T_D),
           nu = (D-1)*((1+ (1/D+1)*(W_D/B_D))^2)
    ) %>% 
    mutate(L = theta_D + qt(p = 0.025,df = nu)*((T_D)^((1/2))),
           U = theta_D + qt(p = 0.975,df = nu)*((T_D)^((1/2))),
           sqrt_T_D = ((T_D)^((1/2)))
    )
  
  return(df)
  
}

clean_mi_conditionalregression <- function(model_list,link = "lmer identity"){
  
  
  if(link %in% c("lm","lmer identity","geeglm identity")){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link == "glmer logit"){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(OR = paste0(round_d(exp(theta_D),2)," \t (",
                         round_d(exp(L),2),", ",
                         round_d(exp(U),2),")"),
             lci = exp(L),
             uci = exp(U)
             
      ) %>% 
      rename(iv = term) 
  }
  
  if(link == "zinf glmmTMB log"){
    res_out <- adjusted_ci(model_list,link) %>% 
      # dplyr::filter(term != "(Intercept)",!is.na(std.error)) %>% 
      mutate(RR = paste0(round_d(exp(theta_D),2)," \t (",
                         round_d(exp(L),2),", ",
                         round_d(exp(U),2),")"),
             lci = exp(L),
             uci = exp(U)
             
      ) %>% 
      rename(iv = term)  
  }
  
  if(link == "parametric G"){
    res_out <- adjusted_ci(model_list,link) %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term)  
  }
  
  
  return(res_out)
  
}


extract_mi_coef_type = function(models_list, coef_name = NULL,coef_type = "Coefficient"){
  if(coef_type == "theta_D"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(theta_D) %>% 
      pull()
  }
  
  
  
  if(coef_type == "lci"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(lci) %>% 
      pull()
  }
  
  if(coef_type == "uci"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(uci) %>% 
      pull()
  }
  if(coef_type == "sqrt_T_D"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(sqrt_T_D) %>% 
      pull()
  }
  
  
  if(coef_type == "Coefficient"){
    out_est = clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
      mutate(sex = "Combined") %>% 
      dplyr::filter(iv == coef_name) %>% 
      dplyr::select(Coefficient) %>% 
      pull()
  }
  
  
  if(coef_type == "beta + se"){
    out_est = c(
      clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
        mutate(sex = "Combined") %>% 
        dplyr::filter(iv == coef_name) %>% 
        dplyr::select(theta_D) %>% 
        pull(),
      clean_mi_conditionalregression(models_list,link = "lmer identity") %>% 
        mutate(sex = "Combined") %>% 
        dplyr::filter(iv == coef_name) %>% 
        dplyr::select(sqrt_T_D) %>% 
        pull()
    )
  }
  
  return(out_est)
  
}





contrasts_geeglm <- function(fit,model_matrix,vcov_type = "robust"){
  
  vcov_gee = if(vcov_type =="robust"){
    fit$geese$vbeta}else{fit$geese$vbeta.naiv}
  
  contrast_est = coef(fit)%*%t(model_matrix)
  contrast_se = sqrt(model_matrix%*%vcov_gee%*% t(model_matrix))
  
  output = data.frame(Estimate = contrast_est[1,],
                      SE = diag(contrast_se)) %>% 
    mutate(LCI = Estimate - 1.96*SE,
           UCI = Estimate + 1.96*SE)
  
  output$term = paste0("Contrast ",c(1:nrow(output)))
  
  return(output)
  
  
  
}



clean_mi_contrasts <- function(model_list,link = "geeglm identity",model_matrix = matrix(),vcov_type="robust"){
  
  D = length(model_list)
  
  if(link %in% c("geeglm identity")){
    
    df = purrr::imap_dfr(model_list ,
                         function(x,name) {
                           contrasts_geeglm(x,model_matrix,vcov_type = vcov_type) %>% 
                             mutate(index = name)
                         }) %>% 
      dplyr::filter(!is.na(SE)) %>% 
      mutate(W_d = SE^2) %>% 
      group_by(term) %>% 
      mutate(B_D = var(Estimate)) %>% 
      dplyr::summarize(B_D = mean(B_D),
                       W_D = mean(W_d),
                       theta_D = mean(Estimate)
      ) %>% 
      ungroup() %>% 
      
      mutate(T_D = W_D + (1 + 1/D)*B_D,
             gamma_D = (1 + 1/D)*(B_D/T_D),
             nu = (D-1)*((1+ (1/D+1)*(W_D/B_D))^2)
      ) %>% 
      mutate(L = theta_D + qt(p = 0.025,df = nu)*((T_D)^((1/2))),
             U = theta_D + qt(p = 0.975,df = nu)*((T_D)^((1/2))),
             sqrt_T_D = ((T_D)^((1/2)))
      ) %>%
      mutate(Coefficient = paste0(round_d(theta_D,2)," \t(",
                                  round_d(L,2),", ",
                                  round_d(U,2),")"),
             lci = L,
             uci = U
             
      ) %>% 
      rename(iv = term) 
  }
  
  return(df)
  
  
}

