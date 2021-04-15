# IPAW -------
censoring_weights = function(c_formula,df,standardized=TRUE,type = "glmm",cluster_var = "d_id_unim"){
  
  # a_formula : CENSORING (YES = 1) ~ TREATMENT + COVARIATES
  censoring_var = str_split(c_formula," ~ ")[[1]][1]
  
  if(type == "glm"){
    d_glm <- glm(as.formula(c_formula),family="binomial",data=df)
    p_d <- 1- predict(d_glm,type="response")
    
    n_formula = str_split(c_formula," \\+ ")[[1]]
    n_glm <- glm(as.formula(n_formula),family="binomial",data=df)
    p_n <- 1 - predict(n_glm,type="response")
  }
  
  if(type == "glmm"){
    d_glm <- glmer(as.formula(paste0(c_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_d <- 1- predict(d_glm,type="response")
    
    n_formula = str_split(c_formula," + ")[[1]]
    n_glm <- glmer(as.formula(paste0(n_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_n <- 1 - predict(n_glm,type="response")
    
    
  }
  
  if(standardized==TRUE){
    w_c = p_n/p_d
  }
  if(standardized==FALSE){
    w_c = 1/p_d
  }
  
  return(w_c)
  
}


# IPTW -------
treatment_weights = function(a_formula,df,standardized=TRUE,type = "glmm",cluster_var = "d_id_unim",cutoff = 3,strata_var = NA){
  # a_formula : TREATMENT (YES = 1) ~ COVARIATES
  treatment_var = str_split(a_formula," ~ ")[[1]][1]
  treatment_vec = df %>% 
    dplyr::select(treatment_var) %>% 
    pull() 
  
  levels_vec <-  unique(treatment_vec) %>% as.character()
  
  if(is.na(strata_var)){
    n_formula = paste0(treatment_var," ~ 1")
  }
  if(!is.na(strata_var)){
    n_formula = paste0(treatment_var," ~ ",strata_var)
  }
  
  
  if(type == "glm"){
    d_glm <- glm(as.formula(a_formula),family="binomial",data=df)
    p_d <- predict(d_glm,type="response")
    
    
    
    
    n_glm <- glm(as.formula(n_formula),family="binomial",data=df)
    p_n <- predict(n_glm,type="response")
  }
  
  
  if(type == "multinom"){
    d_glm <- nnet::multinom(as.formula(a_formula),data=df,trace=FALSE)
    p_d <- predict(d_glm,type="probs")
    
    n_glm <- nnet::multinom(as.formula(n_formula),data=df,trace=FALSE)
    p_n <- predict(n_glm,type="probs")
  }
  
  
  
  if(type == "glmm"){
    d_glmer <- glmer(as.formula(paste0(a_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_d <- predict(d_glmer,type="response")
    
    n_glmer <- glmer(as.formula(paste0(n_formula," + (1|",cluster_var,")")),family="binomial",data=df)
    p_n <- predict(n_glmer,type="response")
    
    
  }
  
  
  if(type == "lm"){
    d_glm <- lm(as.formula(a_formula),data=df)
    pred_d <- predict(d_glm, type = "response")
    p_d <- dnorm(treatment_vec, pred_d, summary(d_glm)$sigma)
    
    n_glm <- lm(as.formula(n_formula),data=df)
    pred_n <- predict(n_glm, type = "response")
    p_n <- dnorm(treatment_vec, pred_n, summary(n_glm)$sigma)
  }
  
  # if(type == "gee"){
  #   df <- df %>% 
  #     mutate_("id" = cluster_var) %>% 
  #     arrange(id)
  #   d_gee <- gee(as.formula(a_formula),id = id,corstr = "unstructured",family="binomial",data=df)
  #   p_d <- predict(d_gee,type="response")
  #   
  #   n_formula = paste0(treatment_var," ~ 1")
  #   n_gee <- gee(as.formula(n_formula),id = id,corstr = "unstructured",family="binomial",data=df)
  #   p_n <- predict(n_gee,type="response")
  #   
  #   
  # }
  
  
  
  
  if(!type %in% c("multinom","lm")){ 
    if(standardized==TRUE){
      
      w_a = ifelse(df[,treatment_var] == levels_vec[1],
                   ((1-p_n)/(1-p_d)),
                   (p_n/p_d)
      )
      
    }
    if(standardized==FALSE){
      w_a = ifelse(df[,treatment_var] == levels_vec[1],
                   (1/(1-p_d)),
                   (1/p_d)
      )
    }
  }
  
  if(type == "lm"){
    if(standardized==TRUE){
      w_a = p_n/p_d
    }
    if(standardized == FALSE){
      w_a = 1/p_d
    }
    
  }
  
  if(type == "multinom"){ 
    if(standardized==TRUE){
      tx_n_p = map(1:nrow(p_n),function(x){p_n[x,treatment_vec[x]]}) %>% as.numeric()
      tx_d_p = map(1:nrow(p_d),function(x){p_d[x,treatment_vec[x]]}) %>% as.numeric()
      
      w_a = tx_n_p/tx_d_p
    }
    if(standardized==FALSE){
      
      tx_d_p = map(1:nrow(p_d),function(x){p_d[x,treatment_vec[x]]}) %>% as.numeric()
      w_a = 1/tx_d_p
      
    }
  }
  
  if(!is.na(cutoff)){
    w_a <- ifelse(w_a >cutoff,cutoff,w_a)
  }
  
  return(w_a)
  
}




# OUTCOME MODEL ----------
outcome_standardization <- function(df=data.frame(),o_formula,
                                    treatment_var = "bin6775",ind_id = "id_uni",
                                    ipw=NA,levels_vec=NA,family="gaussian"){
  
  outcome_var = str_split(o_formula," ~ ")[[1]][1]
  
  updated_formula = paste0("y ~ a + ",paste0(str_split(o_formula,"\\+")[[1]][-1],collapse="+"))
  updated_formula = str_replace_all(updated_formula,treatment_var,"a")
  
  treatment_vec = df %>% 
    dplyr::select(treatment_var) %>% 
    pull()
  
  
  
  
  df <- df %>% 
    mutate_("a" = treatment_var,
            "y" = outcome_var,
            "id"=ind_id)
  
  # if(is.character(levels_vec)|is.factor(levels_vec)){
  #   df <- df %>%  
  #     mutate(a = case_when(a == levels_vec[1] ~ 0,
  #                          a == levels_vec[2] ~ 1,
  #                          TRUE ~ NA_real_)) 
  #   
  # }
  
  
  if(length(levels_vec)<2){
    levels_vec <-  unique(treatment_vec) %>% attr(.,"levels")
  }
  d = df %>% 
    mutate(i = -1,
           wts = ipw)
  
  # CATEGORICAL TREATMENT
  if(!is.numeric(treatment_vec)|length(levels_vec)>0){
    # d0 = df %>% 
    #   mutate(i = 0,
    #          a = 0,
    #          y = NA,
    #          wts = ipw)
    # 
    # d1 = df %>% 
    #   mutate(i = 1,
    #          a = 1,
    #          y = NA,
    #          wts = ipw)
    d0 = df %>% 
      mutate(i = 0,
             a = levels_vec[1],
             y = NA,
             wts = ipw)
    
    d1 = df %>% 
      mutate(i = 1,
             a = levels_vec[2],
             y = NA,
             wts = ipw)
  }

  
  # NUMERIC TREATMENT
  if(is.numeric(treatment_vec) & length(levels_vec)==1){
    
    d0 = df %>% 
      mutate(i = 0,
             a = mean(treatment_vec)%>% round(.,2),
             y = NA,
             wts = ipw)
    
    d1 = df %>% 
      mutate(i = 1,
             a = mean(treatment_vec) + 1 %>% round(.,2),
             y = NA,
             wts = ipw)
  }
  
  
  d_combined <- bind_rows(d,d0,d1)
  
  if(length(ipw) == nrow(df)){
    fit <- geepack::geeglm(as.formula(updated_formula), 
                           data = d_combined,
                           id = id,
                           weights = wts,corstr = "independence")
    
    
  } else if(is.na(ind_id) & family == "gaussian"){
    fit <- glm(as.formula(updated_formula),family = gaussian(), 
               data = d_combined)
  } else if(is.na(ind_id) & family == "binomial"){
    fit <- glm(as.formula(updated_formula),family = binomial(), 
               data = d_combined)
  }
  
  d_combined$predicted_y = NA
  d_combined$predicted_y = predict(fit,d_combined,type="response")
  
  return(c(mean(d_combined$predicted_y[d_combined$i==-1]),
           mean(d_combined$predicted_y[d_combined$i==0]),
           mean(d_combined$predicted_y[d_combined$i==1]),
           mean(d_combined$predicted_y[d_combined$i==1])-
             mean(d_combined$predicted_y[d_combined$i==0])))
  
}
