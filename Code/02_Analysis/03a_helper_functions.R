quantiles_ofint <- seq(0.05, 0.95, by = 0.05)

mean_cols <- c(
  "RAW_E_LEAD", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", "RAW_E_TRAFFIC", "RAW_E_NPDES",
  "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", "RAW_E_O3", "RAW_E_PM25", "RAW_E_UST", "RAW_E_RSEI_AIR"
)

##### FUNCTIONS
# 1. gbm_get_data()
# -------------- extract data from ej_df_trial_percentile for the gbm()

gbm_get_data <- function(state2 = NULL, city2 = NULL, msa2 = NULL, region2 = NULL) {
  #### get data
  new_df <- ej_df_trial_percentile %>% mutate(state = as.character(state)) %>% mutate(city = as.character(city))
  if(!is.null(state2)) {
    new_df2 <- new_df %>% filter(state == state2)
    zone <- state2
    if(!is.null(city2)) {
      new_df2 <- new_df2 %>% filter(city == city2)
      zone <- paste0(zone, "_", city2)
    }
  } else if(!is.null(msa2)) {
    new_df2 <- new_df %>% filter(msa_name == msa2)
    zone <- msa2
  } else if(!is.null(region2)) {
    new_df2 <- new_df %>% filter(region == region2)
    zone <- region2
  } else {
    new_df2 <- new_df
    zone <- "all"
  }
  
  if(nrow(new_df2) == 0) stop("No data found")
  
  list(new_df2, zone)
}

###### gbm_get_data(msa2 = "Oklahoma City, OK")

###### gbm_get_data(region2 = "Midwest")


# 2. gbm_bootstrap()
# -------------- run `boot_i` iterations of the gbm() function

gbm_bootstrap <- function(boot_i = NULL, state3 = NULL, city3 = NULL, msa3 = NULL, region3 = NULL) {
  
  new_df2 <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[1]]
  zone <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[2]]
  
  print(zone)
  
  ############ run `boot_i` iterations of bootstrap gbm
  total_time <- 0
  n_iterations <- boot_i
  for(i in 1:n_iterations) {
    
    tryCatch({
      
      start.time <- Sys.time()
      
      set.seed(4234+i); boostrap_i <- sample(1:nrow(new_df2), nrow(new_df2), replace = TRUE)
      df_gbm_boostrap <- new_df2[boostrap_i,]
      
      set.seed(4234+i); boost_k3 <- gbm(red ~ RAW_E_DIESEL + RAW_E_CANCER + RAW_E_RESP + RAW_E_TRAFFIC + RAW_E_NPDES + RAW_E_NPL + RAW_E_RMP + RAW_E_TSDF + RAW_E_O3 + RAW_E_PM25 + RAW_E_UST + RAW_E_RSEI_AIR +
                                          RAW_D_NO_HS + RAW_D_LOWINC, 
                                        data              = df_gbm_boostrap, 
                                        distribution      =     'bernoulli',
                                        cv.folds          =               5,
                                        shrinkage         =             0.01,
                                        verbose           =           FALSE,
                                        n.trees           =            500, 
                                        bag.fraction      =             0.5, 
                                        interaction.depth =              7)
      
      temp_file <- paste0("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Redlining EJ USA/cumulativeEJ_redlining_usa/Code/02_Analysis/gbm bootstraps/gbm_redlining_env_i", i, "_", zone, ".rds")
      saveRDS(boost_k3, file = temp_file)
      
      end.time <- Sys.time()
      time.taken <- as.numeric(end.time - start.time)
      
      total_time <- total_time + time.taken
      
      if(i %% (boot_i/10) == 0) print(paste0("Iteration: ", i, " of ", n_iterations , " - time elapsed: ", round(total_time/60, 1), " min"))
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }
  
  
}

gbm_bootstrap(boot_i = 1, region3 = "Midwest")


# 3. gbm_pdp()
# -------------- prep quantile data for input into partial dependency function

gbm_pdp <- function(state3 = NULL, city3 = NULL, msa3 = NULL, region3 = NULL) {
  new_df2 <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[1]]
  
  ### get percentiles
  mean_cols <- c(
    "RAW_E_LEAD", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", "RAW_E_TRAFFIC", "RAW_E_NPDES",
    "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", "RAW_E_O3", "RAW_E_PM25", "RAW_E_UST", "RAW_E_RSEI_AIR"
  )
  
  median_env <- c()
  for(i in 1:length(mean_cols)){
    median_env[i] <- median(new_df2[[mean_cols[i]]])
  }
  
  quantiles_ofint <- seq(0.05, 0.95, by = 0.05)
  
  quantiles_env <- list()
  for(i in 1:length(mean_cols)){
    quantiles_env[[i]] <- quantile(new_df2[[mean_cols[i]]], probs = quantiles_ofint)
  }
  
  df_env <- NULL
  for(i in 1:length(mean_cols)){
    temp_df_env <- data.frame(var = quantiles_env[[i]])
    names(temp_df_env)[1] <- mean_cols[i]
    if(i == 1){
      df_env <- temp_df_env
    } else {
      df_env <- cbind(df_env, temp_df_env)
    }
    
  }
  
  #median_lowinc <- median(new_df2$RAW_D_LOWINC)
  #median_hs <- median(new_df2$RAW_D_NO_HS)
  
  # input data into bulding the cumulative association
  df_env2 <- df_env %>%
    mutate(RAW_D_NO_HS = 0,
           RAW_D_LOWINC = 0)
  
  df_env2
}

####### gbm_pdp(msa3 = "Detroit-Warren-Dearborn, MI")

gbm_pdp(region3 = "Midwest")

# 4. gbm_extract_cumulative()
# -------------- extract odds ratio for simultaneous quantile increases in each environmental indicator

gbm_extract_cumulative <- function(boot_i = NULL, ref = 0.5, state3 = NULL, city3 = NULL, msa3 = NULL, region3 = NULL) {
  
  if(!(ref %in% seq(0.05, 0.95, by = 0.05))) stop("Not a valid reference")
  
  new_df2 <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[1]]
  zone <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[2]]
  df_pdp <- gbm_pdp(state3 = state3, city = city3, msa3 = msa3, region3 = region3)
  
  # get the saved gbm data out
  # cumulative association
  ntrees_gbm <- c()
  cumul_OR_gbm <- NULL
  
  n_iterations <- boot_i
  for(i in 1:n_iterations) {
    
    set.seed(4234+i); boostrap_i <- sample(1:nrow(new_df2), nrow(new_df2), replace = TRUE)
    df_gbm_boostrap <- new_df2[boostrap_i,]
    
    temp_read_file <- paste0("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Redlining EJ USA/cumulativeEJ_redlining_usa/Code/02_Analysis/gbm bootstraps/gbm_redlining_env_i", i, "_", zone, ".rds")
    boost_k_red <- readRDS(file = temp_read_file)
    
    # get number of trees according to 5-fold cv
    ntrees_cv <- which.min(boost_k_red$cv.error); ntrees_gbm <- c(ntrees_gbm, ntrees_cv)
    
    cumul_OR_i <- boost_k_red %>%
      pdp::partial(plot=FALSE, n.trees = ntrees_cv, pred.var = c(mean_cols, 
                                                                 "RAW_D_NO_HS", 
                                                                 "RAW_D_LOWINC"),
                   pred.grid = df_pdp
      ) %>%
      mutate(quantile = quantiles_ofint) %>%
      mutate(probs = 1/(1+exp(-yhat))) %>%
      mutate(odds = probs/(1-probs)) %>%
      mutate(median_odds = odds[which(quantile == ref)]) %>%
      mutate(OR = odds/median_odds) %>%
      mutate(iteration = i)
    cumul_OR_gbm <- rbind(cumul_OR_gbm, cumul_OR_i)
    
  }
  
  cumul_OR_gbm
  
}

####### gbm_extract_cumulative(boot_i = 1, ref = 0.05)
gbm_extract_cumulative(boot_i = 1, ref = 0.05, region3 = "Midwest")


# 5. gbm_extract_relInf()
# -------------- calculate relative influence for `boot_i` iterations of gbm()

gbm_extract_relInf <- function(boot_i = NULL, state3 = NULL, city3 = NULL, msa3 = NULL, region3 = NULL) {
  
  zone <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[2]]
  df_pdp <- gbm_pdp(state3 = state3, city3 = city3, msa3 = msa3, region3 = region3)
  
  # get the saved gbm data out
  # cumulative association
  rel_inf_gbm <- NULL
  ntrees_gbm <- c()
  
  n_iterations <- boot_i
  for(i in 1:n_iterations) {
    
    temp_read_file <- paste0("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Redlining EJ USA/cumulativeEJ_redlining_usa/Code/02_Analysis/gbm bootstraps/gbm_redlining_env_i", i, "_", zone, ".rds")
    boost_k_red <- readRDS(file = temp_read_file)
    
    # get number of trees according to 5-fold cv
    ntrees_cv <- which.min(boost_k_red$cv.error); ntrees_gbm <- c(ntrees_gbm, ntrees_cv)
    
    # relative influence
    rel_inf_i <- summary(boost_k_red, plotit = FALSE) %>% as_tibble() %>% mutate(iteration = i)
    rel_inf_gbm <- rbind(rel_inf_gbm, rel_inf_i)
    
  }
  
  rel_inf_gbm
  
}

gbm_extract_relInf(boot_i = 1, msa3 = "Detroit-Warren-Dearborn, MI")
gbm_extract_relInf(boot_i = 1, region3 = "Midwest")


# 6. gbm_extract_exposure()
# -------------- extract partial dependency for each individual exposure indicator (`var_name`) for a single iteration

gbm_extract_exposure <- function(var_name, ntrees, boost, state3 = NULL, city3 = NULL, msa3 = NULL, region3 = NULL) {
  
  new_df2 <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[1]]
  
  #quantiles_var <- quantile(df_bkmr_na2[[var_name]], probs = quantiles_ofint)
  min_var <- min(new_df2[[var_name]])
  median_var <- median(new_df2[[var_name]])
  max_var <- max(new_df2[[var_name]])
  seq_var <- c(seq(min_var, max_var, by = (max_var-min_var)/100), median_var)
  temp_df_gbm <- data.frame(variable = seq_var)
  names(temp_df_gbm)[1] <- var_name
  
  
  temp_df_pdp <- boost %>%
    pdp::partial(plot=FALSE, n.trees = ntrees, 
                 pred.var = c(var_name, "RAW_D_NO_HS", "RAW_D_LOWINC"
                 ),
                 pred.grid = temp_df_gbm %>% mutate(RAW_D_NO_HS = 0.5, RAW_D_LOWINC = 0.5))
  
  names(temp_df_pdp)[1] <- "exposure"
  
  temp_df_pdp %>%
    #mutate(quantile = quantiles_ofint) %>%
    mutate(probs = 1/(1+exp(-yhat))) %>%
    mutate(odds = probs/(1-probs)) %>%
    mutate(median_odds = odds[which(exposure == median_var)[1]]) %>%
    mutate(OR = odds/median_odds)  %>%
    mutate(iteration = i)
  
}

# 7. gbm_extract_boostrap()
# -------------- extract odds ratio for individual exposure indicator for `boot_i` iterations

gbm_extract_boostrap <- function(exposure, boot_i, state3 = NULL, city3 = NULL, msa3 = NULL, region3 = NULL) {
  exposure_var <- exposure
  exposure_OR_gbm <- NULL
  n_iterations <- boot_i
  
  zone <- gbm_get_data(state2 = state3, city2 = city3, msa2 = msa3, region2 = region3)[[2]]
  
  for(i in 1:n_iterations) {
    temp_read_file <- paste0("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Redlining EJ USA/cumulativeEJ_redlining_usa/Code/02_Analysis/gbm bootstraps/gbm_redlining_env_i", i, "_", zone, ".rds")
    boost_temp <- readRDS(file = temp_read_file)
    
    # get number of trees according to 10-fold cv
    ntrees_cv <- which.min(boost_temp$cv.error)
    
    exposure_OR_i <- gbm_extract_exposure(exposure_var, ntrees_cv, boost_temp, state3, city3)
    exposure_OR_gbm <- rbind(exposure_OR_gbm, exposure_OR_i)
    
  }
  
  exposure_OR_gbm %>%
    #filter(quantile >= 0.24 & quantile <= 0.76) %>%
    group_by(exposure) %>%
    summarise(mean = round(median(OR), 2),
              q2.5 = round(quantile(OR, probs = 0.025), 2),
              q97.5 = round(quantile(OR, probs = 0.975), 2)) %>%
    ungroup() %>%
    mutate(var = exposure_var)
  
}



gbm_extract_boostrap("RAW_E_DIESEL", boot_i = 1, msa3 = "Detroit-Warren-Dearborn, MI") %>%
  ggplot(aes(x = exposure, y = mean)) +
  geom_smooth()

gbm_extract_boostrap("RAW_E_DIESEL", boot_i = 1, region3 = "Midwest") %>%
  ggplot(aes(x = exposure, y = mean)) +
  geom_smooth()









