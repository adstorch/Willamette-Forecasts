# willClackMod_fun <- function(curr_year){

curr_year <- 2022
  
  # load input files -----------------------------------------------------------
  load(
    file = paste(
      "Input\\~Input Data\\Input Tables\\",
      curr_year,
      "willClackInpData.rda",
      sep=""
    )
  )
  # data manipulation ----------------------------------------------------------
  ## age 3
  ### generate a data frame to fit model
  willAge3Fit.dat <- data.frame(
    willAge3_32yr =
      head(
        willChsRet.dat$age3_col/willChsRet.dat$age2_will,
        -1
      ),
    
    willAge2_ret=
      head(
        willChsRet.dat$age2_will,
        -1
      )
  )
  
  ### create a vector to generate prediction
  willAge3Pred.dat <- tail(
    willChsRet.dat$age2_will,
    1
  )
  
  ## age 4
  ### generate a data frame to fit model
  willAge4Fit.dat <- na.omit(
    head(
      data.frame(
        willAge4_col_mAge4 = willChsRet.dat$age4_col,
        willAge3_col = willChsRet.dat$age3_col,
        noaa_ranks = willChsCov.dat$noaa_ranks
      ),
      -2
    )
  )
  
  ### create a vector to generate prediction
  willAge4Pred.dat <- tail(
    head(
      data.frame(
        age3_col = willChsRet.dat$age3_col,
        noaa_ranks = willChsCov.dat$noaa_ranks
      ),
      -1
    ),
    1
  )
  
  ## age 5
  ### generate a data frame to fit model
  willAge5Fit.dat <- na.omit(
    head(
      data.frame(
        willAge5_col_mAge5 = willChsRet.dat$age5_col,
        willAge4_col_mAge5 = willChsRet.dat$age4_col,
        sp_pdo = willChsCov.dat$sp_pdo,
        sp_trans = willChsCov.dat$sp_trans,
        ichthy_biom = willChsCov.dat$ichthy_biom,
        cope_rich = willChsCov.dat$cope_rich
      ),
      -3
    )
  )
  
  ### create a vector to generate prediction
  willAge5Pred.dat <- tail(
    head(
      data.frame(
        willAge4_col_mAge5 = willChsRet.dat$age4_col,
        sp_pdo = willChsCov.dat$sp_pdo,
        sp_trans = willChsCov.dat$sp_trans,
        ichthy_biom = willChsCov.dat$ichthy_biom,
        cope_rich = willChsCov.dat$cope_rich
      ),
      -2
    ),
    1
  )
  
  ## age 6
  ### generate a data frame to fit model
  willAge6Fit.dat <- tail(
    head(
      data.frame(
        ratio_65yr = willChsRet.dat$age6_col/willChsRet.dat$age5_col
      ),
      -4
    ),
    5
  )[,1]
  
  ### create a vector to generate prediction
  willAge6Pred.dat <- tail(
    head(
      willChsRet.dat$age5_col,
      -3
    ),
    1
  )
  
  ## clackamas
  ### generate a data frame to fit model
  clackFit.dat <- data.frame(clack_2Yr = head(
    tail(
      with(
        subset(
          clackChsRet.dat,select=c(age3_clack)
        ),
        c(
          age3_clack[1],age3_clack[-1]+
            age3_clack[-nrow(clackChsRet.dat)]
        )
      ),
      -1
    ),
    -1
  ),
  total_clack = tail(
    subset(
      clackChsRet.dat,
      select=c(
        total_clack
      )
    ),
    -2
  )
  )
  
  ### create a vector to generate prediction
  clackPred.dat <- tail(
    with(
      subset(
        clackChsRet.dat,select=c(
          age3_clack
        )
      ),
      c(
        age3_clack[1],
        age3_clack[-1]+age3_clack[-nrow(clackChsRet.dat)]
      )
    ),
    1
  )
  
  # fit model and generate prediction ------------------------------------------
  ## create character string defining model
  willAge3Mod.name <- "Willamette Age-3"
  willAge4Mod.name <- "Willamette Age-4"
  willAge5Mod.name <- "Willamette Age-5"
  willAge6Mod.name <- "Willamette Age-6"
  willAdultMod.name <- "Willamette Adults"
  willTotalMod.name <- "Willamette Total"
  clackMod.name<-"Clackamas"
  willSansClackTotMod.name <- "Willamette no Clack Total"
  willSansClackAdMod.name <- "Willamette no Clack Adults"
  
  ## fit OLS to define initial MCMC values
  ### fit OLS
  #### age 3
  willAge3Init.mod <- lm(
    log(willAge3_32yr) ~
      log(willAge2_ret),
    data=willAge3Fit.dat
  )
  
  #### age 4
  willAge4Init.mod <- lm(
    log(willAge4_col_mAge4) ~
      log(willAge3_col) +
      noaa_ranks,
    data=willAge4Fit.dat
  )
  
  #### age 5
  willAge5Init.mod <- lm(
    log(willAge5_col_mAge5) ~
      log(willAge4_col_mAge5) +
      sp_pdo +
      sp_trans +
      ichthy_biom +
      cope_rich,
    data=willAge5Fit.dat
  )
  
  #### clackamas
  clackInit.mod <- lm(
    log(total_clack)~log(clack_2Yr),
    data=clackFit.dat
  )
  
  ### extract coefficient estimates
  #### age 3
  willAge3Init.beta <- as.numeric(
    willAge3Init.mod$coef[2]
  )
  
  willAge3Init.betaSE <- coef(
    summary(
      willAge3Init.mod
    )
  )[2,2]
  
  #### age 4
  willAge4Init.beta2 <- as.numeric(
    willAge4Init.mod$coef[3]
  )
  
  willAge4Init.beta2SE <- coef(
    summary(
      willAge4Init.mod
    )
  )[3,2]
  
  #### age 5
  willAge5Init.beta1 <- as.numeric(
    willAge5Init.mod$coef[2]
  )
  
  willAge5Init.beta1SE <- coef(
    summary(
      willAge5Init.mod
    )
  )[2,2]
  
  willAge5Init.beta2 <- as.numeric(
    willAge5Init.mod$coef[3]
  )
  
  willAge5Init.beta2SE <- coef(
    summary(willAge5Init.mod
    )
  )[3,2]
  
  willAge5Init.beta3 <- as.numeric(
    willAge5Init.mod$coef[4]
  )
  
  willAge5Init.beta3SE <- coef(
    summary(
      willAge5Init.mod
    )
  )[4,2]
  
  willAge5Init.beta4 <- as.numeric(
    willAge5Init.mod$coef[5]
  )
  
  willAge5Init.beta4SE <- coef(
    summary(
      willAge5Init.mod
    )
  )[5,2]
  
  willAge5Init.beta5 <- as.numeric(
    willAge5Init.mod$coef[6]
  )
  
  willAge5Init.beta5SE <- coef(
    summary(
      willAge5Init.mod
    )
  )[6,2]
  
  #### clackamas
  clackInit.beta <- as.numeric(
    clackInit.mod$coef[2]
  )
  
  clackInit.betaSE <- coef(
    summary(
      clackInit.mod
    )
  )[2,2]
  
  ## create data list to fit model and generate prediction
  willClackCombMod.dat <- list(
    
    ### age 3
    willAge3_32yr = as.numeric(
      c(
        log(willAge3Fit.dat$willAge3_32yr),
        "NA"
      )
    ),
    
    willAge2_ret = log(
      c(
        willAge3Fit.dat$willAge2_ret,
        willAge3Pred.dat
      )
    ),
    
    nObs.willAge3 = length(
      willAge3Fit.dat$willAge2_ret
    ) + 1,
    
    ### age 4
    willAge4_col_mAge4 = as.numeric(
      c(
        log(willAge4Fit.dat$willAge4_col),
        "NA"
      )
    ),
    
    willAge3_col = log(
      c(
        willAge4Fit.dat$willAge3_col,
        willAge4Pred.dat$age3_col
      )
    ),
    
    noaa_ranks = c(
      willAge4Fit.dat$noaa_ranks,
      willAge4Pred.dat$noaa_ranks
    ),
    
    nObs.willAge4 = length(
      willAge4Fit.dat$willAge3_col
    ) + 1,
    
    ### age 5
    willAge5_col_mAge5 = as.numeric(
      c(
        log(willAge5Fit.dat$willAge5_col),
        "NA"
      )
    ),
    
    willAge4_col_mAge5 = log(
      c(
        willAge5Fit.dat$willAge4_col,
        willAge5Pred.dat$willAge4_col
      )
    ),
    
    sp_pdo = c(
      willAge5Fit.dat$sp_pdo,
      willAge5Pred.dat$sp_pdo
    ),
    
    sp_trans = c(
      willAge5Fit.dat$sp_trans,
      willAge5Pred.dat$sp_trans
    ),
    
    ichthy_biom = c(
      willAge5Fit.dat$ichthy_biom,
      willAge5Pred.dat$ichthy_biom
    ),
    
    cope_rich = c(
      willAge5Fit.dat$cope_rich,
      willAge5Pred.dat$cope_rich
    ),
    
    nObs.willAge5 = length(
      willAge5Fit.dat$willAge5_col
    ) + 1,
    
    ### age 6
    ratio_65yr = willAge6Fit.dat,
    willAge5_col_mAge6 = willAge6Pred.dat,
    nObs.willAge6 = length(
      willAge6Fit.dat
    ),
    
    ### hatchery proportion
    Y = c(
      head(
        willChsHWprop.dat,
        -1)[,4],
      NA
    ),
    
    Y1 = head(
      willChsHWprop.dat,
      -1)[,4][1],
    
    nHWobs = length(
      head(
        willChsHWprop.dat,
        -1)[,4]) + 1,
    
    ### clackamas
    total_clack=as.numeric(
      c(
        log(clackFit.dat$total_clack),
        "NA"
      )
    ),
    
    clack_2Yr=log(
      c(
        clackFit.dat$clack_2Yr,clackPred.dat
      )
    ),
    
    nObs.clack=length(
      clackFit.dat$clack_2Yr
    )+1
  )
  
  ## define initial values (user defined initial values are not accepted by 
  ## jags.parallel()-default is to let jags estimate initial values; otherwise
  ## use jags() and specify initial values as below)
  inits.willComb <- function()
  {
    list(
      beta.willAge3 = runif(
        1,
        willAge3Init.beta - (5 * willAge3Init.betaSE),
        willAge3Init.beta + (5 * willAge3Init.betaSE)
      ),
      
      beta2.willAge4 = runif(
        1,
        willAge4Init.beta2 - (5 * willAge4Init.beta2SE),
        willAge4Init.beta2 + (5 * willAge4Init.beta2SE)
      ),
      
      beta1.willAge5 = runif(
        1,
        willAge5Init.beta1 - (5 * willAge5Init.beta1SE),
        willAge5Init.beta1 + (5 * willAge5Init.beta1SE)
      ),
      
      beta2.willAge5 = runif(
        1,
        willAge5Init.beta2 - (5 * willAge5Init.beta2SE),
        willAge5Init.beta2 + (5 * willAge5Init.beta2SE)
      ),
      
      beta3.willAge5 = runif(
        1,
        willAge5Init.beta3 - (5 * willAge5Init.beta3SE),
        willAge5Init.beta3 + (5 * willAge5Init.beta3SE)
      ),
      
      beta4.willAge5 = runif(
        1,
        willAge5Init.beta4 - (5 * willAge5Init.beta4SE),
        willAge5Init.beta4 + (5 * willAge5Init.beta4SE)
      ),
      
      beta5.willAge5 = runif(
        1,
        willAge5Init.beta5 - (5 * willAge5Init.beta5SE),
        willAge5Init.beta5 + (5 * willAge5Init.beta5SE)
      ),
      
      muRatio_65yr=rnorm(
        1,0,1
      ),
      sig.e.willAge6 = 1,
      
      beta.clack = runif(
        1,
        clackInit.beta-(5*clackInit.betaSE),
        clackInit.beta+(5*clackInit.betaSE)
      )
    )
  }
  
  ## specify model
  cat('
  model {
    
    # age 3
    ## observation model
    for (i in 1:nObs.willAge3){
      
      ### liklihood
      willAge3_32yr[i] ~ dnorm(muWillAge3_32yr[i],tau.e.willAge3)
      
      ### age3:age2 predictions in log space
      muWillAge3_32yr[i] <- alpha.willAge3[i] +
        beta.willAge3 * (willAge2_ret[i]-mean(willAge2_ret[]))
      
      ### age return predictions on the arithmetic scale
      pred_willAge3[i] <- exp(muWillAge3_32yr[i]) * exp(willAge2_ret[i])
    }
    
    ## process model
    for (i in 2:nObs.willAge3){
      
      ### define time-varying alpha parameter
      alpha.willAge3[i] <- alpha.willAge3[i-1] + W.willAge3[i]
      
      ### prior for annual deviation among alphas
      W.willAge3[i] ~ dnorm(0,tau.w.willAge3)
    }
    
    # age 4
    ## observation model
    for (j in 1:nObs.willAge4){
      
      ### liklihood
      willAge4_col_mAge4[j] ~ dnorm(muWillAge4_col[j],tau.e.willAge4)
      
      ### age 4 predictions in log space
      muWillAge4_col[j] <- alpha.willAge4[j] +
        beta1.willAge4[j] * (willAge3_col[j] - mean(willAge3_col[])) +
        beta2.willAge4 * (noaa_ranks[j]-mean(noaa_ranks[]))
      
      ### age return predictions on the arithmetic scale
      pred_willAge4[j] <- exp(muWillAge4_col[j])
    }
    
    ## process model for intercept
    for (j in 2:nObs.willAge4){
      ### define time-varying alpha parameter
      alpha.willAge4[j] <- alpha.willAge4[j-1] + Walpha.willAge4[j]
      
      ### prior for annual deviation among alphas
      Walpha.willAge4[j] ~ dnorm(0,tau.Walpha.willAge4)
    }
    
    ## process model for beta1
    for (j in 2:nObs.willAge4){
      ### define time-varying beta1 parameter
      beta1.willAge4[j] <- beta1.willAge4[j-1] + Wbeta1.willAge4[j]
      
      ### prior for annual deviation among beta1s
      Wbeta1.willAge4[j] ~ dnorm(0,tau.Wbeta1.willAge4)
    }
    
    # age 5
    ## observation model
    for (k in 1:nObs.willAge5){
      
      ### liklihood
      willAge5_col_mAge5[k] ~ dnorm(muWillAge5_col[k],tau.e.willAge5)
      
      ### age 4 predictions in log space
      muWillAge5_col[k] <- alpha.willAge5[k] +
        beta1.willAge5 * (willAge4_col_mAge5[k] - mean(willAge4_col_mAge5[])) +
        beta2.willAge5 * (sp_pdo[k] - mean(sp_pdo[])) +
        beta3.willAge5 * (sp_trans[k] - mean(sp_trans[])) +
        beta4.willAge5 * (ichthy_biom[k] - mean(ichthy_biom[])) +
        beta5.willAge5 * (cope_rich[k] - mean(cope_rich[]))
      
      ### age return predictions on the arithmetic scale
      pred_willAge5[k] <- exp(muWillAge5_col[k])
    }
    
    ## process model for intercept
    for (k in 2:nObs.willAge5){
      
      ### define time-varying alpha parameter
      alpha.willAge5[k] <- alpha.willAge5[k - 1] + Walpha.willAge5[k]
      
      ### prior for annual deviation among alphas
      Walpha.willAge5[k] ~ dnorm(0,tau.Walpha.willAge5)
    }
    
    # age 6
    ## liklihood
    for (l in 1:nObs.willAge6){
      ratio_65yr[l] ~ dnorm(muRatio_65yr,tau.willAge6)
    }
    
    # hatchery proportion model
    ## likelihood
    X[1] ~ dnorm(X0 + u, inv.q);
    EY[1] <- X[1];
    Y[1] ~ dnorm(EY[1], inv.r);
    for(m in 2:nHWobs) {
      X[m] ~ dnorm(X[m-1] + u, inv.q);
      EY[m] <- X[m];
      Y[m] ~ dnorm(EY[m], inv.r); 
    }
    
    # clackamas
    ## observation model
    for (i in 1:nObs.clack){
      ### liklihood
      total_clack[i]~dnorm(muTotal_clack[i],tau.e.clack)
      
      ### total return predictions in log space
      muTotal_clack[i] <- alpha.clack[i]+
        beta.clack*(clack_2Yr[i]-mean(clack_2Yr[]))
      
      ### total return predictions on the arithmetic scale
      predTotal_clack[i] <- exp(muTotal_clack[i])
    }
    
    ## process model
    for (i in 2:nObs.clack){
      ## define time-varying alpha parameter
      alpha.clack[i] <- alpha.clack[i-1]+W[i]
      
      # prior for annual deviation among alphas
      W[i]~dnorm(0,tau.w.clack)
    }
    
    
    
    
    
    
    
    # priors and definitions
    ## age 3
    ### define alpha at t=1
    alpha.willAge3[1] ~ dnorm(0,0.001)
    
    ### beta prior
    beta.willAge3 ~ dnorm(0,0.001)
    
    ### prior for observation variance
    sig.e.willAge3 ~ dunif(0,1)
    
    ### process variance
    sig.w.willAge3 ~ dunif(0,1)
    
    ### observation precision
    tau.e.willAge3 <- 1/pow(sig.e.willAge3,2)
    
    ### process precision
    tau.w.willAge3 <- 1/pow(sig.w.willAge3,2)
    
    ## age 4
    ### define alpha at t=1
    alpha.willAge4[1] ~ dnorm(0,0.001)
    
    ### define beta1 at t=1
    beta1.willAge4[1] ~ dnorm(0,0.001)
    
    ### beta2 prior
    beta2.willAge4 ~ dnorm(0,0.001)
    
    ### prior for observation variance
    sig.e.willAge4 ~ dunif(0,1)
    
    ### prior for process variance on intercept
    sig.Walpha.willAge4 ~ dunif(0,1)
    
    ### prior for process variance on beta1
    sig.Wbeta1.willAge4 ~ dunif(0,1)
    
    ### observation precision
    tau.e.willAge4 <- 1/pow(sig.e.willAge4,2)
    
    ### process precision on intercept
    tau.Walpha.willAge4 <- 1/pow(sig.Walpha.willAge4,2)
    
    ### process precision on beta1
    tau.Wbeta1.willAge4 <- 1/pow(sig.Wbeta1.willAge4,2)
    
    ## age 5
    ### define alpha at t=1
    alpha.willAge5[1] ~ dnorm(0,0.001)
    
    ### beta1 prior
    beta1.willAge5 ~ dnorm(0,0.001)
    
    ### beta2 prior
    beta2.willAge5 ~ dnorm(0,0.001)
    
    ### beta3 prior
    beta3.willAge5 ~ dnorm(0,0.001)
    
    ### beta4 prior
    beta4.willAge5 ~ dnorm(0,0.001)
    
    ### beta5 prior
    beta5.willAge5 ~ dnorm(0,0.001)
    
    ### prior for observation variance
    sig.e.willAge5 ~ dunif(0,1)
    
    ### prior for process variance on intercept
    sig.Walpha.willAge5 ~ dunif(0,1)
    
    ### observation precision
    tau.e.willAge5 <- 1/pow(sig.e.willAge5,2)
    
    ### process precision on intercept
    tau.Walpha.willAge5 <- 1/pow(sig.Walpha.willAge5,2)
    
    ## age 6
    muRatio_65yr ~ dnorm(0,0.001)
    sig.e.willAge6 ~ dunif(0,1)
    tau.willAge6 <- 1/pow(sig.e.willAge6,2)
    pred_willAge6 <- willAge5_col_mAge6 * muRatio_65yr
    
    ## hatchery proportion model
    u ~ dnorm(0, 0.01)
    inv.q ~ dgamma(0.001,0.001)
    q <- 1/inv.q
    inv.r ~ dgamma(0.001,0.001)
    r <- 1/inv.r
    X0 ~ dnorm(Y1, 0.001)
    
    
    ## clackamas
    ### define alpha at t=1
    alpha.clack[1]~dnorm(0,0.001)
    
    ### beta prior
    beta.clack~dnorm(0,0.001)
    
    ### prior for observation variance
    sig.e.clack~dunif(0,1)
    
    ### process variance
    sig.w.clack~dunif(0,1)
    
    ### observation precision
    tau.e.clack <- 1/pow(sig.e.clack,2)
    
    ### process precision
    tau.w.clack <- 1/pow(sig.w.clack,2)
    
    
    
    
    
    ## combined projections
    pred_willTotal <- pred_willAge3[nObs.willAge3] +
      pred_willAge4[nObs.willAge4] +
      pred_willAge5[nObs.willAge5] +
      pred_willAge6
    
    pred_willAdult <- pred_willAge4[nObs.willAge4] +
      pred_willAge5[nObs.willAge5] +
      pred_willAge6
    
    ## hatchery projections
    predHat_willAge3 <- pred_willAge3[nObs.willAge3] -
      (pred_willAge3[nObs.willAge3] * EY[nHWobs])
    
    predHat_willAge4 <- pred_willAge4[nObs.willAge4] -
      (pred_willAge4[nObs.willAge4] * EY[nHWobs])
    
    predHat_willAge5 <- pred_willAge5[nObs.willAge5] -
      (pred_willAge5[nObs.willAge5] * EY[nHWobs])
    
    predHat_willAge6 <- pred_willAge6 -
      (pred_willAge6 * EY[nHWobs])
    
    predHat_willTotal <- predHat_willAge3 +
      predHat_willAge4 +
      predHat_willAge5 +
      predHat_willAge6
    
    predHat_willAdult <- predHat_willAge4 +
      predHat_willAge5 +
      predHat_willAge6
      
      predWillSansClack_total <- (pred_willAge3[nObs.willAge3] +
      pred_willAge4[nObs.willAge4] +
      pred_willAge5[nObs.willAge5] +
      pred_willAge6) - predTotal_clack
      
      predWillSansClack_adult <- (pred_willAge4[nObs.willAge4] +
      pred_willAge5[nObs.willAge5] +
      pred_willAge6) - predTotal_clack*((pred_willAge4[nObs.willAge4] +
      pred_willAge5[nObs.willAge5] +
      pred_willAge6)/((pred_willAge3[nObs.willAge3] +
      pred_willAge4[nObs.willAge4] +
      pred_willAge5[nObs.willAge5] +
      pred_willAge6)))
  }',
      file={willComb.mod <- tempfile()})
  
  ## define parameters to monitor
  age3nObs <- length(
    willAge3Fit.dat$willAge2_ret
  ) + 1
  
  age4nObs <- length(
    willAge4Fit.dat$willAge3_col
  ) + 1
  
  age5nObs <- length(
    willAge5Fit.dat$willAge5_col
  ) + 1
  
  age6nObs <- length(
    willAge6Fit.dat
  )
  
  HWobs <- length(
    head(
      willChsHWprop.dat,
      -1
    )[,4]
  ) + 1
  
  clackNobs <- length(
    clackFit.dat$total_clack
  ) + 1
  
  params.willClackComb.dat <- c(
    #"alpha.willAge4",
    #"beta1.willAge4",
    #"beta2.willAge4",
    #"sig.e.willAge4",
    #"sig.Wbeta1.willAge4",
    #"muWillAge4_col",
    #"pred_willAge4",
    #"q",
    #"r",
    #"EY",
    #"u",
    #"predHat_willAge4",
    "pred_willAdult",
    "pred_willTotal",
    "predHat_willTotal",
    "predHat_willAdult",
    "predTotal_clack",
    "predWillSansClack_total",
    "predWillSansClack_adult",
    
    as.character(
      paste(
        "pred_willAge3[",age3nObs,"]",
        sep = ""
      )
    ),
    
    as.character(
      paste(
        "pred_willAge4[",age4nObs,"]",
        sep = ""
      )
    ),
    
    as.character(
      paste(
        "pred_willAge5[",age5nObs,"]",
        sep = ""
      )
    ),
    
    "pred_willAge6",
    "predHat_willAge3",
    "predHat_willAge4",
    "predHat_willAge5",
    "predHat_willAge6",
    
    as.character(
      paste(
        "EY[",HWobs,"]",
        sep = ""
      )
    ),
    
    as.character(
      paste(
        "predTotal_clack[",clackNobs,"]",
        sep = ""
      )
    ),
    
    as.character(
      paste(
        "predWillSansClack_total[",clackNobs,"]",
        sep = ""
      )
    ),
    
    as.character(
      paste(
        "predWillSansClack_adult[",clackNobs,"]",
        sep = ""
      )
    )
    
    
    
  )
  
  ## call jags
  start <- Sys.time()
  
  fit.willComb <- jags.parallel(
    data = willClackCombMod.dat,
    # inits = inits.willComb,  # see above
    parameters.to.save = params.willClackComb.dat,
    model.file = willComb.mod,
    n.chains = 3,
    n.iter = 1000000,
    n.burnin = 75000,
    n.thin = 10,
    n.cluster = 3,
    jags.seed = 1234,
    DIC = F
  )
  
  stop <- Sys.time()
  duration <- stop-start
  print(duration)
  
  # review and summarize output --------------------------------------------------
  ## review bugs object
  fit.willComb
  
  ## extract simulations matrix
  mcmc.willComb <- fit.willComb$BUGSoutput$sims.matrix
  
  ## extract simulations for current predictions
  pred.mcmc.willAge3 <- mcmc.willComb[, paste(
    "pred_willAge3[",age3nObs,"]",
    sep = ""
  )]
  
  pred.mcmc.willAge4 <- mcmc.willComb[, paste(
    "pred_willAge4[",age4nObs,"]",
    sep = ""
  )]
  
  pred.mcmc.willAge5 <- mcmc.willComb[, paste(
    "pred_willAge5[",age5nObs,"]",
    sep = ""
  )]
  
  pred.mcmc.willAge6 <- mcmc.willComb[, "pred_willAge6"]
  pred.mcmc.willAdult <- mcmc.willComb[, "pred_willAdult"]
  pred.mcmc.willTotal <- mcmc.willComb[, "pred_willTotal"]
  predHat.mcmc.willAge3 <- mcmc.willComb[, "predHat_willAge3"]
  predHat.mcmc.willAge4 <- mcmc.willComb[, "predHat_willAge4"]
  predHat.mcmc.willAge5 <- mcmc.willComb[, "predHat_willAge5"]
  predHat.mcmc.willAge6 <- mcmc.willComb[, "predHat_willAge6"]
  predHat.mcmc.willTotal <- mcmc.willComb[, "predHat_willTotal"]
  predHat.mcmc.willAdult <- mcmc.willComb[, "predHat_willAdult"]
  # pred.mcmc.willSansClackTotal <- mcmc.willComb[, "predWillSansClack_total"]
  
  predHat.mcmc.willClpRt <- mcmc.willComb[, paste(
    "EY[",HWobs,"]",
    sep = ""
  )]
  
  pred.mcmc.clack <- mcmc.willComb[, paste(
    "predTotal_clack[",clackNobs,"]",
    sep = ""
  )]
  
  pred.mcmc.willSansClackTotal <- mcmc.willComb[, paste(
    "predWillSansClack_total[",clackNobs,"]",
    sep = ""
  )]
  
  pred.mcmc.willSansClackAdults <- mcmc.willComb[, paste(
    "predWillSansClack_adult[",clackNobs,"]",
    sep = ""
  )]
  
  ## summarize output for current prediction
  ### create data frame to store output
  willClackComb.pred.out <- data.frame(
    model=character(),
    mean.pred=numeric(),
    lwrHDI=numeric(),
    uprHDI=numeric(),
    stringsAsFactors = FALSE
  )
  
  ### append output for age 3 to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willAge3Mod.name,
    round_fun(
      mean(
        pred.mcmc.willAge3
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge3,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge3,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for age 4 to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willAge4Mod.name,
    round_fun(
      mean(
        pred.mcmc.willAge4
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge4,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge4,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for age 5 to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willAge5Mod.name,
    round_fun(
      mean(
        pred.mcmc.willAge5
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge5,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge5,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for age 6 to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willAge6Mod.name,
    round_fun(
      mean(
        pred.mcmc.willAge6
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge6,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAge6,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for adults to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willAdultMod.name,
    round_fun(
      mean(
        pred.mcmc.willAdult
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAdult,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willAdult,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for total to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willTotalMod.name,
    round_fun(
      mean(
        pred.mcmc.willTotal
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willTotal,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willTotal,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for age 3 hatchery to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    paste(willAge3Mod.name,
          "Hatchery"
    ),
    
    round_fun(
      mean(
        predHat.mcmc.willAge3
      ),
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAge3,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAge3,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for age 4 hatchery to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    paste(willAge4Mod.name,
          "Hatchery"
    ),
    
    round_fun(
      mean(
        predHat.mcmc.willAge4
      ),
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAge4,
        credMass = 0.95
      )[1]
      ,0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAge4,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for age 5 hatchery to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    paste(willAge5Mod.name,
          "Hatchery"
    ),
    
    round_fun(
      mean(
        predHat.mcmc.willAge5
      ),
      0
    ),
    round_fun(
      hdi(
        predHat.mcmc.willAge5,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAge5,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for age 6 hatchery to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    paste(willAge6Mod.name,
          "Hatchery"
    ),
    
    round_fun(
      mean(
        predHat.mcmc.willAge6
      ),
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAge6,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAge6,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for adult hatchery to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    paste(willAdultMod.name,
          "Hatchery"
    ),
    
    round_fun(
      mean(
        predHat.mcmc.willAdult
      ),
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAdult,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willAdult,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for total hatchery to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    paste(willTotalMod.name,
          "Hatchery"
    ),
    
    round_fun(
      mean(
        predHat.mcmc.willTotal
      ),
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willTotal,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        predHat.mcmc.willTotal,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for predicted clip rate to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c("Non-clipped Rate",
               round_fun(
                 mean(
                   predHat.mcmc.willClpRt
                 ),
                 4
               ),
               
               round_fun(
                 hdi(
                   predHat.mcmc.willClpRt,
                   credMass = 0.95
                 )[1],
                 4
               ),
               
               round_fun(
                 hdi(
                   predHat.mcmc.willClpRt,
                   credMass = 0.95
                 )[2],
                 4
               )
  )
  
  ### append output for Clackamas total to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    clackMod.name,
    round_fun(
      mean(
        pred.mcmc.clack
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.clack,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.clack,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for Willamette no Clack total to new data frame (from above)
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willSansClackTotMod.name,
    round_fun(
      mean(
        pred.mcmc.willSansClackTotal
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willSansClackTotal,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willSansClackTotal,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### append output for Willamette no Clack adults to new data frame
  willClackComb.pred.out[nrow(
    willClackComb.pred.out
  ) + 1,] <- c(
    willSansClackAdMod.name,
    round_fun(
      mean(
        pred.mcmc.willSansClackAdults
      ),
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willSansClackAdults,
        credMass = 0.95
      )[1],
      0
    ),
    
    round_fun(
      hdi(
        pred.mcmc.willSansClackAdults,
        credMass = 0.95
      )[2],
      0
    )
  )
  
  ### review output data frame
  print(willClackComb.pred.out)
  
  ### save new (curr_year+1) predictions (.rds)
  saveRDS(
    willClackComb.pred.out,
    file = paste(
      'Output\\Predictions\\',
      curr_year+1,
      'willClackComb.pred.out.rds',
      sep = ""
    )
  )
# }