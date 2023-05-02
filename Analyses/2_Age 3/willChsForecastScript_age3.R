# data manipulation -------------------------------------------------------
## generate a data frame to fit model
willAge3Fit.dat <- data.frame(
  willAge3_32yr = head(
    willChsRet.dat$age3_col/willChsRet.dat$age2_will,
    -1
  ),
  
  willAge2_ret = head(
    willChsRet.dat$age2_will,
    -1
  )
)

## create a vector to generate prediction
willAge3Pred.dat <- tail(
  willChsRet.dat$age2_will
  ,1
)

# fit model and generate prediction ---------------------------------------
## create character string defining current model
## (string will have to be changed manually if parameterization changes)
willAge3Mod.name <- "Willamette Age-3"

## fit OLS to define initial MCMC values
### fit OLS
willAge3Init.mod <- lm(
  log(willAge3_32yr)~log(willAge2_ret),
  data=willAge3Fit.dat
)

### extract coefficient estimates
willAge3Init.beta <- as.numeric(
  willAge3Init.mod$coef[2]
)

willAge3Init.betaSE <- coef(
  summary(
    willAge3Init.mod
  )
)[2,2]

## create data list to fit model and generate prediction
willAge3Mod.dat <- list(
  willAge3_32yr=as.numeric(
    c(
      log(willAge3Fit.dat$willAge3_32yr),
      "NA"
    )
  ),
  
  willAge2_ret=log(
    c(
      willAge3Fit.dat$willAge2_ret,
      willAge3Pred.dat
    )
  ),
  
  nObs=length(
    willAge3Fit.dat$willAge2_ret
  )+1,
  
  Y = c(
    head(
      willChsHWprop.dat,
      -1
    )[,4]
    ,NA
  ),
  
  Y1 = head(
    willChsHWprop.dat,
    -1
  )[,4][1],
  nHJWobs = length(
    head(
      willChsHWprop.dat,
      -1
    )[,4]
  )+1
)

## define initial values
## (user defined initial values are not accepted by jags.parallel()
## -default is to let jags estimate initial values; otherwise use jags()
## and specify initial values as below)
inits.willAge3 <- function()
{
  list(
    beta.willAge3 = runif(
      1,
      willAge3Init.beta-(5*willAge3Init.betaSE),
      willAge3Init.beta+(5*willAge3Init.betaSE)
    )
  )
}

## specify model
cat(
  model {
    # observation model
    for (i in 1:nObs){
      ## liklihood
      willAge3_32yr[i]~dnorm(muWillAge3_32yr[i],tau.e.willAge3)
      
      ## age3:age2 predictions in log space
      muWillAge3_32yr[i] <- alpha.willAge3[i]+
        beta.willAge3*(willAge2_ret[i]-mean(willAge2_ret[]))
      
      ## age return predictions on the arithmetic scale
      pred_willAge3[i] <- exp(muWillAge3_32yr[i])*exp(willAge2_ret[i])
    }
    
    # process model
    for (i in 2:nObs){
      ## define time-varying alpha parameter
      alpha.willAge3[i] <- alpha.willAge3[i-1]+W.willAge3[i]
      
      # prior for annual deviation among alphas
      W.willAge3[i]~dnorm(0,tau.w.willAge3)
    }
    
    # hatchery proportion model
    # likelihood
   X[1] ~ dnorm(X0 + u, inv.q);
   EY[1] <- X[1];
   Y[1] ~ dnorm(EY[1], inv.r);
   for(t in 2:nHWobs) {
      X[t] ~ dnorm(X[t-1] + u, inv.q);
      EY[t] <- X[t];
      Y[t] ~ dnorm(EY[t], inv.r); 
   }
    
    # priors and definitions
    ## define alpha at t=1
    alpha.willAge3[1]~dnorm(0,0.001)
    
    ## beta prior
    beta.willAge3~dnorm(0,0.001)
    
    ## prior for observation variance
    sig.e.willAge3~dunif(0,1)
    
    ## process variance
    sig.w.willAge3~dunif(0,1)
    
    ## observation precision
    tau.e.willAge3 <- 1/pow(sig.e.willAge3,2)
    
    # process precision
    tau.w.willAge3 <- 1/pow(sig.w.willAge3,2)
    
    ## hatchery proportion model
    u ~ dnorm(0, 0.01)
    inv.q ~ dgamma(0.001,0.001)
    q <- 1/inv.q
    inv.r ~ dgamma(0.001,0.001)
    r <- 1/inv.r
    X0 ~ dnorm(Y1, 0.001)
    
    predHat_willAge3 <- pred_willAge3[nObs]-(pred_willAge3[nObs]*EY[nHWobs])
  },
  file={willAge3.mod <- tempfile()})

## define parameters to monitor
params.willAge3 <- c("alpha.willAge3",
                     "beta.willAge3",
                     "sig.e.willAge3",
                     "sig.w.willAge3",
                     "muWillAge3_32yr",
                     "pred_willAge3",
                     "q",
                     "r",
                     "EY",
                     "u",
                     "predHat_willAge3")

## call jags
start <- Sys.time()
fit.willAge3 <- jags.parallel(data = willAge3Mod.dat,
                              # inits = inits.willAge3,  # see above
                              parameters.to.save = params.willAge3,
                              model.file = willAge3.mod,
                              n.chains = 3,
                              n.iter = 250000,
                              n.burnin = 25000,
                              n.thin = 10,
                              n.cluster = 3,
                              jags.seed = 123,
                              DIC = F)
stop <- Sys.time()
duration <- stop-start
print(duration)
# review and summarize output ---------------------------------------------
## review bugs object
fit.willAge3

## extract simulations matrix
mcmc.willAge3 <- fit.willAge3$BUGSoutput$sims.matrix

### extract simulations for current predictions
pred.mcmc.willAge3 <- mcmc.willAge3[, paste(
  "pred_willAge3","[",willAge3Mod.dat$nObs,"]",
  sep = ""
)]

### summarize output for current prediction
#### create data frame to store output
willAge3.pred.out<-data.frame(model=character(),
                              mean.pred.willAge3=numeric(),
                              lwrHDI.willAge3=numeric(),
                              UprHDI.willAge3=numeric(),
                              stringsAsFactors = FALSE)

#### append output to new data frame (from above)
willAge3.pred.out[1,]<-c(
  willAge3Mod.name,
  round(
    mean(
      pred.mcmc.willAge3
    ),
    0
  ),
  
  round(
    hdi(
      pred.mcmc.willAge3,
      credMass=0.95
    )[1],
    0
    
  ),
  
  round(
    hdi(
      pred.mcmc.willAge3,
      credMass=0.95)[2]
    ,0
  )
)

### extract simulations for current hatchery predictions
predHat.mcmc.willAge3 <- mcmc.willAge3[, "predHat_willAge3"]

#### create data frame to store output for hatchery predictions
willAge3.predHat.out<-data.frame(
  model=character(),
  mean.pred.willAge3=numeric(),
  lwrHDI.willAge3=numeric(),
  UprHDI.willAge3=numeric(),
  stringsAsFactors = FALSE
)

#### append output to new data frame (from above)
willAge3.predHat.out[1,]<-c(
  paste(
    willAge3Mod.name,"hatchery"
  ),
  
  round(
    mean(
      predHat.mcmc.willAge3
    ),
    0
  ),
  round(
    hdi(
      predHat.mcmc.willAge3,
      credMass=0.95
    )[1],
    0
  ),
  
  round(
    hdi(
      predHat.mcmc.willAge3,
      credMass=0.95
    )[2],
    0
  )
)

## review output data frames
print(willAge3.pred.out)
print(willAge3.predHat.out)
