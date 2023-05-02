# data manipulation -------------------------------------------------------
## generate a data frame to fit model
willAge6Fit.dat <- tail(
  head(
    data.frame(
      ratio_65yr = willChsRet.dat$age6_col/willChsRet.dat$age5_col),
    -4
  ),
  5
)[,1]

## create a vector to generate prediction
willAge6Pred.dat <- tail(
  head(
    willChsRet.dat$age5_col,
    -3
  ),
  1
)

# fit model and generate prediction ---------------------------------------
## create character string defining current model
## (string will have to be changed manually if parameterization changes)
willAge6Mod.name <- "Willamette Age-6"

## create data list to fit model and generate prediction
willAge6Mod.dat <- list(
  ratio_65yr=willAge6Fit.dat,
  
  age5_col=willAge6Pred.dat,
  
  nObs=length(
    willAge6Fit.dat
  ),
  
  Y = c(
    head(
      willChsHWprop.dat,
      -1
    )[,4],
    NA
  ),
  
  Y1 = head(
    willChsHWprop.dat,
    -1
  )[,4][1],
  
  nHWobs = length(
    head(
      willChsHWprop.dat,
      -1
    )[,4]
  )+1
)

## define initial values (user defined initial values are not accepted by jags.parallel()-default is to let jags estimate initial values; otherwise use jags() and specify initial values as below)
inits.willAge6 <- function()
{
  list(
    muRatio_65yr=rnorm(1,0,1),
    sig=1
  )
}

## specify model
cat('
  model{
    # liklihood
    for (i in 1:nObs){
      ratio_65yr[i] ~ dnorm(muRatio_65yr,tau)
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
    muRatio_65yr ~ dnorm(0,0.001)
    sig ~ dunif(0,1)
    tau <- 1/pow(sig,2)
    pred_willAge6 <- age5_col*muRatio_65yr
    
    ## hatchery proportion model
    u ~ dnorm(0, 0.01)
    inv.q ~ dgamma(0.001,0.001)
    q <- 1/inv.q
    inv.r ~ dgamma(0.001,0.001)
    r <- 1/inv.r
    X0 ~ dnorm(Y1, 0.001)
    
    predHat_willAge6 <- pred_willAge6-(pred_willAge6*EY[nHWobs])
  }',
  file={willAge6.mod <- tempfile()})

## define parameters to monitor
params.willAge6 <- c(
  "muRatio_65yr",
  "sig",
  "pred_willAge6",
  "q",
  "r",
  "EY",
  "u",
  "predHat_willAge6"
)

## call jags
start <- Sys.time()

fit.willAge6 <- jags.parallel(
  data = willAge6Mod.dat,
  # inits = inits.willAge6,  # see above
  parameters.to.save = params.willAge6,
  model.file = willAge6.mod,
  n.chains = 3,
  n.iter = 250000,
  n.burnin = 25000,
  n.thin = 10,
  n.cluster = 3,
  jags.seed = 123,
  DIC = F
)

stop <- Sys.time()
duration <- stop-start
# review and summarize output ---------------------------------------------
## review bugs object
fit.willAge6

## extract simulations matrix
mcmc.willAge6 <- fit.willAge6$BUGSoutput$sims.matrix

## extract simulations for current predictions
pred.mcmc.willAge6 <- mcmc.willAge6[, paste(
  "pred_willAge6",
  sep = ""
)]

## summarize output for current prediction
### create data frame to store output
willAge6.pred.out <- data.frame(
  model=character(),
  mean.pred.willAge6=numeric(),
  lwrHDI.willAge6=numeric(),
  UprHDI.willAge6=numeric(),
  stringsAsFactors = FALSE
)

### append output to new data frame (from above)
willAge6.pred.out[1,] <- c(
  willAge6Mod.name,
  
  round(
    mean(
      pred.mcmc.willAge6
    ),
    0
  ),
  
  round(
    hdi(
      pred.mcmc.willAge6,
      credMass=0.95
    )[1],
    0
  ),
  
  round(
    hdi(
      pred.mcmc.willAge6,
      credMass=0.95
    )[2],
    0
  )
)

### extract simulations for current hatchery predictions
predHat.mcmc.willAge6 <- mcmc.willAge6[, "predHat_willAge6"]

#### create data frame to store output for hatchery predictions
willAge6.predHat.out <- data.frame(
  model=character(),
  mean.pred.willAge6=numeric(),
  lwrHDI.willAge6=numeric(),
  UprHDI.willAge6=numeric(),
  stringsAsFactors = FALSE
)

#### append output to new data frame (from above)
willAge6.predHat.out[1,] <- c(
  paste(
    willAge6Mod.name,
    "hatchery"
  ),
  
  round(
    mean(
      predHat.mcmc.willAge6
    ),
    0
  ),
  
  round(
    hdi(
      predHat.mcmc.willAge6,
      credMass=0.95
    )[1],
    0
  ),
  
  round(
    hdi(
      predHat.mcmc.willAge6,
      credMass=0.95
    )[2],
    0
  )
)

### review output data frame
print(willAge6.pred.out)
print(willAge6.predHat.out)