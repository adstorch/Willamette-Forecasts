# data manipulation -------------------------------------------------------
## generate a data frame to fit model
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

## create a vector to generate prediction
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


# fit model and generate prediction ---------------------------------------
## create character string defining current model
## (string will have to be changed manually if parameterization changes)
clackMod.name<-"Clackamas"

## fit OLS to define initial MCMC values
### fit OLS
clackInit.mod <- lm(
  log(total_clack)~log(clack_2Yr),
  data=clackFit.dat
)

### extract coefficient estimates
clackInit.beta <- as.numeric(
  clackInit.mod$coef[2]
)

clackInit.betaSE <- coef(
  summary(
    clackInit.mod
  )
)[2,2]

## create data list to fit model and generate prediction
clackMod.dat <- list(
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
  
  nObs=length(
    clackFit.dat$clack_2Yr
  )+1
)

## define initial values
## (user defined initial values are not accepted by jags.parallel()-
## default is to let jags estimate initial values; otherwise use jags()
## and specify initial values as below)
inits.clack <- function()
{
  list(beta.clack = runif(
    1,
    clackInit.beta-(5*clackInit.betaSE),
    clackInit.beta+(5*clackInit.betaSE)
  )
  )
}

## specify model
cat('
  model {
    # observation model
    for (i in 1:nObs){
      ## liklihood
      total_clack[i]~dnorm(muTotal_clack[i],tau.e.clack)
      
      ## total return predictions in log space
      muTotal_clack[i] <- alpha.clack[i]+
        beta.clack*(clack_2Yr[i]-mean(clack_2Yr[]))
      
      ## total return predictions on the arithmetic scale
      predTotal_clack[i] <- exp(muTotal_clack[i])
    }
    
    # process model
    for (i in 2:nObs){
      ## define time-varying alpha parameter
      alpha.clack[i] <- alpha.clack[i-1]+W[i]
      
      # prior for annual deviation among alphas
      W[i]~dnorm(0,tau.w.clack)
    }
    
    # priors and definitions
    ## define alpha at t=1
    alpha.clack[1]~dnorm(0,0.001)
    
    ## beta prior
    beta.clack~dnorm(0,0.001)
    
    ## prior for observation variance
    sig.e.clack~dunif(0,1)
    
    ## process variance
    sig.w.clack~dunif(0,1)
    
    ## observation precision
    tau.e.clack <- 1/pow(sig.e.clack,2)
    
    ## process precision
    tau.w.clack <- 1/pow(sig.w.clack,2)
  }',
  file={clack.mod <- tempfile()})

## define parameters to monitor
params.clack <- c("alpha.clack",
                "beta.clack",
                "sig.e.clack",
                "sig.w.clack",
                "muTotal_clack",
                "predTotal_clack")

## call jags
fit.clack <- jags.parallel(data = clackMod.dat,
                           # inits = inits.clack,  # see above
                           parameters.to.save = params.clack,
                           model.file = clack.mod,
                           n.chains = 3,
                           n.iter = 250000,
                           n.burnin = 25000,
                           n.thin = 10,
                           n.cluster = 3,
                           jags.seed = 123,
                           DIC = F)

# review and summarize output ---------------------------------------------
## review bugs object
fit.clack

## extract simulations matrix
mcmc.clack <- fit.clack$BUGSoutput$sims.matrix

## extract simulations for current predictions
pred.mcmc.clack <- mcmc.clack[, paste(
  "predTotal_clack","[",clackMod.dat$nObs,"]",
  sep = "")]

## summarize output for current prediction
### create data frame to store output
clack.pred.out<-data.frame(model=character(),
                             mean.pred.clack=numeric(),
                             lwrHDI.clack=numeric(),
                             UprHDI.clack=numeric(),
                             stringsAsFactors = FALSE)

### append output to new data frame (from above)
clack.pred.out[1,]<-c(
  clackMod.name,
  round(
    mean(
      pred.mcmc.clack
    ),
    0
  ),
  
  round(
    hdi(
      pred.mcmc.clack,
      credMass=0.95
    )[1],
    0
  ),
  
  round(
    hdi(
      pred.mcmc.clack,
      credMass=0.95
    )[2],
    0
  )
)

### review output data frame
print(clack.pred.out)

### save new (curr_year+1) predictions (.rds)
saveRDS(
  clack.pred.out,
  file = paste(
    'Output\\Predictions\\',
    curr_year+1,
    'clack.pred.out.rds',
    sep = ""
  )
)