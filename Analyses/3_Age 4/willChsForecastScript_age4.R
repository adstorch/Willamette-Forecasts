 # data manipulation -------------------------------------------------------
## generate a data frame to fit model
 willAge4Fit.dat <- na.omit(
   head(
     data.frame(
       willAge4_col = willChsRet.dat$age4_col,
       willAge3_col = willChsRet.dat$age3_col,
       noaa_ranks = willChsCov.dat$noaa_ranks
     ),
     -2
   )
 )

## create a vector to generate prediction
 willAge4Pred.dat<-tail(
   head(
     data.frame(
       age3_col = willChsRet.dat$age3_col,
       noaa_ranks = willChsCov.dat$noaa_ranks
     ),
     -1
   ),
   1
 )

# fit model and generate prediction ---------------------------------------
## create character string defining current model
## (string will have to be changed manually if parameterization changes)
willAge4Mod.name <- "Willamette Age-4"

## fit OLS to define initial MCMC values
### fit OLS
 willAge4Init.mod <- lm(
   log(willAge4_col)~
     log(willAge3_col)+
     noaa_ranks,
   data=willAge4Fit.dat
 )

### extract coefficient estimates
 willAge4Init.beta2 <- as.numeric(
   willAge4Init.mod$coef[3]
 )

 willAge4Init.beta2SE <- coef(
   summary(
     willAge4Init.mod
   )
 )[3,2]

## create data list to fit model and generate prediction
 willAge4Mod.dat <- list(
   willAge4_col=as.numeric(
     c(
       log(willAge4Fit.dat$willAge4_col),
       "NA"
     )
   ),
   
   willAge3_col=log(
     c(
       willAge4Fit.dat$willAge3_col,
       willAge4Pred.dat$age3_col
     )
   ),
   
   noaa_ranks=c(
     willAge4Fit.dat$noaa_ranks,
     willAge4Pred.dat$noaa_ranks
   ),
   
   nObs.willAge4=length(
     willAge4Fit.dat$willAge3_col
   )+1,
   
   Y = c(
     head(
       willChsHWprop.dat,
       -1
     )[,4],
     NA),
   
   Y1 = head(
     willChsHWprop.dat,
     -1)[,4][1],
   
   nHWobs = length(
     head(
       willChsHWprop.dat,
       -1
     )[,4]
   )+1
 )

## define initial values
## (user defined initial values are not accepted by jags.parallel()-
## default is to let jags estimate initial values; otherwise use jags()
## and specify initial values as below)
 inits.willAge4 <- function()
 {
   list(beta2.willAge4 = runif(
     1,
     willAge4Init.beta2-(5*willAge4Init.beta2SE),
     willAge4Init.beta2+(5*willAge4Init.beta2SE)
   )
   )
 }

## specify model
 cat('
   model {
     # observation model
     for (i in 1:nObs.willAge4){
       ## liklihood
       willAge4_col[i]~dnorm(muWillAge4_col[i],tau.e.willAge4)
       
       ## age 4 predictions in log space
       muWillAge4_col[i] <- alpha.willAge4[i]+
         beta1.willAge4[i]*(willAge3_col[i]-mean(willAge3_col[]))+
         beta2.willAge4*(noaa_ranks[i]-mean(noaa_ranks[]))
       
       ## age return predictions on the arithmetic scale
       pred_willAge4[i] <- exp(muWillAge4_col[i])
       
     }
     
     # process model for intercept
     for (i in 2:nObs.willAge4){
       ## define time-varying alpha parameter
       alpha.willAge4[i] <- alpha.willAge4[i-1]+Walpha.willAge4[i]
       
       ## prior for annual deviation among alphas
       Walpha.willAge4[i]~dnorm(0,tau.Walpha.willAge4)
     }
     
     # process model for beta1
     for (i in 2:nObs.willAge4){
       ## define time-varying beta1 parameter
       beta1.willAge4[i] <- beta1.willAge4[i-1]+Wbeta1.willAge4[i]
       
       ## prior for annual deviation among beta1s
       Wbeta1.willAge4[i]~dnorm(0,tau.Wbeta1.willAge4)
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
     alpha.willAge4[1]~dnorm(0,0.001)
     
     ## define beta1 at t=1
     beta1.willAge4[1]~dnorm(0,0.001)
     
     ## beta2 prior
     beta2.willAge4~dnorm(0,0.001)
     
     ## prior for observation variance
     sig.e.willAge4~dunif(0,1)
     
     ## prior for process variance on intercept
     sig.Walpha.willAge4~dunif(0,1)
     
     ## prior for process variance on beta1
     sig.Wbeta1.willAge4~dunif(0,1)
     
     ## observation precision
     tau.e.willAge4 <- 1/pow(sig.e.willAge4,2)
     
     ## process precision on intercept
     tau.Walpha.willAge4 <- 1/pow(sig.Walpha.willAge4,2)
     
     ## process precision on beta1
     tau.Wbeta1.willAge4 <- 1/pow(sig.Wbeta1.willAge4,2)
     
     ## hatchery proportion model
     u ~ dnorm(0, 0.01)
     inv.q ~ dgamma(0.001,0.001)
     q <- 1/inv.q
     inv.r ~ dgamma(0.001,0.001)
     r <- 1/inv.r
     X0 ~ dnorm(Y1, 0.001)
     
     predHat_willAge4 <- pred_willAge4[nObs]-(pred_willAge4[nObs]*EY[nHWobs])
   }',
   file={willAge4.mod <- tempfile()})

## define parameters to monitor
params.willAge4 <- c("alpha.willAge4",
                     "beta1.willAge4",
                     "beta2.willAge4",
                     "sig.e.willAge4",
                     "sig.Wbeta1.willAge4",
                     "muWillAge4_col",
                     "pred_willAge4",
                     "q",
                     "r",
                     "EY",
                     "u",
                     "predHat_willAge4")

## call jags
start <- Sys.time()
fit.willAge4 <- jags.parallel(
  data = willAge4Mod.dat,
  # inits = inits.willAge4,  # see above
  parameters.to.save = params.willAge4,
  model.file = willAge4.mod,
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
print(duration)

# review and summarize output ---------------------------------------------
## review bugs object
fit.willAge4

## extract simulations matrix
mcmc.willAge4 <- fit.willAge4$BUGSoutput$sims.matrix

## extract simulations for current predictions
pred.mcmc.willAge4 <- mcmc.willAge4[, paste(
  "pred_willAge4","[",willAge4Mod.dat$nObs,"]",
  sep = "")]

## summarize output for current prediction
### create data frame to store output
willAge4.pred.out<-data.frame(
  model=character(),
  mean.pred.willAge4=numeric(),
  lwrHDI.willAge4=numeric(),
  UprHDI.willAge4=numeric(),
  stringsAsFactors = FALSE
)

### append output to new data frame (from above)
willAge4.pred.out[1,]<-c(
  willAge4Mod.name,
  round(
    mean(
      pred.mcmc.willAge4
    ),
    0
  ),
  
  round(
    hdi(
      pred.mcmc.willAge4,
      credMass=0.95
    )[1],
    0
  ),
  
  round(
    hdi(
      pred.mcmc.willAge4,
      credMass=0.95
    )[2],
    0
  )
)

### extract simulations for current hatchery predictions
predHat.mcmc.willAge4 <- mcmc.willAge4[, "predHat_willAge4"]

#### create data frame to store output for hatchery predictions
willAge4.predHat.out <- data.frame(
  model=character(),
  mean.pred.willAge4=numeric(),
  lwrHDI.willAge4=numeric(),
  UprHDI.willAge4=numeric(),
  stringsAsFactors = FALSE
)

#### append output to new data frame (from above)
willAge4.predHat.out[1,]<-c(
  paste(willAge4Mod.name,"hatchery"),
  round(
    mean(
      predHat.mcmc.willAge4),
    0
  ),
  
  round(
    hdi(
      predHat.mcmc.willAge4,
      credMass=0.95
    )[1],
    0
  ),
  
  round(
    hdi(
      predHat.mcmc.willAge4,
      credMass=0.95
    )[2],
    0
  )
)

### review output data frame
print(willAge4.pred.out)
print(willAge4.predHat.out)