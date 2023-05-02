# data manipulation -------------------------------------------------------
## generate a data frame to fit model
willAge5Fit.dat <- na.omit(head(data.frame(willAge5_col = willChsRet.dat$age5_col,
                                           willAge4_col = willChsRet.dat$age4_col,
                                           sp_pdo = willChsCov.dat$sp_pdo,
                                           sp_trans = willChsCov.dat$sp_trans,
                                           ichthy_biom = willChsCov.dat$ichthy_biom,
                                           cope_rich = willChsCov.dat$cope_rich),-3))

## create a vector to generate prediction
willAge5Pred.dat<-tail(head(data.frame(willAge4_col = willChsRet.dat$age4_col,
                                       sp_pdo = willChsCov.dat$sp_pdo,
                                       sp_trans = willChsCov.dat$sp_trans,
                                       ichthy_biom = willChsCov.dat$ichthy_biom,
                                       cope_rich = willChsCov.dat$cope_rich),-2),1)

# fit model and generate prediction ---------------------------------------
## create character string defining current model (string will have to be changed manually if parameterization changes)
willAge5Mod.name <- "Willamette Age-5"

## fit OLS to define initial MCMC values
### fit OLS
willAge5Init.mod <- lm(log(willAge5_col)~log(willAge4_col)+sp_pdo+sp_trans+ichthy_biom+cope_rich,data=willAge5Fit.dat)

### extract coefficient estimates
willAge5Init.beta1<-as.numeric(willAge5Init.mod$coef[2])
willAge5Init.beta1SE<-coef(summary(willAge5Init.mod))[2,2]
willAge5Init.beta2<-as.numeric(willAge5Init.mod$coef[3])
willAge5Init.beta2SE<-coef(summary(willAge5Init.mod))[3,2]
willAge5Init.beta3<-as.numeric(willAge5Init.mod$coef[4])
willAge5Init.beta3SE<-coef(summary(willAge5Init.mod))[4,2]
willAge5Init.beta4<-as.numeric(willAge5Init.mod$coef[5])
willAge5Init.beta4SE<-coef(summary(willAge5Init.mod))[5,2]
willAge5Init.beta5<-as.numeric(willAge5Init.mod$coef[6])
willAge5Init.beta5SE<-coef(summary(willAge5Init.mod))[6,2]

## create data list to fit model and generate prediction
willAge5Mod.dat <- list(willAge5_col=as.numeric(c(log(willAge5Fit.dat$willAge5_col),"NA")),
                        willAge4_col=log(c(willAge5Fit.dat$willAge4_col,willAge5Pred.dat$willAge4_col)),
                        sp_pdo = c(willAge5Fit.dat$sp_pdo,willAge5Pred.dat$sp_pdo),
                        sp_trans = c(willAge5Fit.dat$sp_trans,willAge5Pred.dat$sp_trans),
                        ichthy_biom = c(willAge5Fit.dat$ichthy_biom,willAge5Pred.dat$ichthy_biom),
                        cope_rich = c(willAge5Fit.dat$cope_rich,willAge5Pred.dat$cope_rich),
                        nObs=length(willAge5Fit.dat$willAge5_col)+1,
                        Y = c(head(willChsHWprop.dat, -1)[,4],NA),
                        Y1 = head(willChsHWprop.dat, -1)[,4][1],
                        nHWobs = length(head(willChsHWprop.dat, -1)[,4])+1)

## define initial values (user defined initial values are not accepted by jags.parallel()-default is to let jags estimate initial values; otherwise use jags() and specify initial values as below)
inits.willAge5 <- function()
{
  list(beta1.willAge5 = runif(1,willAge5Init.beta1-(5*willAge5Init.beta1SE),willAge5Init.beta1+(5*willAge5Init.beta1SE)),
       beta2.willAge5 = runif(1,willAge5Init.beta2-(5*willAge5Init.beta2SE),willAge5Init.beta2+(5*willAge5Init.beta2SE)),
       beta3.willAge5 = runif(1,willAge5Init.beta3-(5*willAge5Init.beta3SE),willAge5Init.beta3+(5*willAge5Init.beta3SE)),
       beta4.willAge5 = runif(1,willAge5Init.beta4-(5*willAge5Init.beta4SE),willAge5Init.beta4+(5*willAge5Init.beta4SE)),
       beta5.willAge5 = runif(1,willAge5Init.beta5-(5*willAge5Init.beta5SE),willAge5Init.beta5+(5*willAge5Init.beta5SE)))
}

## specify model
cat('
  model {
    # observation model
    for (i in 1:nObs){
      ## liklihood
      willAge5_col[i]~dnorm(muWillAge5_col[i],tau.e.willAge5)
      ## age 4 predictions in log space
      muWillAge5_col[i]<-alpha.willAge5[i]+beta1.willAge5*(willAge4_col[i]-mean(willAge4_col[]))+beta2.willAge5*(sp_pdo[i]-mean(sp_pdo[]))+beta3.willAge5*(sp_trans[i]-mean(sp_trans[]))+beta4.willAge5*(ichthy_biom[i]-mean(ichthy_biom[]))+beta5.willAge5*(cope_rich[i]-mean(cope_rich[]))
      ## age return predictions on the arithmetic scale
      pred_willAge5[i]<-exp(muWillAge5_col[i])
      
    }
    
    # process model for intercept
    for (i in 2:nObs){
      ## define time-varying alpha parameter
      alpha.willAge5[i]<-alpha.willAge5[i-1]+Walpha.willAge5[i]
      ## prior for annual deviation among alphas
      Walpha.willAge5[i]~dnorm(0,tau.Walpha.willAge5)
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
    alpha.willAge5[1]~dnorm(0,0.001)
    ## beta1 prior
    beta1.willAge5~dnorm(0,0.001)
    ## beta2 prior
    beta2.willAge5~dnorm(0,0.001)
    ## beta3 prior
    beta3.willAge5~dnorm(0,0.001)
    ## beta4 prior
    beta4.willAge5~dnorm(0,0.001)
    ## beta5 prior
    beta5.willAge5~dnorm(0,0.001)
    ## prior for observation variance
    sig.e.willAge5~dunif(0,1)
    ## prior for process variance on intercept
    sig.Walpha.willAge5~dunif(0,1)
    ## observation precision
    tau.e.willAge5<-1/pow(sig.e.willAge5,2)
    ## process precision on intercept
    tau.Walpha.willAge5<-1/pow(sig.Walpha.willAge5,2)
    
    ## hatchery proportion model
    u ~ dnorm(0, 0.01)
    inv.q ~ dgamma(0.001,0.001)
    q <- 1/inv.q
    inv.r ~ dgamma(0.001,0.001)
    r <- 1/inv.r
    X0 ~ dnorm(Y1, 0.001)
    
    predHat_willAge5 <- pred_willAge5[nObs]-(pred_willAge5[nObs]*EY[nHWobs])
  }',
  file={willAge5.mod <- tempfile()})

## define parameters to monitor
params.willAge5 <- c("alpha.willAge5",
                     "beta1.willAge5",
                     "beta2.willAge5",
                     "beta3.willAge5",
                     "beta4.willAge5",
                     "beta5.willAge5",
                     "sig.e.willAge5",
                     "muWillAge5_col",
                     "pred_willAge5",
                     "q",
                     "r",
                     "EY",
                     "u",
                     "predHat_willAge5")

## call jags
start <- Sys.time()
fit.willAge5 <- jags.parallel(data = willAge5Mod.dat,
                              # inits = inits.willAge5,  # see above
                              parameters.to.save = params.willAge5,
                              model.file = willAge5.mod,
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
fit.willAge5

## extract simulations matrix
mcmc.willAge5 <- fit.willAge5$BUGSoutput$sims.matrix

## extract simulations for current predictions
pred.mcmc.willAge5 <- mcmc.willAge5[, paste("pred_willAge5","[",willAge5Mod.dat$nObs,"]",sep = "")]

## summarize output for current prediction
### create data frame to store output
willAge5.pred.out<-data.frame(model=character(),
                              mean.pred.willAge5=numeric(),
                              lwrHDI.willAge5=numeric(),
                              UprHDI.willAge5=numeric(),
                              stringsAsFactors = FALSE)

### append output to new data frame (from above)
willAge5.pred.out[1,]<-c(willAge5Mod.name,
                         round(mean(pred.mcmc.willAge5),0),
                         round(hdi(pred.mcmc.willAge5, credMass=0.95)[1],0),
                         round(hdi(pred.mcmc.willAge5, credMass=0.95)[2],0))

### extract simulations for current hatchery predictions
predHat.mcmc.willAge5 <- mcmc.willAge5[, "predHat_willAge5"]

#### create data frame to store output for hatchery predictions
willAge5.predHat.out<-data.frame(model=character(),
                                 mean.pred.willAge5=numeric(),
                                 lwrHDI.willAge5=numeric(),
                                 UprHDI.willAge5=numeric(),
                                 stringsAsFactors = FALSE)

#### append output to new data frame (from above)
willAge5.predHat.out[1,]<-c(paste(willAge5Mod.name,"hatchery"),
                            round(mean(predHat.mcmc.willAge5),0),
                            round(hdi(predHat.mcmc.willAge5, credMass=0.95)[1],0),
                            round(hdi(predHat.mcmc.willAge5, credMass=0.95)[2],0))

### review output data frame
print(willAge5.pred.out)
print(willAge5.predHat.out)