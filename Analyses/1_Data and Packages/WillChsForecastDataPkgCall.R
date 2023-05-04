# call packages ----------------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "R2jags",
              "HDInterval")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# call exogenous functions -----------------------------------------------------
source("Exogenous functions\\round_fun.R")


# call data --------------------------------------------------------------------
load(file='Input\\Input Data\\willChsRet.rda')

## set current return year (this has to be entered manually)
curr_year <- 2023

time <- data.frame(brd_yr = curr_year - 2,
                   mig_yr = curr_year
)

## enter return estimates from current run reconstruction
age3_col <- 2123
age4_col <- 40123
age5_col <- 20123
age6_col <- 50
age3_will <- 1572
age2_will <- 3000

## append current years to loaded data frame
willChsRet.dat <- rbind(
  willChsRet.dat,
  setNames(
    c(
      time,
      rep(NA, 6)
    ),
    names(willChsRet.dat)
  )
)

## insert new values into data frame
willChsRet.dat[nrow(willChsRet.dat) - 1, 3] = age3_col
willChsRet.dat[nrow(willChsRet.dat) - 2, 4] = age4_col
willChsRet.dat[nrow(willChsRet.dat) - 3, 5] = age5_col
willChsRet.dat[nrow(willChsRet.dat) - 4, 6] = age6_col
willChsRet.dat[nrow(willChsRet.dat) - 1, 7] = age3_will
willChsRet.dat[nrow(willChsRet.dat), 8] = age2_will

## create sum variable
willChsRet.dat$sum23 <- 
  willChsRet.dat$age3_col +
  willChsRet.dat$age2_will

## save backup of updated .rda


# call raw data ----------------------------------------------------------------
## input file path
inPath <- "Input\\Input Data\\WillClackRawData.xlsx"

## Willamette return data
willChsRet.dat <- read.xlsx(
  inPath,
  sheet = 1,
  colNames = TRUE
)

save(willChsRet.dat, file='Input\\Input Data\\willChsRet.rda')

## Willamette covariate data
willChsCov.dat <- read.xlsx(
  inPath,
  sheet = 2,
  colNames = TRUE
)

save(willChsCov.dat, file='Input\\Input Data\\willChsCov.rda')

## Willamette HW proportion data
willChsHWprop.dat <- read.xlsx(
  inPath,
  sheet = 3,
  colNames = TRUE
)

save(willChsHWprop.dat, file='Input\\Input Data\\willChsHWprop.rda')

## Clackamas return data
### call raw data
clackChsRet.dat <- read.xlsx(
  inPath,
  sheet = 4,
  colNames = TRUE
)

### sum diagonals to calculate total returns in year n
clackChsTot.dat <- data.frame(
  total_clack=sapply(
    1:nrow(
      clackChsRet.dat[,2:5]
    )+1,
    function(j) sum(
      clackChsRet.dat[,2:5][row(
        clackChsRet.dat[,2:5]
      )+
        col(
          clackChsRet.dat[,2:5])==j]
    )
  )
)

### replace first three records with adjusted value ('2200')
clackChsTot.dat[1:3,] <- 2200

### create a combined ('manipulated') data frame
clackChsManip.dat <- cbind(
  clackChsRet.dat,
  clackChsTot.dat
)

save(clackChsManip.dat, file='Input\\Input Data\\clackChsManip.rda')