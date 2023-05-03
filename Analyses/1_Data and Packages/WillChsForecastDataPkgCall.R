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