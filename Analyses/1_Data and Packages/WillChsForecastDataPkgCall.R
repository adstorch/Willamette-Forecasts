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

# load existing file -----------------------------------------------------------
load(file='Input\\Input Data\\willChsRet.rda')
load(file='Input\\Input Data\\clackChsRet.rda')

# update existing file(s) ------------------------------------------------------
## set current return year (this applies to all datasets to be updated)
curr_year <- 2023

### Willamette return data
#### create time variable
time <- data.frame(brd_yr = curr_year - 2,
                   mig_yr = curr_year
)

#### enter return estimates from current run reconstruction
age3_col <- 2123
age4_col <- 40123
age5_col <- 20123
age6_col <- 50
age3_will <- 1572
age2_will <- 3000

#### append current year to loaded data frame
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

#### insert new values into data frame
willChsRet.dat[nrow(willChsRet.dat) - 1, 3] = age3_col
willChsRet.dat[nrow(willChsRet.dat) - 2, 4] = age4_col
willChsRet.dat[nrow(willChsRet.dat) - 3, 5] = age5_col
willChsRet.dat[nrow(willChsRet.dat) - 4, 6] = age6_col
willChsRet.dat[nrow(willChsRet.dat) - 1, 7] = age3_will
willChsRet.dat[nrow(willChsRet.dat), 8] = age2_will

#### save backup of updated .rda
save(willChsRet.dat, file=paste('Input\\Input Data\\Backup\\Return\\',
                                'Willamette\\',
                                curr_year,
                                'willChsRet.rda',
                                sep = "")
)

#### save working updated .rda (this will overwrite existing file)
save(willChsRet.dat, file='Input\\Input Data\\willChsRet.rda')



## call raw data ---------------------------------------------------------------
## input file path
# inPath <- "Input\\Input Data\\WillClackRawData.xlsx"
# 
# ## Willamette return data
# willChsRet.dat <- read.xlsx(
#   inPath,
#   sheet = 1,
#   colNames = TRUE
# )
# # 
# save(willChsRet.dat, file='Input\\Input Data\\willChsRet.rda')

# ## Willamette covariate data
# willChsCov.dat <- read.xlsx(
#   inPath,
#   sheet = 2,
#   colNames = TRUE
# )

# save(willChsCov.dat, file='Input\\Input Data\\willChsCov.rda')

# ## Willamette HW proportion data
# clackChsRet.dat <- read.xlsx(
#   inPath,
#   sheet = 4,
#   colNames = TRUE
# )
# 
# save(clackChsRet.dat, file='Input\\Input Data\\clackChsRet.rda')

### Clackamas return data
#### create time variable
time.clack <- data.frame(
  brd_yr = curr_year - 3
  )

#### enter return estimates from current run reconstruction
age3_clack <- 290
age4_clack <- 5796
age5_clack <- 346
age6_clack <- 0

#### append current years to loaded data frame
clackChsRet.dat <- rbind(
  clackChsRet.dat,
  setNames(
    c(
      time.clack,
      rep(NA, 4)
    ),
    names(clackChsRet.dat)
  )
)

#### insert new values into data frame
clackChsRet.dat[nrow(clackChsRet.dat), 2] = age3_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 1, 3] = age4_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 2, 4] = age5_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 3, 5] = age6_clack

#### save backup of updated .rda
save(clackChsRet.dat, file=paste('Input\\Input Data\\Backup\\Return\\',
                                'Clackamas\\',
                                curr_year,
                                'clackChsRet.rda',
                                sep = "")
)

#### save working updated .rda (this will overwrite existing file)
save(clackChsRet.dat, file='Input\\Input Data\\clackChsRet.rda')

# manipulated existing or updated dataset --------------------------------------
## Willamette return
### create sum variable for current analysis
willChsRet.dat$sum23 <- 
  willChsRet.dat$age3_col +
  willChsRet.dat$age2_will

## Clackamas return
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
clackChsRet.dat <- cbind(
  clackChsRet.dat,
  clackChsTot.dat
)

