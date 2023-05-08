# call packages ----------------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "R2jags",
              "HDInterval",
              "clipr")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# call exogenous functions -----------------------------------------------------
source("Exogenous functions\\round_fun.R")

# load existing file -----------------------------------------------------------
load(file='Input\\~Input Data\\willChsRet.rda')
load(file='Input\\~Input Data\\clackChsRet.rda')
load(file='Input\\~Input Data\\willChsHWprop.rda')

# update existing file(s) ------------------------------------------------------
## set current return year (this applies to all datasets to be updated)
curr_year <- 2023

### Willamette return data
#### create time variable
time <- data.frame(brd_yr = curr_year - 2,
                   mig_yr = curr_year
)

#### define new data from big sheets or other sources
#### (1) navigate to spreadsheet containing age-specific data
#### (2) copy (ctrl+c) age-specific data
#### (3) run line (below) corresponding to the age-class of interest
#### for example, to define the "age3_col" variable, open the current
### big sheet, find the estimate for age-3 spring Chinook entering the
#### Columbia, copy (ctrl+c) that value to the clipboard,
#### run the line below corresponding to "age3_col"

age3_col <- as.numeric(
  gsub(",",
       "",
       read_clip()
  )
) # from big sheet (run entering Columbia)

age4_col <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Columbia)

age5_col <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Columbia)

age6_col <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Columbia)

age3_will <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Columbia)

age2_will <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from Will. Falls monthly counts

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
save(willChsRet.dat, file=paste('Input\\~Input Data\\Backup\\Return\\',
                                'Willamette\\',
                                curr_year,
                                'willChsRet.rda',
                                sep = "")
)

#### save working updated .rda (this will overwrite existing file)
save(willChsRet.dat, file='Input\\~Input Data\\willChsRet.rda')


# ## Willamette covariate data
# willChsCov.dat <- read.xlsx(
#   inPath,
#   sheet = 2,
#   colNames = TRUE
# )

# save(willChsCov.dat, file='Input\\~Input Data\\willChsCov.rda')

### Willamette HW proportion data
#### create time variable
time.hwProp <- data.frame(
  p_yr = curr_year - 1,
  ret_yr = curr_year
)

#### define new data from big sheets or other sources
#### (1) navigate to spreadsheet containing variable x record combination
#### (2) copy (ctrl+c) record
#### (3) run line (below) corresponding to the variable of interest
#### for example, to define the "p_yr_ret" variable, open the big sheet from
### curr. year - 1, find the estimate for "Total" spring Chinook entering the
#### Columbia, copy (ctrl+c) that value to the clipboard,
#### run the line below corresponding to "p_yr_ret"
p_yr_ret <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet curr. year - 1 (run entering Columbia)

clp_rt_num <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet curr. year (adult wild run entering Columbia)

clp_rt_denom <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet curr. year (total adult run entering Columbia)

clp_rt <- clp_rt_num/clp_rt_denom

#### append current year to loaded data frame
willChsHWprop.dat <- rbind(
  willChsHWprop.dat,
  setNames(
    c(
      time.hwProp,
      rep(NA, 2)
    ),
    names(willChsHWprop.dat)
  )
)

#### insert new values into data frame
willChsHWprop.dat[nrow(willChsHWprop.dat), 3] = p_yr_ret
willChsHWprop.dat[nrow(willChsHWprop.dat) - 1, 4] = clp_rt

#### save backup of updated .rda
save(willChsHWprop.dat, file=paste('Input\\~Input Data\\Backup\\',
                                 'Hatchery_Wild Proportion\\',
                                 curr_year,
                                 'willChsHWprop.rda',
                                 sep = "")
)

#### save working updated .rda (this will overwrite existing file)
save(willChsHWprop.dat, file='Input\\~Input Data\\willChsHWprop.rda')

### Clackamas return data
#### create time variable
time.clack <- data.frame(
  brd_yr = curr_year - 3
  )

#### define new data from big sheets or other sources
#### (1) navigate to spreadsheet containing age-specific data
#### (2) copy (ctrl+c) age-specific data
#### (3) run line (below) corresponding to the age-class of interest
#### for example, to define the "age3_clack" variable, open the current
#### big sheet, find the estimate for age-3 spring Chinook entering the
#### Clackamas, copy (ctrl+c) that value to the clipboard,
#### run the line below corresponding to "age3_clack"
age3_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Clackamas)

age4_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Clackamas)
       
age5_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Clackamas)
       
age6_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Clackamas)

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
save(clackChsRet.dat, file=paste('Input\\~Input Data\\Backup\\Return\\',
                                'Clackamas\\',
                                curr_year,
                                'clackChsRet.rda',
                                sep = "")
)

#### save working updated .rda (this will overwrite existing file)
save(clackChsRet.dat, file='Input\\~Input Data\\clackChsRet.rda')

# manipulate existing or updated dataset ---------------------------------------
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

