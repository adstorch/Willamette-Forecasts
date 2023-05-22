# call packages ----------------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "R2jags",
              "HDInterval",
              "clipr",
              "gnn")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# call exogenous functions -----------------------------------------------------
source("Exogenous functions\\round_fun.R")

# set current RETURN year (this applies to all datasets to be updated)----------
curr_year <- 2022

# new data entry ---------------------------------------------------------------
# this has to be done manually (line-by-line) using the "read_clip" function
## (1) copy the appropriate value (e.g., ctrl+c) from the source file
## (2) run the corresponding lines of code
## for example, to create a variable for "age3_col", open the current year's
## "Big Sheet". Select and copy the value for "Age 3" corresponding to the 
## row for "Run Entering Columbia" (the value will be copied to the clipboard).
## Run the code in lines 31-36 below.

## Willamette return data
### age-3 return to the Columbia River mouth
### from current years' "Big Sheet" ("Run Entering Columbia")
age3_col <- as.numeric(
  gsub(",",
       "",
       read_clip()
  )
)

### age-4 return to the Columbia River mouth
### from current years' "Big Sheet" ("Run Entering Columbia")
age4_col <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

### age-5 return to the Columbia River mouth
### from current years' "Big Sheet" ("Run Entering Columbia")
age5_col <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

### age-6 return to the Columbia River mouth
### from current years' "Big Sheet" ("Run Entering Columbia")
age6_col <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

### age-3 count at Willamette Falls
### from current years' "Big Sheet" ("Escapement: Willamette Falls Count")
age3_will <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet (run entering Columbia)

### age-2 (mini jack) count at Willamette Falls
### from current years' Will. Falls monthly counts ("Mini Jack: Cum" on 15 Aug.)
age2_will <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

## Willamette covariate data
### "Rank of the mean rank"
### from current years' NOAA Stoplight Chart; typically dist. by CRITFC
noaa_ranks <- as.numeric(
  unlist(
    t(
      strsplit(
        read_clip(),
        "\t"
      )
    )
  )
)

### "Mean of ranks"
### from current years' NOAA Stoplight Chart; typically dist. by CRITFC
mu_noaa_ranks <- as.numeric(
  unlist(
    t(
      strsplit(
        read_clip(),
        "\t"
      )
    )
  )
)

### Nearshore Ichthyoplankton Log(mg C 1,000 m-3; Jan-Mar)" 
### from current years' NOAA Stoplight Chart; typically dist. by CRITFC
ichthy_biom <- as.numeric(
  unlist(
    t(
      strsplit(
        read_clip(),
        "\t"
      )
    )
  )
)

### "Principal Component scores (PC1)"
### from current years' NOAA Stoplight Chart; typically dist. by CRITFC
pc1 <- as.numeric(
  unlist(
    t(
      strsplit(
        read_clip(),
        "\t"
      )
    )
  )
)

### "Physical Spring Trans. UI based (day of year)"
### from current years' NOAA Stoplight Chart; typically dist. by CRITFC
sp_trans <- as.numeric(
  unlist(
    t(
      strsplit(
        read_clip(),
        "\t"
      )
    )
  )
)

### "Copepod richness anom.(no. species; May-Sept)"
### from current years' NOAA Stoplight Chart; typically dist. by CRITFC
cope_rich <- as.numeric(
  unlist(
    t(
      strsplit(
        read_clip(),
        "\t"
      )
    )
  )
)

## Willamette HW proportion data
### prior years' TOTAL return (age-3 - age-6)
### from prior years' "Big Sheet" ("Run Entering Columbia")
p_yr_ret <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

### total WILD return (age-3 - age-6)
### from current years' "Big Sheet" ("Run Entering Columbia")
clp_rt_num <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

## Clackamas return data
### age-3 return to the Clackamas River
### from current years' "Big Sheet" ("Run Entering Clackamas")
age3_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

### age-4 return to the Clackamas River
### from current years' "Big Sheet" ("Run Entering Clackamas")
age4_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

### age-5 return to the Clackamas River
### from current years' "Big Sheet" ("Run Entering Clackamas")
age5_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

### age-6 return to the Clackamas River
### from current years' "Big Sheet" ("Run Entering Clackamas")
age6_clack <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)

# update data (from prior year; code can be run in one chunk)-------------------
## load existing input files (prior year)
load(
  file = paste(
    "Input\\~Input Data\\",
    curr_year-1,
    "willClackInpData.rda",
    sep=""
  )
)

## update existing file(s)
### Willamette return data
#### create time variable
time <- data.frame(brd_yr = curr_year - 2,
                   mig_yr = curr_year
)

#### append current year (time) to loaded data frame
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

### Willamette covariate data
#### spring (May-Aug) pdo
#### raw data are retrieved from the interwebs (URL below) and manipulated here
sp_pdo <- subset(
  data.frame(
    year = read.table(
      "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
      header=TRUE, skip = 1)[,1],
    sp_pdo = rowMeans(
      subset(
        read.table(
          "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
          header=TRUE, skip = 1
        ),
        select = c(May, Jun, Jul, Aug)
      ),
      na.rm = TRUE
    )
  ),
  year >= 1998
)

sp_pdo <- head(sp_pdo, -1)

#### time series development
##### current time series (reporting began in mig. yr. 1998)
willChsCov.dat <- data.frame(brd_yr = seq(1996,curr_year - 2,1),
                             mig_yr = seq(1998,curr_year,1),
                             noaa_ranks = noaa_ranks,
                             mu_noaa_ranks = mu_noaa_ranks,
                             ichthy_biom = ichthy_biom,
                             pc1 = pc1,
                             sp_pdo = sp_pdo[,2], ## NEED TO CHANGES THIS BACK
                             sp_trans = sp_trans,
                             cope_rich = cope_rich)

##### dummy time series to account for period befor reporting began
willChsCovPH.dat<- data.frame(brd_yr = seq(1969,1995,1),
                              mig_yr = seq(1971,1997,1),
                              noaa_ranks = NA,
                              mu_noaa_ranks = NA,
                              ichthy_biom = NA,
                              pc1 = NA,
                              sp_pdo = NA,
                              sp_trans = NA,
                              cope_rich = NA)

##### combine current and dummy time series
willChsCov.dat <- rbind(willChsCovPH.dat,
                        willChsCov.dat)

### Willamette HW proportion data
#### create time variable
time.hwProp <- data.frame(
  p_yr = curr_year - 1,
  ret_yr = curr_year
)

#### create variable (total return to CRM) representing denominator in clp_rt
clp_rt_denom <- as.numeric(
  age3_col + 
    age4_col + 
    age5_col + 
    age6_col
)

#### calculate the unclipped rate for the current return year
clp_rt <- clp_rt_num/clp_rt_denom

#### append current year (time) to loaded data frame
willChsHWprop.dat <- rbind(
  willChsHWprop.dat,
  setNames(
    c(
      time.hwProp,
      rep(NA, 3)
    ),
    names(willChsHWprop.dat)
  )
)

#### insert new values into data frame
willChsHWprop.dat[nrow(willChsHWprop.dat), 3] = p_yr_ret
willChsHWprop.dat[nrow(willChsHWprop.dat), 4] = clp_rt
willChsHWprop.dat[nrow(willChsHWprop.dat), 5] = clp_rt_num

### Clackamas return data
#### create time variable
time.clack <- data.frame(
  brd_yr = curr_year - 3
)

#### append current year (time) to loaded data frame
clackChsRet.dat <- rbind(
  clackChsRet.dat,
  setNames(
    c(
      time.clack,
      rep(NA, 5)
    ),
    names(clackChsRet.dat)
  )
)

#### insert new values into data frame
clackChsRet.dat[nrow(clackChsRet.dat), 2] = age3_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 1, 3] = age4_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 2, 4] = age5_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 3, 5] = age6_clack
clackChsRet.dat[nrow(clackChsRet.dat), 6] = age3_clack + age4_clack +
  age5_clack + age6_clack

## save input data (.rda)
save(willChsRet.dat,
     willChsCov.dat,
     willChsHWprop.dat,
     clackChsRet.dat,
     file=paste(
       "Input\\~Input Data\\",
       curr_year,
       "willClackInpData.rda",
       sep=""
     )
)