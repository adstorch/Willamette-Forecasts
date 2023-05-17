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

# set current RETURN year (this applies to all datasets to be updated ----------
curr_year <- 2022

# load existing file (prior year) ----------------------------------------------
## Willamette return data
load(
  file = paste(
    "Input\\~Input Data\\Return\\Willamette\\",
    curr_year-1,
    "willChsRet.rda",
    sep=""
  )
)

## Clackamas return data
load(
  file = paste(
    "Input\\~Input Data\\Return\\Clackamas\\",
    curr_year-1,
    "clackChsRet.rda",
    sep=""
  )
)

## Willamette H-W proportion data
load(
  file = paste(
    "Input\\~Input Data\\Hatchery_Wild Proportion\\",
    curr_year-1,
    "willChsHWprop.rda",
    sep=""
  )
)

# update existing file(s) ------------------------------------------------------
## Willamette return data
### create time variable
time <- data.frame(brd_yr = curr_year - 2,
                   mig_yr = curr_year
)

### define new data from big sheets or other sources
### (1) navigate to spreadsheet containing age-specific data
### (2) copy (ctrl+c) age-specific data
### (3) run line (below) corresponding to the age-class of interest
### for example, to define the "age3_col" variable, open the current
### big sheet, find the estimate for age-3 spring Chinook entering the
### Columbia, copy (ctrl+c) that value to the clipboard,
### run the line below corresponding to "age3_col"

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

### insert new values into data frame
willChsRet.dat[nrow(willChsRet.dat) - 1, 3] = age3_col
willChsRet.dat[nrow(willChsRet.dat) - 2, 4] = age4_col
willChsRet.dat[nrow(willChsRet.dat) - 3, 5] = age5_col
willChsRet.dat[nrow(willChsRet.dat) - 4, 6] = age6_col
willChsRet.dat[nrow(willChsRet.dat) - 1, 7] = age3_will
willChsRet.dat[nrow(willChsRet.dat), 8] = age2_will

### save current year .rda
save(willChsRet.dat,
     file = paste(
       "Input\\~Input Data\\Return\\Willamette\\",
       curr_year,
       "willChsRet.rda",
       sep=""
     )
)

# ### save working updated .rda (this will overwrite existing file)
# save(willChsRet.dat, file='Input\\~Input Data\\willChsRet.rda')



## Willamette covariate data
### copy "Rank of the mean rank" data and run string below
noaa_ranks <- as.numeric(unlist(t(strsplit(read_clip(),"\t"))))

### copy "Mean of ranks" data and run string below
mu_noaa_ranks <- as.numeric(unlist(t(strsplit(read_clip(),"\t"))))

### copy "Nearshore Ichthyoplankton Log(mg C 1,000 m-3; Jan-Mar)" data 
### and run string below
ichthy_biom <- as.numeric(unlist(t(strsplit(read_clip(),"\t"))))

### copy "Principal Component scores (PC1)" data and run string below
pc1 <- as.numeric(unlist(t(strsplit(read_clip(),"\t"))))

### spring pdo data are from the internet and then manipulated as below
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

### copy "Physical Spring Trans. UI based (day of year)" data
### and run string below
sp_trans <- as.numeric(unlist(t(strsplit(read_clip(),"\t"))))

### copy "Copepod richness anom.(no. species; May-Sept)" data and 
### run string below
cope_rich <- as.numeric(unlist(t(strsplit(read_clip(),"\t"))))

### conmbine time series
willChsCov.dat <- data.frame(brd_yr = seq(1996,curr_year - 2,1),
                             mig_yr = seq(1998,curr_year,1),
                             noaa_ranks = noaa_ranks,
                             mu_noaa_ranks = mu_noaa_ranks,
                             ichthy_biom = ichthy_biom,
                             pc1 = pc1,
                             sp_pdo = sp_pdo[,2], ## NEED TO CHANGES THIS BACK
                             sp_trans = sp_trans,
                             cope_rich = cope_rich)

willChsCovPH.dat<- data.frame(brd_yr = seq(1969,1995,1),
                              mig_yr = seq(1971,1997,1),
                              noaa_ranks = NA,
                              mu_noaa_ranks = NA,
                              ichthy_biom = NA,
                              pc1 = NA,
                              sp_pdo = NA,
                              sp_trans = NA,
                              cope_rich = NA)

willChsCov.dat <- rbind(willChsCovPH.dat,
                        willChsCov.dat)


####save current year .rda
save(willChsCov.dat,
     file = paste(
       'Input\\~Input Data\\',
       'Ocean Covariates\\',
       curr_year,
       'willChsCov.rda',
       sep = ""
     )
)

## Willamette HW proportion data
### create time variable
time.hwProp <- data.frame(
  p_yr = curr_year - 1,
  ret_yr = curr_year
)

### define new data from big sheets or other sources
### (1) navigate to spreadsheet containing variable x record combination
### (2) copy (ctrl+c) record
### (3) run line (below) corresponding to the variable of interest
### for example, to define the "p_yr_ret" variable, open the big sheet from
### curr. year - 1, find the estimate for "Total" spring Chinook entering the
### Columbia, copy (ctrl+c) that value to the clipboard,
### run the line below corresponding to "p_yr_ret"
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
)  # from big sheet curr. year ("Wild" run entering Columbia)

clp_rt_denom <- as.numeric(
  gsub(
    ",",
    "",
    read_clip()
  )
)  # from big sheet curr. year ("Total" run entering Columbia)

clp_rt <- clp_rt_num/clp_rt_denom

### append current year to loaded data frame
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

### insert new values into data frame
willChsHWprop.dat[nrow(willChsHWprop.dat), 3] = p_yr_ret
willChsHWprop.dat[nrow(willChsHWprop.dat), 4] = clp_rt
willChsHWprop.dat[nrow(willChsHWprop.dat), 5] = clp_rt_num

### save current year .rda
save(willChsHWprop.dat,
     file = paste(
       "Input\\~Input Data\\Hatchery_Wild Proportion\\",
       curr_year,
       "willChsHWprop.rda",
       sep=""
     )
)


# #### save working updated .rda (this will overwrite existing file)
# save(willChsHWprop.dat, file='Input\\~Input Data\\willChsHWprop.rda')

## Clackamas return data
### create time variable
time.clack <- data.frame(
  brd_yr = curr_year - 3
  )

### define new data from big sheets or other sources
### (1) navigate to spreadsheet containing age-specific data
### (2) copy (ctrl+c) age-specific data
### (3) run line (below) corresponding to the age-class of interest
### for example, to define the "age3_clack" variable, open the current
### big sheet, find the estimate for age-3 spring Chinook entering the
### Clackamas, copy (ctrl+c) that value to the clipboard,
### run the line below corresponding to "age3_clack"
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

### append current years to loaded data frame
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

### insert new values into data frame
clackChsRet.dat[nrow(clackChsRet.dat), 2] = age3_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 1, 3] = age4_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 2, 4] = age5_clack
clackChsRet.dat[nrow(clackChsRet.dat) - 3, 5] = age6_clack
clackChsRet.dat[nrow(clackChsRet.dat), 6] = age3_clack + age4_clack +
  age5_clack + age6_clack

### save current year .rda
save(clackChsRet.dat,
     file=paste(
       "Input\\~Input Data\\Return\\Clackamas\\",
       curr_year,
       "clackChsRet.rda",
       sep=""
     )
)

# ### save working updated .rda (this will overwrite existing file)
# save(clackChsRet.dat, file='Input\\~Input Data\\clackChsRet.rda')

# # manipulate existing or updated dataset -------------------------------------
# ## Willamette return
# ### create sum variable for current analysis
# willChsRet.dat$sum23 <- 
#   willChsRet.dat$age3_col +
#   willChsRet.dat$age2_will
# 
# ## Clackamas return

### sum diagonals to calculate total returns in year n
# clackChsTot.dat <- data.frame(
#   total_clack=sapply(
#     1:nrow(
#       clackChsRet.dat[,2:5]
#     )+1,
#     function(j) sum(
#       clackChsRet.dat[,2:5][row(
#         clackChsRet.dat[,2:5]
#       )+
#         col(
#           clackChsRet.dat[,2:5])==j]
#     )
#   )
# )
# 
# ### replace first three records with adjusted value ('2200')
# clackChsTot.dat[1:3,] <- 2200
# 
# ### create a combined ('manipulated') data frame
# clackChsRet.dat <- cbind(
#   clackChsRet.dat,
#   clackChsTot.dat
# )

