# call packages ----------------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "R2jags",
              "HDInterval",
              "clipr",
              "gnn",
              "dplyr",
              "stringr",
              "data.table")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# define current return year ---------------------------------------------------
curr_year <- 2022

# call exogenous functions -----------------------------------------------------
source("Exogenous functions\\round_fun.R")
source("Exogenous functions\\openFiles_fun.R")
source("Exogenous functions\\dataManip_fun.R")
source("Exogenous functions\\willMod_fun.R")
source("Exogenous functions\\clackMod_fun.R")

# new data entry ---------------------------------------------------------------
# this has to be done manually (line-by-line) using the "read_clip" function
## (1) copy the appropriate value (e.g., ctrl+c) from the source file
## (2) run the corresponding lines of code
## for example, to create a variable for "age2_will", open the current year's
## monthly counts at Willamette Falls. Select and copy the value for
## "Mini Jacks" corresponding to the row for August 15 (the value will be copied
## to the clipboard).  Run the corresponding code below.

## open excel files needed to define variables (below)
### files must be saved according to the naming convention in:
### ~.Input\Background Data\
openFiles_fun(curr_year)

## Willamette return data
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

# run functions ----------------------------------------------------------------
## data manipulation
dataManip_fun(curr_year)

## Willamette model(s)
willMod_fun(curr_year)

## Clackamas model
clackMod_fun(curr_year)
