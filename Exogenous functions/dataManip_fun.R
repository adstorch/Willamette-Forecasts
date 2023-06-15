dataManip_fun <- function(curr_year){
  
  # load run reconstruction (corresponding to "curr_year") and manipulate data----
  # remaining code can be run in one chunk
  comp_rr <-
    read.xlsx(
      paste0(
        "Input\\Background Data\\Big Sheets\\",
        curr_year,
        "WillametteBigSheet.xlsx",
        sep=""
      ),
      sheet = 3,
      colNames = FALSE
    ) %>% 
    slice(
      as.numeric(
        which(X1 %like% "Catch")
      ):as.numeric(
        which(
          X1 %like% "Run Entering Clackamas")
      )
    )
  
  comp_rr <- comp_rr[which(
    comp_rr$X1 %like% "Catch"):which(
      comp_rr$X1 %like% "Run Entering Clackamas"),
    c(1:5,
      which(
        apply(
          comp_rr,
          2, 
          function(x) any(
            grepl(
              "Wild",
              x
            )
          )
        )
      )
    )] %>%
    `colnames<-`(.[1, ]) %>% 
    .[-1, ] %>% 
    mutate_at(vars("Age 3",
                   "Age 4",
                   "Age 5",
                   "Age 6",
                   "Wild"), as.numeric) %>% 
    rename(catch = "Catch",
           age_3 = "Age 3",
           age_4 = "Age 4",
           age_5 = "Age 5",
           age_6 = "Age 6",
           wild = "Wild") %>%
    filter_at(vars(catch), all_vars(!is.na(.))) %>%
    mutate_if(is.numeric, round) %>% 
    filter(!str_detect(catch, 'Totals')) %>%
    filter(!str_detect(catch, 'Escapement')) %>% 
    filter(!str_detect(catch, 'Percent')) %>% 
    mutate(adults = age_4 + age_5 + age_6) %>%
    mutate(total = age_3 + age_4 + age_5 + age_6) %>% 
    mutate(hatchery = total - wild)
  
  ## save input data (.rda)
  saveRDS(
    comp_rr,
    file = paste(
      'Input\\~Input Data\\Run Reconstruction\\',
      curr_year,
      'willClackRRData.rds',
      sep = ""
    )
  )
  
  # update data ----------------------------------------------------------------
  ## load prior years' run reconstruction table
  prior.willClackRR.dat <- readRDS(
    paste(
      "Input\\~Input Data\\Run Reconstruction\\",
      curr_year-1,
      'willClackRRData.rds',
      sep = ""
    )
  )
  
  ## load existing input files (prior year)
  load(
    file = paste(
      "Input\\~Input Data\\Input Tables\\",
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
  willChsRet.dat[nrow(willChsRet.dat) - 1, 3] = as.numeric(
    subset(
      comp_rr,
      catch == "Run Entering Columbia",
      select = c(age_3)
    )
  )
  willChsRet.dat[nrow(willChsRet.dat) - 2, 4] = as.numeric(
    subset(
      comp_rr,
      catch == "Run Entering Columbia",
      select = c(age_4)
    )
  )
  willChsRet.dat[nrow(willChsRet.dat) - 3, 5] = as.numeric(
    subset(
      comp_rr,
      catch == "Run Entering Columbia",
      select = c(age_5)
    )
  )
  willChsRet.dat[nrow(willChsRet.dat) - 4, 6] = as.numeric(
    subset(
      comp_rr,
      catch == "Run Entering Columbia",
      select = c(age_6)
    )
  )
  willChsRet.dat[nrow(willChsRet.dat) - 1, 7] = as.numeric(
    subset(
      comp_rr,
      catch == "Willamette Falls Count",
      select = c(age_3)
    )
  )
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
  
  sp_pdo <- head(sp_pdo, -1) ####  THIS WILL NEED TO BE COMMENTED-OUT IN FINAL
  
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
  
  #### calculate the unclipped rate for the current return year
  clp_rt <- as.numeric(
    subset(
      comp_rr,
      catch == "Run Entering Columbia",
      select = c(wild)
    )
  ) /
    as.numeric(
      as.numeric(
        subset(
          comp_rr,
          catch == "Run Entering Columbia",
          select = c(age_3)
        )
      ) + 
        as.numeric(
          subset(
            comp_rr,
            catch == "Run Entering Columbia",
            select = c(age_4)
          )
        ) + 
        as.numeric(
          subset(
            comp_rr,
            catch == "Run Entering Columbia",
            select = c(age_5)
          )
        ) + 
        as.numeric(
          subset(
            comp_rr,
            catch == "Run Entering Columbia",
            select = c(age_6)
          )
        )
    )
  
  #### append current year (time) to loaded data frame
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
  willChsHWprop.dat[nrow(willChsHWprop.dat), 3] = as.numeric(
    subset(
      prior.willClackRR.dat,
      catch == "Run Entering Columbia",
      select = c(age_3)
    )
  ) +
    as.numeric(
      subset(
        prior.willClackRR.dat,
        catch == "Run Entering Columbia",
        select = c(age_4)
      )
    ) +
    as.numeric(
      subset(
        prior.willClackRR.dat,
        catch == "Run Entering Columbia",
        select = c(age_5)
      )
    ) +
    as.numeric(
      subset(
        prior.willClackRR.dat,
        catch == "Run Entering Columbia",
        select = c(age_6)
      )
    )
  
  willChsHWprop.dat[nrow(willChsHWprop.dat), 4] = clp_rt
  
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
  clackChsRet.dat[nrow(clackChsRet.dat), 2] = as.numeric(
    subset(
      comp_rr,
      catch == "Run Entering Clackamas",
      select = c(age_3)
    )
  )
  
  clackChsRet.dat[nrow(clackChsRet.dat) - 1, 3] = as.numeric(
    subset(
      comp_rr, 
      catch == "Run Entering Clackamas", 
      select = c(age_4)
    )
  )
  clackChsRet.dat[nrow(clackChsRet.dat) - 2, 4] = as.numeric(
    subset(
      comp_rr, 
      catch == "Run Entering Clackamas", 
      select = c(age_5)
    )
  )
  
  clackChsRet.dat[nrow(clackChsRet.dat) - 3, 5] = as.numeric(
    subset(
      comp_rr, 
      catch == "Run Entering Clackamas", 
      select = c(age_6)
    )
  )
  
  clackChsRet.dat[nrow(clackChsRet.dat), 6] = as.numeric(
    subset(
      comp_rr, 
      catch == "Run Entering Clackamas", 
      select = c(age_3)
    )
  ) +
    as.numeric(
      subset(
        comp_rr, 
        catch == "Run Entering Clackamas", 
        select = c(age_4)
      )
    ) +
    as.numeric(
      subset(
        comp_rr, 
        catch == "Run Entering Clackamas", 
        select = c(age_5)
      )
    ) +
    as.numeric(
      subset(
        comp_rr, 
        catch == "Run Entering Clackamas", 
        select = c(age_6)
      )
    )
  
  ## save input data (.rda)
  save(willChsRet.dat,
       willChsCov.dat,
       willChsHWprop.dat,
       clackChsRet.dat,
       file=paste(
         "Input\\~Input Data\\Input Tables\\",
         curr_year,
         "willClackInpData.rda",
         sep=""
       )
  )
  ## close Excel files
  system("taskkill /IM Excel.exe")
}
