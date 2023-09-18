# define variables --------------------------------------------------------
curr_year <- 2022
inPath <- 'Input\\~Input Data\\Input Tables\\'
predPath <- 'Output\\Predictions\\'
rrPath <- "Input\\~Input Data\\Run Reconstruction\\"
shinyPath <- "Output\\Shiny\\WillRetApp.v.2\\www\\"

# read data ---------------------------------------------------------------
## current year's model input data
load(
  file = paste(
    inPath,
    curr_year,
    "willClackInpData.rda",
    sep=""
  )
)

## new (curr_year+1) predictions
curr.willClack.predns <- readRDS(
  paste(
    predPath,
    curr_year+1,
    'willClackComb.pred.out.rds',
    sep = ""
  )
)

## prior year's (curr_year) predictions
prior.willClack.predns <- readRDS(
  paste(
    predPath,
    curr_year,
    'willClackComb.pred.out.rds',
    sep = ""
  )
)

# prior year's shiny data
willClack.shiny.dat <- readRDS(
  paste(
    shinyPath,
    curr_year-1,
    'willClackComb.shiny.dat.rds',
    sep = ""
  )
)


# add new records to shiny data -------------------------------------------
willClack.shiny.dat[nrow(willClack.shiny.dat),2] <- as.numeric(willChsRet.dat[nrow(willChsRet.dat)-1,3])
willClack.shiny.dat[nrow(willClack.shiny.dat),3] <- as.numeric(willChsRet.dat[nrow(willChsRet.dat)-2,4])
willClack.shiny.dat[nrow(willClack.shiny.dat),4] <- as.numeric(willChsRet.dat[nrow(willChsRet.dat)-3,5])
willClack.shiny.dat[nrow(willClack.shiny.dat),5] <- as.numeric(willChsRet.dat[nrow(willChsRet.dat)-4,6])
willClack.shiny.dat[nrow(willClack.shiny.dat),6] <- as.numeric(willChsRet.dat[nrow(willChsRet.dat)-2,4] +
                                                                 willChsRet.dat[nrow(willChsRet.dat)-3,5] +
                                                                 willChsRet.dat[nrow(willChsRet.dat)-4,6])
willClack.shiny.dat[nrow(willClack.shiny.dat),7] <- as.numeric(willChsRet.dat[nrow(willChsRet.dat)-1,3] +
                                                                 willChsRet.dat[nrow(willChsRet.dat)-2,4] +
                                                                 willChsRet.dat[nrow(willChsRet.dat)-3,5] +
                                                                 willChsRet.dat[nrow(willChsRet.dat)-4,6])
willClack.shiny.dat[nrow(willClack.shiny.dat),8] <- as.numeric(prior.willClack.predns[1,2])
willClack.shiny.dat[nrow(willClack.shiny.dat),9] <- as.numeric(prior.willClack.predns[2,2])
willClack.shiny.dat[nrow(willClack.shiny.dat),10] <- as.numeric(prior.willClack.predns[3,2])
willClack.shiny.dat[nrow(willClack.shiny.dat),11] <- as.numeric(prior.willClack.predns[4,2])
willClack.shiny.dat[nrow(willClack.shiny.dat),12] <- as.numeric(prior.willClack.predns[5,2])
willClack.shiny.dat[nrow(willClack.shiny.dat),13] <- as.numeric(prior.willClack.predns[6,2])
willClack.shiny.dat[nrow(willClack.shiny.dat),14] <- NA
willClack.shiny.dat[nrow(willClack.shiny.dat),15] <- NA
willClack.shiny.dat[nrow(willClack.shiny.dat),16] <- NA
willClack.shiny.dat[nrow(willClack.shiny.dat),17] <- NA
willClack.shiny.dat[nrow(willClack.shiny.dat),18] <- NA
willClack.shiny.dat[nrow(willClack.shiny.dat),19] <- NA


pHolder <- data.frame(ReturnYr = curr_year,
                      Age3 = NA,
                      Age4 = NA,
                      Age5 = NA,
                      Age6 = NA,
                      Adults = NA,
                      Total = NA,
                      predAge3 = NA,
                      predAge4 = NA,
                      predAge5 = NA,
                      predAge6 = NA,
                      predAdults = NA,
                      predTotal = NA,
                      predAge3Curr = curr.willClack.predns[1,2],
                      predAge4Curr = curr.willClack.predns[2,2],
                      predAge5Curr = curr.willClack.predns[3,2],
                      predAge6Curr = curr.willClack.predns[4,2],
                      predAdultsCurr = curr.willClack.predns[5,2],
                      predTotalCurr = curr.willClack.predns[6,2]
)

willClack.shiny.dat <- rbind(willClack.shiny.dat,
                             pHolder)

### save new (curr_year) shiny input data (.rds)
saveRDS(
  willClack.shiny.dat,
  file = paste(
    'Output\\Shiny\\WillRetApp.v.2\\www\\',
    curr_year,
    'willClackComb.shiny.dat.rds',
    sep = ""
  )
)
