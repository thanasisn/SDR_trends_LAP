# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisthanasis@gmail.com> */
#' ---
#' title:         ""
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Read "
#'
#' documentclass:  article
#' classoption:    a4paper,oneside
#' fontsize:       10pt
#' geometry:       "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#' link-citations: yes
#' colorlinks:     yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        7
#'     fig_height:       4.5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#'
#'
#+ echo=F, include=T


####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "pdf"   )
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  T       )
# knitr::opts_chunk$set(fig.pos    = '!h'    )


## TODO
## - go to mean distance
## - do it with clouds cloud trends
## - do it without clouds aerosols trends
## - climatologist analysis


#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("Climatological_") })
if(!interactive()) {
    pdf(  file = paste0("~/Aerosols/REPORTS/runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/Aerosols/REPORTS/runtime/", basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/Aerosols/LOGs/",  basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}

par(pch = ".")


## FIXME this is for pdf output
# options(warn=-1) ## hide warnings
# options(warn=2)  ## stop on warnings

#+ echo=F, include=T
####  External code  ####
library(data.table, quietly = T, warn.conflicts = F)
library(pander,     quietly = T, warn.conflicts = F)


## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_regrassion_capture.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")



####  Variables  ####
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

DATA_DIR  <- "/home/athan/DATA/Broad_Band"
CLEARdir  <- "/home/athan/DATA/Broad_Band/CS_id"
tag       <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))
CS_file   <- "/home/athan/DATA/Common_application/Clear_Sky.Rds"

TEST      <- TRUE


MIN_ELEVA  <- 5  ## use data when sun above that
SZA_BIN    <- 1
MIN_N      <- 4
SEAS_MIN_N <- 3

SUM_THRES  <- 50
SZA_THRES  <- 3


#' ## Data selection
#+ echo=F, include=T

####  Get data from Clear sky id data  ####
input_files <- list.files( path    = CLEARdir,
                           pattern = "Clear_Sky_[0-9]{4}.Rds",
                           full.names = T )

if ( !file.exists(CS_file) | max(file.mtime(input_files)) > file.mtime(CS_file)) {
    cat(paste("Load data from Clear Sky proccess from original\n"))
    DATA <- data.table()
    for (af in input_files) {
        temp <- readRDS(af)
        ## drop some data
        temp$CHP1temp        <- NULL
        temp$CHP1tempSD      <- NULL
        temp$CHP1tempUNC     <- NULL
        temp$CS_ref          <- NULL
        temp$Pressure_Source <- NULL
        temp$chp1TempCF      <- NULL
        temp$wattDIR_tmp_cr  <- NULL
        temp$wattDIR_unc_NT  <- NULL
        temp$wattDIR_unc_WT  <- NULL
        temp$wattHOR_tmp_cr  <- NULL
        temp$wattHOR_unc_NT  <- NULL
        temp$wattHOR_unc_WT  <- NULL

        DATA <- rbind(temp, DATA, fill = TRUE)
        rm(temp)
    }
    ## this is used by old scripts
    myRtools::write_RDS(object = DATA, file = CS_file)
} else {
    cat(paste("Load data from Clear Sky proccess from parsed\n"))
    DATA <- readRDS(CS_file)
}




#' ### Filter min elevation
#' Keep data with Sun elevation above `r MIN_ELEVA`
DATA <- DATA[ Elevat >= MIN_ELEVA ]

#' ### Bais paper obstacle filter
DATA <- DATA[ ! (Azimuth > 35 & Azimuth < 120 & Elevat < 10) ]
#+ echo=F, include=T



#' ### Keep only data characterized as 'good' by the Radiation Quality control procedure
#+ echo=F, include=T
DATA[ QCF_DIR != "good", wattDIR := NA ]
DATA[ QCF_DIR != "good", QCF_DIR := NA ]
DATA[ QCF_GLB != "good", wattGLB := NA ]
DATA[ QCF_GLB != "good", QCF_GLB := NA ]
DATA <- DATA[ ! (is.na(wattDIR) & is.na(wattGLB))  ]



## exclude some data for each instrument
# DATA[ QCF_DIR == "Extremely rare limits min (3)", wattDIR := NA ]
# DATA[ QCF_DIR == "Extremely rare limits min (3)", QCF_DIR := NA ]
#
# dirtb <- table(DATA$QCF_DIR)
# for (aty in names(dirtb)[!names(dirtb) %in% "good"]) {
#     temp <- DATA[ QCF_DIR == aty ]
#     cat(nrow(temp),aty,"\n")
# }
# glbtb <- table(DATA$QCF_GLB)
# for (aty in names(glbtb)[!names(glbtb) %in% "good"]) {
#     temp <- DATA[ QCF_GLB == aty ]
#     cat(nrow(temp),aty,"\n")
# }

## TEST MODE
# if (interactive() | TEST ) {
#     DATA <- DATA[ sample(1:nrow(DATA), 500000 ) ]
# }


#' ## Data preparation
#' ### Move measurements to mean earth distance
DATA[ , wattDIR_1au := wattDIR * (sun_dist ^ 2) ]
DATA[ , wattGLB_1au := wattGLB * (sun_dist ^ 2) ]
#+ echo=F, include=T


#' ### Relative to actual TSI at 1au variable representation
DATA[ , DIR_att := wattDIR_1au / tsi_1au_comb ]
DATA[ , GLB_att := wattGLB_1au / tsi_1au_comb ]

#' Using the actual values gives similar trends.

#+ echo=F, include=T

# #' ### Use original variable representation
# DATA[ , DIR_att := wattDIR_1au ]
# DATA[ , GLB_att := wattGLB_1au ]
# #+ echo=F, include=T




# #' ### Ground effect removal?
# #' Aerosol direct effects on global solar shortwave irradiance at high mountainous station Musala Bulgaria_Nojarov2021.pdf
# DATA$wattGLB_1au <- DATA$wattGLB_1au / cosde(DATA$SZA)
# DATA$wattDIR_1au <- DATA$wattDIR_1au / cosde(DATA$SZA)
# #+ echo=F, include=T



#' ### Calculate Bouguer atmospheric transparency
#' see: Changes in solar radiation and their influence on temperature trend in Estonia 1955 2007_Russak2009.pdf
DATA[, DIR_transp := ( wattDIR_1au / tsi_1au_comb ) ^ ( 1 / cosde(SZA) ) ]
#+ echo=F, include=T



## fix noon just in case
DATA[ Azimuth <= 180 , preNoon := TRUE  ]
DATA[ Azimuth >  180 , preNoon := FALSE ]



#' ### Split data to Clear Sky and non Clear sky
#' Method based and adapted to site from: Identification of Periods of Clear Sky
#' Irradiance in Time Series of GHI Measurements
#' _Matthew J. Reno and Clifford W. Hansen_.
#+ echo=F, include=T
DATA_all   <- DATA

wecare     <- grep("CSflag_", names(DATA), value = T)
## use only cm21 flags
wecare     <- grep("_11", wecare, invert = T, value = T)
DATA_Clear <- DATA[ rowSums( DATA[, ..wecare ], na.rm = T ) == 0, ]
#+ echo=F, include=T

## old flags
# DATA_Clear <- DATA_all[ CSflag == 0 ]
rm(DATA)



# #### . . My CSid method   ####
# #'
# #' | CS Flag | Test |
# #' |:-------:|:--------------------------------------------------------------|
# #' |   NA    | Undefined, untested                                           |
# #' |    0    | Passed as clear sky                                           |
# #' |    1    | Mean value of irradiance during the time period (MeanVIP)     |
# #' |    2    | Max Value of Irradiance during the time Period (MaxVIP)       |
# #' |    3    | Variability in irradiance by the length (VIL)                 |
# #' |    4    | Variance of Changes in the Time series (VCT)                  |
# #' |    5    | Variability in the Shape of the irradiance Measurements (VSM) |
# #' |    6    | Low Direct Irradiance limit (LDI)                             |
# #' |    7    | Low Global Irradiance limit (LGI)                             |
# #' |    8    | Too Few CS point for the day (FCS)                            |
# #' |    9    | Too Few data points for the day                               |
# #' |   10    | Missing Data                                                  |
# #' |   11    | Direct irradiance simple threshold                            |






#### Consistency of trends ####

#' ## Consistency of trends


##TODO t-test and significance

#' #### Calculate SZA means ####
#+ echo=F, include=T

ALL_monthly_meanA <- DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                  GLB_att       = mean(GLB_att,    na.rm = T),
                                  DIR_transp    = mean(DIR_transp, na.rm = T),
                                  DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                  GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                  DIR_transp_sd = sd(DIR_transp, na.rm = T),
                                  GLB_att_N     = sum(!is.na(GLB_att)),
                                  DIR_att_N     = sum(!is.na(DIR_att))  ),
                              by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                     Year    = year(Date),
                                     Month   = month(Date),
                                     preNoon = preNoon  ) ]


ALL_monthly_meanB <- DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                  GLB_att       = mean(GLB_att,    na.rm = T),
                                  DIR_transp    = mean(DIR_transp, na.rm = T),
                                  DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                  GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                  DIR_transp_sd = sd(DIR_transp, na.rm = T),
                                  GLB_att_N     = sum(!is.na(GLB_att)),
                                  DIR_att_N     = sum(!is.na(DIR_att)),
                                  preNoon       = "Daily"),
                              by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                     Year    = year(Date),
                                     Month   = month(Date) ) ]

ALL_monthly_mean <- data.table(rbind( data.frame(ALL_monthly_meanB),
                                      data.frame(ALL_monthly_meanA) ))
rm(ALL_monthly_meanA, ALL_monthly_meanB)


CLEAR_monthly_meanA <- DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                      GLB_att       = mean(GLB_att,    na.rm = T),
                                      DIR_transp    = mean(DIR_transp, na.rm = T),
                                      DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                      GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                      DIR_transp_sd = sd(DIR_transp, na.rm = T),
                                      GLB_att_N     = sum(!is.na(GLB_att)),
                                      DIR_att_N     = sum(!is.na(DIR_att))),
                                  by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                         Year    = year(Date),
                                         Month   = month(Date),
                                         preNoon = preNoon  ) ]


CLEAR_monthly_meanB <- DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                      GLB_att       = mean(GLB_att,    na.rm = T),
                                      DIR_transp    = mean(DIR_transp, na.rm = T),
                                      DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                      GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                      DIR_transp_sd = sd(DIR_transp, na.rm = T),
                                      GLB_att_N     = sum(!is.na(GLB_att)),
                                      DIR_att_N     = sum(!is.na(DIR_att)),
                                      preNoon       = "Daily"),
                                  by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                         Year    = year(Date),
                                         Month   = month(Date) ) ]

CLEAR_monthly_mean <- data.table(rbind( data.frame(CLEAR_monthly_meanB),
                                        data.frame(CLEAR_monthly_meanA) ))
rm(CLEAR_monthly_meanA, CLEAR_monthly_meanB)



## margin of error calculation
CONF_INTERV <- .95
conf_param  <- 1-(1-CONF_INTERV)/2
suppressWarnings({
ALL_monthly_mean[,   DIR_att_EM    := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd   /sqrt(DIR_att_N)]
ALL_monthly_mean[,   GLB_att_EM    := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd   /sqrt(GLB_att_N)]
ALL_monthly_mean[,   DIR_transp_EM := qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd/sqrt(DIR_att_N)]
CLEAR_monthly_mean[, DIR_att_EM    := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd   /sqrt(DIR_att_N)]
CLEAR_monthly_mean[, GLB_att_EM    := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd   /sqrt(GLB_att_N)]
CLEAR_monthly_mean[, DIR_transp_EM := qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd/sqrt(DIR_att_N)]
})


#' #### Exclude means with less than `r SZA_THRES` data points
#+ echo=F, include=T

ALL_monthly_mean[   DIR_att_N <= SZA_THRES, DIR_att       := NA ]
ALL_monthly_mean[   GLB_att_N <= SZA_THRES, GLB_att       := NA ]
ALL_monthly_mean[   DIR_att_N <= SZA_THRES, DIR_transp    := NA ]
ALL_monthly_mean[   DIR_att_N <= SZA_THRES, DIR_att_EM    := NA ]
ALL_monthly_mean[   GLB_att_N <= SZA_THRES, GLB_att_EM    := NA ]
ALL_monthly_mean[   DIR_att_N <= SZA_THRES, DIR_transp_EM := NA ]
CLEAR_monthly_mean[ DIR_att_N <= SZA_THRES, DIR_att       := NA ]
CLEAR_monthly_mean[ GLB_att_N <= SZA_THRES, GLB_att       := NA ]
CLEAR_monthly_mean[ DIR_att_N <= SZA_THRES, DIR_transp    := NA ]
CLEAR_monthly_mean[ DIR_att_N <= SZA_THRES, DIR_att_EM    := NA ]
CLEAR_monthly_mean[ GLB_att_N <= SZA_THRES, GLB_att_EM    := NA ]
CLEAR_monthly_mean[ DIR_att_N <= SZA_THRES, DIR_transp_EM := NA ]


#' #### Calculate monthly seasonal values ####
#+ echo=F, include=T

ALL_monthly_seas <-
    ALL_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                         GLB_att_seas    = mean(GLB_att,    na.rm = T),
                         DIR_transp_seas = mean(DIR_transp, na.rm = T),
                         DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                         GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                         GLB_att_N_seas  = sum(!is.na(GLB_att)),
                         DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .( Month, SZA, preNoon ) ]

CLEAR_monthly_seas <-
    CLEAR_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                           GLB_att_seas    = mean(GLB_att,    na.rm = T),
                           DIR_transp_seas = mean(DIR_transp, na.rm = T),
                           DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                           GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                           GLB_att_N_seas  = sum(!is.na(GLB_att)),
                           DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                       by = .( Month, SZA, preNoon ) ]



#+ echo=F, include=F
## ~ Plots longterm  ####
plot( ALL_monthly_mean$Month, ALL_monthly_mean$DIR_att )
plot( ALL_monthly_mean$Month, ALL_monthly_mean$DIR_att_EM )
plot( ALL_monthly_mean$Month, ALL_monthly_mean$GLB_att )
plot( ALL_monthly_mean$Month, ALL_monthly_mean$GLB_att_EM )
plot( ALL_monthly_mean$Month, ALL_monthly_mean$DIR_transp )
plot( ALL_monthly_mean$Month, ALL_monthly_mean$DIR_transp_EM )

plot( ALL_monthly_mean$SZA, ALL_monthly_mean$DIR_att )
plot( ALL_monthly_mean$SZA, ALL_monthly_mean$DIR_att_EM )
plot( ALL_monthly_mean$SZA, ALL_monthly_mean$GLB_att )
plot( ALL_monthly_mean$SZA, ALL_monthly_mean$GLB_att_EM )
plot( ALL_monthly_mean$SZA, ALL_monthly_mean$DIR_transp )
plot( ALL_monthly_mean$SZA, ALL_monthly_mean$DIR_transp_EM )

hist(ALL_monthly_mean$DIR_att_N)
hist(ALL_monthly_mean$GLB_att_N)

hist(ALL_monthly_mean$DIR_att_EM)
hist(ALL_monthly_mean$GLB_att_EM)
hist(ALL_monthly_mean$DIR_transp_EM)



plot( CLEAR_monthly_mean$Month, CLEAR_monthly_mean$DIR_att )
plot( CLEAR_monthly_mean$Month, CLEAR_monthly_mean$DIR_att_EM )
plot( CLEAR_monthly_mean$Month, CLEAR_monthly_mean$GLB_att )
plot( CLEAR_monthly_mean$Month, CLEAR_monthly_mean$GLB_att_EM )
plot( CLEAR_monthly_mean$Month, CLEAR_monthly_mean$DIR_transp )
plot( CLEAR_monthly_mean$Month, CLEAR_monthly_mean$DIR_transp_EM )

plot( CLEAR_monthly_mean$SZA, CLEAR_monthly_mean$DIR_att )
plot( CLEAR_monthly_mean$SZA, CLEAR_monthly_mean$DIR_att_EM )
plot( CLEAR_monthly_mean$SZA, CLEAR_monthly_mean$GLB_att )
plot( CLEAR_monthly_mean$SZA, CLEAR_monthly_mean$GLB_att_EM )
plot( CLEAR_monthly_mean$SZA, CLEAR_monthly_mean$DIR_transp )
plot( CLEAR_monthly_mean$SZA, CLEAR_monthly_mean$DIR_transp_EM )

hist(CLEAR_monthly_mean$DIR_att_N)
hist(CLEAR_monthly_mean$GLB_att_N)
hist(CLEAR_monthly_mean$DIR_att_EM)
hist(CLEAR_monthly_mean$GLB_att_EM)
hist(CLEAR_monthly_mean$DIR_transp_EM)



# # ## ~ Plots seasonal ####
# #
# # plot( ALL_monthly_seas$doy, ALL_monthly_seas$DIR_att_seas )
# # plot( ALL_monthly_seas$doy, ALL_monthly_seas$GLB_att_seas )
# # plot( ALL_monthly_seas$doy, ALL_monthly_seas$DIR_transp_seas )
# # plot( ALL_monthly_seas$doy, ALL_monthly_seas$GLB_att_N_seas)
# # plot( ALL_monthly_seas$doy, ALL_monthly_seas$DIR_att_N_seas)
# #
# #
# # plot( CLEAR_monthly_seas$doy, CLEAR_monthly_seas$DIR_att_seas )
# # plot( CLEAR_monthly_seas$doy, CLEAR_monthly_seas$GLB_att_seas )
# # plot( CLEAR_monthly_seas$doy, CLEAR_monthly_seas$DIR_transp_seas )
# # plot( CLEAR_monthly_seas$doy, CLEAR_monthly_seas$GLB_att_N_seas)
# # plot( CLEAR_monthly_seas$doy, CLEAR_monthly_seas$DIR_att_N_seas)



#' #### Calculate seasonal anomaly ####
#+ echo=F, include=F

## ~ remove seasonality ####

ALL_monthly_seas   <- merge(  ALL_monthly_mean, ALL_monthly_seas,   by = c("Month", "SZA", "preNoon"),  all = T)
CLEAR_monthly_seas <- merge(CLEAR_monthly_mean, CLEAR_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)


# #+ echo=F, include=T
# ## anomaly
# ALL_monthly_seas[   , DIR_att    := DIR_att    - DIR_att_seas    ]
# ALL_monthly_seas[   , GLB_att    := GLB_att    - GLB_att_seas    ]
# ALL_monthly_seas[   , DIR_transp := DIR_transp - DIR_transp_seas ]
# CLEAR_monthly_seas[ , DIR_att    := DIR_att    - DIR_att_seas    ]
# CLEAR_monthly_seas[ , GLB_att    := GLB_att    - GLB_att_seas    ]
# CLEAR_monthly_seas[ , DIR_transp := DIR_transp - DIR_transp_seas ]
# #+ echo=F, include=F


#+ echo=F, include=T
## relative anomaly
ALL_monthly_seas[  , DIR_att    := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
ALL_monthly_seas[  , GLB_att    := 100 * ( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
ALL_monthly_seas[  , DIR_transp := 100 * ( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
CLEAR_monthly_seas[, DIR_att    := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
CLEAR_monthly_seas[, GLB_att    := 100 * ( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
CLEAR_monthly_seas[, DIR_transp := 100 * ( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
#+ echo=F, include=F

ALL_monthly_seas[   preNoon == TRUE,    preNoon := "am" ]
ALL_monthly_seas[   preNoon == FALSE,   preNoon := "pm" ]
ALL_monthly_seas[   preNoon == "Daily", preNoon := "day" ]
CLEAR_monthly_seas[ preNoon == FALSE,   preNoon := "pm" ]
CLEAR_monthly_seas[ preNoon == "Daily", preNoon := "day" ]
CLEAR_monthly_seas[ preNoon == TRUE,    preNoon := "am" ]


setorder(ALL_monthly_seas,  Year,Month,preNoon,SZA)
setorder(CLEAR_monthly_seas,Year,Month,preNoon,SZA)




####  Cumulative sums ####
#' ## Cumulative sums
#+ echo=F, include=F
timefactor <- 1
vars    <- c("GLB_att", "DIR_att", "DIR_transp")
dbs     <- c("ALL_monthly_seas", "CLEAR_monthly_seas")
basevar <- c("Year","Month","SZA","preNoon")

gather <- expand.grid( Year  = unique(c(ALL_monthly_seas$Year,  CLEAR_monthly_seas$Year)),
                       Month = unique(c(ALL_monthly_seas$Month, CLEAR_monthly_seas$Month)),
                       SZA   = unique(c(ALL_monthly_seas$SZA,   CLEAR_monthly_seas$SZA)),
                       preNoon = unique(c(ALL_monthly_seas$preNoon,   CLEAR_monthly_seas$preNoon)))


for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        for (anoon in unique( DB$preNoon)) {
            for (asza in unique( DB$SZA )) {



                ## set na to zero
                DB[ SZA == asza & preNoon == anoon, ][[avar]][is.na(DB[ SZA == asza & preNoon == anoon, ][[avar]])] <- 0
                DB[ SZA == asza & preNoon == anoon, ][[avar]] <- unlist(cumsum( DB[ SZA == asza & preNoon == anoon, ..avar ] ))
                DB[ SZA == asza & preNoon == anoon, ][[avar]][ DB[ SZA == asza & preNoon == anoon, ][[avar]] == 0 ] <- NA


                # dataset <- DB[ SZA == asza & preNoon == anoon, ..ttt ]
                #
                #
                # dataset[, Data := sub("_.*","", DBn) ]
                #
                # dataset[[avar]][is.na(dataset[[avar]])] <- 0
                # dataset[[avar]] <- cumsum( dataset[[avar]] )
                #
                # gather <- merge(gather, dataset, all = T)


            }
        }
    }
    ttt <- c(basevar,vars)
    assign(sub("seas","sumsum", DBn), DB[, ..ttt]   )
}




ALL_monthly_sumsum[,   FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
CLEAR_monthly_sumsum[, FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


plotsza <- c( 63 )
plotpreNoon <- c("am","pm","day")
plotpNcol   <- c(2,4,6)
vars        <- c("GLB_att", "DIR_att", "DIR_transp")
database    <- c("ALL_monthly_sumsum","CLEAR_monthly_sumsum")

for (adb in database) {
    DB <- get(adb)
    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("FDate","preNoon",avar)
            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]
            pdb   <- pdb[!is.na(pdb[[avar]])]
            xlim  <- range(pdb$FDate)
            ylim  <- range(pdb[[avar]],na.rm = T)

            plot(1, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, xaxt = "n")
            axis.Date(1, pdb$FDate)

            for ( i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon==plotpreNoon[i]]
                lines(pp$FDate, pp[[avar]], col = plotpNcol[i])
            }

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3,cex = 0.8)

            title(paste(adb, asza, avar))
        }
    }
}






stop("hhh")
#+ echo=F, include=F
hist(gather$N[gather$N>50], breaks = 100)

szatrends <- gather
szatrends$preNoon[ szatrends$preNoon == T ] <- 2
szatrends$preNoon[ szatrends$preNoon == F ] <- 3
szatrends <- data.table(szatrends)

hist(szatrends[DATA==dbs[1],N], breaks = 100)
hist(szatrends[DATA==dbs[2],N], breaks = 100)

hist(szatrends[var==vars[1],N], breaks = 100)
hist(szatrends[var==vars[2],N], breaks = 100)

# szatrends <- szatrends[ N > 50]

plot(szatrends$SZA,szatrends$N)

#+ echo=F, include=T
for (type in unique(szatrends$DATA)) {
    for (avar in unique(szatrends$var)) {
        type

        subdata <- szatrends[ szatrends$DATA == type & szatrends$var == avar , ]

        plot(subdata$SZA, subdata$slope, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope",type, avar))
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$slope.sd, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope sd",type, avar))
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$slope.t, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope t",type, avar))
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$slope.p, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope p",type, avar))
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$Rsqrd, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope Rsq",type, avar))
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        # plot(subdata$SZA, subdata$intercept, col = subdata$preNoon, pch = 19)
        # abline(h=0)
        # title(paste("Intercept",type, avar))

        plot(subdata$SZA, subdata$N, col = subdata$preNoon, pch = 19)
        title(paste("N",type, avar))

        # hist(subdata$N, breaks = 100)
    }
}



# #### 2. Other ####
# #' ## 2. Other
#
# #### ~ Aggregation in sza bins ####
# #' ### Aggregation in sza bins
#
# All_mean <- DATA_all[, .(DIR_att    = mean(DIR_att,    na.rm = T),
#                          GLB_att    = mean(GLB_att,    na.rm = T),
#                          DIR_transp = mean(DIR_transp, na.rm = T),
#                          GLB_att_N  = sum(!is.na(GLB_att)),
#                          DIR_att_N  = sum(!is.na(DIR_att)),
#                          DIR_att_sd = sd(  DIR_att,    na.rm = T),
#                          GLB_att_sd = sd(  GLB_att,    na.rm = T),
#                          DIR_transp_sd = sd(DIR_transp, na.rm = T),
#                          .N),
#                      by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
#                             Date    = Day,
#                             preNoon = preNoon ) ]
# All_mean <- All_mean[ N >= MIN_N ]
#
#
# ALL_Seas <- All_mean[ , .(DIR_att    = mean(DIR_att,    na.rm = T),
#                           GLB_att    = mean(GLB_att,    na.rm = T),
#                           DIR_transp = mean(DIR_transp, na.rm = T),
#                           GLB_att_N  = sum(!is.na(GLB_att)),
#                           DIR_att_N  = sum(!is.na(DIR_att)),
#                           DIR_att_sd = sd(  DIR_att,    na.rm = T),
#                           GLB_att_sd = sd(  GLB_att,    na.rm = T),
#                           DIR_transp_sd = sd(DIR_transp, na.rm = T)),
#                       by = .(SZA, preNoon, doy = yday(Date) ) ]
#
#
# # plot(ALL_Seas$doy, ALL_Seas$DIR_att)
# # plot(ALL_Seas$doy, ALL_Seas$GLB_att)
# plot(ALL_Seas$doy, ALL_Seas$GLB_att_N)
# plot(ALL_Seas$doy, ALL_Seas$DIR_att_N)
#
# hist(ALL_Seas$DIR_att_N)
# hist(ALL_Seas$GLB_att_N)
#
#
# ## TODO Calculate transparency after aggregation!!
#
# All_mean[ is.nan(DIR_att), DIR_att := NA ]
# All_mean[ is.nan(GLB_att), DIR_att := NA ]
#
#
#
# hist(All_mean[ ,N], breaks = 100)
# # plot(All_mean$SZA,  All_mean$N, pch  = ".")
# # plot(All_mean$Date, All_mean$N, pch  = ".")
#
#
#
# Clear_mean <- DATA_Clear[, .(DIR_att    = mean(DIR_att,    na.rm = T),
#                              GLB_att    = mean(GLB_att,    na.rm = T),
#                              DIR_transp = mean(DIR_transp, na.rm = T),
#                              .N),
#                          by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
#                                 Date    = Day,
#                                 preNoon = preNoon) ]
# Clear_mean <- Clear_mean[ N >= MIN_N ]
#
# Clear_mean[ is.nan(DIR_att), DIR_att := NA ]
# Clear_mean[ is.nan(GLB_att), DIR_att := NA ]
#
# # plot(Clear_mean$SZA,  Clear_mean$N, pch  = "." )
# # plot(Clear_mean$Date, Clear_mean$N, pch  = "." )
#
#
#
# #### ~ Aggregation daily ####
#
#
# ALL_daily_mean <- DATA_all[, .(DIR_att    = mean(DIR_att,    na.rm = T),
#                                GLB_att    = mean(GLB_att,    na.rm = T),
#                                DIR_transp = mean(DIR_transp, na.rm = T),
#                                DIR_att_sd = sd(  DIR_att,    na.rm = T),
#                                GLB_att_sd = sd(  GLB_att,    na.rm = T),
#                                doy        = yday(Date),
#                                GLB_att_N  = sum(!is.na(GLB_att)),
#                                DIR_att_N  = sum(!is.na(DIR_att))  ),
#                            by = .( Date   = Day ) ]
#
#
# CLEAR_daily_mean <- DATA_Clear[, .(DIR_att    = mean(DIR_att,    na.rm = T),
#                                    GLB_att    = mean(GLB_att,    na.rm = T),
#                                    DIR_transp = mean(DIR_transp, na.rm = T),
#                                    DIR_att_sd = sd(  DIR_att,    na.rm = T),
#                                    GLB_att_sd = sd(  GLB_att,    na.rm = T),
#                                    doy        = yday(Date),
#                                    GLB_att_N  = sum(!is.na(GLB_att)),
#                                    DIR_att_N  = sum(!is.na(DIR_att))  ),
#                                by = .( Date   = Day ) ]
#
#
#
# All_daily_sum <- DATA_all[, .(DIR_att    = sumNA(DIR_att,    na.rm = T),
#                               GLB_att    = sumNA(GLB_att,    na.rm = T),
#                               DIR_transp = sumNA(DIR_transp, na.rm = T),
#                               DIR_att_sd = sd(  DIR_att,    na.rm = T),
#                               GLB_att_sd = sd(  GLB_att,    na.rm = T),
#                               doy        = yday(Date),
#                               GLB_att_N  = sum(!is.na(GLB_att)),
#                               DIR_att_N  = sum(!is.na(DIR_att))  ),
#                           by = .( Date   = Day ) ]
#
#
# Clear_daily_sum <- DATA_Clear[, .(DIR_att    = sumNA(DIR_att,    na.rm = T),
#                                   GLB_att    = sumNA(GLB_att,    na.rm = T),
#                                   DIR_transp = sumNA(DIR_transp, na.rm = T),
#                                   DIR_att_sd = sd(  DIR_att,    na.rm = T),
#                                   GLB_att_sd = sd(  GLB_att,    na.rm = T),
#                                   doy        = yday(Date),
#                                   GLB_att_N  = sum(!is.na(GLB_att)),
#                                   DIR_att_N  = sum(!is.na(DIR_att))  ),
#                               by = .( Date   = Day ) ]
#
#
#
# ## Apply threshold to sums
#
# All_daily_sum[ GLB_att_N <= SUM_THRES, GLB_att    := NA ]
# All_daily_sum[ GLB_att_N <= SUM_THRES, GLB_att_sd := NA ]
# All_daily_sum[ GLB_att_N <= SUM_THRES, GLB_att_N  := NA ]
#
# All_daily_sum[ DIR_att_N <= SUM_THRES, DIR_att    := NA ]
# All_daily_sum[ DIR_att_N <= SUM_THRES, DIR_att_sd := NA ]
# All_daily_sum[ DIR_att_N <= SUM_THRES, DIR_transp := NA ]
# All_daily_sum[ DIR_att_N <= SUM_THRES, DIR_att_N := NA ]
#
#
#
#
# ## daily means
#
#
#
# ## daily sums
#
# plot( All_daily_sum$Date, All_daily_sum$DIR_att )
# plot( All_daily_sum$Date, All_daily_sum$GLB_att )
# plot( All_daily_sum$Date, All_daily_sum$DIR_transp )
#
# plot( All_daily_sum$doy,  All_daily_sum$DIR_att )
# plot( All_daily_sum$doy,  All_daily_sum$GLB_att )
# plot( All_daily_sum$doy,  All_daily_sum$DIR_transp )
#
# plot( All_daily_sum$doy,  All_daily_sum$GLB_att_N)
# plot( All_daily_sum$doy,  All_daily_sum$DIR_att)
# plot( All_daily_sum$doy,  All_daily_sum$DIR_att_N)
#
#
#
# plot( Clear_daily_sum$Date, Clear_daily_sum$DIR_att )
# plot( Clear_daily_sum$Date, Clear_daily_sum$GLB_att )
# plot( Clear_daily_sum$Date, Clear_daily_sum$DIR_transp )
#
# plot( Clear_daily_sum$doy,  Clear_daily_sum$DIR_att )
# plot( Clear_daily_sum$doy,  Clear_daily_sum$GLB_att )
# plot( Clear_daily_sum$doy,  Clear_daily_sum$DIR_transp )
#
# plot( Clear_daily_sum$doy,  Clear_daily_sum$GLB_att_N)
# plot( Clear_daily_sum$doy,  Clear_daily_sum$DIR_att)
# plot( Clear_daily_sum$doy,  Clear_daily_sum$DIR_att_N)
#
#
#
#
#
#
# All_daily_sum_seas <- All_daily_sum[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
#                                         GLB_att_seas    = mean(GLB_att,    na.rm = T),
#                                         DIR_transp_seas = mean(DIR_transp, na.rm = T),
#                                         DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
#                                         GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
#                                         GLB_att_N_seas  = sum(!is.na(GLB_att)),
#                                         DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
#                                     by = .( doy ) ]
#
# Clear_daily_sum_seas <- Clear_daily_sum[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
#                                             GLB_att_seas    = mean(GLB_att,    na.rm = T),
#                                             DIR_transp_seas = mean(DIR_transp, na.rm = T),
#                                             DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
#                                             GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
#                                             GLB_att_N_seas  = sum(!is.na(GLB_att)),
#                                             DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
#                                         by = .( doy ) ]
#
#
#
#
#
#
#
#
# plot( All_daily_sum_seas$doy, All_daily_sum_seas$DIR_att_seas )
# plot( All_daily_sum_seas$doy, All_daily_sum_seas$GLB_att_seas )
# plot( All_daily_sum_seas$doy, All_daily_sum_seas$DIR_transp_seas )
# plot( All_daily_sum_seas$doy, All_daily_sum_seas$GLB_att_N_seas)
# plot( All_daily_sum_seas$doy, All_daily_sum_seas$DIR_att_N_seas)
#
#
# plot( Clear_daily_sum_seas$doy, Clear_daily_sum_seas$DIR_att_seas )
# plot( Clear_daily_sum_seas$doy, Clear_daily_sum_seas$GLB_att_seas )
# plot( Clear_daily_sum_seas$doy, Clear_daily_sum_seas$DIR_transp_seas )
# plot( Clear_daily_sum_seas$doy, Clear_daily_sum_seas$GLB_att_N_seas)
# plot( Clear_daily_sum_seas$doy, Clear_daily_sum_seas$DIR_att_N_seas)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# ###########################################################################################3
#
#
#
#
# ## sum of attributes ####
#
#
# All_sum <- All_mean[ , .( GLB_att    = sumNA(GLB_att,    na.rm = T),
#                           DIR_att    = sumNA(DIR_att,    na.rm = T),
#                           DIR_transp = sumNA(DIR_transp, na.rm = T)) , by = Date ]
#
#
# Clear_sum <- Clear_mean[ , .( GLB_att    = sumNA(GLB_att,    na.rm = T),
#                               DIR_att    = sumNA(DIR_att,    na.rm = T),
#                               DIR_transp = sumNA(DIR_transp, na.rm = T)) , by = Date ]
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# plot(Clear_sum$Date, Clear_sum$GLB_att, pch  = ".")
# title("Clear data sum data")
#
# plot(Clear_sum$Date, Clear_sum$DIR_att, pch  = ".")
# title("Clear data sum data")
#
#
# plot(Clear_sum$Date, ( Clear_sum$GLB_att - mean(Clear_sum$GLB_att, na.rm = T)) /mean# (Clear_sum$GLB_att, na.rm = T), pch  = "." )
# title("Clear data sum data")
#
#
# plot(Clear_sum$Date, ( Clear_sum$DIR_att - mean(Clear_sum$DIR_att, na.rm = T)) /mean# (Clear_sum$DIR_att, na.rm = T), pch  = "." )
# title("Clear data sum data")
#
#
#
#
# data_list  <- list(DATA_all, DATA_Clear)
# data_names <- list("All data points", "All clear sky data points")
#
# wecare    <- c("wattDIR", "wattGLB", "wattDIF", "Clearness_Kt", "DiffuseFraction_Kd")
# wecare    <- c("DIR_att", "GLB_att", "DIR_transp")
#
#
# for(i in 1:length(data_list)) {
#     Dplot <- data_list[[i]]
#
#     ####  Plot some variables  ####
#
#     cat(paste("\n\n## ", i, "\n\n"))
#
#     names(Dplot)
#
#
#     for (var in wecare) {
#
#         vect <- Dplot[[var]]
#
#         hist(vect, main = var, breaks = 100)
#
#         plot(Dplot$Date, vect, pch = ".", main = var)
#
#         # plot(Dplot$SZA, vect, pch = ".", main = var)
#
#         plot(Dplot$SZA, vect, pch = ".", main = var)
#
#         plot(cosde(Dplot$SZA), vect, pch = ".", main = var)
#
#     }
#
# }
#






#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
