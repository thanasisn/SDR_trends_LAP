# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisthanasis@gmail.com> */
#' ---
#' title:         "Trends of SDR in Thessaloniki "
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics,AUTH, natsisthanasis@gmail.com]
#'   - Jane Doe^[Institution Two, jane@example.org]
#' abstract:
#'   "Study of GHI and DNI radiation for 'clear sky' and all sly conditions."
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
knitr::opts_chunk$set(out.width  = "90%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  T       )
# knitr::opts_chunk$set(fig.pos    = '!h'    )



#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("Climatological_") })
if(!interactive()) {
    pdf(  file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/", basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime//",  basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
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

col_DIR_att    <- "#2166ac"
col_DIR_transp <- "#9970ab"
col_GLB_att    <- "#1a9850"

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

#' ### Data range
#' Time data span `r range(DATA$Date)`
#'




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






#### 2. Long term by SZA ####

#' ## 2. Long term by SZA


##TODO t-test and significance

#' #### Calculate daily SZA means ####
#+ echo=F, include=T

ALL_daily_mean <- DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                               GLB_att       = mean(GLB_att,    na.rm = T),
                               DIR_transp    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                               GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                               DIR_transp_sd = sd(DIR_transp, na.rm = T),
                               doy           = yday(Date),
                               GLB_att_N     = sum(!is.na(GLB_att)),
                               DIR_att_N     = sum(!is.na(DIR_att))  ),
                           by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                  Date    = Day,
                                  preNoon = preNoon  ) ]


CLEAR_daily_mean <- DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                   GLB_att       = mean(GLB_att,    na.rm = T),
                                   DIR_transp    = mean(DIR_transp, na.rm = T),
                                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                   DIR_transp_sd = sd(DIR_transp, na.rm = T),
                                   doy           = yday(Date),
                                   GLB_att_N     = sum(!is.na(GLB_att)),
                                   DIR_att_N     = sum(!is.na(DIR_att))  ),
                               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                      Date    = Day,
                                      preNoon = preNoon ) ]



#' #### Margin of error calculation for 0.95 confidence interval ####
#+ echo=T, include=T
CONF_INTERV <- .95
conf_param  <- 1-(1-CONF_INTERV)/2
suppressWarnings({
ALL_daily_mean[,   DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
ALL_daily_mean[,   GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
ALL_daily_mean[,   DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
CLEAR_daily_mean[, DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
CLEAR_daily_mean[, GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
CLEAR_daily_mean[, DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
})
#+ echo=F, include=F


#' #### Exclude means with less than `r SZA_THRES` data points
#+ echo=F, include=T
ALL_daily_mean[   DIR_att_N <= SZA_THRES, DIR_att    := NA ]
ALL_daily_mean[   GLB_att_N <= SZA_THRES, GLB_att    := NA ]
ALL_daily_mean[   DIR_att_N <= SZA_THRES, DIR_transp := NA ]
CLEAR_daily_mean[ DIR_att_N <= SZA_THRES, DIR_att    := NA ]
CLEAR_daily_mean[ GLB_att_N <= SZA_THRES, GLB_att    := NA ]
CLEAR_daily_mean[ DIR_att_N <= SZA_THRES, DIR_transp := NA ]



#' #### Calculate daily seasonal values by SZA ####
#+ echo=F, include=T

ALL_daily_seas <-
    ALL_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                       GLB_att_seas    = mean(GLB_att,    na.rm = T),
                       DIR_transp_seas = mean(DIR_transp, na.rm = T),
                       DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                       GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                       GLB_att_N_seas  = sum(!is.na(GLB_att)),
                       DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                   by = .( doy, SZA, preNoon ) ]

CLEAR_daily_seas <-
    CLEAR_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                         GLB_att_seas    = mean(GLB_att,    na.rm = T),
                         DIR_transp_seas = mean(DIR_transp, na.rm = T),
                         DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                         GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                         GLB_att_N_seas  = sum(!is.na(GLB_att)),
                         DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .( doy, SZA, preNoon ) ]



#+ echo=F, include=F
## ~ Plots longterm  ####
plot( ALL_daily_mean$Date, ALL_daily_mean$DIR_att )
plot( ALL_daily_mean$Date, ALL_daily_mean$GLB_att )
plot( ALL_daily_mean$Date, ALL_daily_mean$DIR_transp )

plot( ALL_daily_mean$doy, ALL_daily_mean$DIR_att )
plot( ALL_daily_mean$doy, ALL_daily_mean$GLB_att )
plot( ALL_daily_mean$doy, ALL_daily_mean$DIR_transp )

plot( ALL_daily_mean$doy, ALL_daily_mean$GLB_att_N)
plot( ALL_daily_mean$doy, ALL_daily_mean$DIR_att_N)

hist(ALL_daily_mean$DIR_att_N)
hist(ALL_daily_mean$GLB_att_N)


plot( CLEAR_daily_mean$Date, CLEAR_daily_mean$DIR_att )
plot( CLEAR_daily_mean$Date, CLEAR_daily_mean$GLB_att )
plot( CLEAR_daily_mean$Date, CLEAR_daily_mean$DIR_transp )

plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$DIR_att )
plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$GLB_att )
plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$DIR_transp )

plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$GLB_att_N)
plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$DIR_att_N)

hist( CLEAR_daily_mean$DIR_att_N)
hist( CLEAR_daily_mean$GLB_att_N)

## ~ Plots seasonal ####

plot( ALL_daily_seas$doy, ALL_daily_seas$DIR_att_seas )
plot( ALL_daily_seas$doy, ALL_daily_seas$GLB_att_seas )
plot( ALL_daily_seas$doy, ALL_daily_seas$DIR_transp_seas )
plot( ALL_daily_seas$doy, ALL_daily_seas$GLB_att_N_seas)
plot( ALL_daily_seas$doy, ALL_daily_seas$DIR_att_N_seas)

plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$DIR_att_seas )
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$GLB_att_seas )
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$DIR_transp_seas )
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$GLB_att_N_seas)
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$DIR_att_N_seas)



#' #### Calculate seasonal anomaly ####
#+ echo=F, include=F

ALL_daily_seas   <- merge(  ALL_daily_mean, ALL_daily_seas,   by = c("doy", "SZA", "preNoon"),  all = T)
CLEAR_daily_seas <- merge(CLEAR_daily_mean, CLEAR_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)

setorder(ALL_daily_seas,Date)
setorder(CLEAR_daily_seas,Date)


## anomaly
# ALL_daily_seas[   , DIR_att    := DIR_att    - DIR_att_seas    ]
# ALL_daily_seas[   , GLB_att    := GLB_att    - GLB_att_seas    ]
# ALL_daily_seas[   , DIR_transp := DIR_transp - DIR_transp_seas ]
# CLEAR_daily_seas[ , DIR_att    := DIR_att    - DIR_att_seas    ]
# CLEAR_daily_seas[ , GLB_att    := GLB_att    - GLB_att_seas    ]
# CLEAR_daily_seas[ , DIR_transp := DIR_transp - DIR_transp_seas ]

## relative anomaly
#+ echo=T, include=T
ALL_daily_seas[  , DIR_att   := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
ALL_daily_seas[  , GLB_att   := 100 * ( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
ALL_daily_seas[  , DIR_transp:= 100 * ( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
CLEAR_daily_seas[, DIR_att   := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
CLEAR_daily_seas[, GLB_att   := 100 * ( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
CLEAR_daily_seas[, DIR_transp:= 100 * ( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
#+ echo=F, include=F



####  Plot of SZA trends for all year ####
#' \newpage
#' ## Plot of SZA trends
#+ echo=F, include=F
timefactor <- 1
vars <- c("DIR_att", "GLB_att", "DIR_transp")
dbs  <- c("ALL_daily_seas", "CLEAR_daily_seas")

gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        for (anoon in unique( DB$preNoon)) {
            for (asza in unique( DB$SZA )) {

                dataset <- DB[ SZA == asza & preNoon == anoon ]

                if (sum(!is.na(dataset[[avar]])) <= 1) next()

                lm1 <- lm( dataset[[avar]] ~ dataset$Date )

                gather <- rbind(gather,
                                data.frame(
                                    linear_regression_capture(lm1),
                                    preNoon   = anoon,
                                    SZA       = asza,
                                    DATA      = DBn,
                                    var       = avar,
                                    N         = sum(!is.na(dataset[[avar]]))
                                ))

                # plot(dataset$Date, dataset[[avar]], pch  = "." )
                # abline(lm1)
                # fit <- lm1[[1]]
                # legend('top', lty = 1, bty = "n",
                #        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
                # title(paste(DBn, asza, anoon))
            }
        }
    }
}

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

##TODO pch by am/pm
##TODO col by variable

plot(szatrends$SZA,szatrends$N)

#+ szatrends, echo=F, include=T
for (type in unique(szatrends$DATA)) {
    for (avar in unique(szatrends$var)) {


        subdata <- szatrends[ szatrends$DATA == type & szatrends$var == avar , ]

        plot(subdata$SZA, subdata$slope, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope",type, avar),cex=0.9)
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$slope.sd, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope sd",type, avar),cex=0.9)
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$slope.t, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope t",type, avar),cex=0.9)
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$slope.p, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope p",type, avar),cex=0.9)
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$Rsqrd, col = subdata$preNoon, pch = 19)
        abline(h=0)
        title(paste("Slope Rsq",type, avar),cex=0.9)
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

        plot(subdata$SZA, subdata$N, col = subdata$preNoon, pch = 19)
        title(paste("N",type, avar),cex=0.9)
        legend("top",
               legend = c("Morning", "Evening"),
               col    = c(2 , 3),
               pch    = 19, ncol = 2, bty = "n")

    }
}














####  Plot of SZA trends for season of year ####
#' \newpage
#' ## Plot of SZA trends for season of year
#+ echo=F, include=F
timefactor <- 1
vars   <- c("DIR_att", "GLB_att", "DIR_transp")
dbs    <- c("ALL_daily_seas", "CLEAR_daily_seas")
season <- c("Winter", "Spring", "Summer", "Automn")





gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 2, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Automn"]


    for (ase in season) {
        for (avar in vars) {
            for (anoon in unique( DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1) next()

                    lm1 <- lm( dataset[[avar]] ~ dataset$Date )

                    gather_seas <- rbind(gather_seas,
                                    data.frame(
                                        linear_regression_capture(lm1),
                                        preNoon   = anoon,
                                        SZA       = asza,
                                        DATA      = DBn,
                                        var       = avar,
                                        Season    = ase,
                                        N         = sum(!is.na(dataset[[avar]]))
                                    ))

                    # plot(dataset$Date, dataset[[avar]], pch  = "." )
                    # abline(lm1)
                    # fit <- lm1[[1]]
                    # legend('top', lty = 1, bty = "n",
                    #        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
                    # title(paste(DBn, asza, anoon))
                }
            }
        }
    }
}

#+ echo=F, include=F
hist(gather_seas$N[gather_seas$N>50], breaks = 100)

szatrends_seas <- gather_seas
szatrends_seas$preNoon[ szatrends_seas$preNoon == T ] <- 2
szatrends_seas$preNoon[ szatrends_seas$preNoon == F ] <- 3
szatrends_seas <- data.table(szatrends_seas)

hist(szatrends_seas[DATA==dbs[1],N], breaks = 100)
hist(szatrends_seas[DATA==dbs[2],N], breaks = 100)

hist(szatrends_seas[var==vars[1],N], breaks = 100)
hist(szatrends_seas[var==vars[2],N], breaks = 100)

# szatrends_seas <- szatrends_seas[ N > 50]

plot(szatrends_seas$SZA,szatrends_seas$N)

#+ echo=F, include=T
for (ase in season) {

    for (type in unique(szatrends_seas$DATA)) {
        for (avar in unique(szatrends_seas$var)) {

            subdata <- szatrends_seas[ szatrends_seas$DATA   == type &
                                       szatrends_seas$var    == avar &
                                       szatrends_seas$Season == ase    , ]

            plot(subdata$SZA, subdata$slope, col = subdata$preNoon, pch = 19)
            abline(h=0)
            title(paste(ase, "Slope",type, avar),cex=0.9)
            legend("top",
                   legend = c("Morning", "Evening"),
                   col    = c(2 , 3),
                   pch    = 19, ncol = 2, bty = "n")

            # plot(subdata$SZA, subdata$slope.sd, col = subdata$preNoon, pch = 19)
            # abline(h=0)
            # title(paste("Slope sd",type, avar),cex=0.9)
            # legend("top",
            #        legend = c("Morning", "Evening"),
            #        col    = c(2 , 3),
            #        pch    = 19, ncol = 2, bty = "n")
            #
            # plot(subdata$SZA, subdata$slope.t, col = subdata$preNoon, pch = 19)
            # abline(h=0)
            # title(paste("Slope t",type, avar),cex=0.9)
            # legend("top",
            #        legend = c("Morning", "Evening"),
            #        col    = c(2 , 3),
            #        pch    = 19, ncol = 2, bty = "n")
            #
            # plot(subdata$SZA, subdata$slope.p, col = subdata$preNoon, pch = 19)
            # abline(h=0)
            # title(paste("Slope p",type, avar),cex=0.9)
            # legend("top",
            #        legend = c("Morning", "Evening"),
            #        col    = c(2 , 3),
            #        pch    = 19, ncol = 2, bty = "n")
            #
            # plot(subdata$SZA, subdata$Rsqrd, col = subdata$preNoon, pch = 19)
            # abline(h=0)
            # title(paste("Slope Rsq",type, avar),cex=0.9)
            # legend("top",
            #        legend = c("Morning", "Evening"),
            #        col    = c(2 , 3),
            #        pch    = 19, ncol = 2, bty = "n")
            #
            # plot(subdata$SZA, subdata$N, col = subdata$preNoon, pch = 19)
            # title(paste("N",type, avar),cex=0.9)
            # legend("top",
            #        legend = c("Morning", "Evening"),
            #        col    = c(2 , 3),
            #        pch    = 19, ncol = 2, bty = "n")

            # hist(subdata$N, breaks = 100)
        }
    }

}























#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
