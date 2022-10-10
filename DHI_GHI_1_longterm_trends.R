# /* #!/usr/bin/env Rscript */
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
#'     toc_depth:        4
#'     fig_width:        7
#'     fig_height:       4.5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ echo=F, include=T


####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "pdf"   )
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "85%"   )
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
    pdf(  file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",  basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",basename(sub("\\.R$",".lock",Script.Name))),timeout = 0)
}

par(pch = ".")


## FIXME this is for pdf output
# options(warn=-1) ## hide warnings
# options(warn=2)  ## stop on warnings

#+ echo=F, include=T
####  External code  ####
library(data.table, quietly = T, warn.conflicts = F)
library(pander,     quietly = T, warn.conflicts = F)

panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )


## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_regrassion_capture.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")

## For this project
source("~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_variables.R")
source("~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_data_input.R")



options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occured!'")
    }
})







#' ## Data selection


#+ echo=F, include=T







#' ### Data range
#' Time data span `r range(DATA$Date)`
#'






#' ## Data preparation
#' ### Move measurements to mean earth distance
DATA[ , wattDIR_1au := wattDIR * (sun_dist ^ 2) ]
DATA[ , wattGLB_1au := wattGLB * (sun_dist ^ 2) ]
DATA[ , wattHOR_1au := wattHOR * (sun_dist ^ 2) ]
#+ echo=F, include=T


# #' ### Relative to actual TSI at 1au variable representation
# DATA[ , DIR_att := wattDIR_1au / tsi_1au_comb ]
# DATA[ , GLB_att := wattGLB_1au / tsi_1au_comb ]
# DATA[ , HOR_att := wattHOR_1au / tsi_1au_comb ]

#' Using the actual values gives similar trends.

#+ echo=F, include=T

#' ### Use original variable representation
DATA[ , DIR_att := wattDIR_1au ]
DATA[ , GLB_att := wattGLB_1au ]
DATA[ , HOR_att := wattHOR_1au ]
#+ echo=F, include=T




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






#### 1. Long term anomaly trends ####

#' ## 1. Long term anomaly trends


##TODO t-test and significance

#' #### Calculate daily means ####
#+ echo=F, include=T

ALL_daily_mean <- DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                               GLB_att       = mean(GLB_att,    na.rm = T),
                               HOR_att       = mean(HOR_att,    na.rm = T),
                               DIR_transp    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                               GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                               HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                               DIR_transp_sd = sd(DIR_transp, na.rm = T),
                               doy           = yday(Date),
                               GLB_att_N     = sum(!is.na(GLB_att)),
                               HOR_att_N     = sum(!is.na(HOR_att)),
                               DIR_att_N     = sum(!is.na(DIR_att))  ),
                           by = .( Date = Day ) ]


CLEAR_daily_mean <- DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                   GLB_att       = mean(GLB_att,    na.rm = T),
                                   HOR_att       = mean(HOR_att,    na.rm = T),
                                   DIR_transp    = mean(DIR_transp, na.rm = T),
                                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                   DIR_transp_sd = sd(DIR_transp, na.rm = T),
                                   doy           = yday(Date),
                                   GLB_att_N     = sum(!is.na(GLB_att)),
                                   HOR_att_N     = sum(!is.na(HOR_att)),
                                   DIR_att_N     = sum(!is.na(DIR_att))  ),
                               by = .( Date = Day ) ]



#' #### Margin of error calculation for 0.95 confidence interval ####
#+ echo=T, include=T
CONF_INTERV <- .95
conf_param  <- 1-(1-CONF_INTERV)/2
suppressWarnings({
    ALL_daily_mean[,  DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
    ALL_daily_mean[,  HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
    ALL_daily_mean[,  GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
    ALL_daily_mean[,  DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
    CLEAR_daily_mean[,DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
    CLEAR_daily_mean[,GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
    CLEAR_daily_mean[,DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
})
#+ echo=F, include=F


#' #### Exclude means with less than `r SUM_THRES` data points
#+ echo=F, include=T
ALL_daily_mean[  DIR_att_N <= SUM_THRES, DIR_att    := NA ]
ALL_daily_mean[  GLB_att_N <= SUM_THRES, GLB_att    := NA ]
ALL_daily_mean[  HOR_att_N <= SUM_THRES, HOR_att    := NA ]
ALL_daily_mean[  DIR_att_N <= SUM_THRES, DIR_transp := NA ]
CLEAR_daily_mean[DIR_att_N <= SUM_THRES, DIR_att    := NA ]
CLEAR_daily_mean[GLB_att_N <= SUM_THRES, GLB_att    := NA ]
CLEAR_daily_mean[DIR_att_N <= SUM_THRES, DIR_transp := NA ]



#####TODO
stop("TODO")



#' #### Calculate daily seasonal values ####
#+ echo=F, include=T

ALL_daily_seas <-
    ALL_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                       GLB_att_seas    = mean(GLB_att,    na.rm = T),
                       DIR_transp_seas = mean(DIR_transp, na.rm = T),
                       DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                       GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                       GLB_att_N_seas  = sum(!is.na(GLB_att)),
                       DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                   by = .( doy ) ]

CLEAR_daily_seas <-
    CLEAR_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                         GLB_att_seas    = mean(GLB_att,    na.rm = T),
                         DIR_transp_seas = mean(DIR_transp, na.rm = T),
                         DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                         GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                         GLB_att_N_seas  = sum(!is.na(GLB_att)),
                         DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .( doy ) ]



#+ echo=F, include=F
## ~ Plots longterm  ####
plot( ALL_daily_mean$Date, ALL_daily_mean$DIR_att   , col = col_DIR_att    )
plot( ALL_daily_mean$Date, ALL_daily_mean$GLB_att   , col = col_GLB_att    )
plot( ALL_daily_mean$Date, ALL_daily_mean$DIR_transp, col = col_DIR_transp )

plot( ALL_daily_mean$doy,  ALL_daily_mean$DIR_att   , col = col_DIR_att    )
plot( ALL_daily_mean$doy,  ALL_daily_mean$GLB_att   , col = col_GLB_att    )
plot( ALL_daily_mean$doy,  ALL_daily_mean$DIR_transp, col = col_DIR_transp )

plot( ALL_daily_mean$doy,  ALL_daily_mean$GLB_att_N, col = col_DIR_att    )
plot( ALL_daily_mean$doy,  ALL_daily_mean$DIR_att_N, col = col_GLB_att    )

hist(ALL_daily_mean$DIR_att_N, col = col_DIR_att    )
hist(ALL_daily_mean$GLB_att_N, col = col_GLB_att    )

plot( CLEAR_daily_mean$Date, CLEAR_daily_mean$DIR_att   , col = col_DIR_att    )
plot( CLEAR_daily_mean$Date, CLEAR_daily_mean$GLB_att   , col = col_GLB_att    )
plot( CLEAR_daily_mean$Date, CLEAR_daily_mean$DIR_transp, col = col_DIR_transp )

plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$DIR_att   , col = col_DIR_att    )
plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$GLB_att   , col = col_GLB_att    )
plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$DIR_transp, col = col_DIR_transp )

plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$GLB_att_N, col = col_DIR_att    )
plot( CLEAR_daily_mean$doy,  CLEAR_daily_mean$DIR_att_N, col = col_GLB_att    )

hist( CLEAR_daily_mean$DIR_att_N)
hist( CLEAR_daily_mean$GLB_att_N)

## ~ Plots seasonal ####

plot( ALL_daily_seas$doy, ALL_daily_seas$DIR_att_seas   , col = col_DIR_att    )
plot( ALL_daily_seas$doy, ALL_daily_seas$GLB_att_seas   , col = col_GLB_att    )
plot( ALL_daily_seas$doy, ALL_daily_seas$DIR_transp_seas, col = col_DIR_transp )
plot( ALL_daily_seas$doy, ALL_daily_seas$DIR_att_N_seas , col = col_DIR_att    )
plot( ALL_daily_seas$doy, ALL_daily_seas$GLB_att_N_seas , col = col_GLB_att    )

plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$DIR_att_seas   , col = col_DIR_att    )
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$GLB_att_seas   , col = col_GLB_att    )
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$DIR_transp_seas, col = col_DIR_transp )
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$DIR_att_N_seas, col = col_DIR_att    )
plot( CLEAR_daily_seas$doy, CLEAR_daily_seas$GLB_att_N_seas, col = col_GLB_att    )



#' #### Calculate seasonal anomaly ####
#+ echo=F, include=F

ALL_daily_seas   <- merge(  ALL_daily_mean, ALL_daily_seas,   by = "doy", all = T)
CLEAR_daily_seas <- merge(CLEAR_daily_mean, CLEAR_daily_seas, by = "doy", all = T)

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
ALL_daily_seas[  , DIR_att   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
ALL_daily_seas[  , GLB_att   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
ALL_daily_seas[  , DIR_transp:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
CLEAR_daily_seas[, DIR_att   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
CLEAR_daily_seas[, GLB_att   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
CLEAR_daily_seas[, DIR_transp:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
#+ echo=F, include=F



#### ~ Plot of trends  ####

#' \newpage
#' ## Trends on all sky conditions data
#+ longtermtrendsALL, echo=F, include=T

gather <- data.frame()
timefactor <- 1

plot(ALL_daily_seas$Date, ALL_daily_seas$DIR_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_att )
lm1 <- lm( DIR_att ~ Date , data = ALL_daily_seas)
gather <- rbind(gather,
                data.table(
                    var  = "DIR_att",
                    data = "ALL",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
title(paste("All day Direct"),cex=0.8)


plot(ALL_daily_seas$Date, ALL_daily_seas$GLB_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_GLB_att )
lm1 <- lm( GLB_att ~ Date , data = ALL_daily_seas)
gather <- rbind(gather,
                data.table(
                    var  = "GLB_att",
                    data = "ALL",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
title(paste("All sky Global"),cex=0.8)


plot(ALL_daily_seas$Date, ALL_daily_seas$DIR_transp, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_transp )
lm1 <- lm( DIR_transp ~ Date , data = ALL_daily_seas)
gather <- rbind(gather,
                data.table(
                    var  = "DIR_transp",
                    data = "ALL",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
title(paste("All sky Direct transparency"),cex=0.8)





#' \newpage
#' ## Trends on clear sky data
#+ longtermtrendsCS, echo=F, include=T

plot(CLEAR_daily_seas$Date, CLEAR_daily_seas$DIR_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_att )
lm1 <- lm( DIR_att ~ Date , data = CLEAR_daily_seas)
gather <- rbind(gather,
                data.table(
                    var  = "DIR_att",
                    data = "CLEAR",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
title(paste("Clear Sky Direct"),cex=0.8)


plot(CLEAR_daily_seas$Date, CLEAR_daily_seas$GLB_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_GLB_att )
lm1 <- lm( GLB_att ~ Date , data = CLEAR_daily_seas)
gather <- rbind(gather,
                data.table(
                    var  = "GBL_att",
                    data = "CLEAR",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
title(paste("Clear Sky Global"), cex=0.8)


plot(CLEAR_daily_seas$Date, CLEAR_daily_seas$DIR_transp, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_transp )
lm1 <- lm( DIR_transp ~ Date , data = CLEAR_daily_mean)
gather <- rbind(gather,
                data.table(
                    var  = "DIR_transp",
                    data = "CLEAR",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
title(paste("Clear Sky Direct transparency"), cex=0.8)


wecare <- grep("intercept", names(gather), value = T, invert = T)
gather <- data.table(gather)


#' ## Table of trends
#+ echo=F, include=T
pprint <- gather[ , ..wecare]

pprint[, slope    := slope/timefactor*365 ]
pprint[, slope.sd := slope.sd/timefactor*365 ]

pander(pprint,
       cap = "Slope is in %/year")
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.dat")
#+ echo=F, include=F

## Test plots of all variables
#+ echo=F, include=F
data_list  <- list(ALL_daily_seas, CLEAR_daily_seas)
data_names <- list("All data points", "All clear sky data points")
by_var     <- "Date"
wecare     <- unique(unlist(lapply(data_list, names)))
wecare     <- grep(by_var, wecare, invert = T, value = T)
for(i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    cat(paste("\n\n## ", i, "\n\n"))
    for (var in wecare) {
        vect <- Dplot[[var]]
        hist(vect, main = var, breaks = 100)
        plot(Dplot[[by_var]], vect, pch = ".", main = var)
        # plot(Dplot$SZA, vect, pch = ".", main = var)
        # plot(Dplot$SZA, vect, pch = ".", main = var)
        # plot(cosde(Dplot$SZA), vect, pch = ".", main = var)
    }
}













#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
