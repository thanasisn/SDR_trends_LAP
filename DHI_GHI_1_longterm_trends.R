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
source("~/CODE/FUNCTIONS/R/data.R")


## For this project
source("~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_variables.R")
source("~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_data_input.R")



options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occured!'")
    }
})





#+ echo=F, include=T

#' ### Data range
#' Time data span `r range(DATA_all$Date)`
#'


#### 1. Long term anomaly trends ####

#' ## 1. Long term anomaly trends

#' #### Calculate daily means ####
#+ echo=F, include=T

ALL_1_daily_mean <- DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
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


CLEAR_1_daily_mean <- DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
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
#+ echo=F, include=T
conf_param  <- 1-(1-Daily_confidence_limit)/2
suppressWarnings({
    ALL_1_daily_mean[,  DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
    ALL_1_daily_mean[,  HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
    ALL_1_daily_mean[,  GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
    ALL_1_daily_mean[,  DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
    CLEAR_1_daily_mean[,DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
    CLEAR_1_daily_mean[,HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
    CLEAR_1_daily_mean[,GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
    CLEAR_1_daily_mean[,DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
})
#+ echo=F, include=F


#' #### Exclude means with less than `r Daily_aggregation_N_lim` data points
#+ echo=F, include=T
ALL_1_daily_mean[  DIR_att_N <= Daily_aggregation_N_lim, DIR_att    := NA ]
ALL_1_daily_mean[  GLB_att_N <= Daily_aggregation_N_lim, GLB_att    := NA ]
ALL_1_daily_mean[  HOR_att_N <= Daily_aggregation_N_lim, HOR_att    := NA ]
ALL_1_daily_mean[  DIR_att_N <= Daily_aggregation_N_lim, DIR_transp := NA ]
CLEAR_1_daily_mean[DIR_att_N <= Daily_aggregation_N_lim, DIR_att    := NA ]
CLEAR_1_daily_mean[DIR_att_N <= Daily_aggregation_N_lim, HOR_att    := NA ]
CLEAR_1_daily_mean[GLB_att_N <= Daily_aggregation_N_lim, GLB_att    := NA ]
CLEAR_1_daily_mean[DIR_att_N <= Daily_aggregation_N_lim, DIR_transp := NA ]



#' #### Calculate daily seasonal values ####
#+ echo=F, include=T

ALL_1_daily_seas <-
    ALL_1_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                       GLB_att_seas    = mean(GLB_att,    na.rm = T),
                       HOR_att_seas    = mean(HOR_att,    na.rm = T),
                       DIR_transp_seas = mean(DIR_transp, na.rm = T),
                       DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                       GLB_att_N_seas  = sum(!is.na(GLB_att)),
                       HOR_att_N_seas  = sum(!is.na(HOR_att)),
                       DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                   by = .( doy ) ]

CLEAR_1_daily_seas <-
    CLEAR_1_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                         GLB_att_seas    = mean(GLB_att,    na.rm = T),
                         HOR_att_seas    = mean(HOR_att,    na.rm = T),
                         DIR_transp_seas = mean(DIR_transp, na.rm = T),
                         DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                         HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                         GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                         GLB_att_N_seas  = sum(!is.na(GLB_att)),
                         HOR_att_N_seas  = sum(!is.na(HOR_att)),
                         DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .( doy ) ]



#+ echo=F, include=F
## ~ Plots longterm  ####

data_list  <- list(ALL   = ALL_1_daily_mean,
                   CLEAR = CLEAR_1_daily_mean)
data_names <- c(
    "GLB_att",
    "DIR_att",
    "DIR_transp",
    "HOR_att",
    "GLB_att_N",
    "DIR_att_N"
)
by_var     <- c("Date","doy")
wecare     <- unique(unlist(lapply(data_list, names)))
wecare     <- grep(paste0(by_var,collapse = "|"), wecare, invert = T, value = T)
for(i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    for (xvar in by_var){
        for (yvar in wecare) {
            col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
            vect <- Dplot[[yvar]]
            plot(Dplot[[xvar]], vect,
                 pch = ".", col = col,
                 main = paste(names(data_list[i]), yvar),
                 xlab = xvar, ylab = yvar)
        }
    }
}
for(i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
        for (yvar in wecare) {
            col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
            vect <- Dplot[[yvar]]
            hist(vect,
                 main = paste(names(data_list[i]), yvar),
                 breaks = 100, col = col)
    }
}

## ~ Plots seasonal ####
data_list  <- list(ALL_Seas   = ALL_1_daily_seas,
                   CLEAR_Seas = CLEAR_1_daily_seas)
data_names <- c(
    "GLB_att",
    "DIR_att",
    "DIR_transp",
    "HOR_att",
    "GLB_att_N",
    "DIR_att_N"
)
by_var     <- c("doy")
wecare     <- unique(unlist(lapply(data_list, names)))
wecare     <- grep(paste0(by_var,collapse = "|"), wecare, invert = T, value = T)
for(i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    for (xvar in by_var){
        for (yvar in wecare) {
            col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
            vect <- Dplot[[yvar]]
            plot(Dplot[[xvar]], vect,
                 pch = ".", col = col,
                 main = paste(names(data_list[i]), yvar),
                 xlab = xvar, ylab = yvar)
        }
    }
}
for(i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    for (yvar in wecare) {
        col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
        vect <- Dplot[[yvar]]
        hist(vect,
             main = paste(names(data_list[i]), yvar),
             breaks = 100, col = col)
    }
}
rm(data_list)



#' #### Calculate seasonal anomaly ####
#+ echo=F, include=F

ALL_daily_DEseas   <- merge(  ALL_1_daily_mean, ALL_1_daily_seas,   by = "doy", all = T)
CLEAR_daily_DEseas <- merge(CLEAR_1_daily_mean, CLEAR_1_daily_seas, by = "doy", all = T)

setorder(ALL_daily_DEseas,Date)
setorder(CLEAR_daily_DEseas,Date)


## anomaly
# ALL_daily_DEseas[   , DIR_att    := DIR_att    - DIR_att_seas    ]
# ALL_daily_DEseas[   , GLB_att    := GLB_att    - GLB_att_seas    ]
# ALL_daily_DEseas[   , DIR_transp := DIR_transp - DIR_transp_seas ]
# CLEAR_daily_DEseas[ , DIR_att    := DIR_att    - DIR_att_seas    ]
# CLEAR_daily_DEseas[ , GLB_att    := GLB_att    - GLB_att_seas    ]
# CLEAR_daily_DEseas[ , DIR_transp := DIR_transp - DIR_transp_seas ]


## relative anomaly
#+ echo=T, include=T
ALL_daily_DEseas[  , DIR_att   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
ALL_daily_DEseas[  , HOR_att   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas    ]
ALL_daily_DEseas[  , GLB_att   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
ALL_daily_DEseas[  , DIR_transp:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
CLEAR_daily_DEseas[, DIR_att   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
CLEAR_daily_DEseas[, HOR_att   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas    ]
CLEAR_daily_DEseas[, GLB_att   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
CLEAR_daily_DEseas[, DIR_transp:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
#+ echo=F, include=F



#### ~ Plot of trends  ####

#' \newpage
#' ## Trends on all sky conditions data
#+ longtermtrendsALL, echo=F, include=T

gather <- data.frame()


plot(ALL_daily_DEseas$Date, ALL_daily_DEseas$DIR_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_att )
lm1 <- lm( DIR_att ~ Date , data = ALL_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("All day Direct"),cex=0.8)


plot(ALL_daily_DEseas$Date, ALL_daily_DEseas$HOR_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_HOR_att )
lm1 <- lm( HOR_att ~ Date , data = ALL_daily_DEseas)
gather <- rbind(gather,
                data.table(
                    var  = "HOR_att",
                    data = "ALL",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("All day Direct HOR"),cex=0.8)




plot(ALL_daily_DEseas$Date, ALL_daily_DEseas$GLB_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_GLB_att )
lm1 <- lm( GLB_att ~ Date , data = ALL_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("All sky Global"),cex=0.8)


plot(ALL_daily_DEseas$Date, ALL_daily_DEseas$DIR_transp, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_transp )
lm1 <- lm( DIR_transp ~ Date , data = ALL_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("All sky Direct transparency"),cex=0.8)





#' \newpage
#' ## Trends on clear sky data
#+ longtermtrendsCS, echo=F, include=T

plot(CLEAR_daily_DEseas$Date, CLEAR_daily_DEseas$DIR_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_att )
lm1 <- lm( DIR_att ~ Date , data = CLEAR_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("Clear Sky Direct"),cex=0.8)



plot(CLEAR_daily_DEseas$Date, CLEAR_daily_DEseas$HOR_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_HOR_att )
lm1 <- lm( HOR_att ~ Date , data = CLEAR_daily_DEseas)
gather <- rbind(gather,
                data.table(
                    var  = "HOR_att",
                    data = "CLEAR",
                    linear_regression_capture(lm1)
                )
)
abline(lm1)
fit <- lm1[[1]]
legend('top', lty = 1, bty = "n",
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("Clear Sky Direct HOR"),cex=0.8)



plot(CLEAR_daily_DEseas$Date, CLEAR_daily_DEseas$GLB_att, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_GLB_att )
lm1 <- lm( GLB_att ~ Date , data = CLEAR_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("Clear Sky Global"), cex=0.8)


plot(CLEAR_daily_DEseas$Date, CLEAR_daily_DEseas$DIR_transp, pch  = ".", xlab = "", ylab = "Seasonal Delta [%]", col = col_DIR_transp )
lm1 <- lm( DIR_transp ~ Date , data = CLEAR_1_daily_mean)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/Days_of_year),3),'* year'))
title(paste("Clear Sky Direct transparency"), cex=0.8)


wecare <- grep("intercept", names(gather), value = T, invert = T)
gather <- data.table(gather)


#' ## Table of trends
#+ echo=F, include=T
pprint <- gather[ , ..wecare]

## convert slope / year
pprint[, slope    := slope    / Days_of_year ]
pprint[, slope.sd := slope.sd / Days_of_year ]

pander(pprint,
       cap = "Slope is in %/year")
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.dat")
#+ echo=F, include=F



# ## Test plots of all variables
# #+ echo=F, include=F
# data_list  <- list(ALL_daily_DEseas, CLEAR_daily_DEseas)
# data_names <- list("All data points", "All clear sky data points")
# by_var     <- "Date"
# wecare     <- unique(unlist(lapply(data_list, names)))
# wecare     <- grep(by_var, wecare, invert = T, value = T)
# for(i in 1:length(data_list)) {
#     Dplot <- data_list[[i]]
#     cat(paste("\n\n## ", i, "\n\n"))
#     for (var in wecare) {
#         vect <- Dplot[[var]]
#         hist(vect, main = var, breaks = 100)
#         plot(Dplot[[by_var]], vect, pch = ".", main = var)
#         # plot(Dplot$SZA, vect, pch = ".", main = var)
#         # plot(Dplot$SZA, vect, pch = ".", main = var)
#         # plot(cosde(Dplot$SZA), vect, pch = ".", main = var)
#     }
# }



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
