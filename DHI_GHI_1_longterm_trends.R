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

## move to data_input for all three
# rm(DATA_Clear)
# rm(DATA_all)


options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occured!'")
    }
})



#+ echo=F, include=T
#' ### Data range
#' Time data span `r range(ALL_1_daily_mean$Date)`
#'
#'
#' ## 1. Long term anomaly trends
#'
#'


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
wecare     <- grep("HOR|GLB|DIR", wecare, value = T)
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

##TODO margin of error for anomaly!!!!

ALL_1_daily_DEseas   <- merge(  ALL_1_daily_mean,   ALL_1_daily_seas, by = "doy", all = T)
CLEAR_1_daily_DEseas <- merge(CLEAR_1_daily_mean, CLEAR_1_daily_seas, by = "doy", all = T)

setorder(ALL_1_daily_DEseas,Date)
setorder(CLEAR_1_daily_DEseas,Date)


## anomaly
# #' #### Use the actuar difference from seasonal
# ALL_1_daily_DEseas[   , DIR_att    := DIR_att    - DIR_att_seas    ]
# ALL_1_daily_DEseas[   , GLB_att    := GLB_att    - GLB_att_seas    ]
# ALL_1_daily_DEseas[   , DIR_transp := DIR_transp - DIR_transp_seas ]
# CLEAR_1_daily_DEseas[ , DIR_att    := DIR_att    - DIR_att_seas    ]
# CLEAR_1_daily_DEseas[ , GLB_att    := GLB_att    - GLB_att_seas    ]
# CLEAR_1_daily_DEseas[ , DIR_transp := DIR_transp - DIR_transp_seas ]


##TODO margin of error for anomaly!!!!

## relative anomaly
#' #### Use the % difference from seasonal values
#+ echo=F, include=T
ALL_1_daily_DEseas[  , DIR_att   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
ALL_1_daily_DEseas[  , HOR_att   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas    ]
ALL_1_daily_DEseas[  , GLB_att   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
ALL_1_daily_DEseas[  , DIR_transp:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
CLEAR_1_daily_DEseas[, DIR_att   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
CLEAR_1_daily_DEseas[, HOR_att   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas    ]
CLEAR_1_daily_DEseas[, GLB_att   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
CLEAR_1_daily_DEseas[, DIR_transp:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
#+ echo=F, include=F



#### ~ Plot of trends  ####

#' \newpage
#' ## Trends on all sky conditions data
#+ longtermtrendsALL, echo=F, include=T

gather <- data.frame()


plot(ALL_1_daily_DEseas$Date, ALL_1_daily_DEseas$DIR_att, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_DIR_att )
lm1 <- lm( DIR_att ~ Date , data = ALL_1_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("All day Direct"),cex=0.8)


plot(ALL_1_daily_DEseas$Date, ALL_1_daily_DEseas$HOR_att, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_HOR_att )
lm1 <- lm( HOR_att ~ Date , data = ALL_1_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("All day Direct HOR"),cex=0.8)



plot(ALL_1_daily_DEseas$Date, ALL_1_daily_DEseas$DIR_transp, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_DIR_transp )
lm1 <- lm( DIR_transp ~ Date , data = ALL_1_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("All sky Direct transparency"),cex=0.8)




plot(ALL_1_daily_DEseas$Date, ALL_1_daily_DEseas$GLB_att, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_GLB_att )
lm1 <- lm( GLB_att ~ Date , data = ALL_1_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("All sky Global"),cex=0.8)




#' \newpage
#' ## Trends on clear sky data
#+ longtermtrendsCS, echo=F, include=T

plot(CLEAR_1_daily_DEseas$Date, CLEAR_1_daily_DEseas$DIR_att, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_DIR_att )
lm1 <- lm( DIR_att ~ Date , data = CLEAR_1_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("Clear Sky Direct"),cex=0.8)



plot(CLEAR_1_daily_DEseas$Date, CLEAR_1_daily_DEseas$HOR_att, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_HOR_att )
lm1 <- lm( HOR_att ~ Date , data = CLEAR_1_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("Clear Sky Direct HOR"),cex=0.8)


plot(CLEAR_1_daily_DEseas$Date, CLEAR_1_daily_DEseas$DIR_transp, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_DIR_transp )
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("Clear Sky Direct transparency"), cex=0.8)




plot(CLEAR_1_daily_DEseas$Date, CLEAR_1_daily_DEseas$GLB_att, pch  = ".", xlab = "", ylab = "Deseasonalized anomaly [%]", col = col_GLB_att )
lm1 <- lm( GLB_att ~ Date , data = CLEAR_1_daily_DEseas)
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
       paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
title(paste("Clear Sky Global"), cex=0.8)















wecare <- grep("intercept", names(gather), value = T, invert = T)
gather <- data.table(gather)





#' ## Table of trends
#+ echo=F, include=T
pprint <- gather[ , ..wecare]

## convert slope / year
pprint[, slope    := slope    * Days_of_year ]
pprint[, slope.sd := slope.sd * Days_of_year ]

setorder(pprint,data,var)

pander(pprint,
       cap = "Slope is in %/year")
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.dat")
#+ echo=F, include=F






## break by season !!




####  Plot of SZA trends for each season of year ####
#' \newpage
#' ## Plot of SZA trends for each season of year
#+ echo=F, include=F
timefactor  <- 1  ## to display % per year
vars        <- c("DIR_att", "GLB_att")
dbs         <- c("ALL_1_daily_DEseas", "CLEAR_1_daily_DEseas")
season      <- c("Winter", "Spring", "Summer", "Automn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Automn"]



    stopifnot( !any(is.na(DB$Season)) )

    for (ase in season) {
        for (avar in vars) {

            dataset <- DB[ season == ase ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            lm1 <- lm( dataset[[avar]] ~ dataset$Date )

            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_regression_capture(lm1),
                                     preNoon   = anoon,
                                     DATA      = DBn,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]]))
                                 ))

        }
    }

}















#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
