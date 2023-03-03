# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisthanasis@gmail.com> */
#' ---
#' title:         "Trends of SDR in Thessaloniki "
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics,AUTH, natsisthanasis@gmail.com]
#'   - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH]
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
#'     number_sections: no
#'     fig_caption:     no
#'     keep_tex:        yes
#'     latex_engine:    xelatex
#'     toc:             yes
#'     toc_depth:       4
#'     fig_width:       7
#'     fig_height:      4.5
#'   html_document:
#'     toc:             true
#'     keep_md:         yes
#'     fig_width:       7.5
#'     fig_height:      5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ echo=F, include=T


####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = "pdf"    )
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  F       )  ## !! breaks calculations
# knitr::opts_chunk$set(fig.pos    = '!h'    )
warning("Don't use cache it breaks computations")

#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("Climatological_") })
if (!interactive()) {
    pdf( file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}

## override plot options
par(pch = ".")

#+ echo=F, include=T
library(data.table, quietly = T, warn.conflicts = F)
library(pander,     quietly = T, warn.conflicts = F)
library(lubridate,  quietly = T, warn.conflicts = F)
library(ggplot2,    quietly = T, warn.conflicts = F)

panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

## Load external functions -----------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")


## Source initial scripts ------------------------------------------------------
source("~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_variables.R")
source("~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_data_input_v2.R")

## notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occurred!'")
    }
})




#+ echo=F, include=T
#'
#' ### Data info
#'
#' Time data span `r range(ALL_1_daily_DESEAS$Date)`
#'
#' Where is a **running mean the window is `r running_mean_window_days` days** or
#' `r running_mean_window_days / Days_of_year` years.
#'
#' ## 1. Long term anomaly trends
#'
#+ echo=F, include=F


## ___ Scatter plots with SZA all data -----------------------------------------
data_list <- c(  "ALL_1_D_monthly_DESEAS",
               "CLEAR_1_D_monthly_DESEAS",
               "CLOUD_1_D_monthly_DESEAS",
                     "ALL_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS")

by_var     <- c("doy")
for (i in data_list) {
    ## get data and y vars to plot
    Dplot  <- get(i)
    wecare <- grep("HOR|GLB|DIR", names(Dplot), value = T)
    ## loop existing x vars
    for (xvar in names(Dplot)[names(Dplot) %in% by_var]){
        for (yvar in wecare) {
            col <- get(paste0(c("col", unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
            vect <- Dplot[[yvar]]
            plot(Dplot[[xvar]], vect,
                 pch  = 19, cex = .3 , col = col,
                 main = paste(i, yvar),
                 xlab = xvar, ylab = yvar)
        }
    }
}

## ___ Histograms Plots all data -----------------------------------------------
for (i in data_list) {
    ## get data and y vars to plot
    Dplot  <- get(i)
    wecare <- grep("HOR|GLB|DIR", names(Dplot), value = T)
    for (yvar in wecare) {
        if (! yvar %in% names(Dplot)) next()
        col <- get(paste0(c("col", unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
        hist(Dplot[[yvar]],
             main = paste(i, yvar),
             xlab = yvar,
             breaks = 100, col = col)
    }
}
#+ echo=F, include=F
rm(data_list)




#### Calculate seasonal anomaly ####
#'
#' ### Calculate seasonal anomaly
#'
#+ echo=F, include=F

##TODO margin of error for anomaly!!!!









#### TOTAL TRENDS  #############################################################

## gather trends
gather <- data.frame()

## ~ plot all sky trends ####
#' \newpage
#' \FloatBarrier
#'
#' ### Trends from daily means
#'
#+ longtermtrends, echo=F, include=T, results="asis"
# vars <- c("HOR_att","DIR_transp", "DIR_att", "GLB_att", "tsi1au_att")
vars <- c("DIR_att_des", "GLB_att_des")
# dbs  <- c("ALL_1_daily_DEseas")

dbs         <- c(  "ALL_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS",
                 "CLOUD_1_daily_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Trends on", translate(sub("_.*","",DBn)), "data\n\n" )

        for (avar in vars) {
            dataset <- DB

            ## linear model
            lm1 <- lm(dataset[[avar]] ~ dataset$Date)
            # lm1 <- lm( dataset[[avar]] ~ dataset$DYear )

            ## capture lm for table
            gather <- rbind(gather,
                            data.frame(
                                linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
                                DATA      = DBn,
                                var       = avar,
                                N         = sum(!is.na(dataset[[avar]]))
                            ))

            par("mar" = c(3, 4, 2, 1))


            ## plot data
            plot(dataset$Date, dataset[[avar]],
                 pch  = ".",
                 col  = get(paste0(c("col",
                                     unlist(strsplit(avar, split = "_" ))[1:2]),
                                   collapse = "_")),
                 cex  = 2,
                 xlab = "",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            ## plot fit line
            abline(lm1, lwd = 2)

            ## plot running mean
            # partial window using adaptive rolling function
            # an = function(n, len) c(seq.int(n), rep(n, len-n))
            # n = an(round(Days_of_year * 5), nrow(dataset))
            # rm <- frollmean(dataset[[avar]], n, adaptive=TRUE,
            #           na.rm = TRUE, algo = "exact")

            rm <- frollmean(dataset[[avar]], round(running_mean_window_days),
                            na.rm = TRUE, algo = "exact", align = "center")

            points(dataset$Date, rm, col = "red", cex = 0.5)

            ## decorations
            fit <- lm1[[1]]
            legend('top', lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2])*Days_of_year,3),'* year'))
            title(paste(translate(sub("_.*","",DBn)), translate(avar)), cex.main = 0.8)
    }
}
#+ echo=F, include=F







## ~ display trends table ####
#' \newpage
#'
#' #### Table of total trends.
#'
#+ echo=F, include=T

wecare      <- grep("intercept", names(gather), value = T, invert = T)
gather      <- data.table(gather)
gather$DATA <- sub("_.*","",gather$DATA)

pprint <- gather[ , ..wecare]

pprint[, slope.stat_sig := 100*(1-slope.p) ]
pprint[, slope.t        := NULL]
pprint[, Rsqrd          := NULL]
pprint[, RsqrdAdj       := NULL]
pprint[, N              := NULL]

## convert slope / year
pprint[,slope              := slope              * Days_of_year ]
pprint[,slope.sd           := slope.sd           * Days_of_year ]
pprint[,slope.ConfInt_0.95 := slope.ConfInt_0.95 * Days_of_year ]
pprint[,slope.ConfInt_0.99 := slope.ConfInt_0.99 * Days_of_year ]
setorder(pprint,var)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F
myRtools::write_dat(pprint,
                    "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.dat")





#### SEASONAL TRENDS  ##########################################################


#' \newpage
#' \FloatBarrier
#'
#' ### Trends for each season of the year
#'
#' We calculated monthly means from the daily means
#'
#+ echo=F, include=F


## ~ plot trends for each season #####

#+ seasonaltrends, echo=F, include=T, results="asis"
# vars        <- c("DIR_att", "GLB_att")
vars        <- c("GLB_att_des")

## Daily aggregation
# dbs         <- c(  "ALL_1_daily_DEseas",
#                  "CLEAR_1_daily_DEseas",
#                  "CLOUD_1_daily_DEseas")

## Monthly aggregation
dbs         <- c(  "ALL_1_D_monthly_DESEAS",
                 "CLEAR_1_D_monthly_DESEAS",
                 "CLOUD_1_D_monthly_DESEAS")

Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Autumn"]
    ## sanity check
    stopifnot( !any(is.na(DB$Season)) )

    cat("\n\\newpage\n")
    cat("\n#### ", translate(sub("_.*","",DBn)), "\n\n" )

    for (ase in Seasons) {
        for (avar in vars) {
            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()
            ## linear model counting days
            # lm1        <- lm( dataset[[avar]] ~ dataset$Date )
            ## linear model counting years
            lm2        <- lm( dataset[[avar]] ~ dataset$Year )

            ## plot
            par("mar" = c(3, 4, 2, 1))
            plot(dataset$Year, dataset[[avar]],
                 pch  = dataset$Month,
                 col = get(paste0("col_",avar)),
                 cex  = .6,
                 xlab = "",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
                 # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            # abline(lm1)
            abline(lm2)

            ## plot running mean
            rm <- frollmean(dataset[[avar]], 3 * running_mean_window_years,
                            na.rm = TRUE, algo = "exact", align = "center")

            # points(dataset$Date, rm, col = "red", cex = 0.5)
            lines(dataset$Year, rm, col = "red", cex = 0.5)


            ## decorations
            # fit <- lm1[[1]]
            # legend('top', lty = 1, bty = "n",
            #        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*(Days_of_year/4) ),3),'* year'))
            fit <- lm2[[1]]
            legend('top', lty = 1, bty = "n",
                   paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]),3),'* year'))
            title(paste(ase, translate(sub("_.*","",DBn)), translate(avar)), cex.main = 0.8)

        }
    }
}
#+ echo=F, include=F


## ~ calculate trends for each season  ####
vars        <- c("DIR_att", "GLB_att")

## is set above
# dbs         <- c("ALL_1_daily_DEseas",
#                  "CLEAR_1_daily_DEseas",
#                  "CLOUD_1_daily_DEseas")

Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Autumn"]
    ## sanity check
    stopifnot( !any(is.na(DB$Season)) )
    for (ase in Seasons) {
        for (avar in vars) {
            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()
            ## linear model counting days
            # lm1        <- lm( dataset[[avar]] ~ dataset$Date )
            ## linear model counting years
            lm2        <- lm( dataset[[avar]] ~ dataset$Year )

            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm2, confidence_interval = Daily_confidence_limit),
                                     DATA      = DBn,
                                     Season    = ase,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]]))
                                 ))
        }
    }
}

## ~ display data table ####

#' \newpage
#' \FloatBarrier
#'
#' #### Table of trends by season.
#+ echo=F, include=T


wecare           <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas      <- data.table(gather_seas)
gather_seas$DATA <- sub("_.*", "", gather_seas$DATA)

pprint           <- gather_seas[ , ..wecare]

pprint[, slope.stat_sig := 100*(1-slope.p) ]
pprint[, slope.t        := NULL]
pprint[, Rsqrd          := NULL]
pprint[, RsqrdAdj       := NULL]
pprint[, N              := NULL]


## convert slope / year
# print[, slope              := slope              * Days_of_year/4 ]
# pprint[, slope.sd           := slope.sd           * Days_of_year/4 ]
# pprint[, slope.ConfInt_0.95 := slope.ConfInt_0.95 * Days_of_year/4 ]
# pprint[, slope.ConfInt_0.99 := slope.ConfInt_0.99 * Days_of_year/4 ]

pprint[, slope.ConfInt_0.99 := NULL ]
pprint[, slope.ConfInt_0.95 := NULL ]

setorder(pprint,DATA,var)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends_season.dat")






#### MONTHLY TRENDS  ###########################################################


#' \newpage
#' \FloatBarrier
#'
#' ### Trends for each month of the year
#'
#' We calculated monthly means from the daily means
#'
#+ echo=F, include=F


## ~ plot trends for each month #####

#+ monthlytrends, echo=F, include=T, results="asis"
# vars        <- c("DIR_att", "GLB_att")
vars        <- c("GLB_att")

## Daily aggregation
# dbs         <- c(  "ALL_1_daily_DEseas",
#                  "CLEAR_1_daily_DEseas",
#                  "CLOUD_1_daily_DEseas")

## Monthly aggregation
dbs         <- c(  "ALL_3_monthly_DEseas",
                   "CLEAR_3_monthly_DEseas",
                   "CLOUD_3_monthly_DEseas")

for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[, Month := month(Date) ]
    ## sanity check
    stopifnot( !any(is.na(DB$Month)) )

    cat("\n\\newpage\n")
    cat("\n#### ", translate(sub("_.*","",DBn)), "\n\n" )


    for (ase in 1:12) {
        for (avar in vars) {
            dataset <- DB[ Month == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()
            ## linear model counting days
            # lm1        <- lm( dataset[[avar]] ~ dataset$Date )

            ## linear model counting years
            lm2        <- lm( dataset[[avar]] ~ dataset$Year )

            ## plot
            par("mar" = c(3, 4, 2, 1))
            plot(dataset$Year, dataset[[avar]],
                 pch  = ".", col = get(paste0("col_",avar)),
                 cex  = 4,
                 xlab = "",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            # abline(lm1)
            abline(lm2)

            ## plot running mean
            rm <- frollmean(dataset[[avar]], round(running_mean_window_days),
                            na.rm = TRUE, algo = "exact", align = "center")

            points(dataset$Date, rm, col = "red", cex = 0.5)

            ## decorations
            # fit <- lm1[[1]]
            # legend('top', lty = 1, bty = "n",
            #        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*(Days_of_year/12) ),3),'* year'))
            fit <- lm2[[1]]

            legend('top', lty = 1, bty = "n",
                   paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]),3),'* year'))

            title(paste(month.name[ase], translate(sub("_.*","",DBn)), translate(avar)), cex.main = 0.8)

        }
    }
}
#+ echo=F, include=F


## ~ calculate trends for each month  ####
vars        <- c("DIR_att", "GLB_att")
## is set above
# dbs         <- c("ALL_1_daily_DEseas",
#                  "CLEAR_1_daily_DEseas",
#                  "CLOUD_1_daily_DEseas")
gather_seas <- data.frame()
for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[, Month := month(Date) ]
    ## sanity check
    stopifnot( !any(is.na(DB$Month)) )
    for (ase in 1:12) {
        for (avar in vars) {
            dataset <- DB[ Month == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()
            ## linear model countint days
            lm1        <- lm( dataset[[avar]] ~ dataset$Date )
            ## linear model counting years
            lm2        <- lm( dataset[[avar]] ~ dataset$Year )


            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm2, confidence_interval = Daily_confidence_limit),
                                     DATA      = DBn,
                                     Month     = ase,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]]))
                                 ))
        }
    }
}

## ~ display data table ####

#'
#' \FloatBarrier
#'
#' #### Table of trends by month.
#+ echo=F, include=T


wecare           <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas      <- data.table(gather_seas)
gather_seas$DATA <- sub("_.*", "", gather_seas$DATA)

pprint           <- gather_seas[ , ..wecare]

pprint[, slope.stat_sig := 100*(1-slope.p) ]
pprint[, slope.t        := NULL]
pprint[, Rsqrd          := NULL]
pprint[, RsqrdAdj       := NULL]


## convert slope / year
# pprint[, slope              := slope              * Days_of_year / 12 ]
# pprint[, slope.sd           := slope.sd           * Days_of_year / 12 ]
# pprint[, slope.ConfInt_0.95 := slope.ConfInt_0.95 * Days_of_year / 12 ]
# pprint[, slope.ConfInt_0.99 := slope.ConfInt_0.99 * Days_of_year / 12 ]

pprint[, slope.ConfInt_0.99 := NULL ]
pprint[, slope.ConfInt_0.95 := NULL ]

setorder(pprint, DATA, var, Month)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends_monthly.dat")








#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
