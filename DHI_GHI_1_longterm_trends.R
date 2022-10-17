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
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
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
## ~ Plots all data  ####

data_list  <- list(ALL   = ALL_1_daily_mean,
                   CLEAR = CLEAR_1_daily_mean)
by_var     <- c("Date","doy")
wecare     <- unique(unlist(lapply(data_list, names)))
wecare     <- grep(paste0(by_var,collapse = "|"), wecare, invert = T, value = T)
for(i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    for (xvar in by_var){
        for (yvar in wecare) {
            if (! yvar %in% names(Dplot)) next()
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
      # intersect(names(Dplot),wecare)
        for (yvar in wecare) {
            if (! yvar %in% names(Dplot)) next()
            col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
            vect <- Dplot[[yvar]]
            hist(vect,
                 main = paste(names(data_list[i]), yvar),
                 breaks = 100, col = col)
    }
}

#+ echo=F, include=F
## ~ Plots seasonal data ####
data_list  <- list(ALL_Seas   =   ALL_1_daily_seas,
                   CLEAR_Seas = CLEAR_1_daily_seas)
by_var     <- c("doy")
wecare     <- unique(unlist(lapply(data_list, names)))
wecare     <- grep("HOR|GLB|DIR", wecare, value = T)
for(i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    for (xvar in by_var){
        for (yvar in wecare) {
            if (! yvar %in% names(Dplot)) next()
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
        if (! yvar %in% names(Dplot)) next()
        col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
        vect <- Dplot[[yvar]]
        hist(vect,
             main = paste(names(data_list[i]), yvar),
             breaks = 100, col = col)
    }
}
rm(data_list)




#### Calculate seasonal anomaly ####
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
## add tsi data to data
ALL_1_daily_mean[ , tsi1au_att := 100*(tsi1au_att - mean(tsi1au_att)) / mean(tsi1au_att)  ]
ALL_1_daily_mean <-
    merge( ALL_1_daily_DEseas,
           ALL_1_daily_mean[, .(Date, tsi1au_att)], by = "Date", all = T )
CLEAR_1_daily_DEseas[, DIR_att   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
CLEAR_1_daily_DEseas[, HOR_att   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas    ]
CLEAR_1_daily_DEseas[, GLB_att   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
CLEAR_1_daily_DEseas[, DIR_transp:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
#+ echo=F, include=F




#### TOTAL TRENDS  #############################################################


## ~ plot all sky trends ####

#' \newpage
#' ## Trends on all sky conditions data
#+ longtermtrendsALL, echo=F, include=T, results="asis"
vars        <- c("HOR_att","DIR_transp","DIR_att","GLB_att","tsi1au_att")
dbs         <- c("ALL_1_daily_DEseas")
for (DBn in dbs) {
    DB <- get(DBn)
        for (avar in vars) {
            dataset <- DB

            ## linear model
            lm1        <- lm( dataset[[avar]] ~ dataset$Date )

            ## plot
            plot(dataset$Date, dataset[[avar]],
                 pch  = ".", col = get(paste0("col_",avar)),
                 xlab = "",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )
            abline(lm1)

            ## decorations
            fit <- lm1[[1]]
            legend('top', lty = 1, bty = "n",
                   paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
            title(paste(translate(sub("_.*","",DBn)),translate(avar)), cex=0.7)
    }
}
#+ echo=F, include=F


## ~ plot clear sky trends ####

#' \newpage
#' ## Trends on Clear sky conditions data
#+ longtermtrendsCS, echo=F, include=T, results="asis"
vars        <- c("HOR_att","DIR_transp","DIR_att","GLB_att")
dbs         <- c("CLEAR_1_daily_DEseas")
for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        dataset <- DB

        ## linear model
        lm1        <- lm( dataset[[avar]] ~ dataset$Date )

        ## plot
        plot(dataset$Date, dataset[[avar]],
             pch  = ".", col = get(paste0("col_",avar)),
             xlab = "",
             ylab = bquote("Seasonal Anomaly [%]" ) )
             # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )
        abline(lm1)

        ## decorations
        fit <- lm1[[1]]
        legend('top', lty = 1, bty = "n",
               paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
        title(paste(translate(sub("_.*","",DBn)),translate(avar)), cex=0.7)
    }
}
#+ echo=F, include=F




## ~ calculate trends  ####
vars   <- c("HOR_att","DIR_transp","DIR_att","GLB_att","tsi1au_att")
dbs    <- c("ALL_1_daily_DEseas", "CLEAR_1_daily_DEseas")
gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        dataset <- DB
        if (! avar %in% names(dataset)) next()
        ## linear model
        lm1        <- lm( dataset[[avar]] ~ dataset$Date )
        ## gather stats
        gather <- rbind(gather,
                        data.frame(
                            linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
                            DATA      = DBn,
                            var       = avar,
                            N         = sum(!is.na(dataset[[avar]]))
                        ))
    }
}



## ~ display table ####

#' ## Table of total trends.
#+ echo=F, include=T

wecare <- grep("intercept", names(gather), value = T, invert = T)
gather <- data.table(gather)
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
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.dat")





#### SEASONAL TRENDS  ##########################################################


####  Plot of trends for each season of year ####
#' \newpage
#' ## Trends for each season of the year
#+ echo=F, include=F


## ~ plot trends for each season #####

#+ seasonaltrends, echo=F, include=T, results="asis"
vars        <- c("DIR_att", "GLB_att")
dbs         <- c("ALL_1_daily_DEseas","CLEAR_1_daily_DEseas")
Seasons     <- c("Winter", "Spring","Summer","Automn")
for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Automn"]
    ## sanity check
    stopifnot( !any(is.na(DB$Season)) )
    for (ase in Seasons) {
        for (avar in vars) {
            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()
            ## linear model
            lm1        <- lm( dataset[[avar]] ~ dataset$Date )
            ## plot
            plot(dataset$Date, dataset[[avar]],
                 pch  = ".", col = get(paste0("col_",avar)),
                 xlab = "",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
                 # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )
            abline(lm1)
            ## decorations
            fit <- lm1[[1]]
            legend('top', lty = 1, bty = "n",
                   paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
            title(paste(ase,translate(sub("_.*","",DBn)),translate(avar)), cex=0.7)
        }
    }
}
#+ echo=F, include=F


## ~ calculate trends for each season  ####
vars        <- c("DIR_att", "GLB_att")
dbs         <- c("ALL_1_daily_DEseas", "CLEAR_1_daily_DEseas")
Seasons     <- c("Winter", "Spring", "Summer", "Automn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Automn"]
    ## sanity check
    stopifnot( !any(is.na(DB$Season)) )
    for (ase in Seasons) {
        for (avar in vars) {
            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()
            ## linear model
            lm1        <- lm( dataset[[avar]] ~ dataset$Date )
            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
                                     DATA      = DBn,
                                     Season    = ase,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]]))
                                 ))
        }
    }
}

## ~ display data table ####

#' ## Table of trends by season.
#+ echo=F, include=T


wecare           <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas      <- data.table(gather_seas)
gather_seas$DATA <- sub("_.*","",gather_seas$DATA)

pprint           <- gather_seas[ , ..wecare]

pprint[, slope.stat_sig := 100*(1-slope.p) ]
pprint[, slope.t        := NULL]
pprint[, Rsqrd          := NULL]
pprint[, RsqrdAdj       := NULL]
pprint[, N              := NULL]


## convert slope / year
pprint[, slope              := slope              * Days_of_year ]
pprint[, slope.sd           := slope.sd           * Days_of_year ]
pprint[, slope.ConfInt_0.95 := slope.ConfInt_0.95 * Days_of_year ]
pprint[, slope.ConfInt_0.99 := slope.ConfInt_0.99 * Days_of_year ]

pprint[, slope.ConfInt_0.99 := NULL ]
pprint[, slope.ConfInt_0.95 := NULL ]

setorder(pprint,DATA,var)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends_season.dat")






#### MONTHLY TRENDS  ###########################################################


####  Plot of trends for each month of year ####
#' \newpage
#' ## Trends for each month of the year
#+ echo=F, include=F


## ~ plot trends for each month #####

#+ monthlytrends, echo=F, include=T, results="asis"
vars        <- c("DIR_att", "GLB_att")
dbs         <- c("ALL_1_daily_DEseas","CLEAR_1_daily_DEseas")
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
            ## linear model
            lm1        <- lm( dataset[[avar]] ~ dataset$Date )
            ## plot
            plot(dataset$Date, dataset[[avar]],
                 pch  = ".", col = get(paste0("col_",avar)),
                 xlab = "",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )
            abline(lm1)
            ## decorations
            fit <- lm1[[1]]
            legend('top', lty = 1, bty = "n",
                   paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]*Days_of_year),3),'* year'))
            title(paste(month.name[ase],translate(sub("_.*","",DBn)),translate(avar)), cex=0.7)
        }
    }
}
#+ echo=F, include=F


## ~ calculate trends for each month  ####
vars        <- c("DIR_att", "GLB_att")
dbs         <- c("ALL_1_daily_DEseas", "CLEAR_1_daily_DEseas")
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
            ## linear model
            lm1        <- lm( dataset[[avar]] ~ dataset$Date )
            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
                                     DATA      = DBn,
                                     Month     = ase,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]]))
                                 ))
        }
    }
}

## ~ display data table ####

#' ## Table of trends by month.
#+ echo=F, include=T


wecare           <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas      <- data.table(gather_seas)
gather_seas$DATA <- sub("_.*","",gather_seas$DATA)

pprint           <- gather_seas[ , ..wecare]

pprint[, slope.stat_sig := 100*(1-slope.p) ]
pprint[, slope.t        := NULL]
pprint[, Rsqrd          := NULL]
pprint[, RsqrdAdj       := NULL]


## convert slope / year
pprint[, slope              := slope              * Days_of_year ]
pprint[, slope.sd           := slope.sd           * Days_of_year ]
pprint[, slope.ConfInt_0.95 := slope.ConfInt_0.95 * Days_of_year ]
pprint[, slope.ConfInt_0.99 := slope.ConfInt_0.99 * Days_of_year ]

pprint[, slope.ConfInt_0.99 := NULL ]
pprint[, slope.ConfInt_0.95 := NULL ]

setorder(pprint,DATA,var,Month)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F
myRtools::write_dat(pprint, "~/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends_monthly.dat")




## reshape table
pprint




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
