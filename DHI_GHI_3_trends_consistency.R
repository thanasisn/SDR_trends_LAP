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
#'     number_sections: no
#'     fig_caption:     no
#'     keep_tex:        no
#'     latex_engine:    xelatex
#'     toc:             yes
#'     toc_depth:       4
#'     fig_width:       7
#'     fig_height:      4.5
#'   html_document:
#'     toc:             true
#'     keep_md:         no
#'     fig_width:       7.5
#'     fig_height:      5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ echo=F, include=T


####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"    )
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

## notification
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
#' Time data span `r range(ALL_1_daily_mean$Date)`
#'
#' Where is a **running mean the window is `r running_mean_window_days` days** or
#' `r running_mean_window_days / Days_of_year` years.
#'
#' ## 3. Consistency of trends
#'
#+ echo=F, include=F
## ~ Plots longterm  ####

data_list  <- list(ALL   =   ALL_3_monthly_mean,
                   CLEAR = CLEAR_3_monthly_mean,
                   CLOUD = CLOUD_3_monthly_mean)
by_var     <- c("Date", "Month", "SZA")
wecare     <- unique(unlist(lapply(data_list, names)))
wecare     <- grep("HOR|GLB|DIR", wecare, value = T)
for (i in 1:length(data_list)) {
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
## ~ Plot longterm histograms  ####
for (i in 1:length(data_list)) {
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

data_list  <- list(ALL   =   ALL_3_monthly_seas,
                   CLEAR = CLEAR_3_monthly_seas,
                   CLOUD = CLOUD_3_monthly_seas)
by_var     <- c("Month", "SZA")
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

## by sza
ALL_3_monthly_DEseas   <- merge(  ALL_3_monthly_mean, ALL_3_monthly_seas,   by = c("Month", "SZA", "preNoon"), all = T)
CLEAR_3_monthly_DEseas <- merge(CLEAR_3_monthly_mean, CLEAR_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
CLOUD_3_monthly_DEseas <- merge(CLOUD_3_monthly_mean, CLOUD_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
## by whole day monthly
ALL_3_monthly_daily_DEseas   <- merge(ALL_3_monthly_daily_mean,   ALL_3_monthly_daily_seas,   by = "Month", all = T)
CLEAR_3_monthly_daily_DEseas <- merge(CLEAR_3_monthly_daily_mean, CLEAR_3_monthly_daily_seas, by = "Month", all = T)
CLOUD_3_monthly_daily_DEseas <- merge(CLOUD_3_monthly_daily_mean, CLOUD_3_monthly_daily_seas, by = "Month", all = T)


# ## anomaly
# ALL_3_monthly_DEseas[   , DIR_att    := DIR_att    - DIR_att_seas    ]
# ALL_3_monthly_DEseas[   , GLB_att    := GLB_att    - GLB_att_seas    ]
# ALL_3_monthly_DEseas[   , DIR_transp := DIR_transp - DIR_transp_seas ]
# CLEAR_3_monthly_DEseas[ , DIR_att    := DIR_att    - DIR_att_seas    ]
# CLEAR_3_monthly_DEseas[ , GLB_att    := GLB_att    - GLB_att_seas    ]
# CLEAR_3_monthly_DEseas[ , DIR_transp := DIR_transp - DIR_transp_seas ]
# #+ echo=F, include=F


#+ echo=F, include=T
## relative anomaly
ALL_3_monthly_DEseas[  ,      DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_3_monthly_DEseas[  ,      GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_3_monthly_DEseas[  ,      DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_3_monthly_DEseas[,      DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_3_monthly_DEseas[,      GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_3_monthly_DEseas[,      DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_3_monthly_DEseas[,      DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_3_monthly_DEseas[,      GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLOUD_3_monthly_DEseas[,      DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]

ALL_3_monthly_daily_DEseas[  ,DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_3_monthly_daily_DEseas[  ,GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_3_monthly_daily_DEseas[  ,DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_3_monthly_daily_DEseas[,DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_3_monthly_daily_DEseas[,GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_3_monthly_daily_DEseas[,DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_3_monthly_daily_DEseas[,DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_3_monthly_daily_DEseas[,GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLOUD_3_monthly_daily_DEseas[,DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
#+ echo=F, include=F

## change flag names
ALL_3_monthly_DEseas[   preNoon == TRUE,    preNoon := "am"    ]
ALL_3_monthly_DEseas[   preNoon == FALSE,   preNoon := "pm"    ]
ALL_3_monthly_DEseas[   preNoon == "Daily", preNoon := "am+pm" ]
CLEAR_3_monthly_DEseas[ preNoon == FALSE,   preNoon := "pm"    ]
CLEAR_3_monthly_DEseas[ preNoon == "Daily", preNoon := "am+pm" ]
CLEAR_3_monthly_DEseas[ preNoon == TRUE,    preNoon := "am"    ]
CLOUD_3_monthly_DEseas[ preNoon == FALSE,   preNoon := "pm"    ]
CLOUD_3_monthly_DEseas[ preNoon == "Daily", preNoon := "am+pm" ]
CLOUD_3_monthly_DEseas[ preNoon == TRUE,    preNoon := "am"    ]

setorder(ALL_3_monthly_DEseas,        Year,Month,preNoon,SZA)
setorder(CLEAR_3_monthly_DEseas,      Year,Month,preNoon,SZA)
setorder(CLOUD_3_monthly_DEseas,      Year,Month,preNoon,SZA)
setorder(ALL_3_monthly_daily_DEseas,  Year,Month)
setorder(CLEAR_3_monthly_daily_DEseas,Year,Month)
setorder(CLOUD_3_monthly_daily_DEseas,Year,Month)


ALL_3_monthly_daily_DEseas[   is.na(GLB_att),    GLB_att    := 0 ]
ALL_3_monthly_daily_DEseas[   is.na(DIR_att),    DIR_att    := 0 ]
ALL_3_monthly_daily_DEseas[   is.na(DIR_transp), DIR_transp := 0 ]
CLEAR_3_monthly_daily_DEseas[ is.na(GLB_att),    GLB_att    := 0 ]
CLEAR_3_monthly_daily_DEseas[ is.na(DIR_att),    DIR_att    := 0 ]
CLEAR_3_monthly_daily_DEseas[ is.na(DIR_transp), DIR_transp := 0 ]
CLOUD_3_monthly_daily_DEseas[ is.na(GLB_att),    GLB_att    := 0 ]
CLOUD_3_monthly_daily_DEseas[ is.na(DIR_att),    DIR_att    := 0 ]
CLOUD_3_monthly_daily_DEseas[ is.na(DIR_transp), DIR_transp := 0 ]

ALL_3_monthly_daily_cumsum   <- ALL_3_monthly_daily_DEseas
CLEAR_3_monthly_daily_cumsum <- CLEAR_3_monthly_daily_DEseas
CLOUD_3_monthly_daily_cumsum <- CLOUD_3_monthly_daily_DEseas

## create a nice data
ALL_3_monthly_daily_cumsum[   , Date := as.POSIXct( paste(Year, Month, 1), format = "%Y %m %d")]
CLEAR_3_monthly_daily_cumsum[ , Date := as.POSIXct( paste(Year, Month, 1), format = "%Y %m %d")]
CLOUD_3_monthly_daily_cumsum[ , Date := as.POSIXct( paste(Year, Month, 1), format = "%Y %m %d")]



ALL_3_monthly_daily_cumsum[,   GLB_att    := cumsum(GLB_att)]
ALL_3_monthly_daily_cumsum[,   DIR_att    := cumsum(DIR_att)]
ALL_3_monthly_daily_cumsum[,   DIR_transp := cumsum(DIR_transp)]
CLEAR_3_monthly_daily_cumsum[, GLB_att    := cumsum(GLB_att)]
CLEAR_3_monthly_daily_cumsum[, DIR_att    := cumsum(DIR_att)]
CLEAR_3_monthly_daily_cumsum[, DIR_transp := cumsum(DIR_transp)]
CLOUD_3_monthly_daily_cumsum[, GLB_att    := cumsum(GLB_att)]
CLOUD_3_monthly_daily_cumsum[, DIR_att    := cumsum(DIR_att)]
CLOUD_3_monthly_daily_cumsum[, DIR_transp := cumsum(DIR_transp)]




####  Cumulative sums ####
#' \newpage
#' ## Cumulative sums
#'
#' Use deseasonalized monthly values to calculate cumulative sums
#'
#+ echo=F, include=F

timefactor <- 1
vars    <- c("GLB_att", "DIR_att", "DIR_transp")
dbs     <- c("ALL_3_monthly_DEseas",
             "CLEAR_3_monthly_DEseas",
             "CLOUD_3_monthly_DEseas")
basevar <- c("Year", "Month", "SZA", "preNoon")

## compute cumulative sums for each category and sza
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
                # dataset[, Data := sub("_.*","", DBn) ]
                # dataset[[avar]][is.na(dataset[[avar]])] <- 0
                # dataset[[avar]] <- cumsum( dataset[[avar]] )
                # gather <- merge(gather, dataset, all = T)
            }
        }
    }
    ttt <- c(basevar,vars)
    assign(sub("DEseas", "cumsum", DBn), DB[, ..ttt] )
}



## create a useful date
ALL_3_monthly_cumsum[,        FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
CLEAR_3_monthly_cumsum[,      FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
CLOUD_3_monthly_cumsum[,      FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
ALL_3_monthly_daily_cumsum[,  FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
CLEAR_3_monthly_daily_cumsum[,FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
CLOUD_3_monthly_daily_cumsum[,FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]





plotsza     <- c( 63 )
# plotpreNoon <- c("am","pm","am+pm", "daily")
plotpreNoon <- c("am","pm","daily")
plotpNcol   <- c(2, 3, 4, 5)
vars        <- c("GLB_att", "DIR_att", "DIR_transp")
database    <- c("ALL_3_monthly_cumsum",
                 "CLEAR_3_monthly_cumsum",
                 "CLOUD_3_monthly_cumsum")

#+ cumulativesums, echo=F, include=T
for (adb in database) {
    DB  <- get(adb)
    DB2 <- get(paste0(sub("_.*", "", adb), "_3_monthly_daily_cumsum"))
    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("FDate", "preNoon", avar)
            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]
            # pdb   <- pdb[!is.na(pdb[[avar]])]
            xlim  <- range(pdb$FDate,DB2$FDate)
            ylim  <- range(pdb[[avar]],DB2[[avar]],na.rm = T)

            par("mar" = c(3,4,2,1))

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Cumulative Seasonal Anomaly [%]" ) )
            axis.Date(1, pdb$FDate)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                lines(pp$FDate, pp[[avar]], col = plotpNcol[i], lwd = 2)
            }
            ## daily from other DT
            lines(DB2$FDate, DB2[[avar]], col = plotpNcol[3], lwd = 2)

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3,cex = 0.8)

            title(paste(sub("_.*","",adb), "monthly cumulative sum ", translate(avar), "for",asza,"deg."), cex.main = 1)
        }
    }
}
#'



plotsza <- c( 63 )
# plotpreNoon <- c("am","pm","am+pm", "daily")
plotpreNoon <- c("am","pm","daily")
plotpNcol   <- c(2,3,4,5)
vars        <- c("DIR_att", "DIR_transp")
database    <- c("ALL_3_monthly_cumsum",
                 "CLEAR_3_monthly_cumsum",
                 "CLOUD_3_monthly_cumsum")

#+  cumulativesumsdir, echo=F, include=T
for (adb in database) {
    DB  <- get(adb)
    DB2 <- get(paste0(sub("_.*","",adb), "_3_monthly_daily_cumsum"))
    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("FDate","preNoon",avar)
            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]

            pdb   <- pdb[!is.na(pdb[[avar]])]
            DB2   <- DB2[!is.na(DB2[[avar]])]

            xlim  <- range(pdb$FDate)
            ylim  <- range(pdb[[avar]],DB2[[avar]],na.rm = T)

            par("mar" = c(3,4,2,1))

            plot(1, type="n",
                 xlab="",
                 xlim=xlim, ylim=ylim, xaxt = "n",
                 ylab = bquote("Cumulative Seasonal Anomaly [%]" ) )
            axis.Date(1, pdb$FDate)
            abline(h=0, lty = 2, lwd=0.8)

            ## for zsa
            for ( i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon==plotpreNoon[i]]
                lines(pp$FDate, pp[[avar]], col = plotpNcol[i], lwd = 3)
            }

            ## for whole day
            lines(DB2$FDate,DB2[[avar]], col = plotpNcol[3], lwd = 3)

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3, cex = 0.9)

            title(paste(sub("_.*","",adb), "monthly cumulative sum ", translate(avar),"for", asza, "deg."), cex.main = 1)
        }
    }
}
#'













#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
