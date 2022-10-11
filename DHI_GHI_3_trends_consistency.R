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
knitr::opts_chunk$set(out.width  = "80%"   )
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
#' Time data span `r range(DATA_all$Date)`
#'




#' ## 3. Consistency of trends



#' #### Calculate monthly SZA means ####
#+ echo=F, include=T



#+ echo=F, include=F
## ~ Plots longterm  ####
plot( ALL_3_monthly_mean$Date, ALL_3_monthly_mean$DIR_att )
plot( ALL_3_monthly_mean$Date, ALL_3_monthly_mean$DIR_att_EM )
plot( ALL_3_monthly_mean$Date, ALL_3_monthly_mean$GLB_att )
plot( ALL_3_monthly_mean$Date, ALL_3_monthly_mean$GLB_att_EM )
plot( ALL_3_monthly_mean$Date, ALL_3_monthly_mean$DIR_transp )
plot( ALL_3_monthly_mean$Date, ALL_3_monthly_mean$DIR_transp_EM )

plot( ALL_3_monthly_mean$SZA, ALL_3_monthly_mean$DIR_att )
plot( ALL_3_monthly_mean$SZA, ALL_3_monthly_mean$DIR_att_EM )
plot( ALL_3_monthly_mean$SZA, ALL_3_monthly_mean$GLB_att )
plot( ALL_3_monthly_mean$SZA, ALL_3_monthly_mean$GLB_att_EM )
plot( ALL_3_monthly_mean$SZA, ALL_3_monthly_mean$DIR_transp )
plot( ALL_3_monthly_mean$SZA, ALL_3_monthly_mean$DIR_transp_EM )

hist(ALL_3_monthly_mean$DIR_att_N)
hist(ALL_3_monthly_mean$GLB_att_N)

plot(ALL_3_monthly_mean$Date, ALL_3_monthly_mean$DIR_att_N)
plot(ALL_3_monthly_mean$Date, ALL_3_monthly_mean$HOR_att_N)
plot(ALL_3_monthly_mean$Date, ALL_3_monthly_mean$GLB_att_N)



hist(ALL_3_monthly_mean$DIR_att_EM)
hist(ALL_3_monthly_mean$GLB_att_EM)
hist(ALL_3_monthly_mean$DIR_transp_EM)



plot( CLEAR_3_monthly_mean$Month, CLEAR_3_monthly_mean$DIR_att )
plot( CLEAR_3_monthly_mean$Month, CLEAR_3_monthly_mean$DIR_att_EM )
plot( CLEAR_3_monthly_mean$Month, CLEAR_3_monthly_mean$GLB_att )
plot( CLEAR_3_monthly_mean$Month, CLEAR_3_monthly_mean$GLB_att_EM )
plot( CLEAR_3_monthly_mean$Month, CLEAR_3_monthly_mean$DIR_transp )
plot( CLEAR_3_monthly_mean$Month, CLEAR_3_monthly_mean$DIR_transp_EM )

plot( CLEAR_3_monthly_mean$SZA, CLEAR_3_monthly_mean$DIR_att )
plot( CLEAR_3_monthly_mean$SZA, CLEAR_3_monthly_mean$DIR_att_EM )
plot( CLEAR_3_monthly_mean$SZA, CLEAR_3_monthly_mean$GLB_att )
plot( CLEAR_3_monthly_mean$SZA, CLEAR_3_monthly_mean$GLB_att_EM )
plot( CLEAR_3_monthly_mean$SZA, CLEAR_3_monthly_mean$DIR_transp )
plot( CLEAR_3_monthly_mean$SZA, CLEAR_3_monthly_mean$DIR_transp_EM )

hist(CLEAR_3_monthly_mean$DIR_att_N)
hist(CLEAR_3_monthly_mean$GLB_att_N)
hist(CLEAR_3_monthly_mean$DIR_att_EM)
hist(CLEAR_3_monthly_mean$GLB_att_EM)
hist(CLEAR_3_monthly_mean$DIR_transp_EM)



# # ## ~ Plots seasonal ####
# #
# # plot( ALL_3_monthly_seas$doy, ALL_3_monthly_seas$DIR_att_seas )
# # plot( ALL_3_monthly_seas$doy, ALL_3_monthly_seas$GLB_att_seas )
# # plot( ALL_3_monthly_seas$doy, ALL_3_monthly_seas$DIR_transp_seas )
# # plot( ALL_3_monthly_seas$doy, ALL_3_monthly_seas$GLB_att_N_seas)
# # plot( ALL_3_monthly_seas$doy, ALL_3_monthly_seas$DIR_att_N_seas)
# #
# #
# # plot( CLEAR_3_monthly_seas$doy, CLEAR_3_monthly_seas$DIR_att_seas )
# # plot( CLEAR_3_monthly_seas$doy, CLEAR_3_monthly_seas$GLB_att_seas )
# # plot( CLEAR_3_monthly_seas$doy, CLEAR_3_monthly_seas$DIR_transp_seas )
# # plot( CLEAR_3_monthly_seas$doy, CLEAR_3_monthly_seas$GLB_att_N_seas)
# # plot( CLEAR_3_monthly_seas$doy, CLEAR_3_monthly_seas$DIR_att_N_seas)



#' #### Calculate seasonal anomaly ####
#+ echo=F, include=F

## ~ remove seasonality ####

## by sza
ALL_3_monthly_DEseas   <- merge(  ALL_3_monthly_mean, ALL_3_monthly_seas,   by = c("Month", "SZA", "preNoon"), all = T)
CLEAR_3_monthly_DEseas <- merge(CLEAR_3_monthly_mean, CLEAR_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
## by whole day monthly
ALL_3_monthly_daily_DEseas   <- merge(ALL_3_monthly_daily_mean,   ALL_3_monthly_daily_seas,   by = "Month", all = T)
CLEAR_3_monthly_daily_DEseas <- merge(CLEAR_3_monthly_daily_mean, CLEAR_3_monthly_daily_seas, by = "Month", all = T)


# #+ echo=F, include=T
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
ALL_3_monthly_DEseas[  ,    DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_3_monthly_DEseas[  ,    GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_3_monthly_DEseas[  ,    DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_3_monthly_DEseas[,    DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_3_monthly_DEseas[,    GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_3_monthly_DEseas[,    DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
ALL_3_monthly_daily_DEseas[  ,DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_3_monthly_daily_DEseas[  ,GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_3_monthly_daily_DEseas[  ,DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_3_monthly_daily_DEseas[,DIR_att   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_3_monthly_daily_DEseas[,GLB_att   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_3_monthly_daily_DEseas[,DIR_transp:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
#+ echo=F, include=F

ALL_3_monthly_DEseas[   preNoon == TRUE,    preNoon := "am"    ]
ALL_3_monthly_DEseas[   preNoon == FALSE,   preNoon := "pm"    ]
ALL_3_monthly_DEseas[   preNoon == "Daily", preNoon := "am+pm" ]
CLEAR_3_monthly_DEseas[ preNoon == FALSE,   preNoon := "pm"    ]
CLEAR_3_monthly_DEseas[ preNoon == "Daily", preNoon := "am+pm" ]
CLEAR_3_monthly_DEseas[ preNoon == TRUE,    preNoon := "am"    ]


setorder(ALL_3_monthly_DEseas,  Year,Month,preNoon,SZA)
setorder(CLEAR_3_monthly_DEseas,Year,Month,preNoon,SZA)
setorder(ALL_3_monthly_daily_DEseas,Year,Month)
setorder(CLEAR_3_monthly_daily_DEseas,Year,Month)


ALL_3_monthly_daily_DEseas[   is.na(GLB_att),    GLB_att    := 0 ]
ALL_3_monthly_daily_DEseas[   is.na(DIR_att),    DIR_att    := 0 ]
ALL_3_monthly_daily_DEseas[   is.na(DIR_transp), DIR_transp := 0 ]
CLEAR_3_monthly_daily_DEseas[ is.na(GLB_att),    GLB_att    := 0 ]
CLEAR_3_monthly_daily_DEseas[ is.na(DIR_att),    DIR_att    := 0 ]
CLEAR_3_monthly_daily_DEseas[ is.na(DIR_transp), DIR_transp := 0 ]

ALL_3_monthly_daily_cumsum   <- ALL_3_monthly_daily_DEseas
CLEAR_3_monthly_daily_cumsum <- CLEAR_3_monthly_daily_DEseas


ALL_3_monthly_daily_cumsum[,   GLB_att    := cumsum(GLB_att)]
ALL_3_monthly_daily_cumsum[,   DIR_att    := cumsum(DIR_att)]
ALL_3_monthly_daily_cumsum[,   DIR_transp := cumsum(DIR_transp)]
CLEAR_3_monthly_daily_cumsum[, GLB_att    := cumsum(GLB_att)]
CLEAR_3_monthly_daily_cumsum[, DIR_att    := cumsum(DIR_att)]
CLEAR_3_monthly_daily_cumsum[, DIR_transp := cumsum(DIR_transp)]



####  Cumulative sums ####
#' \newpage
#' ## Cumulative sums
#+ echo=F, include=F


timefactor <- 1
vars    <- c("GLB_att", "DIR_att", "DIR_transp")
dbs     <- c("ALL_3_monthly_DEseas", "CLEAR_3_monthly_DEseas")
basevar <- c("Year","Month","SZA","preNoon")


## compute cumsums for each category and sza
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
    assign(sub("DEseas","cumsum", DBn), DB[, ..ttt]   )
}



## create a useful date
ALL_3_monthly_cumsum[,        FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
CLEAR_3_monthly_cumsum[,      FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
ALL_3_monthly_daily_cumsum[,  FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
CLEAR_3_monthly_daily_cumsum[,FDate := as.Date(paste(Year, Month, 1), "%Y %m %d") ]





plotsza     <- c( 63 )
# plotpreNoon <- c("am","pm","am+pm", "daily")
plotpreNoon <- c("am","pm","daily")
plotpNcol   <- c(2,4,6,7)
vars        <- c("GLB_att", "DIR_att", "DIR_transp")
database    <- c("ALL_3_monthly_cumsum","CLEAR_3_monthly_cumsum")

#+ echo=F, include=T
for (adb in database) {
    DB  <- get(adb)
    DB2 <- get(paste0(sub("_.*","",adb),"_3_monthly_daily_cumsum"))
    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("FDate","preNoon",avar)
            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]
            # pdb   <- pdb[!is.na(pdb[[avar]])]
            xlim  <- range(pdb$FDate,DB2$FDate)
            ylim  <- range(pdb[[avar]],DB2[[avar]],na.rm = T)

            plot(1, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, xaxt = "n")
            axis.Date(1, pdb$FDate)
            abline(h=0, lty = 2, lwd=0.8)


            for ( i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon==plotpreNoon[i]]
                lines(pp$FDate, pp[[avar]], col = plotpNcol[i])
            }

            lines(DB2$FDate,DB2[[avar]], col = plotpNcol[4])

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3,cex = 0.8)

            title(paste(sub("_.*","",adb), "cumsum", asza, avar))
        }
    }
}




plotsza <- c( 63 )
# plotpreNoon <- c("am","pm","am+pm", "daily")
plotpreNoon <- c("am","pm","daily")
plotpNcol   <- c(2,4,6,7)
vars        <- c("DIR_att", "DIR_transp")
database    <- c("ALL_3_monthly_cumsum","CLEAR_3_monthly_cumsum")

#+ echo=F, include=T
for (adb in database) {
    DB  <- get(adb)
    DB2 <- get(paste0(sub("_.*","",adb),"_3_monthly_daily_cumsum"))
    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("FDate","preNoon",avar)
            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]

            pdb   <- pdb[!is.na(pdb[[avar]])]
            DB2   <- DB2[!is.na(DB2[[avar]])]

            xlim  <- range(pdb$FDate)
            ylim  <- range(pdb[[avar]],DB2[[avar]],na.rm = T)

            plot(1, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, xaxt = "n")
            axis.Date(1, pdb$FDate)
            abline(h=0, lty = 2, lwd=0.8)


            for ( i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon==plotpreNoon[i]]
                lines(pp$FDate, pp[[avar]], col = plotpNcol[i])
            }

            lines(DB2$FDate,DB2[[avar]], col = plotpNcol[4])

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3,cex = 0.8)

            title(paste(sub("_.*","",adb), "cumsum", asza, avar))
        }
    }
}
















#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
