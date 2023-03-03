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
knitr::opts_chunk$set(fig.pos    = '!h'    )
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
library(ggplot2)

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
#' ## 3. Consistency of trends
#'
#+ echo=F, include=F

## ___ Scatter plots with SZA all data -----------------------------------------
data_list <- c(  "ALL_1_D_monthly_DESEAS",
               "CLEAR_1_D_monthly_DESEAS",
               "CLOUD_1_D_monthly_DESEAS",
                     "ALL_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS")

by_var     <- c("Date", "SZA")
for (i in data_list) {
    ## get data and y vars to plot
    Dplot  <- get(i)
    wecare <- grep("HOR|GLB|DIR", names(Dplot), value = T)
    ## loop existing x vars
    for (xvar in names(Dplot)[names(Dplot) %in% by_var]){
        for (yvar in wecare) {
            col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
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
        col <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
        hist(Dplot[[yvar]],
             main = paste(i, yvar),
             xlab = yvar,
             breaks = 100, col = col)
    }
}
#+ echo=F, include=F
rm(data_list)







## ~ Calculate daily cum sum ---------------------------------------------------

setorder(  ALL_1_daily_DESEAS, Date)
setorder(CLEAR_1_daily_DESEAS, Date)
setorder(CLOUD_1_daily_DESEAS, Date)

## with NAs in place
#   ALL_1_daily_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
#   ALL_1_daily_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
#   ALL_1_daily_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]
# CLEAR_1_daily_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
# CLEAR_1_daily_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
# CLEAR_1_daily_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]
# CLOUD_1_daily_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
# CLOUD_1_daily_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
# CLOUD_1_daily_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]

## Calculate cumsum with zeroing NA
  ALL_1_daily_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
  ALL_1_daily_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
  ALL_1_daily_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLEAR_1_daily_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLEAR_1_daily_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLEAR_1_daily_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLOUD_1_daily_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLOUD_1_daily_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLOUD_1_daily_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]



## ~ Calculate monthly cum sum -------------------------------------------------

setorder(  ALL_1_D_monthly_DESEAS, Date)
setorder(CLEAR_1_D_monthly_DESEAS, Date)
setorder(CLOUD_1_D_monthly_DESEAS, Date)



# ## with NAs in place
#   ALL_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
#   ALL_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
#   ALL_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]
# CLEAR_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
# CLEAR_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
# CLEAR_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]
# CLOUD_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
# CLOUD_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
# CLOUD_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]

## Calculate cumsum with zeroing NA
  ALL_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
  ALL_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
  ALL_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLEAR_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLEAR_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLEAR_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLOUD_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLOUD_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLOUD_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]




### Whole day daily cumulative sum ---------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Whole day daily cumulative sum
#'
#' All data in this section are refered to daily means.
#'
#+ echo=F, include=T

# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c(  "ALL_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS",
                 "CLOUD_1_daily_DESEAS")

#+ cumulativedailycumsums, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n#### Daily cum sums for", translate(sub("_.*", "", adb)), "\n\n")

    for (avar in vars) {
        ## get only the vars we need
        wcare <- c("Date", paste0(avar,"_des"), paste0(avar,"_cusum"))
        pdb   <- DB[, ..wcare]
        rm(DB)
        xlim  <- range(pdb$Date)
        ylim  <- range(pdb[[paste0(avar,"_cusum")]],na.rm = T)
        col   <- get(paste0(c("col",
                              unlist(strsplit(avar, split = "_" ))[1:2]),
                            collapse = "_"))

        par("mar" = c(3,4,2,1))
        par(pch = 19)

        plot(1, type = "n",
             xlab = "",
             xlim = xlim, ylim = ylim,
             xaxt = "n",
             ylab = bquote("Cumulative Seasonal Anomaly [%]"))
        axis.Date(1, pdb$Date)
        abline(h = 0, lty = 2, lwd = 0.8)

        ## daily from other DT
        lines(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = col, lwd = 2)

        title(paste(sub("_.*","",adb), "mean daily cumulative sum ",
                    translate(avar) ), cex.main = 1)


        ## test plot for reference
        ## ## linear model
        lm1 <- lm(pdb[[paste0(avar,"_des")]] ~ pdb$Date)
        plot(pdb$Date, pdb[[paste0(avar,"_des")]],
             ylab = bquote("Seasonal Anomaly [%]"),
             cex = 0.3,
             col = col)
        abline(h = 0, lty = 2, lwd = 0.8)
        title(paste(sub("_.*","",adb), "mean daily values ",
                    translate(avar) ), cex.main = 1)
        abline(lm1, lwd = 2)

        rm <- frollmean(pdb[[paste0(avar,"_des")]], round(running_mean_window_days),
                        na.rm = TRUE, algo = "exact", align = "center")
        points(pdb$Date, rm, col = "red", cex = 0.5)

        ## decorations
        fit <- lm1[[1]]
        legend('top', lty = 1, bty = "n", lwd = 2, cex = 1,
               paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2])*Days_of_year,3),'* year'))


        # hist(pdb$GLB_att_cusum)
        # hist(pdb$GLB_att_des)
        # sum(pdb[ GLB_att_des > 0, GLB_att_des ])
        # sum(pdb[ , GLB_att_des ], na.rm = T)
        # tail(pdb[ , GLB_att_cusum ])

        cat("\n\\newpage\n")
        cat("\n\\footnotesize\n")
        # cat("\n\\small\n")

        testdb <-
            merge(
                merge(
                    merge(
                        merge(
                            pdb[get(paste0(avar,"_des")) > 0, .(PositiveSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)],
                            pdb[get(paste0(avar,"_des")) < 0, .(NegativeSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                        pdb[get(paste0(avar,"_des")) > 0, .(PositiveMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                    pdb[get(paste0(avar,"_des")) < 0, .(NegativeMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]
                ),
                pdb[ , .(Sum = sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date) ]
            )

        cat(pander(testdb))
        cat("\n\\normalsize\n")


        ylim <- range(testdb$PositiveSum, -testdb$NegativeSum)
        plot( testdb$year,  testdb$PositiveSum, type = "l", col = "blue", ylim = ylim)
        lines(testdb$year, -testdb$NegativeSum, type = "l", col = "red")
        title("Sum of negative and positive values by year")

        legend("top", ncol = 2, bty = "n",
               lty = 1, pch = NA, cex = 0.7,
               legend = c("Sum of positive anomaly",
                          "Sum of negative anomaly"),
               col    = c("blue", "red")
        )

        # ylim <- c(-max(abs(testdb$Sum), na.rm = T), max(abs(testdb$Sum), na.rm = T))
        # plot( testdb$year,  testdb$Sum, type = "l", col = "black", ylim = ylim)
        # abline(h = 0, lty = 2, lwd = 0.8)
        # title("Sum of all yearly values")


        gp <- ggplot(data = testdb, aes(x = year, y = Sum)) +
              geom_col(data = testdb[Sum <= 0], fill = "red") +
              geom_col(data = testdb[Sum >= 0], fill = "blue") +
              ggtitle("Sum of all year values") +
              xlab("Year")     +
              ylab("Year Sum") +
              theme_bw()       +
              theme(plot.title = element_text(hjust = 0.5))

        print(gp)

    }
}
#'
#+ echo=F, include=T







### Whole day monthly cumulative sum -------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Whole day monthly cumulative sum
#'
#' In this section the monthly values have been calculated from daily mean values.
#'
#+ echo=F, include=T

# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c(  "ALL_1_D_monthly_DESEAS",
                 "CLEAR_1_D_monthly_DESEAS",
                 "CLOUD_1_D_monthly_DESEAS")

#+ cumulativemonthlycumsums, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n#### Monthly cum sums for", translate(sub("_.*", "", adb)), "\n\n")

    for (avar in vars) {
        ## get only the vars we need
        wcare <- c("Date", paste0(avar,"_des"), paste0(avar,"_cusum"))
        pdb   <- DB[, ..wcare]
        rm(DB)
        xlim  <- range(pdb$Date)
        ylim  <- range(pdb[[paste0(avar,"_cusum")]],na.rm = T)
        col   <- get(paste0(c("col",
                              unlist(strsplit(avar, split = "_" ))[1:2]),
                            collapse = "_"))

        par("mar" = c(3,4,2,1))
        par(pch = 19)

        plot(1, type = "n",
             xlab = "",
             xlim = xlim, ylim = ylim,
             xaxt = "n",
             ylab = bquote("Cumulative Seasonal Anomaly [%]"))
        axis.Date(1, pdb$Date)
        abline(h = 0, lty = 2, lwd = 0.8)

        ## daily from other DT
        lines(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = col, lwd = 2)

        title(paste(sub("_.*","",adb), "mean monthly cumulative sum ",
                    translate(avar) ), cex.main = 1)


        ## test plot for reference
        ## linear model
        lm1 <- lm(pdb[[paste0(avar,"_des")]] ~ pdb$Date)
        plot(pdb$Date, pdb[[paste0(avar,"_des")]],
             ylab = bquote("Seasonal Anomaly [%]"),
             cex = 0.5,
             col = col)
        abline(h = 0, lty = 2, lwd = 0.8)
        title(paste(sub("_.*","",adb), "mean monthly values ",
                    translate(avar) ), cex.main = 1)
        abline(lm1, lwd = 2)

        rm <- frollmean(pdb[[paste0(avar,"_des")]], round(running_mean_window_days),
                        na.rm = TRUE, algo = "exact", align = "center")
        points(pdb$Date, rm, col = "red", cex = 0.5)

        ## decorations
        fit <- lm1[[1]]
        legend('top', lty = 1, bty = "n", lwd = 2, cex = 1,
               paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2])*Days_of_year,3),'* year'))


        cat("\n\\newpage\n")
        cat("\n\\footnotesize\n")
        # cat("\n\\small\n")

        testdb <-
            merge(
                merge(
                    merge(
                        merge(
                            pdb[get(paste0(avar,"_des")) > 0, .(PositiveSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)],
                            pdb[get(paste0(avar,"_des")) < 0, .(NegativeSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                        pdb[get(paste0(avar,"_des")) > 0, .(PositiveMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                    pdb[get(paste0(avar,"_des")) < 0, .(NegativeMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]
                ),
                pdb[ , .(Sum = sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date) ]
            )

        cat(pander(testdb))
        cat("\n\\normalsize\n")


        ylim <- range(testdb$PositiveSum, -testdb$NegativeSum)
        plot( testdb$year,  testdb$PositiveSum, type = "l", col = "blue", ylim = ylim)
        lines(testdb$year, -testdb$NegativeSum, type = "l", col = "red")
        title("Sum of negative and positive values by year")

        legend("top", ncol = 2, bty = "n",
               lty = 1, pch = NA, cex = 0.7,
               legend = c("Sum of positive anomaly",
                          "Sum of negative anomaly"),
               col    = c("blue", "red")
               )


        # ylim <- c(-max(abs(testdb$Sum), na.rm = T), max(abs(testdb$Sum), na.rm = T))
        # plot( testdb$year,  testdb$Sum, type = "l", col = "black", ylim = ylim)
        # abline(h = 0, lty = 2, lwd = 0.8)
        # title("Sum of all yearly values")

        # polygon(
        #     c(min(testdb$year), testdb$year , max(testdb$year)) ,
        #     c(min(testdb$Sum) , testdb$Sum , min(testdb$Sum)) ,
        #     col=rgb(0.2,0.1,0.5,0.2) , border=F
        # )


        gp <- ggplot(data = testdb, aes(x = year, y = Sum)) +
            geom_col(data = testdb[Sum <= 0], fill = "red") +
            geom_col(data = testdb[Sum >= 0], fill = "blue") +
            ggtitle("Sum of all year values") +
            xlab("Year")     +
            ylab("Year Sum") +
            theme_bw()       +
            theme(plot.title = element_text(hjust = 0.5))

        print(gp)

        # hist(pdb$GLB_att_cusum)
        # hist(pdb$GLB_att_des)
        # sum(pdb[ GLB_att_des > 0, GLB_att_des ])
        # sum(pdb[ , GLB_att_des ], na.rm = T)
        # tail(pdb[ , GLB_att_cusum ])
        # pdb[ , sum(get(paste0(avar,"_des")), na.rm = T), by = year(Date) ]
    }
}
#'
#+ echo=F, include=T






## ~ Calculate monthly sum sum by SZA ------------------------------------------

## set data order before cumsum
setorder(  ALL_3_monthly_DESEAS, Date)
setorder(CLEAR_3_monthly_DESEAS, Date)
setorder(CLOUD_3_monthly_DESEAS, Date)

  ALL_3_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0)), by = .(SZA, preNoon)]
  ALL_3_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0)), by = .(SZA, preNoon)]
  ALL_3_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0)), by = .(SZA, preNoon)]
CLEAR_3_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0)), by = .(SZA, preNoon)]
CLEAR_3_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0)), by = .(SZA, preNoon)]
CLEAR_3_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0)), by = .(SZA, preNoon)]
CLOUD_3_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0)), by = .(SZA, preNoon)]
CLOUD_3_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0)), by = .(SZA, preNoon)]
CLOUD_3_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0)), by = .(SZA, preNoon)]





#### Plot Cumulative sums  -----------------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Cumulative sums by SZA and part of day
#'
#' Use deseasonalized monthly values to calculate cumulative sums
#' by a SZA bin and a period of day.
#'
#+ echo=F, include=F

plotsza     <- c( 63 )
# plotpreNoon <- c("am","pm","am+pm", "daily")
plotpreNoon <- c("am","pm","am+pm")
plotpNcol   <- c(2, 3, 4, 5)
# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c(  "ALL_3_monthly_DESEAS",
                 "CLEAR_3_monthly_DESEAS",
                 "CLOUD_3_monthly_DESEAS")

#+ cumulativemonthlySZAcumsum, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n\\FloatBarrier\n")
    cat("\n#### Monthly cum sums for", translate(sub("_.*", "", adb)), "by SZA\n\n")


    cat("\n\n \\ \n\n")


    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("Date", "preNoon", grep(avar, names(DB), value = T))

            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]

            ## start empty plot
            xlim  <- range(pdb$Date)
            ylim  <- range(pdb[[paste0(avar,"_cusum")]], na.rm = T)

            par("mar" = c(3,4,2,1))
            par(pch = 19)

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Cumulative Seasonal Anomaly [%]" ) )
            axis.Date(1, pdb$Date)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                lines(pp$Date, pp[[paste0(avar,"_cusum")]], col = plotpNcol[i], lwd = 2)
            }

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3, cex = 0.8)

            title(paste(sub("_.*","",adb), "monthly cumulative sum ",
                        translate(avar), "for SZA", asza,""), cex.main = 1)

            # cat("\n \n \n")


            gpa <- ggplot(data = pdb,
                         aes(x     = Date,
                             y     = get(paste0(avar,"_des")),
                             color = preNoon))          +
                geom_point(size = .8)                   +
                geom_smooth(method = "lm", se = FALSE)  +
                ggtitle(paste0("Anomaly % for ", asza, " SZA bin "))   +
                xlab("Year")                            +
                ylab("Anomaly %")                       +
                scale_color_manual(breaks = plotpreNoon,
                                   values = plotpNcol)  +
                theme_bw()                              +
                labs(color = "Period of day with `lm`") +
                theme(plot.title = element_text(hjust = 0.5,
                                                size  = 10,
                                                face  = "bold" ),
                      legend.position   = "bottom",
                      legend.margin     = margin( t = -5 ),
                      legend.background = element_rect(fill = alpha("white", 0.5)),
                      legend.title      = element_text(size = 10),
                      legend.text       = element_text(size = 10))

            suppressWarnings(suppressMessages(  print(gpa)  ))
            # cat("\n \n \n")

            gpb <- ggplot(data = pdb,
                         aes(x     = Date,
                             y     = get(paste0(avar,"_des")),
                             color = preNoon))          +
                geom_point(size = .8)                   +
                geom_smooth(method = "loess", se = F)   +
                ggtitle(paste0("Anomaly % for ", asza, " SZA bin "))   +
                xlab("Year")                            +
                ylab("Anomaly %")                       +
                scale_color_manual(breaks = plotpreNoon,
                                   values = plotpNcol)  +
                theme_bw()                              +
                labs(color = "Period of day with `loess`") +
                theme(plot.title = element_text(hjust = 0.5,
                                                size  = 10,
                                                face  = "bold" ),
                      legend.position   = "bottom",
                      legend.margin     = margin( t = -5 ),
                      legend.background = element_rect(fill = alpha("white", 0.5)),
                      legend.title      = element_text(size = 10),
                      legend.text       = element_text(size = 10))

            plot.new()
            suppressWarnings(suppressMessages(  print(gpb)  ))

            # ## test plot for reference
            # plot(DB$Date, DBn[avar]], col = plotpNcol[3],
            #      ylab = bquote("Seasonal Anomaly [%]"),
            #      cex = 0.5)
            # for (i in 1:length(plotpreNoon) ) {
            #     pp <- DBn[preNoon == plotpreNoon[i] & SZA == asza]
            #     points(as.Date(pp$Date), pp[[avar]], col = plotpNcol[i], lwd = 2, cex = 0.5)
            # }
        }
    }
}
#'
#+ echo=F, include=T





# ## Plot direct only time scale -------------------------------------------------
#
# plotsza     <- c( 63 )
# # plotpreNoon <- c("am","pm","am+pm", "daily")
# plotpreNoon <- c("am","pm","daily")
# plotpNcol   <- c(2,3,4,5)
# vars        <- c("DIR_att", "DIR_transp")
# database    <- c("ALL_3_monthly_cumsum",
#                  "CLEAR_3_monthly_cumsum",
#                  "CLOUD_3_monthly_cumsum")
#
# #+  cumulativeSZAsumsdir, echo=F, include=T
# for (adb in database) {
#     DB  <- get(adb)
#     DB2 <- get(paste0(sub("_.*","",adb), "_3_monthly_daily_cumsum"))
#     for (asza in plotsza) {
#         for (avar in vars) {
#             wcare <- c("FDate","preNoon",avar)
#             pdb   <- DB[ SZA == asza ]
#             pdb   <- pdb[, ..wcare]
#
#             pdb   <- pdb[!is.na(pdb[[avar]])]
#             DB2   <- DB2[!is.na(DB2[[avar]])]
#
#             xlim  <- range(pdb$FDate)
#             ylim  <- range(pdb[[avar]],DB2[[avar]],na.rm = T)
#
#             par("mar" = c(3,4,2,1))
#
#             plot(1, type="n",
#                  xlab="",
#                  xlim=xlim, ylim=ylim, xaxt = "n",
#                  ylab = bquote("Cumulative Seasonal Anomaly [%]" ) )
#             axis.Date(1, pdb$FDate)
#             abline(h=0, lty = 2, lwd=0.8)
#
#             ## for zsa
#             for ( i in 1:length(plotpreNoon) ) {
#                 pp <- pdb[preNoon==plotpreNoon[i]]
#                 lines(pp$FDate, pp[[avar]], col = plotpNcol[i], lwd = 3)
#             }
#
#             ## for whole day
#             lines(DB2$FDate,DB2[[avar]], col = plotpNcol[3], lwd = 3)
#
#             legend("top", legend = plotpreNoon, col = plotpNcol,
#                    lty = 1, bty = "n", ncol = 3, cex = 0.9)
#
#             title(paste(sub("_.*","",adb), "monthly cumulative sum ", translate(avar),"for", asza, "deg."), cex.main = 1)
#         }
#     }
# }
# #'













#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
