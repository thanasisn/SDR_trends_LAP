# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends of SDR in Thessaloniki "
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisphysicist@gmail.com]
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
#'     fig_width:       7
#'     fig_height:      4.5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ echo=F, include=T


## __ Document options ---------------------------------------------------------

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = c("pdf", "png"))
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
# knitr::opts_chunk$set(fig.pos    = '!h'    )
warning("Don't use cache it breaks computations")

#+ include=F, echo=F
## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./DHI_GHI_1_longterm_trends.R"

if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander,     quietly = TRUE, warn.conflicts = FALSE)
library(lubridate,  quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2,    quietly = TRUE, warn.conflicts = FALSE)
library(fANCOVA,    quietly = TRUE, warn.conflicts = FALSE)


panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")


## __ Source initial scripts ---------------------------------------------------
source("./DHI_GHI_0_data_input.R")
source("./DHI_GHI_0_variables.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")

## check previous steps
if (! file.exists(I1_longterm) |
    file.mtime(I1_longterm) < file.mtime("./DHI_GHI_0_variables.R") |
    file.mtime(I1_longterm) < file.mtime("./DHI_GHI_00_raw_data.R") |
    file.mtime(I1_longterm) < file.mtime("./DHI_GHI_01_Input_longterm.R") )
{
    # source("./DHI_GHI_01_Input_longterm.R")
    tryCatch(source("./DHI_GHI_01_Input_longterm.R"), exit=function(cond) {
        message( conditionMessage(cond) )
    })
    dummy <- gc()
}

## load data
load(I1_longterm)

tic <- Sys.time()

## notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occurred!'")
    }
    traceback()
})

## __ Flags --------------------------------------------------------------------

DRAFT <- TRUE
DRAFT <- FALSE

## override plot options
par(pch = ".")

## choose to grid some plots
FIGURESGRID <- TRUE
# FIGURESGRID <- FALSE

## choose loess criterion for span
LOESS_CRITERIO <-  c("aicc", "gcv")[1]



#+ echo=F, include=T
#'
#' ## 1. Long term anomaly trends
#'
#' ### Data info
#'
#' Time data span `r range(ALL_1_daily_DESEAS$Date)`
#'
#' Where is a **running mean the window is `r running_mean_window_days` days** or
#' `r running_mean_window_days / Days_of_year` years.
#'
#' ### Process
#'
#' - Daily means are used to produce monthly means.
#' - Some quality constrains have been applied
#' - In each case the seasonal data have been created with the same process as
#' the data that are de-seasonalised.
#'
#'
#+ echo=F, include=F



## ____ Scatter plots with SZA all data ----------------------------------------
data_list <- c(  "ALL_1_D_monthly_DESEAS",
               "CLEAR_1_D_monthly_DESEAS",
               "CLOUD_1_D_monthly_DESEAS",
                     "ALL_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS")

by_var     <- c("doy")
#+ echo=F, include=F
for (i in data_list) {
    ## get data and y vars to plot
    Dplot  <- get(i)
    wecare <- grep("HOR|GLB|DIR", names(Dplot), value = T)
    ## loop existing x vars
    for (xvar in names(Dplot)[names(Dplot) %in% by_var]) {
        for (yvar in wecare) {
            col <- get(paste0(c("col", unlist(strsplit(yvar, split = "_"))[1:2]),
                              collapse = "_"))
            vect <- Dplot[[yvar]]
            if (all(is.na(vect))) next()
            plot(Dplot[[xvar]], vect,
                 pch  = 19,
                 cex  = .3,
                 col  = col,
                 main = paste(i, yvar),
                 xlab = xvar, ylab = yvar)
        }
    }
}

## ____ Histograms Plots all data ----------------------------------------------
#+ echo=F, include=F
for (i in data_list) {
    ## get data and y vars to plot
    Dplot  <- get(i)
    wecare <- grep("HOR|GLB|DIR", names(Dplot), value = TRUE)
    for (yvar in wecare) {
        if (!yvar %in% names(Dplot))   next()
        if (all(is.na(Dplot[[yvar]]))) next()

        col <- get(paste0(c("col", unlist(strsplit(yvar,split = "_" ))[1:2]),
                          collapse = "_"))
        hist(Dplot[[yvar]],
             main   = paste(i, yvar),
             xlab   = yvar,
             breaks = 100, col = col)
    }
}
#+ echo=F, include=F
rm(data_list)





##  TOTAL TRENDS  ##############################################################


## __ Plot data trends  --------------------------------------------------------
#'
#' \newpage
#' \FloatBarrier
#'
#' ### Trends from daily means
#'
#+ LongtermTrends, echo=F, include=T, results="asis"
# vars <- c("HOR_att","DIR_transp", "DIR_att", "GLB_att", "tsi1au_att")
vars <- c("DIR_att_des", "GLB_att_des", "tsi1au_att")

dbs         <- c(  "ALL_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS",
                 "CLOUD_1_daily_DESEAS")
## gather trends
gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Trends on", translate(DBn), "data\n\n" )

        for (avar in vars) {
            dataset <- DB

            if (all(is.na(dataset[[avar]]))) next()

            ## linear model by day step
            lm1 <- lm(dataset[[avar]] ~ dataset$Date)

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
                                     unlist(strsplit(avar, split = "_"))[1:2]),
                                   collapse = "_")),
                 cex      = 2,
                 # main     = paste(translate(DBn), translate(avar)),
                 cex.main = 0.8,
                 yaxt     = "n",
                 xlab     = "",
                 ylab     = bquote("Anomaly [%]")
            )
            # y axis
            axis(2, pretty(dataset[[avar]]), las = 2 )

            # x axis
            axis.Date(1,
                      at = seq(as.Date("1993-01-01"), max(dataset$Date), by = "year"),
                      format = "%Y",
                      labels = NA,
                      tcl = -0.25)



            if (DRAFT == TRUE) {
                title(main = paste(translate(DBn), translate(avar)),
                      cex.main = 0.8 )
            }


            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            ## plot fit line
            abline(lm1, lwd = 2)

            if (DRAFT == TRUE) {
                ## Running mean
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                round(running_mean_window_days),
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")

                # points(dataset$Date, rm, col = "red", cex = 0.5)
                lines(dataset$Date[first:last], rm, col = "red", lwd = 1.5)

                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Date[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Date)
                lines(dataset$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }


            ## display trend on graph
            fit <- lm1[[1]]

            legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) "+" else "-",
                         signif(abs(fit[2]) * Days_of_year, 3),
                         "% per year")
            )

    }
}
#+ echo=F, include=F





#+ LongtermTrendsRuMe, echo=F, include=T, results="asis"
# vars <- c("HOR_att","DIR_transp", "DIR_att", "GLB_att", "tsi1au_att")
vars <- c("DIR_att_des", "GLB_att_des", "tsi1au_att")

dbs         <- c(  "ALL_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS",
                 "CLOUD_1_daily_DESEAS")
## gather trends
gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Trends on", translate(DBn), "data with running mean\n\n" )

        for (avar in vars) {
            dataset <- DB

            if (all(is.na(dataset[[avar]]))) next()

            ## linear model by day step
            lm1 <- lm(dataset[[avar]] ~ dataset$Date)

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
                                     unlist(strsplit(avar, split = "_"))[1:2]),
                                   collapse = "_")),
                 cex      = 2,
                 main     = paste(translate(DBn), translate(avar)),
                 cex.main = 0.8,
                 yaxt     = "n",
                 xlab     = "",
                 ylab     = bquote("Anomaly [%]")
            )
            axis(2, pretty(dataset[[avar]]), las = 2 )

            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            ## plot fit line
            abline(lm1, lwd = 2)

            ## Running mean
            first <- head(which(!is.na(dataset[[avar]])),1)
            last  <- tail(which(!is.na(dataset[[avar]])),1)

            rm <- frollmean(dataset[[avar]][first:last],
                            round(running_mean_window_days),
                            na.rm = TRUE,
                            algo  = "exact",
                            align = "center")

            # points(dataset$Date, rm, col = "red", cex = 0.5)
            lines(dataset$Date[first:last], rm, col = "red", lwd = 1.5)

            ## LOESS curve
            vec <- !is.na(dataset[[avar]])
            FTSE.lo3 <- loess.as(dataset$Date[vec], dataset[[avar]][vec],
                                 degree = 1,
                                 criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
            FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Date)
            lines(dataset$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)


            ## decorations
            fit <- lm1[[1]]

            legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) "+" else "-",
                         signif(abs(fit[2]) * Days_of_year, 3),
                         "% per year")
            )

    }
}
#+ echo=F, include=F


#+ LongtermSeasonal, echo=F, include=T, results="asis"
# vars <- c("HOR_att","DIR_transp", "DIR_att", "GLB_att", "tsi1au_att")
vars <- c("DIR_att_seas", "GLB_att_seas")

dbs         <- c(  "ALL_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS",
                   "CLOUD_1_daily_DESEAS")
rmw_days <- 15

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Climatology on", translate(DBn), "data with", rmw_days, "d running mean\n\n" )

    for (avar in vars) {
        dataset <- DB
        dataset <- unique(dataset[, .( doy, get(avar))])
        setorder(dataset, doy)

        if (all(is.na(dataset$V2))) next()

        par("mar" = c(3, 4, 2, 1))

        ## plot data
        plot(dataset$doy, dataset$V2,
             pch  = ".",
             col  = get(paste0(c("col",
                                 unlist(strsplit(avar, split = "_"))[1:2]),
                               collapse = "_")),
             cex      = 2,
             main     = paste("Climatology", translate(DBn), translate(avar)),
             cex.main = 0.8,
             yaxt     = "n",
             xlab     = "",
             ylab     = bquote("SDR")
        )
        axis(2, pretty(dataset$V2), las = 2 )


        ## Running mean
        rm <- frollmean(dataset$V2,
                        15,
                        na.rm = TRUE,
                        algo  = "exact",
                        align = "center")

        # points(dataset$Date, rm, col = "red", cex = 0.5)
        lines(dataset$doy, rm, col = "red", lwd = 1.5)

        ## LOESS curve
        vec <- !is.na(dataset$V2)
        FTSE.lo3 <- loess.as(dataset$doy[vec], dataset$V2[vec],
                             degree = 1,
                             criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
        FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$doy)
        lines(dataset$doy, FTSE.lo.predict3, col = "cyan", lwd = 2.5)

    }
}
#+ echo=F, include=F







## __ Trends table  ------------------------------------------------------------
#'
#' \newpage
#'
#' #### Table of total trends.
#'
#+ echo=F, include=T

wecare      <- names(gather)
# wecare      <- grep("intercept", names(gather), value = T, invert = T)
gather      <- data.table(gather)
gather$DATA <- sub("_.*", "", gather$DATA)

pprint <- gather[ , ..wecare]

pprint[, slope.stat_sig := 100 * (1 - slope.p)]
pprint[, slope.t        := NULL]
pprint[, Rsqrd          := NULL]
pprint[, RsqrdAdj       := NULL]
pprint[, N              := NULL]

saveRDS(pprint,
        "./figures/tbl_longterm_trends.Rds")
## convert slope / year
pprint[, slope              := slope              * Days_of_year]
pprint[, slope.sd           := slope.sd           * Days_of_year]
pprint[, slope.ConfInt_0.95 := slope.ConfInt_0.95 * Days_of_year]
pprint[, slope.ConfInt_0.99 := slope.ConfInt_0.99 * Days_of_year]
wecare <- grep("intercept", names(pprint), value = T, invert = T)
pprint <- pprint[ , ..wecare]
pprint[, DATA               := translate(DATA)                  ]
pprint[, var                := translate(var)                   ]



# \scriptsize
# \footnotesize
# \small

#+ echo=F, include=T
#' \scriptsize
#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#'
#' \normalsize
#+ echo=F, include=T

write_dat(pprint,
          "./figures/tbl_longterm_trends.dat",
          clean = TRUE)





##  SEASONAL TRENDS  ###########################################################

#' \newpage
#' \FloatBarrier
#'
#' ### Trends for each season of the year
#'
#' We calculated Seasonal means from the daily means
#'
#+ echo=F, include=F

## __ Plot trends for each season  ---------------------------------------------

## ____ by sky conditions  -----------------------------------------------------

#+ SeasonalTrends, echo=F, include=T, results="asis, fig.asp = 0.5"
# vars        <- c("DIR_att", "GLB_att")
vars        <- c("GLB_att_des")

## Monthly aggregation
dbs         <- c(  "ALL_1_D_bySeason_DESEAS",
                 "CLEAR_1_D_bySeason_DESEAS",
                 "CLOUD_1_D_bySeason_DESEAS")

Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
for (DBn in dbs) {
    DB <- get(DBn)

    if (!FIGURESGRID) {
        cat("\n\\newpage\n")
        cat("\n#### ", translate(DBn), "\n\n")
    }

    for (avar in vars) {
        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(2, 2))
            cat("\n\\newpage\n")
            cat("\n#### ", translate(DBn), translate(avar) , "\n\n")
        }

        ylim <- range(DB[[avar]], na.rm = TRUE )

        for (ase in Seasons) {

            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm( dataset[[avar]] ~ dataset$Year )

            ## plot
            par("mar" = c(2, 3.6, 2, 0.5))

            plot(dataset$Year, dataset[[avar]],
                 # ylim = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .5,
                 main     = paste(ase, translate(DBn), translate(avar)),
                 ylab     = "",
                 yaxt     = "n",
                 xlab     = "",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )
            axis(2, pretty(dataset[[avar]]), las = 2)
            mtext(text = bquote("Anomaly [%]"),
                  cex  = 0.8,
                  side = 2,
                  line = 2.6)
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]"))

            abline(lm2)

            if (DRAFT) {
                ## Running mean years * months in data set
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                running_mean_window_years,
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")

                # points(dataset$Date, rm, col = "red", cex = 0.5)
                lines(dataset$Year[first:last], rm, col = "red", cex = 0.5)

                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Year[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Year)
                lines(dataset$Year, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }

            ## decorations
            fit <- lm2[[1]]

            legend("bottom", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 3),
                         "% / year")
            )

        }
        par(mfrow = c(1, 1)) ## just reset layout

        ## __ Extreme values table ---------------------------------------------
        wca <- c("Year", "Season", grep(paste0("^", strsplit(avar, "_")[[1]][1]), names(DB), value = T ))

        cat("\n \n \\footnotesize \n ")
        cat(
            pander(
                DB[order(abs(DB[[avar]]), decreasing = T )[1:5] , ..wca ],
                cap = "Extreme anomaly values"
            )
        )
        cat("\n \n \\normalsize \n ")
    }
}
#+ echo=F, include=F


# ____ by season  --------------------------------------------------------------

#+ SeasonalTrendsTogether, echo=F, include=T
# vars        <- c("DIR_att", "GLB_att")
vars        <- c("GLB_att_des")

## Monthly aggregation
dbs         <- c(  "ALL_1_D_bySeason_DESEAS",
                 "CLEAR_1_D_bySeason_DESEAS",
                 "CLOUD_1_D_bySeason_DESEAS")

Seasons     <- c("Winter", "Spring", "Summer", "Autumn")

## variable
for (avar in vars) {

    par(mfcol = c(length(Seasons), length(dbs)))
    cplots <- 0

    ## get global ylim
    gylim <- c()
    for (DBn in dbs) {
        DB   <- get(DBn)
        gylim <- c(gylim, range(DB[[avar]], na.rm = TRUE))
    }
    ylim <- range(gylim, na.rm = TRUE)


    ## sky conditions
    for (DBn in dbs) {
        DB <- get(DBn)

        for (ase in Seasons) {
            cplots <- cplots + 1

            dataset <- DB[ Season == ase, ]

            # if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(2, 3.6, 2, 0))

            plot(dataset$Year, dataset[[avar]],
                 ylim     = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .5,
                 main     = paste(ase, translate(DBn), translate(avar)),
                 ylab     = "",
                 yaxt     = "n",
                 xlab     = "",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )
            # axis(2, pretty(dataset[[avar]]), las = 2)
            axis(2, pretty(ylim), las = 2)
            if (cplots <= 4) {
                mtext(text = bquote("Anomaly [%]"),
                      cex  = 0.8,
                      side = 2,
                      line = 2.6)
            }
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]"))

            abline(lm2)

            if (DRAFT) {
                ## Running mean years * months in data set
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                running_mean_window_years,
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")

                # points(dataset$Date, rm, col = "red", cex = 0.5)
                lines(dataset$Year[first:last], rm, col = "red", cex = 0.5)

                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Year[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Year)
                lines(dataset$Year, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }

            ## decorations
            fit <- lm2[[1]]

            legend("bottom", lty = 1, bty = "n", lwd = 2, cex = 0.8,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 2),
                         "% / year")
            )
        }
    }
    par(mfrow = c(1, 1)) ## just reset layout
}
#+ echo=F, include=F






## ____ by season in a nice grid -----------------------------------------------

#+ SeasonalTrendsTogether2, echo=F, include=T
{
    vars        <- c("GLB_att_des")
    avar        <- vars[1]
    dbs         <- c(  "ALL_1_D_bySeason_DESEAS",
                       "CLEAR_1_D_bySeason_DESEAS",
                       "CLOUD_1_D_bySeason_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.2,   1,1,1, 0.1),
        heights = c(0.1, 1,1,1,1, 0.24)
    )
    layout.show(nf)

    # 1
    par("mar"=c(0,0,0,0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.6, y = 0.4,
         adj  = c(0.6,0.5),
         "All sky cond.",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.6, y = 0.4,
         adj  = c(0.5,0.5),
         "Clear sky cond.",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.6, y = 0.4,
         adj  = c(0.5,0.5),
         "Cloudy sky cond.", cex = 0.9, font = 2)

    # 5
    plot.new()



    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }
        if (i == 10) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }

        if (i == 11) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        # ## fill right column
        # if (i %in% c(10, 15, 20, 25)) {
        #     plot.new()
        # }

        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {

            kk       <- expanded[1,]
            expanded <- expanded[-1, ]

            DB      <- get(kk$Dataset)
            dataset <- DB[ Season == kk$Seasons, ]

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(1, 2.5, 1, 0))

            plot(dataset$Year, dataset[[avar]],
                 ylim     = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .5,
                 ylab     = "",
                 yaxt     = "n",
                 xlab     = "",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )
            # axis(2, pretty(dataset[[avar]]), las = 2)

            axis(2, pretty(ylim), las = 2, cex.axis = 0.8)

            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]"))

            abline(lm2)


            ## decorations
            fit <- lm2[[1]]

            legend("top", lty = 1, bty = "n", lwd = 1, cex = 0.8,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 2),
                         "% / year")
            )

            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Anomaly [%]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }

            par("mar" = c(0,0,0,0))
        }

    }

    # ## fill bottom empty row
    # for (i in 1:5) {
    #     plot.new()
    # }
    #

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.6, y = 0.3,
         adj  = c(0.6,0.5),
         "All sky cond.",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.6, y = 0.3,
         adj  = c(0.5, 0.5),
         "Clear sky cond.",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.6, y = 0.3,
         adj  = c(0.5, 0.5),
         "Cloudy sky cond.", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1,1))
}
#+ echo=F, include=F


#'
#' ### Grid of trends for each season of year from daily
#'
#+ echo=F, include=F

## ____ by season in a tight grid ----------------------------------------------
#+ SeasonalTrendsTogether3, echo=F, include=T
{
    vars        <- c("GLB_att_des")
    avar        <- vars[1]
    dbs         <- c(  "ALL_1_D_bySeason_DESEAS",
                       "CLEAR_1_D_bySeason_DESEAS",
                       "CLOUD_1_D_bySeason_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.25,   1,1,1, 0.05),
        heights = c(0.1,  1,1,1,1, 0.5 )
    )
    layout.show(nf)

    # 1
    par("mar" = c(0, 0, 0, 0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.5,0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.5,0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }
        if (i == 10) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }

        if (i == 11) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {

            kk       <- expanded[1,]
            expanded <- expanded[-1, ]

            DB      <- get(kk$Dataset)
            dataset <- DB[ Season == kk$Seasons, ]

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(0, 0, 0, 0))

            plot(dataset$Year, dataset[[avar]],
                 ylim     = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .6,
                 ylab     = "",
                 xlab     = "",
                 xaxt     = "n",
                 yaxt     = "n",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )

            ## y axis
            if (i %in% c(7,12,17,22)){
                axis(2, pretty(ylim), las = 2, cex.axis = 0.8)
            } else {
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck = -0.03)
            }

            ## x axis
            if (i %in% c(22, 23, 24)) {
                ## bottom row axis
                axis(1, pretty(dataset$Year), las = 1, cex.axis = 0.8, line =  0,   labels = NA)
                axis(1, pretty(dataset$Year), las = 1, cex.axis = 0.8, line = -0.5, tck = 0, lwd = 0)
                ## minor ticks
                axis(1, at = seq(1993, year(Sys.time()), by = 1), labels = NA,
                        tcl = -0.25)
            } else {
                ## major ticks
                axis(1, pretty(dataset$Year), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(1, pretty(dataset$Year), cex.axis = 0.8, labels = NA, tck = -0.03)
                ## minor ticks
                axis(1, at = seq(1993, year(Sys.time()), by = 1), labels = NA,
                     tcl = -0.25/2)
                axis(1, at = seq(1993, year(Sys.time()), by = 1), labels = NA,
                     tcl = +0.25/2)

            }

            abline(lm2)


            ## decorations
            fit <- lm2[[1]]

            legend("top", lty = 1, bty = "n", lwd = 1, cex = 0.8,
                   inset = 0.1,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 2),
                         "% / year")
            )

            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Anomaly [%]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }

            par("mar" = c(0,0,0,0))
        }
    }

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.5, y = 0.25,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.25,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.25,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1,1))
}
#+ echo=F, include=F











## __ Calculate trends for each season  ----------------------------------------
vars        <- c("DIR_att_des", "GLB_att_des")

## vars are  set above
gather_seas <- data.frame()
for (DBn in dbs) {
    DB <- get(DBn)

    for (ase in Seasons) {
        for (avar in vars) {
            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm( dataset[[avar]] ~ dataset$Year )

            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm2,
                                                      confidence_interval = Daily_confidence_limit),
                                     DATA   = DBn,
                                     Season = ase,
                                     var    = avar,
                                     N      = sum(!is.na(dataset[[avar]]))
                                 ))
        }
    }
}

## __ Display data table  ------------------------------------------------------
#'
#' \newpage
#' \FloatBarrier
#'
#' #### Table of trends by season.
#'
#+ echo=F, include=T

wecare           <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas      <- data.table(gather_seas)
gather_seas$DATA <- sub("_.*", "", gather_seas$DATA)
pprint           <- gather_seas[ , ..wecare]

pprint[, slope.stat_sig     := 100 * (1 - slope.p)]
pprint[, slope.t            := NULL               ]
pprint[, Rsqrd              := NULL               ]
pprint[, RsqrdAdj           := NULL               ]
pprint[, N                  := NULL               ]
pprint[, slope.ConfInt_0.99 := NULL               ]
pprint[, slope.ConfInt_0.95 := NULL               ]
pprint[, DATA               := translate(DATA)    ]
pprint[, var                := translate(var)     ]

setorder(pprint, DATA, var)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F

write_dat(pprint,
          "./figures/tbl_longterm_trends_season.dat",
          clean = TRUE)






##  MONTHLY TRENDS  ############################################################

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Monthly trends
#'
#' We calculated monthly means from the daily means and the we produce the
#' seasonal data and the departure from the seasonal value in %.
#'
#+ echo=F, include=F


## __ Plot each month ----------------------------------------------------------

#+ TrendByMonth, echo=F, include=T, results="asis", fig.width=7, fig.height=11, out.height="97%"
# vars   <- c("DIR_att", "GLB_att")
vars   <- c("GLB_att_des")

## Monthly aggregation
dbs <- c(  "ALL_1_D_monthly_DESEAS",
         "CLEAR_1_D_monthly_DESEAS",
         "CLOUD_1_D_monthly_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[, Month := month(Date) ]
    ## sanity check
    stopifnot(!any(is.na(DB$Month)))

    if (!FIGURESGRID) {
        cat("\n\\newpage\n")
        cat("\n#### ", translate(DBn), "\n\n")
    }

    for (avar in vars) {

        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(6, 2))
            cat("\n\\newpage\n")
            cat("\n#### ", translate(DBn), translate(avar) , "\n\n")
        }

        ## common range
        ylim <- range(DB[[avar]],na.rm = TRUE)

        for (ase in 1:12) {

            dataset <- DB[Month == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm( dataset[[avar]] ~ dataset$Year )

            ## plot
            par("mar" = c(2, 3.4, 2, 0.5))

            plot(dataset$Year, dataset[[avar]],
                 # ylim = ylim,
                 pch  = 19,
                 col  = get(paste0(c("col",
                                     unlist(strsplit(avar, split = "_" ))[1:2]),
                                   collapse = "_")),
                 cex      = 0.5,
                 main     = paste(month.name[ase], translate(DBn), translate(avar)),
                 xlab     = "",
                 ylab     = bquote("Anomaly [%]"),
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp      = c(2, 0.5, 0)
            )
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            abline(lm2)


            if (DRAFT) {
                ## plot running mean
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                running_mean_window_years,
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")
                lines(dataset$Year[first:last], rm, col = "red")


                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Year[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Year)
                lines(dataset$Year, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }


            ## decorations
            fit <- lm2[[1]]

            legend("bottom", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) "+" else "-",
                         signif(abs(fit[2]), 3),
                         "% per year")
            )

        }
        par(mfrow = c(1, 1)) ## just reset layout


        ## __ Extreme values table ---------------------------------------------
        wca <- c("Year", "Month", grep(paste0("^", strsplit(avar, "_")[[1]][1]), names(DB), value = T ))

        cat("\n \n \\scriptsize \n ")
        cat(
            pander(
                DB[ order(abs(DB[[avar]]), decreasing = T )[1:10] , ..wca ],
                cap = "Extreme anomaly values"
            )
        )
        cat("\n \n \\normalsize \n ")

    }
}
#+ echo=F, include=F


## __ Calculate trend by each month  -------------------------------------------
vars        <- c("DIR_att_des", "GLB_att_des")
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

            ## linear model counting years
            lm2  <- lm( dataset[[avar]] ~ dataset$Year )

            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm2,
                                                      confidence_interval = Daily_confidence_limit),
                                     DATA      = DBn,
                                     Month     = ase,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]]))
                                 ))
        }
    }
}

## __ Display table with trends and stats  -------------------------------------
#'
#' \FloatBarrier
#'
#' #### Table of trends by month.
#'
#+ echo=F, include=T

wecare        <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas   <- data.table(gather_seas)
pprint        <- gather_seas[, ..wecare]
pprint[, DATA := translate(DATA)]
pprint[, var  := translate(var) ]

pprint[, slope.stat_sig     := 100 * (1 - slope.p)]
pprint[, slope.t            := NULL]
pprint[, Rsqrd              := NULL]
pprint[, RsqrdAdj           := NULL]
pprint[, slope.ConfInt_0.99 := NULL]
pprint[, slope.ConfInt_0.95 := NULL]

setorder(pprint, DATA, var, Month)


# \scriptsize
# \footnotesize
# \small

#+ echo=F, include=T
#' \scriptsize
#+ echo=F, include=T
pander(pprint,
           cap = "Slope is in %/year")
#'
#' \normalsize
#+ echo=F, include=T

write_dat(pprint,
          "./figures/tbl_longterm_trends_monthly.dat",
          clean = TRUE)





#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
