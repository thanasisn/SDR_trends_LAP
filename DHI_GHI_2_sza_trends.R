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
knitr::opts_chunk$set(dev        = c("pdf", "png")    )
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
# knitr::opts_chunk$set(fig.pos    = '!h'    )
warning("Don't use cache it breaks computations")

#+ include=F, echo=F
## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./DHI_GHI_2_sza_trends.R"

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
FIGURESGRID <- FALSE

## choose loess criterion for span
LOESS_CRITERIO <-  c("aicc", "gcv")[1]

## cex value for side by side
ccex_sbs <- 1.3


#+ echo=F, include=T
#'
#' ## 2. Long term by SZA
#'
#' ### Data info
#'
#' Time data span `r range(ALL_1_daily_DESEAS$Date)`
#'
#' ### Process
#'
#+ echo=F, include=F


# ## ____ Scatter plots with SZA all data ----------------------------------------
# data_list <- c(  "ALL_2_daily_DESEAS",
#                "CLEAR_2_daily_DESEAS",
#                "CLEAR_2_daily_DESEAS")
# ## x variables
# by_var    <- c("doy","SZA")
# for (i in data_list) {
#     ## get data and y vars to plot
#     Dplot  <- get(i)
#     wecare <- grep("HOR|GLB|DIR", names(Dplot), value = T)
#     ## loop existing x vars
#     for (xvar in names(Dplot)[names(Dplot) %in% by_var]) {
#         for (yvar in wecare) {
#             col <- get(paste0(c("col", unlist(strsplit(yvar, split = "_"))[1:2]),
#                               collapse = "_"))
#             vect <- Dplot[[yvar]]
#             plot(Dplot[[xvar]], vect,
#                  pch  = 19,
#                  cex  = .3,
#                  col  = col,
#                  main = paste(i, yvar),
#                  xlab = xvar, ylab = yvar)
#         }
#     }
# }
#
# ## ____ Histograms Plots all data ----------------------------------------------
# for (i in data_list) {
#     ## get data and y vars to plot
#     Dplot  <- get(i)
#     wecare <- grep("HOR|GLB|DIR", names(Dplot), value = TRUE)
#     for (yvar in wecare) {
#         if (!yvar %in% names(Dplot)) next()
#         col <- get(paste0(c("col", unlist(strsplit(yvar,split = "_" ))[1:2]),
#                           collapse = "_"))
#         hist(Dplot[[yvar]],
#              main   = paste(i, yvar),
#              xlab   = yvar,
#              breaks = 100, col = col)
#     }
# }
# #+ echo=F, include=F
# rm(data_list)











##  SZA trends for all year  ---------------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Plot of SZA trends
#'
#+ echo=F, include=F


## __ Calculate SZA ~ Year -----------------------------------------------------

#  vars <- c("DIR_att_des", "GLB_att_des", "DIR_transp_des")
vars <- c("DIR_att_des", "GLB_att_des")
dbs  <- c(  "ALL_2_daily_DESEAS",
          "CLEAR_2_daily_DESEAS",
          "CLOUD_2_daily_DESEAS")

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
                                    linear_fit_stats(lm1),
                                    preNoon   = anoon,
                                    SZA       = asza,
                                    DATA      = DBn,
                                    var       = avar,
                                    N         = sum(!is.na(dataset[[avar]]))
                                ))
            }
        }
    }
}

#+ echo=F, include=F
hist( gather$N[gather$N > 50], breaks = 100)

szatrends <- data.table(gather)
setorder(szatrends, SZA)


## covert to trend per year
szatrends[, slope    := slope    * Days_of_year ]
szatrends[, slope.sd := slope.sd * Days_of_year ]

## set some plot option for data
szatrends[var == "DIR_att_des",    col := col_DIR_att    ]
szatrends[var == "GLB_att_des",    col := col_GLB_att    ]
szatrends[var == "DIR_transp_des", col := col_DIR_transp ]
szatrends[preNoon == T, pch := pch_am ]
szatrends[preNoon == F, pch := pch_pm ]



# hist(szatrends[DATA == dbs[1], N], breaks = 100)
# hist(szatrends[DATA == dbs[2], N], breaks = 100)

# hist(szatrends[var == vars[1], N], breaks = 100)
# hist(szatrends[var == vars[2], N], breaks = 100)

# szatrends <- szatrends[ N > 50]

# plot(szatrends$SZA,szatrends$N)

test1 <- szatrends[DATA == "CLEAR_2_daily_DESEAS" & var == "DIR_att_des"]
test2 <- szatrends[DATA == "CLEAR_2_daily_DESEAS" & var == "GLB_att_des"]
# plot(test1$SZA, test1$N, pch = 19)
# abline(h=50)
# plot(test2$SZA, test2$N, pch = 19)
# abline(h=300)

# szatrends[ var == "GLB_att_des"    & N <= 300, slope := NA ]
# szatrends[ var == "DIR_att_des"    & N <=  50, slope := NA ]
# szatrends[ var == "DIR_transp_des" & N <=  50, slope := NA ]



## __ Plot SZA ~ Year stats ----------------------------------------------------

## stats vars to plot
wecare <- grep("^slope|^N",  names(szatrends), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)

## TODO separate plots by direct global

#+ SzaTrends, echo=F, include=T, results = "asis"
## DIR - GLB - transp
for (avar in unique(szatrends$var)) {
    ## ALL - CS
    for (type in unique(szatrends$DATA)) {

        cat("\n\\newpage\n\n")
        cat("\n#### ", translate(type), translate(avar) , "\n\n")

        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(ceiling(length(wecare)/2), 2))
        }

        par("mar" = c(4, 5, 2, 2))

        ## statistic variable
        for (awe in wecare) {
            awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

            ## Replace variable name
            if (awename == "Slope") { awename <- "Anomaly Trend [%/y]" }

            ## limit plot p-values
            p_lim     <- 0.05

            szatrends <- data.table(szatrends)

            ## select All/CS and DIR/GLB/trans
            subdata <- szatrends[DATA == type &
                                 var  == avar, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


            ## plot only under accepted p-value limit
            # subdata <- subdata[ slope.p < p_lim, ]

            xlim <- range(subdata$SZA,    na.rm = T)
            ylim <- range(subdata[[awe]], na.rm = T)

            pam  <- subdata[preNoon == TRUE ]
            ppm  <- subdata[preNoon == FALSE]

            ccex <- ccex_sbs
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            if (DRAFT == TRUE) {
                par("mar" = c(4,   5,   2,   2))
            } else {
                par("mar" = c(4, 4.5, 0.5, 0.5))
            }

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = awename,
                 xlim = xlim,
                 ylim = ylim,
                 yaxt = "n")

            ## y axis
            axis(2, pretty(ylim), las = 2)

            ## x axis
            axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                      tcl = -0.25)
            title(xlab = bquote("Solar zenith angle (SZA)"),
                  line = 2.5)

            ## zero line
            abline(h = 0, lty = 3)

            if (DRAFT == TRUE) {
                # title(paste(translate(avar), awename, "for", translate(type)), cex.main =  .8 * ccex)
                title(paste(translate(avar), "trend", "for", translate(type)), cex.main =  .8 * ccex)
            } else {
                legend("bottom", 0, translate(type),
                       bty   = "n",
                       cex   = .8 * ccex,
                       xjust = 0.5,      # 0.5 means center adjusted
                       yjust = 0.5,      # 0.5 means center adjusted
                       x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                       y.intersp =  0.2, # adjust character interspacing to effect box height
                       adj = c(0, 0.5))  # adjust string position (default values used here)
                # text.font = 2)  # bold the text if you like (not used here)
                par("mar" = c(4,   5,   2,   2))
            }



            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch = pam$pch,
                   col = 2,
                   cex = 1)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch = ppm$pch,
                   col = 3,
                   cex = 1)

            legend("top",
                   legend = c("Morning", "Evening"),
                   # col    = c(unique(pam$col), unique(ppm$col)),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

            ccex <- 1
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            cat("\n\n")
        }
        par(mfrow = c(1, 1)) ## just reset layout
    }
}







##  SZA trends for season of year  ---------------------------------------------

#'
#' ## Plot of SZA trends for each season of year
#'
#+ echo=F, include=F


## __ Calculate SZA ~ Season stats  --------------------------------------------


# vars        <- c("DIR_att_des", "GLB_att_des", "DIR_transp_des")
vars        <- c("DIR_att_des", "GLB_att_des")
vars        <- c("GLB_att_des")


dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                 "CLEAR_2_bySeason_daily_DESEAS",
                 "CLOUD_2_bySeason_daily_DESEAS")
seasons     <- c("Winter", "Spring", "Summer", "Autumn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    stopifnot( !any(is.na(DB$Season)) )

    for (ase in seasons) {
        for (avar in vars) {
            for (anoon in unique( DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & Season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1) next()

                    lm1 <- lm(dataset[[avar]] ~ dataset$Date)

                    gather_seas <- rbind(gather_seas,
                                    data.frame(
                                        linear_fit_stats(lm1),
                                        preNoon   = anoon,
                                        SZA       = asza,
                                        DATA      = DBn,
                                        var       = avar,
                                        Season    = ase,
                                        N         = sum(!is.na(dataset[[avar]]))
                                    ))
                }
            }
        }
    }
}
#+ echo=F, include=F
hist(gather_seas$N[gather_seas$N > 50], breaks = 100)

szatrends_seas <- data.table(gather_seas)
setorder(szatrends_seas, SZA)


## covert to trend per year
szatrends_seas[, slope    := slope    * Days_of_year ]
szatrends_seas[, slope.sd := slope.sd * Days_of_year ]

## define plot colors
szatrends_seas[ var == "DIR_att_des",    col := col_DIR_att    ]
szatrends_seas[ var == "GLB_att_des",    col := col_GLB_att    ]
szatrends_seas[ var == "DIR_transp_des", col := col_DIR_transp ]
szatrends_seas[ preNoon == T, pch := pch_am ]
szatrends_seas[ preNoon == F, pch := pch_pm ]



# hist(szatrends_seas[DATA == dbs[1],N],  breaks = 100)
# hist(szatrends_seas[DATA == dbs[2],N],  breaks = 100)
# hist(szatrends_seas[DATA == dbs[3],N],  breaks = 100)
# hist(szatrends_seas[var  == vars[1],N], breaks = 100)
# hist(szatrends_seas[var  == vars[2],N], breaks = 100)

# szatrends_seas <- szatrends_seas[ N > 50]

# plot(szatrends_seas$SZA, szatrends_seas$N)

# test <- szatrends_seas[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == "DIR_att_des" ]
# plot(test$SZA, test$N, pch = 19)


szatrends[ N <= 30, slope := NA]


# test1 <- szatrends_seas[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == "DIR_att_des" ]
# test2 <- szatrends_seas[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == "GLB_att_des" ]
# plot(test1$SZA, test1$N, pch = 19)
# plot(test2$SZA, test2$N, pch = 19)

# szatrends[ var == "GLB_att_des"    & N <= 300, slope := NA ]
# szatrends[ var == "DIR_att_des"    & N <=  50, slope := NA ]
# szatrends[ var == "DIR_transp_des" & N <=  50, slope := NA ]




## __ Plot SZA ~ Season stats  -------------------------------------------------


## stats vars to plot
wecare <- grep( "^slope|^N", names(szatrends_seas), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)

wecare <- "slope"

# FIGURESGRID <- TRUE

#+ SzaTrendsSeas, echo=F, include=T, results = "asis"
## Winter - Summer ....
for (ase in seasons) {
    ## ALL - Clear sky
    for (type in unique(szatrends_seas$DATA)) {
        ## DIR - GLB - transp
        for (avar in unique(szatrends_seas$var)) {

            cat("\n\\newpage\n\n")
            cat(paste("###",ase, translate(type), translate(avar),"\n\n"))

            ## plot in a grid
            if (FIGURESGRID) {
                par(mfrow = c(ceiling(length(wecare)/2), 2))
            }


            ## statistic variable
            for (awe in wecare) {
                awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

                par("mar" = c(4,4,1,0))

                ## limit plot p-values
                p_lim     <- 0.05

                ## select All/CS  DIR/GLB/trans winter/summer
                subdata <- szatrends_seas[ DATA   == type &
                                           var    == avar &
                                           Season == ase    , ]

                ## set symbols for plotting
                subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
                subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
                subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
                subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


                # xlim <- range( subdata$SZA,        na.rm = T )
                ## use same axis for all
                xlim <- range(szatrends_seas$SZA, na.rm = T)
                ylim <- range(subdata[[awe]],     na.rm = T)

                ## test always show zero on plots
                ylim <- range(0, subdata[[awe]], na.rm = T)


                pam  <- subdata[ preNoon == T ]
                ppm  <- subdata[ preNoon == F ]


                ccex <- ccex_sbs
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                if (DRAFT == TRUE) {
                    par("mar" = c(4,   5,   2,   2))
                } else {
                    par("mar" = c(4.5, 4.5, 0.5, 0.5))
                }


                plot(1, type = "n",
                     xlab = "",
                     ylab = awename,
                     xlim = xlim,
                     ylim = ylim,
                     yaxt = "n")

                ## y axis
                axis(2, pretty(ylim), las = 2)

                ## x axis
                axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                     tcl = -0.25)
                title(xlab = bquote("Solar zenith angle (SZA)"),
                      line = 2.5)

                ## zero line
                abline(h = 0, lty = 3)

                ## test for some plots
                if (grepl("CLEAR", type, ignore.case = T)) typeP <- "Clear Sky (Deseas.)"
                if (grepl("ALL",   type, ignore.case = T)) typeP <- "All Sky (Deseas.)"

                title(paste(ase, awename, typeP, translate(avar)), cex.main = 0.8,)



                ## morning lines
                lines(pam$SZA, pam[[awe]],
                      col  = 2,
                      type = "c",
                      lwd  = ccex,
                      cex = 1)
                ## morning points
                points(pam$SZA, pam[[awe]],
                       pch = pam$pch,
                       col = 2,
                       cex = 1)

                ## evening lines
                lines(ppm$SZA, ppm[[awe]],
                      col  = 3,
                      type = "c",
                      lwd  = ccex,
                      cex = 1)
                ## evening points
                points(ppm$SZA, ppm[[awe]],
                       pch = ppm$pch,
                       col = 3,
                       cex = 1)

                # legend("top",
                #        legend = c("Morning", "Evening"),
                #        # col    = c(unique(pam$col), unique(ppm$col)),
                #        col    = c( 2,  3),
                #        pch    = c(16, 17), ncol = 2, bty = "n",
                #        cex    = ccex)

                ## reset fonts
                ccex <- 1
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                legend("bottom",
                       legend = c("Morning",       "Evening"),
                       # col    = c(unique(pam$col), unique(ppm$col)),
                       col    = c(2, 3),
                       pch    = c(16, 17), ncol = 2, bty = "n")
            }
            par(mfrow = c(1, 1)) ## just reset layout
        }
    }
}








#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
