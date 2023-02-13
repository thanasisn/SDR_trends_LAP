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
#' ## 2. Long term by SZA
#'
#+ echo=F, include=F
## ~ Plot longterm scatter plots  ####
data_list  <- list(ALL   = ALL_2_daily_mean,
                   CLEAR = CLEAR_2_daily_mean)
by_var     <- c("doy","SZA")
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
## ~ Plots seasonal data scatter plots ####
data_list  <- list(ALL_Seas   =   ALL_2_daily_seas,
                   CLEAR_Seas = CLEAR_2_daily_seas)
by_var     <- c("doy")
wecare     <- unique(unlist(lapply(data_list, names)))
wecare     <- grep("HOR|GLB|DIR", wecare, value = T)
for (i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    for (xvar in by_var){
        for (yvar in wecare) {
            if (!yvar %in% names(Dplot)) next()
            col  <- get(paste0(c("col",unlist(strsplit(yvar,split = "_" ))[1:2]),collapse = "_"))
            vect <- Dplot[[yvar]]
            plot(Dplot[[xvar]], vect,
                 pch = ".", col = col,
                 main = paste(names(data_list[i]), yvar),
                 xlab = xvar, ylab = yvar)
        }
    }
}
## ~ Plots seasonal data histograms ####
for (i in 1:length(data_list)) {
    Dplot <- data_list[[i]]
    for (yvar in wecare) {
        if (!yvar %in% names(Dplot)) next()
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

ALL_daily_DEseas   <- merge(  ALL_2_daily_mean, ALL_2_daily_seas,   by = c("doy", "SZA", "preNoon"), all = T)
CLEAR_daily_DEseas <- merge(CLEAR_2_daily_mean, CLEAR_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)

setorder(ALL_daily_DEseas,Date)
setorder(CLEAR_daily_DEseas,Date)


## anomaly
# #' #### Use the actuar difference from seasonal
# ALL_daily_DEseas[   , DIR_att    := DIR_att    - DIR_att_seas    ]
# ALL_daily_DEseas[   , GLB_att    := GLB_att    - GLB_att_seas    ]
# ALL_daily_DEseas[   , DIR_transp := DIR_transp - DIR_transp_seas ]
# CLEAR_daily_DEseas[ , DIR_att    := DIR_att    - DIR_att_seas    ]
# CLEAR_daily_DEseas[ , GLB_att    := GLB_att    - GLB_att_seas    ]
# CLEAR_daily_DEseas[ , DIR_transp := DIR_transp - DIR_transp_seas ]



##TODO margin of error for anomaly!!!!




## relative anomaly
#' #### Use the % difference from seasonal values
#+ echo=F, include=T
ALL_daily_DEseas[  , DIR_att   := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
ALL_daily_DEseas[  , HOR_att   := 100 * ( HOR_att    - HOR_att_seas    ) / HOR_att_seas    ]
ALL_daily_DEseas[  , GLB_att   := 100 * ( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
ALL_daily_DEseas[  , DIR_transp:= 100 * ( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
CLEAR_daily_DEseas[, DIR_att   := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
CLEAR_daily_DEseas[, HOR_att   := 100 * ( HOR_att    - HOR_att_seas    ) / HOR_att_seas    ]
CLEAR_daily_DEseas[, GLB_att   := 100 * ( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
CLEAR_daily_DEseas[, DIR_transp:= 100 * ( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
#+ echo=F, include=F



####  Plot of SZA trends for all year ####
#' \newpage
#' ## Plot of SZA trends
#+ echo=F, include=F
timefactor <- 1
vars <- c("DIR_att", "GLB_att", "DIR_transp")
dbs  <- c("ALL_daily_DEseas", "CLEAR_daily_DEseas")

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

szatrends <- gather


szatrends <- data.table(szatrends)
setorder(szatrends,SZA)



## covert to trend per year
szatrends[, slope    := slope    * Days_of_year ]
szatrends[, slope.sd := slope.sd * Days_of_year ]

## set some plot option for data
szatrends[ var == "DIR_att",    col := col_DIR_att    ]
szatrends[ var == "GLB_att",    col := col_GLB_att    ]
szatrends[ var == "DIR_transp", col := col_DIR_transp ]
szatrends[ preNoon == T, pch := pch_am ]
szatrends[ preNoon == F, pch := pch_pm ]



hist(szatrends[DATA==dbs[1],N], breaks = 100)
hist(szatrends[DATA==dbs[2],N], breaks = 100)

hist(szatrends[var==vars[1],N], breaks = 100)
hist(szatrends[var==vars[2],N], breaks = 100)

# szatrends <- szatrends[ N > 50]


plot(szatrends$SZA,szatrends$N)

test1 <- szatrends[ DATA == "CLEAR_daily_DEseas" & var == "DIR_att" ]
test2 <- szatrends[ DATA == "CLEAR_daily_DEseas" & var == "GLB_att" ]
plot(test1$SZA, test1$N, pch = 19)
abline(h=50)
plot(test2$SZA, test2$N, pch = 19)
abline(h=300)

# szatrends[ var == "GLB_att"    & N <= 300, slope := NA ]
# szatrends[ var == "DIR_att"    & N <=  50, slope := NA ]
# szatrends[ var == "DIR_transp" & N <=  50, slope := NA ]





## stats vars to plot
wecare <- grep( "^slope|^N",names(szatrends),ignore.case = T, value = T)
wecare <- grep("^slope\\.t",wecare,ignore.case = T,value = T, invert = T)


#+ szatrends, echo=F, include=T, results = "asis"
## ALL - CS
for (type in unique(szatrends$DATA)) {
    ## DIR - GLB - transp
    for (avar in unique(szatrends$var)) {

        cat("\n\\newpage\n\n")
        cat(paste("\n###", type, avar,"\n\n"))

        par("mar" = c(3,4,2,1))

        ## statistic variable
        for (awe in wecare) {
            awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

            ## select All/CS and DIR/GLB/trans
            subdata <- szatrends[ szatrends$DATA == type &    ##
                                  szatrends$var  == avar , ]

            xlim <- range( subdata$SZA,    na.rm = T )
            ylim <- range( subdata[[awe]], na.rm = T )

            pam  <- subdata[ preNoon == T ]
            ppm  <- subdata[ preNoon == F ]

            plot(1, type = "n",
                 xlab = "SZA", ylab = awename,
                 xlim = xlim, ylim = ylim )

            abline(h = 0, lty = 3 )

            title(paste(awename, type, translate(avar) ), cex.main = 1)

            # lines(pam$SZA, pam[[awe]], pch =  pch_am, col = pam$col, type = "b")
            # lines(pam$SZA, ppm[[awe]], pch =  pch_pm, col = pam$col, type = "b")

            lines(pam$SZA, pam[[awe]], pch =  pch_am, col = 2, type = "b", lwd = 2)
            lines(ppm$SZA, ppm[[awe]], pch =  pch_pm, col = 3, type = "b", lwd = 2)

            legend("top",
                   legend = c("Morning",       "Evening"),
                   # col    = c(unique(pam$col), unique(ppm$col)),
                   col    = c(2, 3),
                   pch    = c(unique(pam$pch), unique(ppm$pch)), ncol = 2, bty = "n")

            cat("\n\n")
        }
    }
}








####  Plot of SZA trends for each season of year ####
#' \newpage
#' ## Plot of SZA trends for each season of year
#+ echo=F, include=F
timefactor  <- 1  ## to display % per year
vars        <- c("DIR_att", "GLB_att", "DIR_transp")
dbs         <- c("ALL_daily_DEseas", "CLEAR_daily_DEseas")
seasons     <- c("Winter", "Spring", "Summer", "Autumn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Autumn"]

    stopifnot( !any(is.na(DB$Season)) )

    for (ase in seasons) {
        for (avar in vars) {
            for (anoon in unique( DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & Season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1) next()

                    lm1 <- lm( dataset[[avar]] ~ dataset$Date )

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
szatrends_seas[ var == "DIR_att",    col := col_DIR_att    ]
szatrends_seas[ var == "GLB_att",    col := col_GLB_att    ]
szatrends_seas[ var == "DIR_transp", col := col_DIR_transp ]
szatrends_seas[ preNoon == T, pch := pch_am ]
szatrends_seas[ preNoon == F, pch := pch_pm ]





hist(szatrends_seas[DATA == dbs[1],N],  breaks = 100)
hist(szatrends_seas[DATA == dbs[2],N],  breaks = 100)
hist(szatrends_seas[var  == vars[1],N], breaks = 100)
hist(szatrends_seas[var  == vars[2],N], breaks = 100)

# szatrends_seas <- szatrends_seas[ N > 50]

plot(szatrends_seas$SZA,szatrends_seas$N)

test <- szatrends_seas[ DATA == "CLEAR_daily_DEseas" & var == "DIR_att" ]
plot(test$SZA, test$N, pch = 19)
abline(h=50/4)

szatrends[ N <= 30, slope := NA]


test1 <- szatrends_seas[ DATA == "CLEAR_daily_DEseas" & var == "DIR_att" ]
test2 <- szatrends_seas[ DATA == "CLEAR_daily_DEseas" & var == "GLB_att" ]
plot(test1$SZA, test1$N, pch = 19)
abline(h=50/4)
plot(test2$SZA, test2$N, pch = 19)
abline(h=300/4)

# szatrends[ var == "GLB_att"    & N <= 300, slope := NA ]
# szatrends[ var == "DIR_att"    & N <=  50, slope := NA ]
# szatrends[ var == "DIR_transp" & N <=  50, slope := NA ]



## stats vars to plot
wecare <- grep( "^slope|^N",names(szatrends_seas),ignore.case = T, value = T)
wecare <- grep("^slope\\.t",wecare,ignore.case = T,value = T, invert = T)


#+ szatrendsseas, echo=F, include=T, results = "asis"
## Winter - Summer ....
for (ase in seasons) {
    ## ALL - Clear sky
    for (type in unique(szatrends_seas$DATA)) {
        ## DIR - GLB - transp
        for (avar in unique(szatrends_seas$var)) {

            cat("\n\\newpage\n\n")
            cat(paste("###",ase, type, avar,"\n\n"))

            ## statistic variable
            for (awe in wecare) {
                awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

                par("mar" = c(4,4,1,0))

                ## select All/CS  DIR/GLB/trans winter/summer
                subdata <- szatrends_seas[ DATA   == type &
                                           var    == avar &
                                           Season == ase    , ]

                # xlim <- range( subdata$SZA,        na.rm = T )
                ## use same axis for all
                xlim <- range( szatrends_seas$SZA, na.rm = T )


                ylim <- range( subdata[[awe]], na.rm = T )

                ## test always show zero on plots
                ylim <- range(0, subdata[[awe]], na.rm = T )


                pam  <- subdata[ preNoon == T ]
                ppm  <- subdata[ preNoon == F ]


                plot(1, type = "n",
                     xlab = "SZA", ylab = awename,
                     xlim = xlim, ylim = ylim )

                abline(h = 0, lty = 3 )

                ## test for some plots
                if (grepl("CLEAR",type, ignore.case = T) ) typeP <- "Clear Sky (Deseas.)"
                if (grepl("ALL",  type, ignore.case = T) ) typeP <- "All Sky (Deseas.)"

                title(paste(ase, awename, typeP, translate(avar)), cex.main = 1 )

                # lines(pam$SZA, pam[[awe]], pch =  pch_am, col = pam$col, type = "b")
                # lines(pam$SZA, ppm[[awe]], pch =  pch_pm, col = pam$col, type = "b")

                lines(pam$SZA, pam[[awe]], pch =  pch_am, col = 2, type = "b", lwd = 2)
                lines(ppm$SZA, ppm[[awe]], pch =  pch_pm, col = 3, type = "b", lwd = 2)

                legend("bottom",
                       legend = c("Morning",       "Evening"),
                       # col    = c(unique(pam$col), unique(ppm$col)),
                       col    = c(2, 3),
                       pch    = c(unique(pam$pch), unique(ppm$pch)), ncol = 2, bty = "n")
            }
        }
    }
}








#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
