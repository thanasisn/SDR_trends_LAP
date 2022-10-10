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
    sink( file = paste0("~/MANUSCRIPTS/2022_sdr_trends/runtime/",  basename(sub("\\.R$",".out", Script.Name))),split=TRUE)
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
#'
#' ## 2. Long term by SZA


#' #### Calculate daily SZA means ####
#+ echo=F, include=T

ALL_2_daily_mean <- DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                 HOR_att       = mean(HOR_att,    na.rm = T),
                                 GLB_att       = mean(GLB_att,    na.rm = T),
                                 DIR_transp    = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                 DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                 doy           = yday(Date),
                                 GLB_att_N     = sum(!is.na(GLB_att)),
                                 HOR_att_N     = sum(!is.na(HOR_att)),
                                 DIR_att_N     = sum(!is.na(DIR_att))  ),
                           by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                  Date    = Day,
                                  preNoon = preNoon  ) ]


CLEAR_2_daily_mean <- DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                     HOR_att       = mean(HOR_att,    na.rm = T),
                                     GLB_att       = mean(GLB_att,    na.rm = T),
                                     DIR_transp    = mean(DIR_transp, na.rm = T),
                                     DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                     HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                     GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                     DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                     doy           = yday(Date),
                                     GLB_att_N     = sum(!is.na(GLB_att)),
                                     HOR_att_N     = sum(!is.na(HOR_att)),
                                     DIR_att_N     = sum(!is.na(DIR_att))  ),
                               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                                      Date    = Day,
                                      preNoon = preNoon ) ]



#' #### Margin of error calculation for `r SZA_confidence_limit` confidence interval ####
#+ echo=T, include=T

conf_param  <- 1-(1-SZA_confidence_limit)/2
suppressWarnings({
ALL_2_daily_mean[,   DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
ALL_2_daily_mean[,   HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
ALL_2_daily_mean[,   GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
ALL_2_daily_mean[,   DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
CLEAR_2_daily_mean[, DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
CLEAR_2_daily_mean[, HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
CLEAR_2_daily_mean[, GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
CLEAR_2_daily_mean[, DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
})
#+ echo=F, include=F


#' #### Exclude means with less than `r SZA_aggregation_N_lim` data points
#+ echo=F, include=T
ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att    := NA ]
ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att    := NA ]
ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att    := NA ]
ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp := NA ]
CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att    := NA ]
CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att    := NA ]
CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att    := NA ]
CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp := NA ]



#' #### Calculate daily seasonal values by SZA ####
#+ echo=F, include=T

ALL_2_daily_seas <-
    ALL_2_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                         HOR_att_seas    = mean(HOR_att,    na.rm = T),
                         GLB_att_seas    = mean(GLB_att,    na.rm = T),
                         DIR_transp_seas = mean(DIR_transp, na.rm = T),
                         DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                         HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                         GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                         GLB_att_N_seas  = sum(!is.na(GLB_att)),
                         HOR_att_N_seas  = sum(!is.na(HOR_att)),
                         DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .( doy, SZA, preNoon ) ]

CLEAR_2_daily_seas <-
    CLEAR_2_daily_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                           HOR_att_seas    = mean(HOR_att,    na.rm = T),
                           GLB_att_seas    = mean(GLB_att,    na.rm = T),
                           DIR_transp_seas = mean(DIR_transp, na.rm = T),
                           DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                           GLB_att_N_seas  = sum(!is.na(GLB_att)),
                           HOR_att_N_seas  = sum(!is.na(HOR_att)),
                           DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .( doy, SZA, preNoon ) ]



#+ echo=F, include=F
## ~ Plots longterm  ####

data_list  <- list(ALL   = ALL_2_daily_mean,
                   CLEAR = CLEAR_2_daily_mean)
data_names <- c(
    "GLB_att",
    "DIR_att",
    "DIR_transp",
    "HOR_att",
    "GLB_att_N",
    "DIR_att_N"
)
by_var     <- c("doy","SZA")
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

## ~ Plots seasonal ####
data_list  <- list(ALL_Seas   = ALL_2_daily_seas,
                   CLEAR_Seas = CLEAR_2_daily_seas)
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

ALL_daily_DEseas   <- merge(  ALL_2_daily_mean, ALL_2_daily_seas,   by = c("doy", "SZA", "preNoon"),  all = T)
CLEAR_daily_DEseas <- merge(CLEAR_2_daily_mean, CLEAR_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)

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
ALL_daily_DEseas[  , DIR_att   := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
ALL_daily_DEseas[  , GLB_att   := 100 * ( GLB_att    - GLB_att_seas    ) / GLB_att_seas    ]
ALL_daily_DEseas[  , DIR_transp:= 100 * ( DIR_transp - DIR_transp_seas ) / DIR_transp_seas ]
CLEAR_daily_DEseas[, DIR_att   := 100 * ( DIR_att    - DIR_att_seas    ) / DIR_att_seas    ]
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
                                    linear_regression_capture(lm1),
                                    preNoon   = anoon,
                                    SZA       = asza,
                                    DATA      = DBn,
                                    var       = avar,
                                    N         = sum(!is.na(dataset[[avar]]))
                                ))

                # plot(dataset$Date, dataset[[avar]], pch  = "." )
                # abline(lm1)
                # fit <- lm1[[1]]
                # legend('top', lty = 1, bty = "n",
                #        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
                # title(paste(DBn, asza, anoon))
            }
        }
    }
}

#+ echo=F, include=F
hist(gather$N[gather$N>50], breaks = 100)

szatrends <- gather

szatrends <- data.table(szatrends)
setorder(szatrends,SZA)

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
plot(test1$SZA, test1$N, pch =19)
abline(h=50)
plot(test2$SZA, test2$N, pch =19)
abline(h=300)

# szatrends[ var == "GLB_att"    & N <= 300, slope := NA ]
# szatrends[ var == "DIR_att"    & N <=  50, slope := NA ]
# szatrends[ var == "DIR_transp" & N <=  50, slope := NA ]





## stats vars to plot
wecare <- grep( "^slope|^N",names(szatrends),ignore.case = T, value = T)


#+ szatrends, echo=F, include=T, results = "asis"
## ALL - CS
for (type in unique(szatrends$DATA)) {
    ## DIR - GLB - transp
    for (avar in unique(szatrends$var)) {

        cat("\n\\newpage\n\n")
        cat(paste("\n###", type, avar,"\n\n"))

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

            plot(1, type="n", xlab="SZA", ylab=awename, xlim=xlim, ylim=ylim )
            abline(h=0)
            title(paste(awename,type, avar),cex=0.9)

            # lines(pam$SZA, pam[[awe]], pch =  pch_am, col = pam$col, type = "b")
            # lines(pam$SZA, ppm[[awe]], pch =  pch_pm, col = pam$col, type = "b")

            lines(pam$SZA, pam[[awe]], pch =  pch_am, col = 2, type = "b")
            lines(ppm$SZA, ppm[[awe]], pch =  pch_pm, col = 3, type = "b")

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
season      <- c("Winter", "Spring", "Summer", "Automn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    ## set seasons in each data base
    DB[ month(Date) %in% c(12, 1, 2), Season := "Winter"]
    DB[ month(Date) %in% c( 2, 4, 5), Season := "Spring"]
    DB[ month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    DB[ month(Date) %in% c( 9,10,11), Season := "Automn"]

    for (ase in season) {
        for (avar in vars) {
            for (anoon in unique( DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1) next()

                    lm1 <- lm( dataset[[avar]] ~ dataset$Date )

                    gather_seas <- rbind(gather_seas,
                                    data.frame(
                                        linear_regression_capture(lm1),
                                        preNoon   = anoon,
                                        SZA       = asza,
                                        DATA      = DBn,
                                        var       = avar,
                                        Season    = ase,
                                        N         = sum(!is.na(dataset[[avar]]))
                                    ))

                    # plot(dataset$Date, dataset[[avar]], pch  = "." )
                    # abline(lm1)
                    # fit <- lm1[[1]]
                    # legend('top', lty = 1, bty = "n",
                    #        paste('Y =', signif(fit[1],2),if(fit[2]>0)'+'else'-',signif(abs(fit[2]/timefactor*365),3),'* year'))
                    # title(paste(DBn, asza, anoon))
                }
            }
        }
    }
}

#+ echo=F, include=F
hist(gather_seas$N[gather_seas$N>50], breaks = 100)

szatrends_seas <- data.table(gather_seas)
setorder(szatrends_seas,SZA)


## define plot colors
szatrends_seas[ var == "DIR_att",    col := col_DIR_att    ]
szatrends_seas[ var == "GLB_att",    col := col_GLB_att    ]
szatrends_seas[ var == "DIR_transp", col := col_DIR_transp ]
szatrends_seas[ preNoon == T, pch := pch_am ]
szatrends_seas[ preNoon == F, pch := pch_pm ]





hist(szatrends_seas[DATA==dbs[1],N], breaks = 100)
hist(szatrends_seas[DATA==dbs[2],N], breaks = 100)
hist(szatrends_seas[var==vars[1],N], breaks = 100)
hist(szatrends_seas[var==vars[2],N], breaks = 100)

# szatrends_seas <- szatrends_seas[ N > 50]

plot(szatrends_seas$SZA,szatrends_seas$N)

test <- szatrends_seas[ DATA == "CLEAR_daily_DEseas" & var == "DIR_att" ]
plot(test$SZA, test$N, pch =19)
abline(h=50/4)

szatrends[ N <= 30, slope := NA]


test1 <- szatrends_seas[ DATA == "CLEAR_daily_DEseas" & var == "DIR_att" ]
test2 <- szatrends_seas[ DATA == "CLEAR_daily_DEseas" & var == "GLB_att" ]
plot(test1$SZA, test1$N, pch =19)
abline(h=50/4)
plot(test2$SZA, test2$N, pch =19)
abline(h=300/4)

# szatrends[ var == "GLB_att"    & N <= 300, slope := NA ]
# szatrends[ var == "DIR_att"    & N <=  50, slope := NA ]
# szatrends[ var == "DIR_transp" & N <=  50, slope := NA ]



## stats vars to plot
wecare <- grep( "^slope|^N",names(szatrends_seas),ignore.case = T, value = T)


#+ szatrendsseas, echo=F, include=T, results = "asis"
## Winter - Summer ....
for (ase in season) {
    ## ALL - Clear sky
    for (type in unique(szatrends_seas$DATA)) {
        ## DIR - GLB - transp
        for (avar in unique(szatrends_seas$var)) {

            cat("\n\\newpage\n\n")
            cat(paste("###",ase, type, avar,"\n\n"))

            ## statistic variable
            for (awe in wecare) {
                awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

                ## select All/CS  DIR/GLB/trans winter/summer
                subdata <- szatrends_seas[ DATA   == type &
                                           var    == avar &
                                           Season == ase    , ]

                xlim <- range( subdata$SZA,    na.rm = T )
                ylim <- range( subdata[[awe]], na.rm = T )

                pam  <- subdata[ preNoon == T ]
                ppm  <- subdata[ preNoon == F ]

                plot(1, type="n", xlab="SZA", ylab=awename, xlim=xlim, ylim=ylim )
                abline(h=0)
                title(paste(ase,awename,type, avar),cex=0.9)

                # lines(pam$SZA, pam[[awe]], pch =  pch_am, col = pam$col, type = "b")
                # lines(pam$SZA, ppm[[awe]], pch =  pch_pm, col = pam$col, type = "b")

                lines(pam$SZA, pam[[awe]], pch =  pch_am, col = 2, type = "b")
                lines(ppm$SZA, ppm[[awe]], pch =  pch_pm, col = 3, type = "b")

                legend("top",
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
