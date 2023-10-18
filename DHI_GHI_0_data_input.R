

## Data input for this paper

## to force a rebuild of the dataset remove stored
# file.remove(common_data)

require(data.table)
require(zoo)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")
source("./DHI_GHI_0_variables.R")

#  Run data construction  ------------------------------------------------------

D_14_2 <- FALSE
D_14   <- FALSE
D_13   <- FALSE

D_14_2 <- TRUE
# D_14   <- TRUE
# D_13   <- TRUE

TEST <- TRUE
TEST <- FALSE

if (TEST) {
    warning("Running in TEST mode!!")
}


## new new implementation with corrected limits
if (D_14_2) {
    common_data <- common_data_14_2
    CS_file     <- CS_file_14_2
    inpatern    <- "Clear_sky_id_Reno-Hansen_apply_v14_2_[0-9]{4}.Rds"
}

## new implementation with corrected limits
if (D_14) {
    common_data <- common_data_14
    CS_file     <- CS_file_14
    inpatern    <- "Clear_sky_id_Reno-Hansen_apply_v14_[0-9]{4}.Rds"
}

## old implementation with corrected limits
if (D_13) {
    common_data <- common_data_13
    CS_file     <- CS_file_13
    inpatern    <- "Clear_Sky_[0-9]{4}.Rds"
}

## create local folders
dir.create(dirname(common_data), showWarnings = FALSE)
dir.create("./figures",          showWarnings = FALSE)
dir.create("./images",           showWarnings = FALSE)
dir.create("./runtime",          showWarnings = FALSE)


#_  Check if we need to run data production  -----------------------------------
havetorun <- !file.exists(common_data) |
    file.mtime(CS_file)          > file.mtime(common_data) |
    file.mtime(variables_fl)     > file.mtime(common_data) |
    file.mtime(variables_fl)     > file.mtime(common_data) |
    file.mtime(data_procsess_fl) > file.mtime(common_data)

if (havetorun) {
    cat(paste("\n !! Create environment and data input ->", common_data),"\n")

    #_  Get data from Clear sky id data  ---------------------------------------
    input_files <- list.files(path       = CLEARdir,
                              pattern    = inpatern,
                              full.names = T )
    input_files <- grep("_stats_", input_files, value = TRUE, invert = TRUE)

    if (TEST) {
        warning("\nTEST MODE IS ACTIVE!!\n\n")
        input_files <- sample(input_files, 2)
    }

    if (TEST | !file.exists(CS_file) | max(file.mtime(input_files)) > file.mtime(CS_file)) {
        cat(paste("Load data from Clear Sky process from original\n"))
        DATA <- data.table()
        for (af in input_files) {
            cat("READING:", af, "\n")
            temp <- readRDS(af)
            ## drop some data
            temp$CHP1temp        <- NULL
            temp$CHP1tempSD      <- NULL
            temp$CHP1tempUNC     <- NULL
            temp$CS_ref          <- NULL
            temp$Pressure_Source <- NULL
            temp$chp1TempCF      <- NULL
            temp$wattDIR_tmp_cr  <- NULL
            temp$wattDIR_unc_NT  <- NULL
            temp$wattDIR_unc_WT  <- NULL
            temp$wattHOR_tmp_cr  <- NULL
            temp$wattHOR_unc_NT  <- NULL
            temp$wattHOR_unc_WT  <- NULL
            temp$chp1TempCF      <- NULL
            temp$GLBINC_SD_wpsm  <- NULL
            temp$GLBINC_strict   <- NULL
            temp$GLBINC_wpsm     <- NULL
            temp$Pressure        <- NULL
            temp$Pressure_source <- NULL
            temp$HOR_strict      <- NULL
            temp$DIR_strict      <- NULL
            temp$GLB_strict      <- NULL
            temp$DIFF_strict     <- NULL
            temp$pressure        <- NULL

            temp <- unique(temp)
            DATA <- rbind(temp, DATA, fill = TRUE)
            rm(temp)
        }
        DATA <- unique(DATA)
        gc()

        ## TODO warn duplicate dates
        if (sum(duplicated(DATA$Date)) > 0) {
            warning("There are duplicate dates in the data")
        }

        ## There are some duplicates introduced at some point!!
        test <- DATA[duplicated(DATA$Date) | duplicated(DATA$Date, fromLast = TRUE)]
        stopifnot( nrow(test) < 1000 )

        ## Workaround for duplicates
        test_vec <- DATA[is.na(wattGLB) &
                             (duplicated(DATA$Date) | duplicated(DATA$Date, fromLast = TRUE)),
                         which = TRUE]
        ## Drop some data
        DATA <- DATA[!test_vec]

        ## retest
        test <- DATA[duplicated(DATA$Date) | duplicated(DATA$Date, fromLast = TRUE)]
        cat("\nThere are ", nrow(test), " duplicate dates remaining!\n")

        ## this is used by old scripts
        setorder(DATA,Date)
        myRtools::write_RDS(object = DATA, file = CS_file, clean = TRUE)
    } else {
        DATA <- readRDS(CS_file)
    }


    ## __ Skip data ranges for CM-21 --------------------------------------------
    for (as in nrow(SKIP_cm21)) {
        skip <- SKIP_cm21[as,]
        DATA[ Date >= skip$From & Date <= skip$Until, wattGLB    := NA ]
        DATA[ Date >= skip$From & Date <= skip$Until, wattGLB_SD := NA ]
    }
    # DATA[ Date >= skip$From & Date <= skip$Until, wattGLB]

    # ## Sunset and sunrise
    # hist(DATA[ Elevat < 5, Elevat ])
    # which(diff(sign(DATA$Elevat))!=0)
    #
    # vec1      <- data.frame(Sign = sign(DATA$Elevat),
    #                         Date = DATA$Date,
    #                         Elev = DATA$Elevat)
    # vec1$Diff <- c(0,diff(vec1$Sign))
    # vec1[which(vec1$Diff != 0), ]

    #  Select data for this project  -------------------------------------------

    #__  Set date range to use  ------------------------------------------------
    DATA <- DATA[Date < LAST_DAY ]
    DATA <- DATA[Date > FIRST_DAY]

    #__  Keep daylight only  ---------------------------------------------------
    DATA <- DATA[Elevat >= 0, ]

    #__  Exclude low Sun elevation  --------------------------------------------
    DATA[Elevat < MIN_ELEVA, wattDIR     := NA ]
    DATA[Elevat < MIN_ELEVA, wattDIR_sds := NA ]
    DATA[Elevat < MIN_ELEVA, wattGLB     := NA ]
    DATA[Elevat < MIN_ELEVA, wattGLB_sds := NA ]
    DATA[Elevat < MIN_ELEVA, wattHOR     := NA ]
    DATA[Elevat < MIN_ELEVA, wattHOR_sds := NA ]

    ## show included data
    ## FIXME there is som error in Azimuth/Elevation angles see plot!!
    # plot(DATA[ !is.na(wattGLB) ,Elevat, Azimuth])

    #__  Bais paper obstacle filter  -------------------------------------------
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattDIR     := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattDIR_sds := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattGLB     := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattGLB_sds := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattHOR     := NA ]
    DATA[Azimuth > 35 & Azimuth < 120 & Elevat < 10, wattHOR_sds := NA ]

    ## show included data
    # plot(DATA[ !is.na(wattGLB) ,Elevat, Azimuth])


    ## Filter min elevation
    # DATA <- DATA[Elevat >= MIN_ELEVA, ]

    ## Bais paper obstacle filter
    # DATA <- DATA[!(Azimuth > 35 & Azimuth < 120 & Elevat < 10)]


    #__  Keep data characterized as 'good' by Radiation Quality control v13 ----
    if (D_13) {
        keepQF <- c("good",
                    "Possible Direct Obstruction (23)",
                    "Biology Building (22)")
        DATA[!QCF_DIR %in% keepQF, wattDIR := NA]
        DATA[!QCF_DIR %in% keepQF, wattHOR := NA]
        DATA[!QCF_GLB %in% keepQF, wattGLB := NA]
    }

    #__  Keep data characterized as 'TRUE' by Radiation Quality control v14 ----
    if (D_14 | D_14_2) {
        DATA[QCF_DIR == FALSE, wattDIR := NA]
        DATA[QCF_DIR == FALSE, wattHOR := NA]
        DATA[QCF_GLB == FALSE, wattGLB := NA]
    }

    #__  Count daylight length  ------------------------------------------------
    DATA[, DayLength := .N, by = Day]

    #__  DROP MISSING RECORDS!! ------------------------------------------------
    DATA <- DATA[ !(is.na(wattDIR) & is.na(wattGLB)) ]

    #__  Info for TIS time span source used  -----------------------------------
    TSI_info <- DATA[, .(Start = min(Date),
                         End   = max(Date)), by = TSI_Source]
    myRtools::write_dat(object = TSI_info,
                        file   = "./figures/tbl_tsi_info.dat",
                        clean  = TRUE)
    rm(TSI_info)


    #  Data preparation  -------------------------------------------------------

    #_  Move measurements to mean earth distance  ------------------------------
    DATA[, wattDIR_1au := wattDIR * (sun_dist ^ 2)]
    DATA[, wattGLB_1au := wattGLB * (sun_dist ^ 2)]
    DATA[, wattHOR_1au := wattHOR * (sun_dist ^ 2)]

    #_  Relative to actual TSI at 1au variable representation
    # DATA[ , DIR_att := wattDIR_1au / tsi_1au_comb ]
    # DATA[ , GLB_att := wattGLB_1au / tsi_1au_comb ]
    # DATA[ , HOR_att := wattHOR_1au / tsi_1au_comb ]

    ## Use original variable representation for conviniance
    DATA[, DIR_att := wattDIR_1au]
    DATA[, GLB_att := wattGLB_1au]
    DATA[, HOR_att := wattHOR_1au]

    ## Ground effect removal?
    ## Aerosol direct effects on global solar shortwave irradiance at high mountainous station Musala Bulgaria_Nojarov2021.pdf
    # DATA$wattGLB_1au <- DATA$wattGLB_1au / cosde(DATA$SZA)
    # DATA$wattDIR_1au <- DATA$wattDIR_1au / cosde(DATA$SZA)


    #_ Calculate Bouguer atmospheric transparency  -----------------------------
    ## Changes in solar radiation and their influence on temperature trend in Estonia 1955 2007_Russak2009.pdf
    DATA[, DIR_transp := ( wattDIR_1au / tsi_1au_comb ) ^ ( 1 / cosde(SZA) ) ]

    ## fix noon just in case
    DATA[Azimuth <= 180 , preNoon := TRUE ]
    DATA[Azimuth >  180 , preNoon := FALSE]

    #_  DROP SOME DATA  --------------------------------------------------------
    DATA[, wattDIR_1au        := NULL]
    DATA[, wattGLB_1au        := NULL]
    DATA[, wattHOR_1au        := NULL]
    DATA[, DiffuseFraction_Kd := NULL]
    DATA[, Elevat             := NULL]
    DATA[, Clearness_Kt       := NULL]

    rm.cols.DT(DATA, "QCv9*")
    rm.cols.DT(DATA, "*Clim_lim")
    rm.cols.DT(DATA, "QCF_*")
    rm.cols.DT(DATA, "VIL_*")


    #  Split data to Clear Sky, non Clear sky and cloud sky data  --------------
    ## Method based and adapted from: Identification of Periods of Clear Sky Irradiance in Time Series of GHI Measurements _Matthew J. Reno and Clifford W. Hansen_.

    #  GLB Representation filtering  -------------------------------------------
    temp <- DATA[!is.na(GLB_att),
                 .(Day_N = .N,
                   DayLim = max(DayLength) * All_daily_ratio_lim),
                 by = Day]

    Days_with_all_glb_data      <- temp[ , .N ]
    Days_with_filtered_glb_data <- temp[ Day_N >= DayLim, .N ]
    cat("\nExcluded days:", Days_with_all_glb_data - Days_with_filtered_glb_data, "\n\n")

    all_days_to_keep <- temp[ Day_N >= DayLim, Day ]
    rm(temp)

    ## Keep only good enough days
    all_glb_datapoints     <- DATA[Day %in% all_days_to_keep, .N]
    filterd_glb_datapoints <- DATA[, .N]
    cat("\nKeeping:", 100 * all_glb_datapoints / filterd_glb_datapoints, "% of ALL data\n\n")

    DATA <- DATA[Day %in% all_days_to_keep ]


    #__  ALL data  -------------------------------------------------------------
    DATA_all   <- DATA

    ## use only cm21 flags for trends
    wecare     <- grep("CSflag_", names(DATA), value = T)
    wecare     <- grep("_11", wecare, invert = T, value = T)

    #__  CLEAR data  -----------------------------------------------------------
    DATA_Clear <- DATA[rowSums(DATA[, ..wecare ], na.rm = T) == 0,]

    #__  CLOUD data  -----------------------------------------------------------
    DATA_Cloud <- DATA[rowSums(DATA[, ..wecare ], na.rm = T) != 0,]

    ## old flags usage
    # DATA_Clear <- DATA_all[ CSflag == 0 ]
    rm(DATA)
    gc()

    ## remove unused columns
    rm.cols.DT(DATA_all,   "CSflag_*")
    rm.cols.DT(DATA_Clear, "CSflag_*")
    rm.cols.DT(DATA_Cloud, "CSflag_*")
    DATA_all[  , CS_ref_HOR := NULL]
    DATA_Clear[, CS_ref_HOR := NULL]
    DATA_Cloud[, CS_ref_HOR := NULL]


    # ..................................................................... ----
    ##  1. long-term  ----------------------------------------------------------

    ##  Daily means  -----------------------------------------------------------

    ALL_1_daily_mean <-
        DATA_all[,.(DIR_att       = mean(DIR_att,      na.rm = T),
                    GLB_att       = mean(GLB_att,      na.rm = T),
                    HOR_att       = mean(HOR_att,      na.rm = T),
                    DIR_transp    = mean(DIR_transp,   na.rm = T),
                    tsi1au_att    = mean(tsi_1au_comb, na.rm = T),
                    DIR_att_sd    = sd(  DIR_att,      na.rm = T),
                    GLB_att_sd    = sd(  GLB_att,      na.rm = T),
                    HOR_att_sd    = sd(  HOR_att,      na.rm = T),
                    DIR_transp_sd = sd(  DIR_transp,   na.rm = T),
                    tsi1au_att_sd = sd(  tsi_1au_comb, na.rm = T),
                    doy           = yday(Date),
                    GLB_att_N     = sum(!is.na(GLB_att)),
                    HOR_att_N     = sum(!is.na(HOR_att)),
                    DIR_att_N     = sum(!is.na(DIR_att))),
                 by = .( Date = Day ) ]

    CLEAR_1_daily_mean <-
        DATA_Clear[,.(DIR_att       = mean(DIR_att,      na.rm = T),
                      GLB_att       = mean(GLB_att,      na.rm = T),
                      HOR_att       = mean(HOR_att,      na.rm = T),
                      DIR_transp    = mean(DIR_transp,   na.rm = T),
                      tsi1au_att    = mean(tsi_1au_comb, na.rm = T),
                      DIR_att_sd    = sd(  DIR_att,      na.rm = T),
                      GLB_att_sd    = sd(  GLB_att,      na.rm = T),
                      HOR_att_sd    = sd(  HOR_att,      na.rm = T),
                      DIR_transp_sd = sd(  DIR_transp,   na.rm = T),
                      tsi1au_att_sd = sd(  tsi_1au_comb, na.rm = T),
                      doy           = yday(Date),
                      GLB_att_N     = sum(!is.na(GLB_att)),
                      HOR_att_N     = sum(!is.na(HOR_att)),
                      DIR_att_N     = sum(!is.na(DIR_att))),
                   by = .( Date = Day ) ]

    CLOUD_1_daily_mean <-
        DATA_Cloud[,.(DIR_att       = mean(DIR_att,      na.rm = T),
                      GLB_att       = mean(GLB_att,      na.rm = T),
                      HOR_att       = mean(HOR_att,      na.rm = T),
                      DIR_transp    = mean(DIR_transp,   na.rm = T),
                      tsi1au_att    = mean(tsi_1au_comb, na.rm = T),
                      DIR_att_sd    = sd(  DIR_att,      na.rm = T),
                      GLB_att_sd    = sd(  GLB_att,      na.rm = T),
                      HOR_att_sd    = sd(  HOR_att,      na.rm = T),
                      DIR_transp_sd = sd(  DIR_transp,   na.rm = T),
                      tsi1au_att_sd = sd(  tsi_1au_comb, na.rm = T),
                      doy           = yday(Date),
                      GLB_att_N     = sum(!is.na(GLB_att)),
                      HOR_att_N     = sum(!is.na(HOR_att)),
                      DIR_att_N     = sum(!is.na(DIR_att))),
                   by = .( Date = Day ) ]

    ## _ Margin of error for confidence interval -------------------------------
    conf_param  <- 1 - ( 1 - Daily_confidence_limit ) / 2
    suppressWarnings({
        ALL_1_daily_mean[,   DIR_att_EM    := qt(conf_param,df=DIR_att_N - 1) * DIR_att_sd    / sqrt(DIR_att_N)]
        ALL_1_daily_mean[,   HOR_att_EM    := qt(conf_param,df=HOR_att_N - 1) * HOR_att_sd    / sqrt(HOR_att_N)]
        ALL_1_daily_mean[,   GLB_att_EM    := qt(conf_param,df=GLB_att_N - 1) * GLB_att_sd    / sqrt(GLB_att_N)]
        ALL_1_daily_mean[,   DIR_transp_EM := qt(conf_param,df=DIR_att_N - 1) * DIR_transp_sd / sqrt(DIR_att_N)]
        CLEAR_1_daily_mean[, DIR_att_EM    := qt(conf_param,df=DIR_att_N - 1) * DIR_att_sd    / sqrt(DIR_att_N)]
        CLEAR_1_daily_mean[, HOR_att_EM    := qt(conf_param,df=HOR_att_N - 1) * HOR_att_sd    / sqrt(HOR_att_N)]
        CLEAR_1_daily_mean[, GLB_att_EM    := qt(conf_param,df=GLB_att_N - 1) * GLB_att_sd    / sqrt(GLB_att_N)]
        CLEAR_1_daily_mean[, DIR_transp_EM := qt(conf_param,df=DIR_att_N - 1) * DIR_transp_sd / sqrt(DIR_att_N)]
        CLOUD_1_daily_mean[, DIR_att_EM    := qt(conf_param,df=DIR_att_N - 1) * DIR_att_sd    / sqrt(DIR_att_N)]
        CLOUD_1_daily_mean[, HOR_att_EM    := qt(conf_param,df=HOR_att_N - 1) * HOR_att_sd    / sqrt(HOR_att_N)]
        CLOUD_1_daily_mean[, GLB_att_EM    := qt(conf_param,df=GLB_att_N - 1) * GLB_att_sd    / sqrt(GLB_att_N)]
        CLOUD_1_daily_mean[, DIR_transp_EM := qt(conf_param,df=DIR_att_N - 1) * DIR_transp_sd / sqrt(DIR_att_N)]
    })

    ## _ Exclude means with less than Daily_aggregation_N_lim data points ------
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_att       := NA]
    ALL_1_daily_mean[   GLB_att_N <= Daily_aggregation_N_lim, GLB_att       := NA]
    ALL_1_daily_mean[   HOR_att_N <= Daily_aggregation_N_lim, HOR_att       := NA]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_transp    := NA]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_att_sd    := NA]
    ALL_1_daily_mean[   GLB_att_N <= Daily_aggregation_N_lim, GLB_att_sd    := NA]
    ALL_1_daily_mean[   HOR_att_N <= Daily_aggregation_N_lim, HOR_att_sd    := NA]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_sd := NA]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_att_EM    := NA]
    ALL_1_daily_mean[   GLB_att_N <= Daily_aggregation_N_lim, GLB_att_EM    := NA]
    ALL_1_daily_mean[   HOR_att_N <= Daily_aggregation_N_lim, HOR_att_EM    := NA]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_EM := NA]

    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att       := NA]
    CLEAR_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att       := NA]
    CLEAR_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att       := NA]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp    := NA]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att_sd    := NA]
    CLEAR_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att_sd    := NA]
    CLEAR_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att_sd    := NA]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_sd := NA]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att_EM    := NA]
    CLEAR_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att_EM    := NA]
    CLEAR_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att_EM    := NA]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_EM := NA]

    CLOUD_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att       := NA]
    CLOUD_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att       := NA]
    CLOUD_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att       := NA]
    CLOUD_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp    := NA]
    CLOUD_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att_sd    := NA]
    CLOUD_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att_sd    := NA]
    CLOUD_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att_sd    := NA]
    CLOUD_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_sd := NA]
    CLOUD_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att_EM    := NA]
    CLOUD_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att_EM    := NA]
    CLOUD_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att_EM    := NA]
    CLOUD_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_EM := NA]


    ## _ Daily seasonal values from daily --------------------------------------
    ALL_1_daily_seas <-
        ALL_1_daily_mean[,.(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                            GLB_att_seas       = mean(GLB_att,    na.rm = T),
                            HOR_att_seas       = mean(HOR_att,    na.rm = T),
                            DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                            DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                            HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                            GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                            DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                            GLB_att_N_seas     = sum(!is.na(GLB_att)),
                            HOR_att_N_seas     = sum(!is.na(HOR_att)),
                            DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                         by = .( doy ) ]

    CLEAR_1_daily_seas <-
        CLEAR_1_daily_mean[,.(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                              GLB_att_seas       = mean(GLB_att,    na.rm = T),
                              HOR_att_seas       = mean(HOR_att,    na.rm = T),
                              DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                              DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                              DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                              GLB_att_N_seas     = sum(!is.na(GLB_att)),
                              HOR_att_N_seas     = sum(!is.na(HOR_att)),
                              DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                           by = .( doy ) ]

    CLOUD_1_daily_seas <-
        CLOUD_1_daily_mean[,.(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                              GLB_att_seas       = mean(GLB_att,    na.rm = T),
                              HOR_att_seas       = mean(HOR_att,    na.rm = T),
                              DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                              DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                              DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                              GLB_att_N_seas     = sum(!is.na(GLB_att)),
                              HOR_att_N_seas     = sum(!is.na(HOR_att)),
                              DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                           by = .( doy ) ]



    ## _ Margin of error for confidence interval on seasonal data --------------
    conf_param  <- 1 - ( 1 - Daily_confidence_limit ) / 2
    suppressWarnings({
        ALL_1_daily_seas[,  DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas-1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        ALL_1_daily_seas[,  HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas-1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        ALL_1_daily_seas[,  GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas-1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        ALL_1_daily_seas[,  DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas-1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLEAR_1_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas-1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLEAR_1_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas-1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLEAR_1_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas-1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        CLEAR_1_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas-1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLOUD_1_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas-1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLOUD_1_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas-1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLOUD_1_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas-1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        CLOUD_1_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas-1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
    })


    ## _ Daily de-seasonal anomaly ---------------------------------------------

      ALL_1_daily_DESEAS <- merge(  ALL_1_daily_mean,   ALL_1_daily_seas, by = "doy", all = T)
    CLEAR_1_daily_DESEAS <- merge(CLEAR_1_daily_mean, CLEAR_1_daily_seas, by = "doy", all = T)
    CLOUD_1_daily_DESEAS <- merge(CLOUD_1_daily_mean, CLOUD_1_daily_seas, by = "doy", all = T)

    setorder(  ALL_1_daily_DESEAS, Date)
    setorder(CLEAR_1_daily_DESEAS, Date)
    setorder(CLOUD_1_daily_DESEAS, Date)

    ## Using the % departure from seasonal values

      ALL_1_daily_DESEAS[, DIR_att_des   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas   ]
      ALL_1_daily_DESEAS[, HOR_att_des   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas   ]
      ALL_1_daily_DESEAS[, GLB_att_des   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas   ]
      ALL_1_daily_DESEAS[, DIR_transp_des:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas]
    CLEAR_1_daily_DESEAS[, DIR_att_des   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas   ]
    CLEAR_1_daily_DESEAS[, HOR_att_des   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas   ]
    CLEAR_1_daily_DESEAS[, GLB_att_des   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas   ]
    CLEAR_1_daily_DESEAS[, DIR_transp_des:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas]
    CLOUD_1_daily_DESEAS[, DIR_att_des   := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas   ]
    CLOUD_1_daily_DESEAS[, HOR_att_des   := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas   ]
    CLOUD_1_daily_DESEAS[, GLB_att_des   := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas   ]
    CLOUD_1_daily_DESEAS[, DIR_transp_des:= 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas]

    ## add TSI data process
    ## data departure from mean value
    ALL_1_daily_mean    [, tsi1au_att_des := 100*(tsi1au_att - mean(tsi1au_att)) / mean(tsi1au_att)]
    ALL_1_daily_mean <-
        merge(ALL_1_daily_DESEAS,
              ALL_1_daily_mean[, .(Date, tsi1au_att)], by = "Date", all = T )

    ## just for completeness or to see if there is any selection bias
    CLEAR_1_daily_DESEAS[, tsi1au_att_des := 100*(tsi1au_att - mean(tsi1au_att)) / mean(tsi1au_att)]
    CLOUD_1_daily_DESEAS[, tsi1au_att_des := 100*(tsi1au_att - mean(tsi1au_att)) / mean(tsi1au_att)]
    CLEAR_1_daily_mean <-
        merge(CLEAR_1_daily_DESEAS,
              CLEAR_1_daily_mean[, .(Date, tsi1au_att)], by = "Date", all = T )

    CLOUD_1_daily_mean <-
        merge(CLOUD_1_daily_DESEAS,
              CLOUD_1_daily_mean[, .(Date, tsi1au_att)], by = "Date", all = T )



    ## Monthly means from daily means ------------------------------------------

    ALL_1_monthly_daily_mean <-
        ALL_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                            GLB_att    = mean(GLB_att,    na.rm = T),
                            HOR_att    = mean(HOR_att,    na.rm = T),
                            DIR_transp = mean(DIR_transp, na.rm = T),
                            DIR_att_sd = sd(  DIR_att,    na.rm = T),
                            HOR_att_sd = sd(  HOR_att,    na.rm = T),
                            GLB_att_sd = sd(  GLB_att,    na.rm = T),
                            GLB_att_N  = sum(!is.na(GLB_att)),
                            HOR_att_N  = sum(!is.na(HOR_att)),
                            DIR_att_N  = sum(!is.na(DIR_att))  ),
                         by = .(
                             Year = year(Date),
                             Month = month(Date)
                         ) ]

    CLEAR_1_monthly_daily_mean <-
        CLEAR_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                              GLB_att    = mean(GLB_att,    na.rm = T),
                              HOR_att    = mean(HOR_att,    na.rm = T),
                              DIR_transp = mean(DIR_transp, na.rm = T),
                              DIR_att_sd = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd = sd(  GLB_att,    na.rm = T),
                              GLB_att_N  = sum(!is.na(GLB_att)),
                              HOR_att_N  = sum(!is.na(HOR_att)),
                              DIR_att_N  = sum(!is.na(DIR_att))  ),
                           by = .(
                               Year  = year(Date),
                               Month = month(Date)
                           ) ]

    CLOUD_1_monthly_daily_mean <-
        CLOUD_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                              GLB_att    = mean(GLB_att,    na.rm = T),
                              HOR_att    = mean(HOR_att,    na.rm = T),
                              DIR_transp = mean(DIR_transp, na.rm = T),
                              DIR_att_sd = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd = sd(  GLB_att,    na.rm = T),
                              GLB_att_N  = sum(!is.na(GLB_att)),
                              HOR_att_N  = sum(!is.na(HOR_att)),
                              DIR_att_N  = sum(!is.na(DIR_att))  ),
                           by = .(
                               Year  = year(Date),
                               Month = month(Date)
                           ) ]



    ## _ Exclude means with less than Monthly_aggegation_N_lim data points -----
    ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att       := NA]
    ALL_1_monthly_daily_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att       := NA]
    ALL_1_monthly_daily_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att       := NA]
    ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp    := NA]
    ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_sd    := NA]
    ALL_1_monthly_daily_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_sd    := NA]
    ALL_1_monthly_daily_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_sd    := NA]
    ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_sd := NA]
    ALL_1_monthly_daily_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_EM    := NA]
    ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_EM    := NA]
    ALL_1_monthly_daily_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_EM    := NA]
    ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_EM := NA]

    CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
    CLEAR_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
    CLEAR_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
    CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
    CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
    CLEAR_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
    CLEAR_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
    CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
    CLEAR_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
    CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
    CLEAR_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
    CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]

    CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
    CLOUD_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
    CLOUD_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
    CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
    CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
    CLOUD_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
    CLOUD_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
    CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
    CLOUD_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
    CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
    CLOUD_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
    CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]



    ## _ Seasonal monthly daily values -----------------------------------------

    ALL_1_monthly_daily_seas <-
        ALL_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                            GLB_att_seas    = mean(GLB_att,    na.rm = T),
                            HOR_att_seas    = mean(HOR_att,    na.rm = T),
                            DIR_transp_seas = mean(DIR_transp, na.rm = T),
                            DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                            HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                            GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                            GLB_att_N_seas  = sum(!is.na(GLB_att)),
                            HOR_att_N_seas  = sum(!is.na(HOR_att)),
                            DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                         by = .( Month = month(Date) ) ]

    CLEAR_1_monthly_daily_seas <-
        CLEAR_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                              GLB_att_seas    = mean(GLB_att,    na.rm = T),
                              HOR_att_seas    = mean(HOR_att,    na.rm = T),
                              DIR_transp_seas = mean(DIR_transp, na.rm = T),
                              DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                              GLB_att_N_seas  = sum(!is.na(GLB_att)),
                              HOR_att_N_seas  = sum(!is.na(HOR_att)),
                              DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                           by = .( Month = month(Date) ) ]

    CLOUD_1_monthly_daily_seas <-
        CLOUD_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                              GLB_att_seas    = mean(GLB_att,    na.rm = T),
                              HOR_att_seas    = mean(HOR_att,    na.rm = T),
                              DIR_transp_seas = mean(DIR_transp, na.rm = T),
                              DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                              GLB_att_N_seas  = sum(!is.na(GLB_att)),
                              HOR_att_N_seas  = sum(!is.na(HOR_att)),
                              DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                           by = .( Month = month(Date) ) ]



    ## _ Monthly daily de-seasonal anomaly -------------------------------------

      ALL_1_D_monthly_DESEAS <- merge(  ALL_1_monthly_daily_mean,   ALL_1_monthly_daily_seas, by = "Month", all = T)
    CLEAR_1_D_monthly_DESEAS <- merge(CLEAR_1_monthly_daily_mean, CLEAR_1_monthly_daily_seas, by = "Month", all = T)
    CLOUD_1_D_monthly_DESEAS <- merge(CLOUD_1_monthly_daily_mean, CLOUD_1_monthly_daily_seas, by = "Month", all = T)


    ## forget data
    rm(  ALL_1_monthly_daily_mean,   ALL_1_monthly_daily_seas,
       CLEAR_1_monthly_daily_mean, CLEAR_1_monthly_daily_seas,
       CLOUD_1_monthly_daily_mean, CLOUD_1_monthly_daily_seas)
    gc()


    ## create date
      ALL_1_D_monthly_DESEAS[, Date := as.Date(paste(Year, Month, "1"), format = "%Y %m %d")]
    CLEAR_1_D_monthly_DESEAS[, Date := as.Date(paste(Year, Month, "1"), format = "%Y %m %d")]
    CLOUD_1_D_monthly_DESEAS[, Date := as.Date(paste(Year, Month, "1"), format = "%Y %m %d")]

    setorder(  ALL_1_D_monthly_DESEAS, Date)
    setorder(CLEAR_1_D_monthly_DESEAS, Date)
    setorder(CLOUD_1_D_monthly_DESEAS, Date)


    ## Using the % departure from seasonal values
      ALL_1_D_monthly_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
      ALL_1_D_monthly_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
      ALL_1_D_monthly_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
      ALL_1_D_monthly_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLEAR_1_D_monthly_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLEAR_1_D_monthly_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLEAR_1_D_monthly_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    CLEAR_1_D_monthly_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLOUD_1_D_monthly_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLOUD_1_D_monthly_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLOUD_1_D_monthly_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    CLOUD_1_D_monthly_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]





    ## Season of year daily aggregation ----------------------------------------

    ## Quarter of year with one month shift to include December in the next years winter
      ALL_1_daily_mean[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
    CLEAR_1_daily_mean[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
    CLOUD_1_daily_mean[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]

    ## Flag seasons using quarters
      ALL_1_daily_mean[season_Yqrt %% 1 == 0   , Season := "Winter"]
      ALL_1_daily_mean[season_Yqrt %% 1 == 0.25, Season := "Spring"]
      ALL_1_daily_mean[season_Yqrt %% 1 == 0.50, Season := "Summer"]
      ALL_1_daily_mean[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLEAR_1_daily_mean[season_Yqrt %% 1 == 0   , Season := "Winter"]
    CLEAR_1_daily_mean[season_Yqrt %% 1 == 0.25, Season := "Spring"]
    CLEAR_1_daily_mean[season_Yqrt %% 1 == 0.50, Season := "Summer"]
    CLEAR_1_daily_mean[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLOUD_1_daily_mean[season_Yqrt %% 1 == 0   , Season := "Winter"]
    CLOUD_1_daily_mean[season_Yqrt %% 1 == 0.25, Season := "Spring"]
    CLOUD_1_daily_mean[season_Yqrt %% 1 == 0.50, Season := "Summer"]
    CLOUD_1_daily_mean[season_Yqrt %% 1 == 0.75, Season := "Autumn"]


    ## _ Create variables by season from daily means ---------------------------
    ALL_1_bySeason_daily_mean <-
        ALL_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                            GLB_att    = mean(GLB_att,    na.rm = T),
                            HOR_att    = mean(HOR_att,    na.rm = T),
                            DIR_transp = mean(DIR_transp, na.rm = T),
                            DIR_att_sd = sd(  DIR_att,    na.rm = T),
                            HOR_att_sd = sd(  HOR_att,    na.rm = T),
                            GLB_att_sd = sd(  GLB_att,    na.rm = T),
                            GLB_att_N  = sum(!is.na(GLB_att)),
                            HOR_att_N  = sum(!is.na(HOR_att)),
                            DIR_att_N  = sum(!is.na(DIR_att)),
                            minDate    = min(Date),
                            maxDate    = max(Date),
                            medDate    = median(Date)    ),
                         by = .( Yqrt = season_Yqrt) ]

    CLEAR_1_bySeason_daily_mean <-
        CLEAR_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                              GLB_att    = mean(GLB_att,    na.rm = T),
                              HOR_att    = mean(HOR_att,    na.rm = T),
                              DIR_transp = mean(DIR_transp, na.rm = T),
                              DIR_att_sd = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd = sd(  GLB_att,    na.rm = T),
                              GLB_att_N  = sum(!is.na(GLB_att)),
                              HOR_att_N  = sum(!is.na(HOR_att)),
                              DIR_att_N  = sum(!is.na(DIR_att)),
                              minDate    = min(Date),
                              maxDate    = max(Date),
                              medDate    = median(Date)    ),
                           by = .( Yqrt = season_Yqrt) ]

    CLOUD_1_bySeason_daily_mean <-
        CLOUD_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                              GLB_att    = mean(GLB_att,    na.rm = T),
                              HOR_att    = mean(HOR_att,    na.rm = T),
                              DIR_transp = mean(DIR_transp, na.rm = T),
                              DIR_att_sd = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd = sd(  GLB_att,    na.rm = T),
                              GLB_att_N  = sum(!is.na(GLB_att)),
                              HOR_att_N  = sum(!is.na(HOR_att)),
                              DIR_att_N  = sum(!is.na(DIR_att)),
                              minDate    = min(Date),
                              maxDate    = max(Date),
                              medDate    = median(Date)    ),
                           by = .( Yqrt = season_Yqrt) ]


    ## Flag seasons using quarters
      ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
      ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
      ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
      ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]






    ## _ Seasonal by season daily values ---------------------------------------

    ALL_1_bySeason_daily_seas <-
        ALL_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                            GLB_att_seas    = mean(GLB_att,    na.rm = T),
                            HOR_att_seas    = mean(HOR_att,    na.rm = T),
                            DIR_transp_seas = mean(DIR_transp, na.rm = T),
                            DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                            HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                            GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                            GLB_att_N_seas  = sum(!is.na(GLB_att)),
                            HOR_att_N_seas  = sum(!is.na(HOR_att)),
                            DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                         by = .(Season)]

    CLEAR_1_bySeason_daily_seas <-
        CLEAR_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                              GLB_att_seas    = mean(GLB_att,    na.rm = T),
                              HOR_att_seas    = mean(HOR_att,    na.rm = T),
                              DIR_transp_seas = mean(DIR_transp, na.rm = T),
                              DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                              GLB_att_N_seas  = sum(!is.na(GLB_att)),
                              HOR_att_N_seas  = sum(!is.na(HOR_att)),
                              DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                           by = .(Season)]

    CLOUD_1_bySeason_daily_seas <-
        CLOUD_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                              GLB_att_seas    = mean(GLB_att,    na.rm = T),
                              HOR_att_seas    = mean(HOR_att,    na.rm = T),
                              DIR_transp_seas = mean(DIR_transp, na.rm = T),
                              DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                              GLB_att_N_seas  = sum(!is.na(GLB_att)),
                              HOR_att_N_seas  = sum(!is.na(HOR_att)),
                              DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                           by = .(Season)]



    ## _ De-seasonal by season daily mean  -------------------------------------

      ALL_1_D_bySeason_DESEAS <- merge(  ALL_1_bySeason_daily_mean,   ALL_1_bySeason_daily_seas, by = "Season", all = T)
    CLEAR_1_D_bySeason_DESEAS <- merge(CLEAR_1_bySeason_daily_mean, CLEAR_1_bySeason_daily_seas, by = "Season", all = T)
    CLOUD_1_D_bySeason_DESEAS <- merge(CLOUD_1_bySeason_daily_mean, CLOUD_1_bySeason_daily_seas, by = "Season", all = T)


    rm(  ALL_1_bySeason_daily_mean,   ALL_1_bySeason_daily_seas,
       CLEAR_1_bySeason_daily_mean, CLEAR_1_bySeason_daily_seas,
       CLOUD_1_bySeason_daily_mean, CLOUD_1_bySeason_daily_seas)
    gc()

    ## calculate anomaly
      ALL_1_D_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
      ALL_1_D_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
      ALL_1_D_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
      ALL_1_D_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLEAR_1_D_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLEAR_1_D_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLEAR_1_D_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    CLEAR_1_D_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLOUD_1_D_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLOUD_1_D_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLOUD_1_D_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    CLOUD_1_D_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]

    ## Create year from quarter!
    warning("Years in by Season are shifted by a month to match seasons")
      ALL_1_D_bySeason_DESEAS[, Year := year(Yqrt)]
    CLEAR_1_D_bySeason_DESEAS[, Year := year(Yqrt)]
    CLOUD_1_D_bySeason_DESEAS[, Year := year(Yqrt)]


    setorder(  ALL_1_D_bySeason_DESEAS, Yqrt)
    setorder(CLEAR_1_D_bySeason_DESEAS, Yqrt)
    setorder(CLOUD_1_D_bySeason_DESEAS, Yqrt)


    rm(  ALL_1_daily_mean,   ALL_1_daily_seas,
       CLEAR_1_daily_mean, CLEAR_1_daily_seas,
       CLOUD_1_daily_mean, CLOUD_1_daily_seas)
    gc()



    ## TODO
    #### WRDC ? ####

    # ## add all minutes of the days
    # xlim      <- range(DATA_all$Date)
    # alldates  <- data.frame(Date = seq(xlim[1], xlim[2], by = "min"))

    # DATA_ALLQ <- merge( DATA_all, alldates, all = T )
    # Q_all <- DATA_ALLQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
    #                       GLB_att       = mean(GLB_att,     na.rm = T),
    #                       HOR_att       = mean(HOR_att,     na.rm = T),
    #                       DIR_transp    = mean(DIR_transp,  na.rm = T),
    #                       tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
    #                       DIR_att_sd    = sd(  DIR_att,     na.rm = T),
    #                       GLB_att_sd    = sd(  GLB_att,     na.rm = T),
    #                       HOR_att_sd    = sd(  HOR_att,     na.rm = T),
    #                       DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
    #                       tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
    #                       GLB_att_N     = sum(!is.na(GLB_att)),
    #                       HOR_att_N     = sum(!is.na(HOR_att)),
    #                       DIR_att_N     = sum(!is.na(DIR_att)),
    #                       Elevat        = mean(Elevat,  na.rm = TRUE),
    #                       Azimuth       = mean(Azimuth, na.rm = TRUE),
    #                       SZA           = mean(SZA,     na.rm = TRUE)),
    #                    by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
    #                                             origin = "1970-01-01" ))
    # ]
    # rm(DATA_ALLQ)


    # DATA_CLEARQ <- merge( DATA_Clear, alldates, all = T )
    # Q_clear <- DATA_CLEARQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
    #                           GLB_att       = mean(GLB_att,     na.rm = T),
    #                           HOR_att       = mean(HOR_att,     na.rm = T),
    #                           DIR_transp    = mean(DIR_transp,  na.rm = T),
    #                           tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
    #                           DIR_att_sd    = sd(  DIR_att,     na.rm = T),
    #                           GLB_att_sd    = sd(  GLB_att,     na.rm = T),
    #                           HOR_att_sd    = sd(  HOR_att,     na.rm = T),
    #                           DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
    #                           tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
    #                           GLB_att_N     = sum(!is.na(GLB_att)),
    #                           HOR_att_N     = sum(!is.na(HOR_att)),
    #                           DIR_att_N     = sum(!is.na(DIR_att)),
    #                           Elevat        = mean(Elevat,  na.rm = TRUE),
    #                           Azimuth       = mean(Azimuth, na.rm = TRUE),
    #                           SZA           = mean(SZA,     na.rm = TRUE)),
    #                        by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
    #                                                 origin = "1970-01-01" ))
    # ]
    # rm(DATA_CLEARQ)


    # DATA_CLOUDQ <- merge( DATA_Cloud, alldates, all = T )
    # Q_cloud <- DATA_CLOUDQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
    #                           GLB_att       = mean(GLB_att,     na.rm = T),
    #                           HOR_att       = mean(HOR_att,     na.rm = T),
    #                           DIR_transp    = mean(DIR_transp,  na.rm = T),
    #                           tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
    #                           DIR_att_sd    = sd(  DIR_att,     na.rm = T),
    #                           GLB_att_sd    = sd(  GLB_att,     na.rm = T),
    #                           HOR_att_sd    = sd(  HOR_att,     na.rm = T),
    #                           DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
    #                           tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
    #                           GLB_att_N     = sum(!is.na(GLB_att)),
    #                           HOR_att_N     = sum(!is.na(HOR_att)),
    #                           DIR_att_N     = sum(!is.na(DIR_att)),
    #                           Elevat        = mean(Elevat,  na.rm = TRUE),
    #                           Azimuth       = mean(Azimuth, na.rm = TRUE),
    #                           SZA           = mean(SZA,     na.rm = TRUE)),
    #                        by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
    #                                                 origin = "1970-01-01" ))
    # ]
    # rm(DATA_CLOUDQ)



    # QH_all <- Q_all[,.(DIR_att       = mean(DIR_att,     na.rm = T),
    #                    GLB_att       = mean(GLB_att,     na.rm = T),
    #                    HOR_att       = mean(HOR_att,     na.rm = T),
    #                    DIR_transp    = mean(DIR_transp,  na.rm = T),
    #                    tsi1au_att    = mean(tsi1au_att,  na.rm = T),
    #                    DIR_att_sd    = sd(  DIR_att,     na.rm = T),
    #                    GLB_att_sd    = sd(  GLB_att,     na.rm = T),
    #                    HOR_att_sd    = sd(  HOR_att,     na.rm = T),
    #                    DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
    #                    tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
    #                    GLB_att_N     = sum(!is.na(GLB_att)),
    #                    HOR_att_N     = sum(!is.na(HOR_att)),
    #                    DIR_att_N     = sum(!is.na(DIR_att)),
    #                    Elevat        = mean(Elevat,  na.rm = TRUE),
    #                    Azimuth       = mean(Azimuth, na.rm = TRUE),
    #                    SZA           = mean(SZA,     na.rm = TRUE)),
    #                 by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
    #                                          origin = "1970-01-01" ))]

    # QH_clear <- Q_clear[,.(DIR_att       = mean(DIR_att,     na.rm = T),
    #                        GLB_att       = mean(GLB_att,     na.rm = T),
    #                        HOR_att       = mean(HOR_att,     na.rm = T),
    #                        DIR_transp    = mean(DIR_transp,  na.rm = T),
    #                        tsi1au_att    = mean(tsi1au_att,  na.rm = T),
    #                        DIR_att_sd    = sd(  DIR_att,     na.rm = T),
    #                        GLB_att_sd    = sd(  GLB_att,     na.rm = T),
    #                        HOR_att_sd    = sd(  HOR_att,     na.rm = T),
    #                        DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
    #                        tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
    #                        GLB_att_N     = sum(!is.na(GLB_att)),
    #                        HOR_att_N     = sum(!is.na(HOR_att)),
    #                        DIR_att_N     = sum(!is.na(DIR_att)),
    #                        Elevat        = mean(Elevat,  na.rm = TRUE),
    #                        Azimuth       = mean(Azimuth, na.rm = TRUE),
    #                        SZA           = mean(SZA,     na.rm = TRUE)),
    #                     by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
    #                                              origin = "1970-01-01" ))]

    # QH_cloud <- Q_cloud[,.(DIR_att       = mean(DIR_att,     na.rm = T),
    #                        GLB_att       = mean(GLB_att,     na.rm = T),
    #                        HOR_att       = mean(HOR_att,     na.rm = T),
    #                        DIR_transp    = mean(DIR_transp,  na.rm = T),
    #                        tsi1au_att    = mean(tsi1au_att,  na.rm = T),
    #                        DIR_att_sd    = sd(  DIR_att,     na.rm = T),
    #                        GLB_att_sd    = sd(  GLB_att,     na.rm = T),
    #                        HOR_att_sd    = sd(  HOR_att,     na.rm = T),
    #                        DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
    #                        tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
    #                        GLB_att_N     = sum(!is.na(GLB_att)),
    #                        HOR_att_N     = sum(!is.na(HOR_att)),
    #                        DIR_att_N     = sum(!is.na(DIR_att)),
    #                        Elevat        = mean(Elevat,  na.rm = TRUE),
    #                        Azimuth       = mean(Azimuth, na.rm = TRUE),
    #                        SZA           = mean(SZA,     na.rm = TRUE)),
    #                     by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
    #                                              origin = "1970-01-01" ))]


    # hist(QH_all[ Elevat < 20, Elevat ] )

    # ## keep only whole hours
    # QH_all <- QH_all[ !is.na(SZA) ]
    # QH_all[ GLB_att_N < 4 , GLB_att := NA ]
    # QH_all[ DIR_att_N < 4 , DIR_att := NA ]
    # QH_all[ HOR_att_N < 4 , HOR_att := NA ]

    # QH_clear <- QH_clear[ !is.na(SZA) ]
    # QH_clear[ GLB_att_N < 4 , GLB_att := NA ]
    # QH_clear[ DIR_att_N < 4 , DIR_att := NA ]
    # QH_clear[ HOR_att_N < 4 , HOR_att := NA ]

    # QH_cloud <- QH_cloud[ !is.na(SZA) ]
    # QH_cloud[ GLB_att_N < 4 , GLB_att := NA ]
    # QH_cloud[ DIR_att_N < 4 , DIR_att := NA ]
    # QH_cloud[ HOR_att_N < 4 , HOR_att := NA ]


    # QHD_all <- QH_all[,.(DIR_att       = mean(DIR_att   ),
    #                      GLB_att       = mean(GLB_att   ),
    #                      HOR_att       = mean(HOR_att   ),
    #                      DIR_transp    = mean(DIR_transp)),
    #                   by = .(Date = as.Date(Date))]

    # QHD_clear <- QH_clear[,.(DIR_att       = mean(DIR_att   ),
    #                          GLB_att       = mean(GLB_att   ),
    #                          HOR_att       = mean(HOR_att   ),
    #                          DIR_transp    = mean(DIR_transp)),
    #                       by = .(Date = as.Date(Date))]

    # QHD_cloud <- QH_cloud[,.(DIR_att       = mean(DIR_att   ),
    #                          GLB_att       = mean(GLB_att   ),
    #                          HOR_att       = mean(HOR_att   ),
    #                          DIR_transp    = mean(DIR_transp)),
    #                       by = .(Date = as.Date(Date))]

    # ## Too few days remain
    # QHD_all[   !is.na(GLB_att), .N ]
    # QHD_clear[ !is.na(GLB_att), .N ]
    # QHD_cloud[ !is.na(GLB_att), .N ]


    # ..................................................................... ----
    ####  2. Long term by SZA  -------------------------------------------------


    ## SZA test ----------------------------------------------------------------
    # test_all <- DATA_all[, .(GLB_att       = mean(GLB_att,    na.rm = T),
    #                          GLB_att_sd    = sd(  GLB_att,    na.rm = T),
    #                          GLB_att_N     = sum(!is.na(GLB_att))),
    #                      by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
    #                             Date    = as.Date(Date),
    #                             preNoon = preNoon ) ]
    #
    # test_all_year <- DATA_all[, .(GLB_att       = mean(GLB_att,    na.rm = T),
    #                               GLB_att_sd    = sd(  GLB_att,    na.rm = T),
    #                               GLB_att_N     = sum(!is.na(GLB_att))),
    #                           by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
    #                                  Year    = year(Date),
    #                                  preNoon = preNoon ) ]
    #
    # test_all_monthly <- DATA_all[, .(GLB_att       = mean(GLB_att,    na.rm = T),
    #                                  GLB_att_sd    = sd(  GLB_att,    na.rm = T),
    #                                  GLB_att_N     = sum(!is.na(GLB_att))),
    #                              by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
    #                                     Year    = year(Date),
    #                                     Month   = month(Date),
    #                                     preNoon = preNoon ) ]
    # test_all_monthly[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
    #
    # gather1 <- data.table()
    # for (asza in unique(test_all_year$SZA)) {
    #     for (pday in unique(test_all_year$preNoon)) {
    #         subdata <- test_all_year[SZA == asza & preNoon == pday]
    #         ll1     <- coefficients(lm(as.numeric(Year) ~ GLB_att, data = subdata))
    #         gather1  <- rbind(gather1,
    #                          data.frame(t(ll1),
    #                                     SZA = asza,
    #                                     preNoon = pday)
    #         )
    #     }
    # }
    # plot(gather1$SZA, gather1$GLB_att)
    #
    #
    # gather2 <- data.table()
    # for (asza in unique(test_all$SZA)) {
    #     for (pday in unique(test_all$preNoon)) {
    #         subdata <- test_all[SZA == asza & preNoon == pday]
    #         ll1     <- coefficients(lm(as.numeric(Date) ~ GLB_att, data = subdata))
    #         gather2  <- rbind(gather2,
    #                          data.frame(t(ll1),
    #                                     SZA = asza,
    #                                     preNoon = pday)
    #         )
    #     }
    # }
    # plot(gather2$SZA, gather2$GLB_att)
    #
    #
    # gather3 <- data.table()
    # for (asza in unique(test_all$SZA)) {
    #     for (pday in unique(test_all_monthly$preNoon)) {
    #         subdata <- test_all_monthly[SZA == asza & preNoon == pday]
    #         ll1     <- coefficients(lm(as.numeric(Date) ~ GLB_att, data = subdata))
    #         gather3  <- rbind(gather3,
    #                           data.frame(t(ll1),
    #                                      SZA = asza,
    #                                      preNoon = pday)
    #         )
    #     }
    # }
    # plot(gather3$SZA, gather3$GLB_att)


    ## SZA trends new approach -------------------------------------------------

    # ## create bin id
    # DATA_all[,   SZAbin := (SZA - SZA_BIN / 2 ) %/% SZA_BIN ]
    # DATA_Clear[, SZAbin := (SZA - SZA_BIN / 2 ) %/% SZA_BIN ]
    # DATA_Cloud[, SZAbin := (SZA - SZA_BIN / 2 ) %/% SZA_BIN ]
    #
    #
    # datadbs    <- c("DATA_all", "DATA_Clear", "DATA_Cloud")
    # datadbs    <- c("DATA_all")
    #
    # szabins    <- unique(DATA_all$SZAbin)
    # dayparts   <- unique(DATA_all$preNoon)
    # gridsearch <- expand.grid(dbs     = datadbs,
    #                           SZA     = szabins,
    #                           preNoon = dayparts,
    #                           stringsAsFactors = F)
    # source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
    #
    # SZA_slope     <- data.table()
    # SZA_slope_inv <- data.table()
    #
    # for (ii in 1:nrow(gridsearch)) {
    #     dd      <- gridsearch[ii,]
    #     DB      <- get(dd$dbs)
    #     subdata <- DB[SZAbin == dd$SZA & preNoon == dd$preNoon]
    #
    #     DDaily <- subdata[, .(GLB_att   = mean(GLB_att, na.rm = T),
    #                           GLB_att_N = sum(!is.na(GLB_att)) ),
    #                       by = .(Date = as.Date(Date))]
    #
    #     DMonthly <- subdata[, .(GLB_att   = mean(GLB_att, na.rm = T),
    #                             GLB_att_N = sum(!is.na(GLB_att)) ),
    #                         by = .(Year  = year(Date),
    #                                Month = month(Date))]
    #     DMonthly[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
    #
    #     DYearly <- subdata[, .(GLB_att   = mean(GLB_att, na.rm = T),
    #                            GLB_att_N = sum(!is.na(GLB_att)) ),
    #                        by = .(Year = year(Date))]
    #
    #     # lmD <- linear_fit_stats(lm(as.numeric(Date) ~ GLB_att, data = DDaily))
    #     # lmM <- linear_fit_stats(lm(as.numeric(Date) ~ GLB_att, data = DMonthly))
    #     # lmY <- linear_fit_stats(lm(as.numeric(Year) ~ GLB_att, data = DYearly))
    #
    #     lmD_inv <- linear_fit_stats(lm(GLB_att ~ as.numeric(Date), data = DDaily))
    #     lmM_inv <- linear_fit_stats(lm(GLB_att ~ as.numeric(Date), data = DMonthly))
    #     lmY_inv <- linear_fit_stats(lm(GLB_att ~ as.numeric(Year), data = DYearly))
    #
    #     ##plots
    #     plot(DDaily$Date, DDaily$GLB_att)
    #     title(paste("Daily", dd$dbs, dd$SZA))
    #     # abline(lm(as.numeric(Date) ~ GLB_att, data = DDaily),   col = "red"  )
    #     abline(lm(GLB_att ~ as.numeric(Date), data = DDaily),   col = "green")
    #     abline(a = lmD_inv$intercept,
    #            b = lmD_inv$slope, col = "blue", lty = 3)
    #
    #     plot(DMonthly$Date, DMonthly$GLB_att)
    #     title(paste("Monthly", dd$dbs, dd$SZA))
    #     # abline(lm(as.numeric(Date) ~ GLB_att, data = DMonthly), col = "red"  )
    #     abline(lm(GLB_att ~ as.numeric(Date), data = DMonthly), col = "green")
    #     abline(a = lmM_inv$intercept,
    #            b = lmM_inv$slope, col = "blue", lty = 3)
    #     #
    #     #
    #     plot(DYearly$Year, DYearly$GLB_att)
    #     title(paste("Yearly", dd$dbs, dd$SZA))
    #     # abline(lm(as.numeric(Year) ~ GLB_att, data = DYearly),  col = "red"  )
    #     abline(lm(GLB_att ~ as.numeric(Year), data = DYearly),  col = "green")
    #     abline(a = lmY_inv$intercept,
    #            b = lmY_inv$slope, col = "blue", lty = 3)
    #
    #
    #
    #     # SZA_slope <-
    #     #     rbind(SZA_slope,
    #     #           data.frame(lmD,
    #     #                      lm_N    = sum(!is.na(DDaily$GLB_att)),
    #     #                      SZA     = dd$SZA,
    #     #                      preNoon = dd$preNoon,
    #     #                      DATA    = dd$dbs,
    #     #                      aggr    = "Daily"),
    #     #           data.frame(lmM,
    #     #                      lm_N    = sum(!is.na(DMonthly$GLB_att)),
    #     #                      SZA     = dd$SZA,
    #     #                      preNoon = dd$preNoon,
    #     #                      DATA    = dd$dbs,
    #     #                      aggr    = "Monthly"),
    #     #           data.frame(lmY,
    #     #                      lm_N    = sum(!is.na(DYearly$GLB_att)),
    #     #                      SZA     = dd$SZA,
    #     #                      preNoon = dd$preNoon,
    #     #                      DATA    = dd$dbs,
    #     #                      aggr    = "Yearly"))
    #
    #     SZA_slope_inv <-
    #         rbind(SZA_slope_inv,
    #               data.frame(lmD_inv,
    #                          lm_N    = sum(!is.na(DDaily$GLB_att)),
    #                          SZA     = dd$SZA,
    #                          preNoon = dd$preNoon,
    #                          DATA    = dd$dbs,
    #                          aggr    = "Daily"),
    #               data.frame(lmM_inv,
    #                          lm_N    = sum(!is.na(DMonthly$GLB_att)),
    #                          SZA     = dd$SZA,
    #                          preNoon = dd$preNoon,
    #                          DATA    = dd$dbs,
    #                          aggr    = "Monthly"),
    #               data.frame(lmY_inv,
    #                          lm_N    = sum(!is.na(DYearly$GLB_att)),
    #                          SZA     = dd$SZA,
    #                          preNoon = dd$preNoon,
    #                          DATA    = dd$dbs,
    #                          aggr    = "Yearly"))
    #
    # }

    #_ One way -----------------------------------------------------------------

    ## This should be wrong!!!

    # SZA_slope[aggr == "Yearly",
    #           slopePC := slope  ]
    #
    # SZA_slope[aggr %in% c("Daily"),
    #           slopePC := 100 * slope / Days_of_year ]
    #
    # SZA_slope[aggr %in% c("Monthly"),
    #           slopePC := (100 * slope / Days_of_year) / 12 ]
    #
    # # SZA_slope[ SZA > 70, slopePC := NA]
    # # SZA_slope[ slope.p > 0.1, slopePC := NA]
    #
    # for (aDATA in unique( SZA_slope$DATA )) {
    #     for (aAggr in unique( SZA_slope$aggr )) {
    #         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
    #
    #         plot(pp$SZA, pp$slopePC )
    #         title(paste(aDATA, aAggr))
    #
    #     }
    # }



    # for (aDATA in unique( SZA_slope$DATA )) {
    #     for (aAggr in  c("Daily", "Monthly")) {
    #         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
    #         plot(pp$SZA, pp$slopePC )
    #         title(paste(aDATA, aAggr))
    #
    #     }
    # }
    #
    # for (aDATA in unique( SZA_slope$DATA )) {
    #     for (aAggr in c("Daily", "Monthly")) {
    #         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
    #
    #         plot(pp$SZA, 100 * pp$slope / Days_of_year )
    #         title(paste(aDATA, aAggr))
    #
    #     }
    # }
    #
    # for (aDATA in unique( SZA_slope$DATA )) {
    #     for (aAggr in c("Yearly")) {
    #         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
    #
    #         plot(pp$SZA, 100 * pp$slope )
    #         title(paste(aDATA, aAggr))
    #
    #     }
    # }

    #_ Inverted way ------------------------------------------------------------

    # for (aDATA in unique( SZA_slope_inv$DATA )) {
    #     for (aAggr in unique( SZA_slope_inv$aggr )) {
    #         pp <- SZA_slope_inv[DATA == aDATA & aggr == aAggr]
    #
    #         plot(pp$SZA, pp$slope)
    #         title(paste(aDATA, aAggr))
    #
    #     }
    # }


# save.image(file="temp.Rdata")
# stop()
# load(file="temp.Rdata")


    ##  Daily SZA means --------------------------------------------------------

    ## _ daily means  ----------------------------------------------------------
    ALL_2_daily_mean <-
        DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                     HOR_att       = mean(HOR_att,    na.rm = T),
                     GLB_att       = mean(GLB_att,    na.rm = T),
                     # DIR_transp    = mean(DIR_transp, na.rm = T),
                     DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                     HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                     GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                     # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                     doy           = yday(Date),
                     GLB_att_N     = sum(!is.na(GLB_att)),
                     HOR_att_N     = sum(!is.na(HOR_att)),
                     DIR_att_N     = sum(!is.na(DIR_att))  ),
                 by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                        Date    = Day,
                        preNoon = preNoon ) ]

    CLEAR_2_daily_mean <-
        DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       # DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       doy           = yday(Date),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Date    = Day,
                          preNoon = preNoon ) ]

    CLOUD_2_daily_mean <-
        DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       # DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       doy           = yday(Date),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Date    = Day,
                          preNoon = preNoon ) ]


    ## _ Aggregation limit with SZA_aggregation_N_lim data points --------------
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    # ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    # ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    # ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]

    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    # CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    # CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    # CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]

    CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    CLOUD_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    CLOUD_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    # CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    CLOUD_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    CLOUD_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    # CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    CLOUD_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    CLOUD_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    # CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]





    ##  _ Seasonal daily -------------------------------------------------------
    ALL_2_daily_seas <-
        ALL_2_daily_mean[,
                         .(
                             DIR_att_seas       = mean(DIR_att,    na.rm = T),
                             HOR_att_seas       = mean(HOR_att,    na.rm = T),
                             GLB_att_seas       = mean(GLB_att,    na.rm = T),
                             # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                             DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                             # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                             GLB_att_N_seas     = sum(!is.na(GLB_att)),
                             HOR_att_N_seas     = sum(!is.na(HOR_att)),
                             DIR_att_N_seas     = sum(!is.na(DIR_att))
                         ),
                         by = .(
                             doy,
                             SZA,
                             preNoon
                         )]

    CLEAR_2_daily_seas <-
        CLEAR_2_daily_mean[,
                           .(
                               DIR_att_seas       = mean(DIR_att,    na.rm = T),
                               HOR_att_seas       = mean(HOR_att,    na.rm = T),
                               GLB_att_seas       = mean(GLB_att,    na.rm = T),
                               # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                               # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                               GLB_att_N_seas     = sum(!is.na(GLB_att)),
                               HOR_att_N_seas     = sum(!is.na(HOR_att)),
                               DIR_att_N_seas     = sum(!is.na(DIR_att))
                           ),
                           by = .(
                               doy,
                               SZA,
                               preNoon
                           )]

    CLOUD_2_daily_seas <-
        CLOUD_2_daily_mean[,
                           .(
                               DIR_att_seas       = mean(DIR_att,    na.rm = T),
                               HOR_att_seas       = mean(HOR_att,    na.rm = T),
                               GLB_att_seas       = mean(GLB_att,    na.rm = T),
                               # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                               # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                               GLB_att_N_seas     = sum(!is.na(GLB_att)),
                               HOR_att_N_seas     = sum(!is.na(HOR_att)),
                               DIR_att_N_seas     = sum(!is.na(DIR_att))
                           ),
                           by = .(
                               doy,
                               SZA,
                               preNoon
                           )]


    ## _ Margin of error for confidence interval  ------------------------------
    conf_param  <- 1 - (1 - Daily_confidence_limit) / 2
    suppressWarnings({
        ALL_2_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        ALL_2_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        ALL_2_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        # ALL_2_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLEAR_2_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLEAR_2_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLEAR_2_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        # CLEAR_2_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLOUD_2_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLOUD_2_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLOUD_2_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        # CLOUD_2_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
    })


    ## _ Daily de-seasonal relative anomaly ------------------------------------

    ALL_2_daily_DESEAS   <- merge(  ALL_2_daily_mean,   ALL_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)
    CLEAR_2_daily_DESEAS <- merge(CLEAR_2_daily_mean, CLEAR_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)
    CLOUD_2_daily_DESEAS <- merge(CLOUD_2_daily_mean, CLOUD_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)

    setorder(  ALL_2_daily_DESEAS, Date)
    setorder(CLEAR_2_daily_DESEAS, Date)
    setorder(CLOUD_2_daily_DESEAS, Date)

    ## forget some daily data
    rm(  ALL_2_daily_seas,
       CLEAR_2_daily_seas,
       CLOUD_2_daily_seas)
    gc()

    ### Using the % departure from seasonal values

      ALL_2_daily_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
      ALL_2_daily_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
      ALL_2_daily_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    # ALL_2_daily_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLEAR_2_daily_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLEAR_2_daily_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLEAR_2_daily_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    # CLEAR_2_daily_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLOUD_2_daily_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLOUD_2_daily_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLOUD_2_daily_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    # CLOUD_2_daily_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]





    ## Monthly SZA means -------------------------------------------------------

    ## _ monthly means ---------------------------------------------------------
    ALL_2_monthly_mean <-
        ALL_2_daily_mean[,
                         .(
                             DIR_att       = mean(DIR_att,    na.rm = T),
                             HOR_att       = mean(HOR_att,    na.rm = T),
                             GLB_att       = mean(GLB_att,    na.rm = T),
                             # DIR_transp    = mean(DIR_transp, na.rm = T),
                             DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                             # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                             GLB_att_N     = sum(!is.na(GLB_att)),
                             HOR_att_N     = sum(!is.na(HOR_att)),
                             DIR_att_N     = sum(!is.na(DIR_att))
                         ),
                         by = .(
                             # SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                             SZA     = SZA,
                             Year    = year(Date),
                             Month   = month(Date),
                             preNoon = preNoon
                         ) ]
    ALL_2_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]



    CLEAR_2_monthly_mean <-
        CLEAR_2_daily_mean[,
                           .(
                               DIR_att       = mean(DIR_att,    na.rm = T),
                               HOR_att       = mean(HOR_att,    na.rm = T),
                               GLB_att       = mean(GLB_att,    na.rm = T),
                               # DIR_transp    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                               # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                               GLB_att_N     = sum(!is.na(GLB_att)),
                               HOR_att_N     = sum(!is.na(HOR_att)),
                               DIR_att_N     = sum(!is.na(DIR_att))
                           ),
                           by = .(
                               # SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                               SZA     = SZA,
                               Year    = year(Date),
                               Month   = month(Date),
                               preNoon = preNoon
                           ) ]
    CLEAR_2_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


    CLOUD_2_monthly_mean <-
        CLOUD_2_daily_mean[,
                           .(
                               DIR_att       = mean(DIR_att,    na.rm = T),
                               HOR_att       = mean(HOR_att,    na.rm = T),
                               GLB_att       = mean(GLB_att,    na.rm = T),
                               # DIR_transp    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                               # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                               GLB_att_N     = sum(!is.na(GLB_att)),
                               HOR_att_N     = sum(!is.na(HOR_att)),
                               DIR_att_N     = sum(!is.na(DIR_att))
                           ),
                           by = .(
                               # SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                               SZA     = SZA,
                               Year    = year(Date),
                               Month   = month(Date),
                               preNoon = preNoon
                           ) ]
    CLOUD_2_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]

    rm(   ALL_2_daily_mean,
          CLEAR_2_daily_mean,
          CLOUD_2_daily_mean )
    gc()



    hist(CLOUD_2_monthly_mean[, GLB_att_N], breaks = 100)
    hist(CLEAR_2_monthly_mean[, GLB_att_N], breaks = 100)
    hist(  ALL_2_monthly_mean[, GLB_att_N], breaks = 100)

    table(CLOUD_2_monthly_mean[, GLB_att_N])
    table(CLEAR_2_monthly_mean[, GLB_att_N])


    ## _ Aggregation Representation limits?? -----------------------------------

    CLOUD_2_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]
    CLEAR_2_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]
      ALL_2_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]

stop("TTt")
    ## _ Seasonal monthly ------------------------------------------------------
    ALL_2_monthly_seas <-
        ALL_2_monthly_mean[,
                           .(
                               DIR_att_seas       = mean(DIR_att,    na.rm = T),
                               HOR_att_seas       = mean(HOR_att,    na.rm = T),
                               GLB_att_seas       = mean(GLB_att,    na.rm = T),
                               # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                               # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                               GLB_att_N_seas     = sum(!is.na(GLB_att)),
                               HOR_att_N_seas     = sum(!is.na(HOR_att)),
                               DIR_att_N_seas     = sum(!is.na(DIR_att))
                           ),
                           by = .(
                               Month   = month(Date),
                               SZA,
                               preNoon
                           )]

    CLEAR_2_monthly_seas <-
        CLEAR_2_monthly_mean[,
                             .(
                                 DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                 HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                 GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                 # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                 # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                                 GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                 HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                 DIR_att_N_seas     = sum(!is.na(DIR_att))
                             ),
                             by = .(
                                 Month   = month(Date),
                                 SZA,
                                 preNoon
                             )]

    CLOUD_2_monthly_seas <-
        CLOUD_2_monthly_mean[,
                             .(
                                 DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                 HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                 GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                 # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                 # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                                 GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                 HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                 DIR_att_N_seas     = sum(!is.na(DIR_att))
                             ),
                             by = .(
                                 Month   = month(Date),
                                 SZA,
                                 preNoon
                             )]


    ## _ Monthly de-seasonal relative anomaly ----------------------------------

      ALL_2_monthly_DESEAS <- merge(  ALL_2_monthly_mean,   ALL_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
    CLEAR_2_monthly_DESEAS <- merge(CLEAR_2_monthly_mean, CLEAR_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
    CLOUD_2_monthly_DESEAS <- merge(CLOUD_2_monthly_mean, CLOUD_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)

    setorder(  ALL_2_monthly_DESEAS, Date)
    setorder(CLEAR_2_monthly_DESEAS, Date)
    setorder(CLOUD_2_monthly_DESEAS, Date)

    ## forget some daily data (want monthly_mean for yearly)
    rm(  ALL_2_monthly_seas,
       CLEAR_2_monthly_seas,
       CLOUD_2_monthly_seas)
    gc()

    ### Using the % departure from seasonal values
      ALL_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att - GLB_att_seas ) / GLB_att_seas ]
    CLEAR_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att - GLB_att_seas ) / GLB_att_seas ]
    CLOUD_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att - GLB_att_seas ) / GLB_att_seas ]




    ## Yearly SZA means --------------------------------------------------------
    ALL_2_yearly_mean <-
        ALL_2_monthly_mean[,
                           .(
                               DIR_att       = mean(DIR_att,    na.rm = T),
                               HOR_att       = mean(HOR_att,    na.rm = T),
                               GLB_att       = mean(GLB_att,    na.rm = T),
                               # DIR_transp    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                               # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                               GLB_att_N     = sum(!is.na(GLB_att)),
                               HOR_att_N     = sum(!is.na(HOR_att)),
                               DIR_att_N     = sum(!is.na(DIR_att))
                           ),
                           by = .(
                               SZA     = SZA,
                               Year    = year(Date),
                               preNoon = preNoon
                           ) ]

    CLEAR_2_yearly_mean <-
        CLEAR_2_monthly_mean[,
                             .(
                                 DIR_att       = mean(DIR_att,    na.rm = T),
                                 HOR_att       = mean(HOR_att,    na.rm = T),
                                 GLB_att       = mean(GLB_att,    na.rm = T),
                                 # DIR_transp    = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                 # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                 GLB_att_N     = sum(!is.na(GLB_att)),
                                 HOR_att_N     = sum(!is.na(HOR_att)),
                                 DIR_att_N     = sum(!is.na(DIR_att))  ),
                             by = .(
                                 SZA     = SZA,
                                 Year    = year(Date),
                                 preNoon = preNoon
                             ) ]

    CLOUD_2_yearly_mean <-
        CLOUD_2_monthly_mean[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       # DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(
                       SZA     = SZA,
                       Year    = year(Date),
                       preNoon = preNoon
                   ) ]



    ## forget some daily data (want monthly_mean for yearly)
    rm(  ALL_2_monthly_mean,
       CLEAR_2_monthly_mean,
       CLOUD_2_monthly_mean)
    gc()





    ## _ Margin of error calculation for confidence interval -------------------
    # conf_param  <- 1 - ( 1 - SZA_confidence_limit ) / 2
    # suppressWarnings({
    #     ALL_2_daily_mean[,   DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
    #     ALL_2_daily_mean[,   HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
    #     ALL_2_daily_mean[,   GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
    #     # ALL_2_daily_mean[,   DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
    #     CLEAR_2_daily_mean[, DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
    #     CLEAR_2_daily_mean[, HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
    #     CLEAR_2_daily_mean[, GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
    #     # CLEAR_2_daily_mean[, DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
    #     CLOUD_2_daily_mean[, DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
    #     CLOUD_2_daily_mean[, HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
    #     CLOUD_2_daily_mean[, GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
    #     # CLOUD_2_daily_mean[, DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
    # })



    ## Season of year SZA ------------------------------------------------------

    ## Quarter of year with one month shift to include December in the next years winter
      DATA_all[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
    DATA_Clear[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
    DATA_Cloud[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]

    ## Flag seasons using quarters
      DATA_all[season_Yqrt %% 1 == 0   , Season := "Winter"]
      DATA_all[season_Yqrt %% 1 == 0.25, Season := "Spring"]
      DATA_all[season_Yqrt %% 1 == 0.50, Season := "Summer"]
      DATA_all[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
    DATA_Clear[season_Yqrt %% 1 == 0   , Season := "Winter"]
    DATA_Clear[season_Yqrt %% 1 == 0.25, Season := "Spring"]
    DATA_Clear[season_Yqrt %% 1 == 0.50, Season := "Summer"]
    DATA_Clear[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
    DATA_Cloud[season_Yqrt %% 1 == 0   , Season := "Winter"]
    DATA_Cloud[season_Yqrt %% 1 == 0.25, Season := "Spring"]
    DATA_Cloud[season_Yqrt %% 1 == 0.50, Season := "Summer"]
    DATA_Cloud[season_Yqrt %% 1 == 0.75, Season := "Autumn"]


    # ## add season of year tag
    #   DATA_all[  month(Date) %in% c(12, 1, 2), Season := "Winter"]
    #   DATA_all[  month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    #   DATA_all[  month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    #   DATA_all[  month(Date) %in% c( 9,10,11), Season := "Autumn"]
    # DATA_Clear[month(Date) %in% c(12, 1, 2), Season := "Winter"]
    # DATA_Clear[month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    # DATA_Clear[month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    # DATA_Clear[month(Date) %in% c( 9,10,11), Season := "Autumn"]
    # DATA_Cloud[month(Date) %in% c(12, 1, 2), Season := "Winter"]
    # DATA_Cloud[month(Date) %in% c( 3, 4, 5), Season := "Spring"]
    # DATA_Cloud[month(Date) %in% c( 6, 7, 8), Season := "Summer"]
    # DATA_Cloud[month(Date) %in% c( 9,10,11), Season := "Autumn"]

    ## _ Daily mean by season --------------------------------------------------
    ALL_2_bySeason_daily_mean <-
        DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                     HOR_att       = mean(HOR_att,    na.rm = T),
                     GLB_att       = mean(GLB_att,    na.rm = T),
                     # DIR_transp    = mean(DIR_transp, na.rm = T),
                     DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                     HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                     GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                     # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                     doy           = yday(Date),
                     GLB_att_N     = sum(!is.na(GLB_att)),
                     HOR_att_N     = sum(!is.na(HOR_att)),
                     DIR_att_N     = sum(!is.na(DIR_att))  ),
                 by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                        Date    = Day,
                        preNoon = preNoon,
                        Yqrt    = season_Yqrt)]

    CLEAR_2_bySeason_daily_mean <-
        DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       # DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       doy           = yday(Date),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Date    = Day,
                          preNoon = preNoon,
                          Yqrt    = season_Yqrt)]

    CLOUD_2_bySeason_daily_mean <-
        DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       # DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       doy           = yday(Date),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Date    = Day,
                          preNoon = preNoon,
                          Yqrt    = season_Yqrt)]

    ## _ Exclude means with less than SZA_aggregation_N_lim data points --------
    ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    ALL_2_bySeason_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    ALL_2_bySeason_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    # ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    ALL_2_bySeason_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    ALL_2_bySeason_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    # ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    ALL_2_bySeason_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    ALL_2_bySeason_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    # ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]

    CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    CLEAR_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    CLEAR_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    # CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    CLEAR_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    CLEAR_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    # CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    CLEAR_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    CLEAR_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    # CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]

    CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    CLOUD_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    CLOUD_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    # CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    CLOUD_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    CLOUD_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    # CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    CLOUD_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    CLOUD_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    # CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]






    ## __ Monthly mean by season  ----------------------------------------------
    ALL_2_bySeason_monthly_mean <-
        ALL_2_bySeason_daily_mean[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                      HOR_att       = mean(HOR_att,    na.rm = T),
                                      GLB_att       = mean(GLB_att,    na.rm = T),
                                      # DIR_transp    = mean(DIR_transp, na.rm = T),
                                      DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                      HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                      GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                      # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                      doy           = yday(Date),
                                      GLB_att_N     = sum(!is.na(GLB_att)),
                                      HOR_att_N     = sum(!is.na(HOR_att)),
                                      DIR_att_N     = sum(!is.na(DIR_att))  ),
                                  by = .(SZA     = SZA,
                                         Month   = month(Date),
                                         Year    = year(Date),
                                         preNoon = preNoon,
                                         Yqrt    = Yqrt)]
    ALL_2_bySeason_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


    CLEAR_2_bySeason_monthly_mean <-
        CLEAR_2_bySeason_daily_mean[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                                        HOR_att       = mean(HOR_att,    na.rm = T),
                                        GLB_att       = mean(GLB_att,    na.rm = T),
                                        # DIR_transp    = mean(DIR_transp, na.rm = T),
                                        DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                        HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                        GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                        # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                        doy           = yday(Date),
                                        GLB_att_N     = sum(!is.na(GLB_att)),
                                        HOR_att_N     = sum(!is.na(HOR_att)),
                                        DIR_att_N     = sum(!is.na(DIR_att))  ),
                                    by = .(SZA     = SZA,
                                           Month   = month(Date),
                                           Year    = year(Date),
                                           preNoon = preNoon,
                                           Yqrt    = Yqrt)]
    CLEAR_2_bySeason_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


    CLOUD_2_bySeason_monthly_mean <-
        CLOUD_2_bySeason_daily_mean[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       # DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       doy           = yday(Date),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(SZA     = SZA,
                          Month   = month(Date),
                          Year    = year(Date),
                          preNoon = preNoon,
                          Yqrt    = Yqrt)]
    CLOUD_2_bySeason_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


    ## __ Yearly mean by season  -----------------------------------------------
    ALL_2_bySeason_yearly_mean <-
        DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                     HOR_att       = mean(HOR_att,    na.rm = T),
                     GLB_att       = mean(GLB_att,    na.rm = T),
                     DIR_transp    = mean(DIR_transp, na.rm = T),
                     DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                     HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                     GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                     DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                     GLB_att_N     = sum(!is.na(GLB_att)),
                     HOR_att_N     = sum(!is.na(HOR_att)),
                     DIR_att_N     = sum(!is.na(DIR_att))  ),
                 by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                        Year    = year(Day),
                        preNoon = preNoon,
                        Yqrt    = season_Yqrt)]

    CLEAR_2_bySeason_yearly_mean <-
        DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Year    = year(Day),
                          preNoon = preNoon,
                          Yqrt    = season_Yqrt)]

    CLOUD_2_bySeason_yearly_mean <-
        DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Year    = year(Day),
                          preNoon = preNoon,
                          Yqrt    = season_Yqrt)]



    ## Flag seasons of year using quarters
      ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
      ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
      ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
      ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]

      ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
      ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
      ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
      ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]

      ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
      ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
      ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
      ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
    CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
    CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
    CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
    CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]





    ## _ Seasonal by season SZA values -----------------------------------------

    ALL_2_bySeason_daily_seas <-
        ALL_2_bySeason_daily_mean[, .(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                      HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                      GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                      # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                      DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                      HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                      GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                      # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                      GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                      HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                      DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                                  by = .(SZA     = SZA,
                                         doy     = yday(Date),
                                         preNoon = preNoon,
                                         Season  = Season) ]

    CLEAR_2_bySeason_daily_seas <-
        CLEAR_2_bySeason_daily_mean[, .(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                        HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                        GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                        # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                        DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                        HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                        GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                        # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                        GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                        HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                        DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                                    by = .(SZA     = SZA,
                                           doy     = yday(Date),
                                           preNoon = preNoon,
                                           Season  = Season) ]

    CLOUD_2_bySeason_daily_seas <-
        CLOUD_2_bySeason_daily_mean[, .(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                        HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                        GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                        # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                        DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                        HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                        GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                        # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                        GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                        HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                        DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                                    by = .(SZA     = SZA,
                                           doy     = yday(Date),
                                           preNoon = preNoon,
                                           Season  = Season) ]


      ALL_2_bySeason_daily_DESEAS <- merge(  ALL_2_bySeason_daily_mean,   ALL_2_bySeason_daily_seas, by = c("SZA", "doy", "preNoon", "Season"), all = T)
    CLEAR_2_bySeason_daily_DESEAS <- merge(CLEAR_2_bySeason_daily_mean, CLEAR_2_bySeason_daily_seas, by = c("SZA", "doy", "preNoon", "Season"), all = T)
    CLOUD_2_bySeason_daily_DESEAS <- merge(CLOUD_2_bySeason_daily_mean, CLOUD_2_bySeason_daily_seas, by = c("SZA", "doy", "preNoon", "Season"), all = T)

    ## _ Relative anomaly by season --------------------------------------------
      ALL_2_bySeason_daily_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
      ALL_2_bySeason_daily_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
      ALL_2_bySeason_daily_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
      # ALL_2_bySeason_daily_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLEAR_2_bySeason_daily_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLEAR_2_bySeason_daily_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLEAR_2_bySeason_daily_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    # CLEAR_2_bySeason_daily_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLOUD_2_bySeason_daily_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLOUD_2_bySeason_daily_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLOUD_2_bySeason_daily_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    # CLOUD_2_bySeason_daily_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]

    ## Create year from quarter!
    warning("Years in by Season are shifted by a month to match seasons")
      ALL_2_bySeason_daily_DESEAS[, Year := year(Yqrt)]
    CLEAR_2_bySeason_daily_DESEAS[, Year := year(Yqrt)]
    CLOUD_2_bySeason_daily_DESEAS[, Year := year(Yqrt)]

    setorder(  ALL_2_bySeason_daily_DESEAS, Yqrt)
    setorder(CLEAR_2_bySeason_daily_DESEAS, Yqrt)
    setorder(CLOUD_2_bySeason_daily_DESEAS, Yqrt)

    rm(  ALL_2_bySeason_daily_mean,   ALL_2_bySeason_daily_seas,
       CLEAR_2_bySeason_daily_mean, CLEAR_2_bySeason_daily_seas,
       CLOUD_2_bySeason_daily_mean, CLOUD_2_bySeason_daily_seas)
    gc()




    ##  Daily seasonal values by doy prenoon SZA  ------------------------------




    ##  Monthly seasonal values by doy prenoon SZA  ----------------------------

    ##  _ Seasonal monthly -----------------------------------------------------
    ALL_2_monthly_seas <-
        ALL_2_monthly_mean[,
                           .(
                               DIR_att_seas       = mean(DIR_att,    na.rm = T),
                               HOR_att_seas       = mean(HOR_att,    na.rm = T),
                               GLB_att_seas       = mean(GLB_att,    na.rm = T),
                               # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                               DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                               # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                               GLB_att_N_seas     = sum(!is.na(GLB_att)),
                               HOR_att_N_seas     = sum(!is.na(HOR_att)),
                               DIR_att_N_seas     = sum(!is.na(DIR_att))
                           ),
                           by = .(
                               Month,
                               SZA,
                               preNoon
                           )
        ]

    CLEAR_2_monthly_seas <-
        CLEAR_2_monthly_mean[,
                             .(
                                 DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                 HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                 GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                 # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                 # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                                 GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                 HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                 DIR_att_N_seas     = sum(!is.na(DIR_att))
                             ),
                             by = .(
                                 Month,
                                 SZA,
                                 preNoon
                             )
        ]

    CLOUD_2_monthly_seas <-
        CLOUD_2_monthly_mean[,
                             .(
                                 DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                 HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                 GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                 # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                 # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                                 GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                 HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                 DIR_att_N_seas     = sum(!is.na(DIR_att))
                             ),
                             by = .(
                                 Month,
                                 SZA,
                                 preNoon
                             )
        ]


    ## _ Margin of error for confidence interval  ------------------------------
    conf_param  <- 1 - (1 - Daily_confidence_limit) / 2
    suppressWarnings({
          ALL_2_monthly_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
          ALL_2_monthly_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
          ALL_2_monthly_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
          # ALL_2_monthly_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLEAR_2_monthly_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLEAR_2_monthly_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLEAR_2_monthly_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        # CLEAR_2_monthly_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLOUD_2_monthly_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLOUD_2_monthly_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLOUD_2_monthly_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        # CLOUD_2_monthly_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
    })


    ## _ Monthly de-seasonal anomaly ---------------------------------------------

      ALL_2_monthly_DESEAS <- merge(  ALL_2_monthly_mean,   ALL_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
    CLEAR_2_monthly_DESEAS <- merge(CLEAR_2_monthly_mean, CLEAR_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
    CLOUD_2_monthly_DESEAS <- merge(CLOUD_2_monthly_mean, CLOUD_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)

    setorder(  ALL_2_monthly_DESEAS, Date)
    setorder(CLEAR_2_monthly_DESEAS, Date)
    setorder(CLOUD_2_monthly_DESEAS, Date)

    ## forget monthly data
    rm(  ALL_2_monthly_mean,   ALL_2_monthly_seas,
       CLEAR_2_monthly_mean, CLEAR_2_monthly_seas,
       CLOUD_2_monthly_mean, CLOUD_2_monthly_seas)


    ## _ Monthly relative anomaly ------------------------------------------------

    ### Using the % departure from seasonal values

      ALL_2_monthly_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
      ALL_2_monthly_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
      ALL_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
      # ALL_2_monthly_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLEAR_2_monthly_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLEAR_2_monthly_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLEAR_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    # CLEAR_2_monthly_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLOUD_2_monthly_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLOUD_2_monthly_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
    CLOUD_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    # CLOUD_2_monthly_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]




    # ..................................................................... ----
    ####  3. Consistency of trends  ############################################

    ## _ Monthly means by SZA prenoon month ------------------------------------

    ## Will create values for am, pm, and daily

    ALL_3_monthly_meanA <-
        DATA_all[,.(DIR_att       = mean(DIR_att,    na.rm = T),
                    GLB_att       = mean(GLB_att,    na.rm = T),
                    HOR_att       = mean(HOR_att,    na.rm = T),
                    DIR_transp    = mean(DIR_transp, na.rm = T),
                    DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                    HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                    GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                    DIR_transp_sd = sd(DIR_transp, na.rm = T),
                    HOR_att_N     = sum(!is.na(HOR_att)),
                    GLB_att_N     = sum(!is.na(GLB_att)),
                    DIR_att_N     = sum(!is.na(DIR_att))  ),
                 by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                        Year    = year(Date),
                        Month   = month(Date),
                        preNoon = preNoon)]

    ALL_3_monthly_meanB <-
        DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                     GLB_att       = mean(GLB_att,    na.rm = T),
                     HOR_att       = mean(HOR_att,    na.rm = T),
                     DIR_transp    = mean(DIR_transp, na.rm = T),
                     DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                     HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                     GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                     DIR_transp_sd = sd(DIR_transp, na.rm = T),
                     HOR_att_N     = sum(!is.na(HOR_att)),
                     GLB_att_N     = sum(!is.na(GLB_att)),
                     DIR_att_N     = sum(!is.na(DIR_att)),
                     preNoon       = "am+pm"),
                 by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                        Year    = year(Date),
                        Month   = month(Date))]

    ALL_3_monthly_mean <- data.table(rbind( data.frame(ALL_3_monthly_meanB),
                                            data.frame(ALL_3_monthly_meanA) ))
    rm(ALL_3_monthly_meanA, ALL_3_monthly_meanB)
    ALL_3_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


    CLEAR_3_monthly_meanA <-
        DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       DIR_transp_sd = sd(DIR_transp, na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Year    = year(Date),
                          Month   = month(Date),
                          preNoon = preNoon)]

    CLEAR_3_monthly_meanB <-
        DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       DIR_transp_sd = sd(DIR_transp, na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att)),
                       preNoon       = "am+pm"),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Year    = year(Date),
                          Month   = month(Date))]

    CLEAR_3_monthly_mean <- data.table(rbind(data.frame(CLEAR_3_monthly_meanB),
                                             data.frame(CLEAR_3_monthly_meanA) ))
    rm(CLEAR_3_monthly_meanA, CLEAR_3_monthly_meanB)
    CLEAR_3_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


    CLOUD_3_monthly_meanA <-
        DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       DIR_transp_sd = sd(DIR_transp, na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att))),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Year    = year(Date),
                          Month   = month(Date),
                          preNoon = preNoon)]

    CLOUD_3_monthly_meanB <-
        DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                       HOR_att       = mean(HOR_att,    na.rm = T),
                       GLB_att       = mean(GLB_att,    na.rm = T),
                       DIR_transp    = mean(DIR_transp, na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                       DIR_transp_sd = sd(DIR_transp, na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att)),
                       preNoon       = "am+pm"),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Year    = year(Date),
                          Month   = month(Date))]

    CLOUD_3_monthly_mean <- data.table(rbind(data.frame(CLOUD_3_monthly_meanB),
                                             data.frame(CLOUD_3_monthly_meanA) ))
    rm(CLOUD_3_monthly_meanA, CLOUD_3_monthly_meanB)
    CLOUD_3_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]

    ## _ Margin of error calculation -------------------------------------------
    conf_param  <- 1 - ( 1 - Monthly_confidence_limit ) / 2
    suppressWarnings({
        ALL_3_monthly_mean[,  DIR_att_EM   :=qt(conf_param,df=DIR_att_N -1)* DIR_att_sd   /sqrt(DIR_att_N)]
        ALL_3_monthly_mean[,  HOR_att_EM   :=qt(conf_param,df=HOR_att_N -1)* HOR_att_sd   /sqrt(HOR_att_N)]
        ALL_3_monthly_mean[,  GLB_att_EM   :=qt(conf_param,df=GLB_att_N -1)* GLB_att_sd   /sqrt(GLB_att_N)]
        ALL_3_monthly_mean[,  DIR_transp_EM:=qt(conf_param,df=DIR_att_N -1)* DIR_transp_sd/sqrt(DIR_att_N)]
        CLEAR_3_monthly_mean[,DIR_att_EM   :=qt(conf_param,df=DIR_att_N -1)* DIR_att_sd   /sqrt(DIR_att_N)]
        CLEAR_3_monthly_mean[,HOR_att_EM   :=qt(conf_param,df=HOR_att_N -1)* HOR_att_sd   /sqrt(HOR_att_N)]
        CLEAR_3_monthly_mean[,GLB_att_EM   :=qt(conf_param,df=GLB_att_N -1)* GLB_att_sd   /sqrt(GLB_att_N)]
        CLEAR_3_monthly_mean[,DIR_transp_EM:=qt(conf_param,df=DIR_att_N -1)* DIR_transp_sd/sqrt(DIR_att_N)]
        CLOUD_3_monthly_mean[,DIR_att_EM   :=qt(conf_param,df=DIR_att_N -1)* DIR_att_sd   /sqrt(DIR_att_N)]
        CLOUD_3_monthly_mean[,HOR_att_EM   :=qt(conf_param,df=HOR_att_N -1)* HOR_att_sd   /sqrt(HOR_att_N)]
        CLOUD_3_monthly_mean[,GLB_att_EM   :=qt(conf_param,df=GLB_att_N -1)* GLB_att_sd   /sqrt(GLB_att_N)]
        CLOUD_3_monthly_mean[,DIR_transp_EM:=qt(conf_param,df=DIR_att_N -1)* DIR_transp_sd/sqrt(DIR_att_N)]
    })

    ## _ Exclude means with less than Monthly_aggegation_N_lim data points -----
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att       := NA]
    ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att       := NA]
    ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att       := NA]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp    := NA]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_sd    := NA]
    ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_sd    := NA]
    ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_sd    := NA]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_sd := NA]
    ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_EM    := NA]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_EM    := NA]
    ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_EM    := NA]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_EM := NA]

    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
    CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
    CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
    CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
    CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
    CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
    CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]

    CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
    CLOUD_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
    CLOUD_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
    CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
    CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
    CLOUD_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
    CLOUD_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
    CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
    CLOUD_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
    CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
    CLOUD_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
    CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]


    ## _  Monthly seasonal values ----------------------------------------------
    ALL_3_monthly_seas <-
        ALL_3_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                               GLB_att_seas    = mean(GLB_att,    na.rm = T),
                               HOR_att_seas    = mean(HOR_att,    na.rm = T),
                               DIR_transp_seas = mean(DIR_transp, na.rm = T),
                               DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                               HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                               GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                               GLB_att_N_seas  = sum(!is.na(GLB_att)),
                               HOR_att_N_seas  = sum(!is.na(HOR_att)),
                               DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                           by = .( Month, SZA, preNoon ) ]

    CLEAR_3_monthly_seas <-
        CLEAR_3_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                                 GLB_att_seas    = mean(GLB_att,    na.rm = T),
                                 HOR_att_seas    = mean(HOR_att,    na.rm = T),
                                 DIR_transp_seas = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                                 GLB_att_N_seas  = sum(!is.na(GLB_att)),
                                 HOR_att_N_seas  = sum(!is.na(HOR_att)),
                                 DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                             by = .( Month, SZA, preNoon ) ]

    CLOUD_3_monthly_seas <-
        CLOUD_3_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                                 GLB_att_seas    = mean(GLB_att,    na.rm = T),
                                 HOR_att_seas    = mean(HOR_att,    na.rm = T),
                                 DIR_transp_seas = mean(DIR_transp, na.rm = T),
                                 DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                                 HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                                 GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                                 GLB_att_N_seas  = sum(!is.na(GLB_att)),
                                 HOR_att_N_seas  = sum(!is.na(HOR_att)),
                                 DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                             by = .( Month, SZA, preNoon ) ]





    ## _ Seasonal anomaly by SZA and period of day -----------------------------

    ALL_3_monthly_DESEAS <- merge(  ALL_3_monthly_mean,   ALL_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
    CLEAR_3_monthly_DESEAS <- merge(CLEAR_3_monthly_mean, CLEAR_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
    CLOUD_3_monthly_DESEAS <- merge(CLOUD_3_monthly_mean, CLOUD_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)

    ## _ forget data
    rm(  ALL_3_monthly_mean,   ALL_3_monthly_seas,
         CLEAR_3_monthly_mean, CLEAR_3_monthly_seas,
         CLOUD_3_monthly_mean, CLOUD_3_monthly_seas)

    ## Using the % departure from seasonal values

    ALL_3_monthly_DESEAS[, DIR_att_des    := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    ALL_3_monthly_DESEAS[, GLB_att_des    := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    ALL_3_monthly_DESEAS[, DIR_transp_des := 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLEAR_3_monthly_DESEAS[, DIR_att_des    := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLEAR_3_monthly_DESEAS[, GLB_att_des    := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    CLEAR_3_monthly_DESEAS[, DIR_transp_des := 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
    CLOUD_3_monthly_DESEAS[, DIR_att_des    := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
    CLOUD_3_monthly_DESEAS[, GLB_att_des    := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
    CLOUD_3_monthly_DESEAS[, DIR_transp_des := 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]

    ## create a nice data
    ALL_3_monthly_DESEAS[, Date := as.Date(paste(Year, Month, 1), format = "%Y %m %d")]
    CLEAR_3_monthly_DESEAS[, Date := as.Date(paste(Year, Month, 1), format = "%Y %m %d")]
    CLOUD_3_monthly_DESEAS[, Date := as.Date(paste(Year, Month, 1), format = "%Y %m %d")]

    ## change rest of flags names
    ALL_3_monthly_DESEAS[preNoon == TRUE,  preNoon := "am"]
    ALL_3_monthly_DESEAS[preNoon == FALSE, preNoon := "pm"]
    CLEAR_3_monthly_DESEAS[preNoon == TRUE,  preNoon := "am"]
    CLEAR_3_monthly_DESEAS[preNoon == FALSE, preNoon := "pm"]
    CLOUD_3_monthly_DESEAS[preNoon == TRUE,  preNoon := "am"]
    CLOUD_3_monthly_DESEAS[preNoon == FALSE, preNoon := "pm"]



    ## forget original data
    rm(DATA_all)
    rm(DATA_Clear)
    rm(DATA_Cloud)

    # ..................................................................... ----
    ##  Save the whole work space  ---------------------------------------------
    # if (!TEST) {
    save(list = ls(all = TRUE), file = common_data)
    cat("\nSaved workspace:", common_data, "\n\n")
    # }
} else {
    cat(paste("\n\nLoad environment and data from: ", common_data,"\n\n"))
    load(file = common_data)
}


# ......................................................................... ----


# #### run on all quarter of the hour
# ayear$quarter <- ((as.numeric( ayear$Date ) %/% (3600/4) ) )
#
# selectqua  <- list(ayear$quarter)
#
# qDates     <- aggregate(ayear$Date,       by = selectqua, FUN = min)
#
# qGlobal    <- aggregate(ayear$wattGLB,    by = selectqua, FUN = mean, na.rm = TRUE )
# qGlobalCNT <- aggregate(ayear$wattGLB,    by = selectqua, FUN = function(x) sum(!is.na(x)) )
# qGlobalSTD <- aggregate(ayear$wattGLB,    by = selectqua, FUN = sd,   na.rm = TRUE )
#
# qElevaMEAN <- aggregate(ayear$Eleva,      by = selectqua, FUN = mean, na.rm = TRUE )
#
# qGLstd     <- aggregate(ayear$wattGLB_SD, by = selectqua, FUN = mean, na.rm = TRUE )
# qGLstdCNT  <- aggregate(ayear$wattGLB_SD, by = selectqua, FUN = function(x) sum(!is.na(x)) )
# qGLstdSTD  <- aggregate(ayear$wattGLB_SD, by = selectqua, FUN = sd,   na.rm = TRUE )
#
# #### output of quarterly data
# ayearquarter <- data.frame( Dates      = qDates$x,
#                             qGlobal    = qGlobal$x,
#                             qGlobalCNT = qGlobalCNT$x,
#                             qGlobalSTD = qGlobalSTD$x,
#                             qElevaMEAN = qElevaMEAN$x,
#                             qGLstd     = qGLstd$x,
#                             qGLstdCNT  = qGLstdCNT$x,
#                             qGLstdSTD  = qGLstdSTD$x)
#
# #### run on 4 quarters of every hour
# ayearquarter$hourly <- as.numeric( ayearquarter$Dates ) %/% 3600
# hposic              <- as.POSIXct( ayearquarter$hourly * 3600, origin = "1970-01-01" )
#
# selecthour <- list(ayearquarter$hourly)
#
# hDates     <- aggregate( ayearquarter$Dates,   by = selecthour, FUN = min )
#
# hGlobal    <- aggregate( ayearquarter$qGlobal, by = selecthour, FUN = mean, na.rm = FALSE )  ## na.rm must be FALSE!
# hGlobalCNT <- aggregate( ayearquarter$qGlobal, by = selecthour, FUN = function(x) sum(!is.na(x)))
