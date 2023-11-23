

## Data input for this paper

## to force a rebuild of the dataset remove stored
# file.remove(common_data)

require(data.table)
require(zoo)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("./DHI_GHI_0_variables.R")

#  Run data construction  ------------------------------------------------------

D_14_2 <- FALSE
D_14   <- FALSE
D_13   <- FALSE
D_15   <- FALSE

# D_14_2 <- TRUE
# D_14   <- TRUE
# D_13   <- TRUE
D_15   <- TRUE

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

## Test for missing data
if (D_15) {
    common_data <- common_data_15
    CS_file     <- CS_file_15
    inpatern    <- "Clear_sky_id_Reno-Hansen_apply_v15_[0-9]{4}.Rds"
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
        write_RDS(object = DATA, file = CS_file, clean = TRUE)
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
    write_dat(object = TSI_info,
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
    save(list = ls(all = TRUE), file = common_data, compress = "xz")
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
