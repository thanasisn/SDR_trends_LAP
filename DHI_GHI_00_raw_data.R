
#  Prepare raw data for use in the paper ---------------------------------------
#
#  This collects raw data and prepare it for SDR trend analysis.
#  It uses the data output of ClearSky algorithm.
#  It caches the raw data in order to reuse it in different states of analyses.
#  Creates:
#    - Clear_Sky_##.Rds
#    - Raw_Input.Rds
#

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

if (TEST == TRUE) {
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


##_  Check if we need to run data export  --------------------------------------
havetorun <- !file.exists(common_data) |
    file.mtime(CS_file)          > file.mtime(common_data) |
    file.mtime(variables_fl)     > file.mtime(common_data) |
    file.mtime("./DHI_GHI_00_raw_data.R") > file.mtime(common_data)


if (havetorun) {
    cat(paste("\n !! Create raw input data ->", raw_input_data),"\n")

    ##_  Get data from Clear sky id data  --------------------------------------
    input_files <- list.files(path       = CLEARdir,
                              pattern    = inpatern,
                              full.names = T )
    input_files <- grep("_stats_", input_files, value = TRUE, invert = TRUE)

    if (TEST == TRUE) {
        warning("\nTEST MODE IS ACTIVE!!\n\n")
        input_files <- sample(input_files, 2)
    }

    cat(paste("\n    Will read", length(input_files), "input files\n"))

    if ( !file.exists(CS_file) | max(file.mtime(input_files)) > file.mtime(CS_file)) {
        cat(paste("\n    Load data from Clear Sky process from original\n\n"))
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

        ## FIXME do we still need this?
        ## this is used by old scripts
        setorder(DATA, Date)
        myRtools::write_RDS(object = DATA, file = CS_file, clean = TRUE)
    } else {
        DATA <- readRDS(CS_file)
    }


    ##_  Skip data ranges for CM-21  -------------------------------------------
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

    #   Select data for this project  ------------------------------------------

    ##_  Set date range to use  ------------------------------------------------
    DATA <- DATA[Date < LAST_DAY ]
    DATA <- DATA[Date > FIRST_DAY]

    ##_  Keep daylight only  ---------------------------------------------------
    DATA <- DATA[Elevat >= 0, ]

    ##_  Exclude low Sun elevation  --------------------------------------------
    DATA[Elevat < MIN_ELEVA, wattDIR     := NA ]
    DATA[Elevat < MIN_ELEVA, wattDIR_sds := NA ]
    DATA[Elevat < MIN_ELEVA, wattGLB     := NA ]
    DATA[Elevat < MIN_ELEVA, wattGLB_sds := NA ]
    DATA[Elevat < MIN_ELEVA, wattHOR     := NA ]
    DATA[Elevat < MIN_ELEVA, wattHOR_sds := NA ]

    ## show included data
    ## FIXME there is some error in Azimuth/Elevation angles see plot!!
    # plot(DATA[ !is.na(wattGLB) ,Elevat, Azimuth])

    ##_  Bais paper obstacle filter  -------------------------------------------
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


    ##_  Keep data characterized as 'good' by Radiation Quality control v13 ----
    if (D_13) {
        keepQF <- c("good",
                    "Possible Direct Obstruction (23)",
                    "Biology Building (22)")
        DATA[!QCF_DIR %in% keepQF, wattDIR := NA]
        DATA[!QCF_DIR %in% keepQF, wattHOR := NA]
        DATA[!QCF_GLB %in% keepQF, wattGLB := NA]
    }

    ##_  Keep data characterized as 'TRUE' by Radiation Quality control v14 ----
    if (D_14 | D_14_2) {
        DATA[QCF_DIR == FALSE, wattDIR := NA]
        DATA[QCF_DIR == FALSE, wattHOR := NA]
        DATA[QCF_GLB == FALSE, wattGLB := NA]
    }

    ##_  Count daylight length  ------------------------------------------------
    DATA[, DayLength := .N, by = Day]

    ##_  DROP MISSING RECORDS!! ------------------------------------------------
    DATA <- DATA[ !(is.na(wattDIR) & is.na(wattGLB)) ]

    ##_  Info for TIS time span source used  -----------------------------------
    TSI_info <- DATA[, .(Start = min(Date),
                         End   = max(Date)), by = TSI_Source]
    myRtools::write_dat(object = TSI_info,
                        file   = "./figures/tbl_tsi_info.dat",
                        clean  = TRUE)
    rm(TSI_info)


    #  Data preparation  -------------------------------------------------------

    ##_  Move measurements to mean earth distance  -----------------------------
    DATA[, wattDIR_1au := wattDIR * (sun_dist ^ 2)]
    DATA[, wattGLB_1au := wattGLB * (sun_dist ^ 2)]
    DATA[, wattHOR_1au := wattHOR * (sun_dist ^ 2)]

    ##_  Relative to actual TSI at 1au variable representation
    # DATA[ , DIR_att := wattDIR_1au / tsi_1au_comb ]
    # DATA[ , GLB_att := wattGLB_1au / tsi_1au_comb ]
    # DATA[ , HOR_att := wattHOR_1au / tsi_1au_comb ]

    ## !! Replace original variable representation for convenience !!
    DATA[, DIR_att := wattDIR_1au]
    DATA[, GLB_att := wattGLB_1au]
    DATA[, HOR_att := wattHOR_1au]

    ## Ground effect removal?
    ## Aerosol direct effects on global solar shortwave irradiance at high mountainous station Musala Bulgaria_Nojarov2021.pdf
    # DATA$wattGLB_1au <- DATA$wattGLB_1au / cosde(DATA$SZA)
    # DATA$wattDIR_1au <- DATA$wattDIR_1au / cosde(DATA$SZA)


    ##_  Calculate Bouguer atmospheric transparency  ---------------------------
    ## Changes in solar radiation and their influence on temperature trend in Estonia 1955 2007_Russak2009.pdf
    DATA[, DIR_transp := ( wattDIR_1au / tsi_1au_comb ) ^ ( 1 / cosde(SZA) ) ]

    ## fix noon just in case
    DATA[Azimuth <= 180 , preNoon := TRUE ]
    DATA[Azimuth >  180 , preNoon := FALSE]

    ##_  DROP SOME DATA  -------------------------------------------------------
    DATA[, CS_ref_HOR         := NULL]
    DATA[, ClearnessIndex_kt  := NULL]
    DATA[, Clearness_Kt       := NULL]
    DATA[, ClrSW              := NULL]
    DATA[, ClrSW_ref2         := NULL]
    DATA[, DIF_HOR            := NULL]
    DATA[, DiffuseFraction_Kd := NULL]
    DATA[, DiffuseFraction_kd := NULL]
    DATA[, Direct_max         := NULL]
    DATA[, Elevat             := NULL]
    DATA[, Glo_max_ref        := NULL]
    DATA[, Global_max         := NULL]
    DATA[, RaylDIFF           := NULL]
    DATA[, wattDIR_1au        := NULL]
    DATA[, wattGLB_1au        := NULL]
    DATA[, wattHOR_1au        := NULL]

    rm.cols.DT(DATA, "QCv9*")
    rm.cols.DT(DATA, "*Clim_lim")
    rm.cols.DT(DATA, "QCF_*")
    rm.cols.DT(DATA, "VIL_*")


    #  GLB Representation filtering  -------------------------------------------
    #
    #  Remove days with too few data, as they can not be representative of a
    #  normal day.
    #
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


    #  Split data to Clear Sky, non Clear sky and cloud sky data  --------------
    #
    #  Method based and adapted from: Identification of Periods of Clear Sky
    #  Irradiance in Time Series of GHI Measurements _Matthew J. Reno and
    #  Clifford W. Hansen.
    #
    #  For this paper we use only flags derived from CM-21.
    #

    ##_ Select only CM-21 flags for trends -------------------------------------
    wecare     <- grep("CSflag_", names(DATA), value = T)
    wecare     <- grep("_11", wecare, invert = T, value = T)

    ##_ Set flag for sky conditions --------------------------------------------
    DATA[rowSums(DATA[, ..wecare ], na.rm = T) == 0, TYPE := "Clear"]
    DATA[rowSums(DATA[, ..wecare ], na.rm = T) != 0, TYPE := "Cloud"]

    ## remove unused columns
    rm.cols.DT(DATA,   "CSflag_*")


    ## legacy flags usage
    # DATA_Clear <- DATA_all[ CSflag == 0 ]

    #  Save raw input data  ----------------------------------------------------
    if (TEST == FALSE) {
        saveRDS(DATA, file = raw_input_data, compress = "xz")
        cat("\nSaved raw input data:", raw_input_data, "\n\n")
    }
# } else {
#     cat(paste("\n\nLoad raw input data: ", raw_input_data,"\n\n"))
#     readRDS(file = raw_input_data)
}

