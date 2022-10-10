

## Data input

if ( !file.exists(common_data) | file.mtime(CS_file) > file.mtime(common_data) ){

    ####  Get data from Clear sky id data  ####
    input_files <- list.files( path    = CLEARdir,
                               pattern = "Clear_Sky_[0-9]{4}.Rds",
                               full.names = T )

    if ( !file.exists(CS_file) | max(file.mtime(input_files)) > file.mtime(CS_file)) {
        cat(paste("Load data from Clear Sky proccess from original\n"))
        DATA <- data.table()
        for (af in input_files) {
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

            DATA <- rbind(temp, DATA, fill = TRUE)
            rm(temp)
        }
        ## this is used by old scripts
        myRtools::write_RDS(object = DATA, file = CS_file)
    } else {
        DATA <- readRDS(CS_file)
    }


    #' ### Filter min elevation
    #' Keep data with Sun elevation above `r MIN_ELEVA`
    DATA <- DATA[ Elevat >= MIN_ELEVA ]

    #' ### Bais paper obstacle filter
    DATA <- DATA[ ! (Azimuth > 35 & Azimuth < 120 & Elevat < 10) ]
    #+ echo=F, include=T


    #' ### Keep only data characterized as 'good' by the Radiation Quality control procedure
    #+ echo=F, include=T
    DATA[ QCF_DIR != "good", wattDIR := NA ]
    DATA[ QCF_DIR != "good", QCF_DIR := NA ]
    DATA[ QCF_GLB != "good", wattGLB := NA ]
    DATA[ QCF_GLB != "good", QCF_GLB := NA ]
    DATA <- DATA[ ! (is.na(wattDIR) & is.na(wattGLB))  ]


    #' ## Data preparation
    #' ### Move measurements to mean earth distance
    DATA[ , wattDIR_1au := wattDIR * (sun_dist ^ 2) ]
    DATA[ , wattGLB_1au := wattGLB * (sun_dist ^ 2) ]
    DATA[ , wattHOR_1au := wattHOR * (sun_dist ^ 2) ]
    #+ echo=F, include=T


    # #' ### Relative to actual TSI at 1au variable representation
    # DATA[ , DIR_att := wattDIR_1au / tsi_1au_comb ]
    # DATA[ , GLB_att := wattGLB_1au / tsi_1au_comb ]
    # DATA[ , HOR_att := wattHOR_1au / tsi_1au_comb ]


    #' ### Use original variable representation
    #+ echo=F, include=T
    DATA[ , DIR_att := wattDIR_1au ]
    DATA[ , GLB_att := wattGLB_1au ]
    DATA[ , HOR_att := wattHOR_1au ]
    #+ echo=F, include=T


    # #' ### Ground effect removal?
    # #' Aerosol direct effects on global solar shortwave irradiance at high mountainous station Musala Bulgaria_Nojarov2021.pdf
    # DATA$wattGLB_1au <- DATA$wattGLB_1au / cosde(DATA$SZA)
    # DATA$wattDIR_1au <- DATA$wattDIR_1au / cosde(DATA$SZA)
    # #+ echo=F, include=T


    #' ### Calculate Bouguer atmospheric transparency
    #' see: Changes in solar radiation and their influence on temperature trend in Estonia 1955 2007_Russak2009.pdf
    DATA[, DIR_transp := ( wattDIR_1au / tsi_1au_comb ) ^ ( 1 / cosde(SZA) ) ]
    #+ echo=F, include=T


    ## fix noon just in case
    DATA[ Azimuth <= 180 , preNoon := TRUE  ]
    DATA[ Azimuth >  180 , preNoon := FALSE ]


    #' ### Split data to Clear Sky and non Clear sky
    #' Method based and adapted to site from: Identification of Periods of Clear Sky
    #' Irradiance in Time Series of GHI Measurements
    #' _Matthew J. Reno and Clifford W. Hansen_.
    #+ echo=F, include=T
    DATA_all   <- DATA

    wecare     <- grep("CSflag_", names(DATA), value = T)
    ## use only cm21 flags
    wecare     <- grep("_11", wecare, invert = T, value = T)
    DATA_Clear <- DATA[ rowSums( DATA[, ..wecare ], na.rm = T ) == 0, ]
    #+ echo=F, include=T

    ## old flags usage
    # DATA_Clear <- DATA_all[ CSflag == 0 ]
    rm(DATA)

    ## save workspace
    save(list = ls(all = TRUE),file = common_data)

} else {
    cat(paste("Load environment and data from: ", common_data))
    load( file = common_data)
}
