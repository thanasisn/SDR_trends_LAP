

## Data input

## to force a rebuild
# file.remove(common_data)

require(data.table)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")

####  Run data construction ####################################################

D_14 <- FALSE
D_13 <- FALSE


# D_14 <- TRUE
D_13 <- TRUE


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



## check if we need to run data production
havetorun <- !file.exists(common_data) |
    file.mtime(CS_file)          > file.mtime(common_data) |
    file.mtime(variables_fl)     > file.mtime(common_data) |
    file.mtime(variables_fl)     > file.mtime(common_data) |
    file.mtime(data_procsess_fl) > file.mtime(common_data)

if ( havetorun ) {
    cat(paste("\n !! (Re)Create environment and data input ->", common_data),"\n")

    #### 0. Get data from Clear sky id data  ###################################
    input_files <- list.files( path       = CLEARdir,
                               pattern    = inpatern,
                               full.names = T )
    input_files <- grep("_stats_", input_files, value = TRUE, invert = TRUE)

    if ( !file.exists(CS_file) | max(file.mtime(input_files)) > file.mtime(CS_file)) {
        cat(paste("Load data from Clear Sky proccess from original\n"))
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

            DATA <- rbind(temp, DATA, fill = TRUE)
            rm(temp)
        }

        ## this is used by old scripts
        setorder(DATA,Date)
        myRtools::write_RDS(object = DATA, file = CS_file)
    } else {
        DATA <- readRDS(CS_file)
    }

    # ## Sunset and sunrise
    # hist(DATA[ Elevat < 5, Elevat ])
    # which(diff(sign(DATA$Elevat))!=0)
    #
    # vec1      <- data.frame(Sign = sign(DATA$Elevat),
    #                         Date = DATA$Date,
    #                         Elev = DATA$Elevat)
    # vec1$Diff <- c(0,diff(vec1$Sign))
    #
    # vec1[which(vec1$Diff != 0), ]

    #### 0. Process data for this project  #####################################

    #' ### Filter min elevation
    #' Keep data with Sun elevation above `r MIN_ELEVA`
    DATA <- DATA[ Elevat >= MIN_ELEVA, ]

    #' ### Bais paper obstacle filter
    DATA <- DATA[ !(Azimuth > 35 & Azimuth < 120 & Elevat < 10) ]
    #+ echo=F, include=T

    if (D_13) {
        keepQF <- c("good","Possible Direct Obstruction (23)","Biology Building (22)")
        #' ### Keep only data characterized as 'good' by the Radiation Quality control procedure v13
        #' Keep data marked as `r cat(paste(keepQF,collapse = ", "))`.
        #+ echo=F, include=T
        DATA[ ! QCF_DIR %in% keepQF, wattDIR := NA ]
        DATA[ ! QCF_DIR %in% keepQF, wattHOR := NA ]
        DATA[ ! QCF_GLB %in% keepQF, wattGLB := NA ]
    }

    if (D_14) {
        #' ### Keep only data characterized as 'TRUE' by the Radiation Quality control procedure v14
        #+ echo=F, include=T
        DATA[ QCF_DIR == FALSE, wattDIR := NA ]
        DATA[ QCF_DIR == FALSE, wattHOR := NA ]
        DATA[ QCF_GLB == FALSE, wattGLB := NA ]
    }

    DATA <- DATA[ !(is.na(wattDIR) & is.na(wattGLB)) ]

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

    ## remove unused columns
    rm.cols.DT(DATA_all,   "CSflag_*")
    rm.cols.DT(DATA_Clear, "CSflag_*")
    DATA_all[  , CS_ref_HOR := NULL ]
    DATA_Clear[, CS_ref_HOR := NULL ]


    mean(DATA_all$tsi_1au_comb)

    #### 1. long-term   ########################################################

    ## ~ daily means of all data ####
    ALL_1_daily_mean <-
        DATA_all[,.(DIR_att       = mean(DIR_att,     na.rm = T),
                    GLB_att       = mean(GLB_att,     na.rm = T),
                    HOR_att       = mean(HOR_att,     na.rm = T),
                    DIR_transp    = mean(DIR_transp,  na.rm = T),
                    tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
                    DIR_att_sd    = sd(  DIR_att,     na.rm = T),
                    GLB_att_sd    = sd(  GLB_att,     na.rm = T),
                    HOR_att_sd    = sd(  HOR_att,     na.rm = T),
                    DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
                    tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
                    doy           = yday(Date),
                    GLB_att_N     = sum(!is.na(GLB_att)),
                    HOR_att_N     = sum(!is.na(HOR_att)),
                    DIR_att_N     = sum(!is.na(DIR_att))       ),
                 by = .( Date = Day ) ]

    ## ~ daily means of clear sky data ####
    CLEAR_1_daily_mean <-
        DATA_Clear[,.(DIR_att       = mean(DIR_att,   na.rm = T),
                      GLB_att       = mean(GLB_att,   na.rm = T),
                      HOR_att       = mean(HOR_att,   na.rm = T),
                      DIR_transp    = mean(DIR_transp,na.rm = T),
                      DIR_att_sd    = sd(  DIR_att,   na.rm = T),
                      GLB_att_sd    = sd(  GLB_att,   na.rm = T),
                      HOR_att_sd    = sd(  HOR_att,   na.rm = T),
                      DIR_transp_sd = sd(  DIR_transp,na.rm = T),
                      doy           = yday(Date),
                      GLB_att_N     = sum(!is.na(GLB_att)),
                      HOR_att_N     = sum(!is.na(HOR_att)),
                      DIR_att_N     = sum(!is.na(DIR_att))  ),
                   by = .( Date = Day ) ]

    ## ~ compute margin of error for confidence interval ####
    conf_param  <- 1 - ( 1 - Daily_confidence_limit ) / 2
    suppressWarnings({
        ALL_1_daily_mean[,  DIR_att_EM   :=qt(conf_param,df=DIR_att_N    -1)* DIR_att_sd   /sqrt(DIR_att_N   )]
        ALL_1_daily_mean[,  HOR_att_EM   :=qt(conf_param,df=HOR_att_N    -1)* HOR_att_sd   /sqrt(HOR_att_N   )]
        ALL_1_daily_mean[,  GLB_att_EM   :=qt(conf_param,df=GLB_att_N    -1)* GLB_att_sd   /sqrt(GLB_att_N   )]
        ALL_1_daily_mean[,  DIR_transp_EM:=qt(conf_param,df=DIR_att_N    -1)* DIR_transp_sd/sqrt(DIR_att_N   )]
        CLEAR_1_daily_mean[,DIR_att_EM   :=qt(conf_param,df=DIR_att_N    -1)* DIR_att_sd   /sqrt(DIR_att_N   )]
        CLEAR_1_daily_mean[,HOR_att_EM   :=qt(conf_param,df=HOR_att_N    -1)* HOR_att_sd   /sqrt(HOR_att_N   )]
        CLEAR_1_daily_mean[,GLB_att_EM   :=qt(conf_param,df=GLB_att_N    -1)* GLB_att_sd   /sqrt(GLB_att_N   )]
        CLEAR_1_daily_mean[,DIR_transp_EM:=qt(conf_param,df=DIR_att_N    -1)* DIR_transp_sd/sqrt(DIR_att_N   )]
    })

    ## ~ Exclude means with less than `r Daily_aggregation_N_lim` data points ####
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_att       := NA ]
    ALL_1_daily_mean[   GLB_att_N <= Daily_aggregation_N_lim, GLB_att       := NA ]
    ALL_1_daily_mean[   HOR_att_N <= Daily_aggregation_N_lim, HOR_att       := NA ]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_transp    := NA ]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_att_sd    := NA ]
    ALL_1_daily_mean[   GLB_att_N <= Daily_aggregation_N_lim, GLB_att_sd    := NA ]
    ALL_1_daily_mean[   HOR_att_N <= Daily_aggregation_N_lim, HOR_att_sd    := NA ]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_sd := NA ]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_att_EM    := NA ]
    ALL_1_daily_mean[   GLB_att_N <= Daily_aggregation_N_lim, GLB_att_EM    := NA ]
    ALL_1_daily_mean[   HOR_att_N <= Daily_aggregation_N_lim, HOR_att_EM    := NA ]
    ALL_1_daily_mean[   DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_EM := NA ]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att       := NA ]
    CLEAR_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att       := NA ]
    CLEAR_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att       := NA ]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp    := NA ]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att_sd    := NA ]
    CLEAR_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att_sd    := NA ]
    CLEAR_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att_sd    := NA ]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_sd := NA ]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_att_EM    := NA ]
    CLEAR_1_daily_mean[ GLB_att_N <= Daily_aggregation_N_lim, GLB_att_EM    := NA ]
    CLEAR_1_daily_mean[ HOR_att_N <= Daily_aggregation_N_lim, HOR_att_EM    := NA ]
    CLEAR_1_daily_mean[ DIR_att_N <= Daily_aggregation_N_lim, DIR_transp_EM := NA ]

    ## ~ Calculate daily seasonal values for all data ####
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

    ## ~ Calculate daily seasonal values for clear sky data ####
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

    ## ~ compute margin of error for confidence interval ####
    conf_param  <- 1 - ( 1 - Daily_confidence_limit ) / 2
    suppressWarnings({
        ALL_1_daily_seas[,  DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        ALL_1_daily_seas[,  HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        ALL_1_daily_seas[,  GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        ALL_1_daily_seas[,  DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLEAR_1_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLEAR_1_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLEAR_1_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        CLEAR_1_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
    })


    #### WRDC ? ####

    ## add all minutes of the days
    xlim      <- range(DATA_all$Date)
    alldates  <- data.frame(Date = seq(xlim[1], xlim[2], by = "min"))

    DATA_ALLQ <- merge( DATA_all, alldates, all = T )


    Q_all <- DATA_ALLQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
                          GLB_att       = mean(GLB_att,     na.rm = T),
                          HOR_att       = mean(HOR_att,     na.rm = T),
                          DIR_transp    = mean(DIR_transp,  na.rm = T),
                          tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
                          DIR_att_sd    = sd(  DIR_att,     na.rm = T),
                          GLB_att_sd    = sd(  GLB_att,     na.rm = T),
                          HOR_att_sd    = sd(  HOR_att,     na.rm = T),
                          DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
                          tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
                          GLB_att_N     = sum(!is.na(GLB_att)),
                          HOR_att_N     = sum(!is.na(HOR_att)),
                          DIR_att_N     = sum(!is.na(DIR_att)),
                          Elevat        = mean(Elevat,  na.rm = TRUE),
                          Azimuth       = mean(Azimuth, na.rm = TRUE),
                          SZA           = mean(SZA,     na.rm = TRUE)),
                       by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
                                                origin = "1970-01-01" ))
    ]
    rm(DATA_ALLQ)

    DATA_CLEARQ <- merge( DATA_Clear, alldates, all = T )

    Q_clear <- DATA_CLEARQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
                              GLB_att       = mean(GLB_att,     na.rm = T),
                              HOR_att       = mean(HOR_att,     na.rm = T),
                              DIR_transp    = mean(DIR_transp,  na.rm = T),
                              tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
                              DIR_att_sd    = sd(  DIR_att,     na.rm = T),
                              GLB_att_sd    = sd(  GLB_att,     na.rm = T),
                              HOR_att_sd    = sd(  HOR_att,     na.rm = T),
                              DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
                              tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
                              GLB_att_N     = sum(!is.na(GLB_att)),
                              HOR_att_N     = sum(!is.na(HOR_att)),
                              DIR_att_N     = sum(!is.na(DIR_att)),
                              Elevat        = mean(Elevat,  na.rm = TRUE),
                              Azimuth       = mean(Azimuth, na.rm = TRUE),
                              SZA           = mean(SZA,     na.rm = TRUE)),
                           by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
                                                    origin = "1970-01-01" ))
    ]
    rm(DATA_CLEARQ)


    QH_all <- Q_all[,.(DIR_att       = mean(DIR_att,     na.rm = T),
                       GLB_att       = mean(GLB_att,     na.rm = T),
                       HOR_att       = mean(HOR_att,     na.rm = T),
                       DIR_transp    = mean(DIR_transp,  na.rm = T),
                       tsi1au_att    = mean(tsi1au_att,  na.rm = T),
                       DIR_att_sd    = sd(  DIR_att,     na.rm = T),
                       GLB_att_sd    = sd(  GLB_att,     na.rm = T),
                       HOR_att_sd    = sd(  HOR_att,     na.rm = T),
                       DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
                       tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
                       GLB_att_N     = sum(!is.na(GLB_att)),
                       HOR_att_N     = sum(!is.na(HOR_att)),
                       DIR_att_N     = sum(!is.na(DIR_att)),
                       Elevat        = mean(Elevat,  na.rm = TRUE),
                       Azimuth       = mean(Azimuth, na.rm = TRUE),
                       SZA           = mean(SZA,     na.rm = TRUE)),
                    by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
                                             origin = "1970-01-01" ))]

    QH_clear <- Q_clear[,.(DIR_att       = mean(DIR_att,     na.rm = T),
                           GLB_att       = mean(GLB_att,     na.rm = T),
                           HOR_att       = mean(HOR_att,     na.rm = T),
                           DIR_transp    = mean(DIR_transp,  na.rm = T),
                           tsi1au_att    = mean(tsi1au_att,  na.rm = T),
                           DIR_att_sd    = sd(  DIR_att,     na.rm = T),
                           GLB_att_sd    = sd(  GLB_att,     na.rm = T),
                           HOR_att_sd    = sd(  HOR_att,     na.rm = T),
                           DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
                           tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
                           GLB_att_N     = sum(!is.na(GLB_att)),
                           HOR_att_N     = sum(!is.na(HOR_att)),
                           DIR_att_N     = sum(!is.na(DIR_att)),
                           Elevat        = mean(Elevat,  na.rm = TRUE),
                           Azimuth       = mean(Azimuth, na.rm = TRUE),
                           SZA           = mean(SZA,     na.rm = TRUE)),
                        by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
                                                 origin = "1970-01-01" ))]

    hist(QH_all[ Elevat < 20, Elevat ] )

    ## keep only whole hours
    QH_all <- QH_all[ !is.na(SZA) ]
    QH_all[ GLB_att_N < 4 , GLB_att := NA ]
    QH_all[ DIR_att_N < 4 , DIR_att := NA ]
    QH_all[ HOR_att_N < 4 , HOR_att := NA ]

    QH_clear <- QH_clear[ !is.na(SZA) ]
    QH_clear[ GLB_att_N < 4 , GLB_att := NA ]
    QH_clear[ DIR_att_N < 4 , DIR_att := NA ]
    QH_clear[ HOR_att_N < 4 , HOR_att := NA ]


    QHD_all <- QH_all[,.(DIR_att       = mean(DIR_att   ),
                         GLB_att       = mean(GLB_att   ),
                         HOR_att       = mean(HOR_att   ),
                         DIR_transp    = mean(DIR_transp)),
                      by = .(Date = as.Date(Date))]

    QHD_clear <- QH_clear[,.(DIR_att       = mean(DIR_att   ),
                             GLB_att       = mean(GLB_att   ),
                             HOR_att       = mean(HOR_att   ),
                             DIR_transp    = mean(DIR_transp)),
                          by = .(Date = as.Date(Date))]

    ## Too few days remain
    QHD_all[   !is.na(GLB_att), .N ]
    QHD_clear[ !is.na(GLB_att), .N ]




    ####  2. Long term by SZA  #################################################

    ## ~ Calculate daily SZA means ####
    ALL_2_daily_mean <-
        DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
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

    CLEAR_2_daily_mean <-
        DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
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

    ## ~ margin of error calculation for `r SZA_confidence_limit` confidence interval ####
    conf_param  <- 1 - ( 1 - SZA_confidence_limit ) / 2
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

    ## ~ Exclude means with less than `r SZA_aggregation_N_lim` data points ####
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
    CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
    CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
    CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
    CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
    CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
    CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
    CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]

    ## ~ Calculate daily seasonal values by SZA  ####
    ALL_2_daily_seas <-
        ALL_2_daily_mean[,.(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                            HOR_att_seas       = mean(HOR_att,    na.rm = T),
                            GLB_att_seas       = mean(GLB_att,    na.rm = T),
                            DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                            DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                            HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                            GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                            DIR_transp_sd_seas = sd(DIR_transp, na.rm = T),
                            GLB_att_N_seas     = sum(!is.na(GLB_att)),
                            HOR_att_N_seas     = sum(!is.na(HOR_att)),
                            DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                         by = .( doy, SZA, preNoon ) ]

    CLEAR_2_daily_seas <-
        CLEAR_2_daily_mean[,.(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                              HOR_att_seas       = mean(HOR_att,    na.rm = T),
                              GLB_att_seas       = mean(GLB_att,    na.rm = T),
                              DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                              DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                              DIR_transp_sd_seas = sd(DIR_transp, na.rm = T),
                              GLB_att_N_seas     = sum(!is.na(GLB_att)),
                              HOR_att_N_seas     = sum(!is.na(HOR_att)),
                              DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                           by = .( doy, SZA, preNoon ) ]


    ## ~ compute margin of error for confidence interval ####
    conf_param  <- 1 - ( 1 - Daily_confidence_limit ) / 2
    suppressWarnings({
        ALL_2_daily_seas[,  DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        ALL_2_daily_seas[,  HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        ALL_2_daily_seas[,  GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        ALL_2_daily_seas[,  DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
        CLEAR_2_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
        CLEAR_2_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
        CLEAR_2_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
        CLEAR_2_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
    })



    ####  3. Consistency of trends  ############################################

    ## ~ monthly sza prenoon ####
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
                        preNoon = preNoon  ) ]

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
                     preNoon       = "Daily"),
                 by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                        Year    = year(Date),
                        Month   = month(Date) ) ]

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
                          preNoon = preNoon  ) ]

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
                       preNoon       = "Daily"),
                   by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                          Year    = year(Date),
                          Month   = month(Date) ) ]

    CLEAR_3_monthly_mean <- data.table(rbind(data.frame(CLEAR_3_monthly_meanB),
                                             data.frame(CLEAR_3_monthly_meanA) ))
    rm(CLEAR_3_monthly_meanA, CLEAR_3_monthly_meanB)
    CLEAR_3_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


    ## ~ margin of error calculation ####
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
    })

    ## ~ Exclude means with less than `r Monthly_aggegation_N_lim` data points ####
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att       := NA ]
    ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att       := NA ]
    ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att       := NA ]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp    := NA ]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_sd    := NA ]
    ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_sd    := NA ]
    ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_sd    := NA ]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_sd := NA ]
    ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_EM    := NA ]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_EM    := NA ]
    ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_EM    := NA ]
    ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_EM := NA ]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim, DIR_att       := NA ]
    CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim, HOR_att       := NA ]
    CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim, GLB_att       := NA ]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp    := NA ]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_sd    := NA ]
    CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_sd    := NA ]
    CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_sd    := NA ]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_sd := NA ]
    CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_EM    := NA ]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_EM    := NA ]
    CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_EM    := NA ]
    CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_EM := NA ]

    ## ~  Calculate monthly seasonal values ####
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

    ## ~ monthly daily values ####
    ALL_3_monthly_daily_mean <-
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
                         by = .( Year = year(Date), Month = month(Date) ) ]

    CLEAR_3_monthly_daily_mean <-
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
                           by = .( Year = year(Date), Month = month(Date) ) ]

    ## ~ seasonal monthly daily values ####
    ALL_3_monthly_daily_seas <-
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

    CLEAR_3_monthly_daily_seas <-
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



    ## remove unwanted data frames from memory
    rm(DATA_all)
    rm(DATA_Clear)

    ####  save the whole work space  ####
    save(list = ls(all = TRUE),file = common_data)
} else {
    cat(paste("\n\nLoad environment and data from: ", common_data,"\n\n"))
    load( file = common_data)
}




# #### run on all quarter of the hour #####################################
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
# #### output of quarterly data #######################################
# ayearquarter <- data.frame( Dates      = qDates$x,
#                             qGlobal    = qGlobal$x,
#                             qGlobalCNT = qGlobalCNT$x,
#                             qGlobalSTD = qGlobalSTD$x,
#                             qElevaMEAN = qElevaMEAN$x,
#                             qGLstd     = qGLstd$x,
#                             qGLstdCNT  = qGLstdCNT$x,
#                             qGLstdSTD  = qGLstdSTD$x)
#
#
# #### run on 4 quarters of every hour ################################
# ayearquarter$hourly <- as.numeric( ayearquarter$Dates ) %/% 3600
# hposic              <- as.POSIXct( ayearquarter$hourly * 3600, origin = "1970-01-01" )
#
# selecthour <- list(ayearquarter$hourly)
#
# hDates     <- aggregate( ayearquarter$Dates,   by = selecthour, FUN = min )
#
# hGlobal    <- aggregate( ayearquarter$qGlobal, by = selecthour, FUN = mean, na.rm = FALSE )  ## na.rm must be FALSE!
# hGlobalCNT <- aggregate( ayearquarter$qGlobal, by = selecthour, FUN = function(x) sum(!is.na(x)))

