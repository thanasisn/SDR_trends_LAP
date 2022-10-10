

## Data input

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


