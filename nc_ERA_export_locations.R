#!/usr/bin/env Rscript
#
# Read all nc files from ERA5 and get data for specific locations.
#


closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name = c("nc_ERA_export_locations.R")



library(RNetCDF)
library(data.table)
library(akima)

source("~/FUNCTIONS/R/data.R")


locations <- data.frame(
    Name  = c("Thessaloniki"),
    LongX = c(22.956055),
    LatiY = c(40.633610)
)






####    ERA5 exporter    #######################################################
nc_folder <- "/home/athan/DATA/Clouds ERA5/"

## for each nc file will interpolate and produce a timeseries of each location

filelist <- list.files(path        = nc_folder,
                       pattern     = "*.nc",
                       full.names  = T,
                       ignore.case = T,
                       recursive   = F
)
filelist <- sort(filelist)


for (afile in filelist) {
    sfile = sub("nc$","Rds",afile)
    cat(paste(basename(afile), "\n"))

    ## dont overwrite existing exports
    if (file.exists(sfile)) { next() }

    ## load file and read data
    anc  <- open.nc(afile,write = F)
    data <- read.nc(anc, unpack=TRUE)
    ## store data for saving
    gather          <- list()
    gather$filename <- basename(afile)

    ## variables in file
    variables <- grep("time|longitude|latitude|tp", names(data), ignore.case = T, invert = T,value = T)

    #### get variables info #####
    var_info <- data.table()
    for (avar in variables) {
        var_info <- rbind(var_info,
                          data.table(sort_name = avar,
                                     long_name = att.get.nc(anc,avar,"long_name"),
                                     units     = att.get.nc(anc,avar,"units"))
        )
    }
    gather$var_info <- var_info

    #### get data from nc ####

    ## create proper dates from nc file
    time_desc <- att.get.nc(anc, "time", "units")
    Dates     <- utcal.nc(time_desc, data$time, type = "s")

    ## match indexes with coordinates
    x_inx <- data$longitude
    ## we have to reverse the y axis values and matrix
    y_inx <- rev(data$latitude)

    for (avvv in variables) {
        tempd <- data[[avvv]]

        str(tempd)

        ## era5 data are points on locations

        x_near_in <- which.min(abs(x_inx - locations$LongX))
        y_near_in <- which.min(abs(y_inx - locations$LatiY))


        tempd[x_near_in, y_near_in, ]

        ## Change NA to -9999
        tempd[is.na(tempd)] <- -99999

        ## interpolate temperature data to location
        out <- apply(tempd, MARGIN = c(3), function(z) bilinear(x = x_inx, y = y_inx, flip_matrix_v(z), locations$LongX, locations$LatiY))



        ## get data coordinates
        x_long <- out[[1]]$x
        y_lat  <- out[[1]]$y

        ## prepare data
        outt <- lapply( out , "[[" , "z" )

        do.call(rbind, out)
        do.call(rbind, outt)

        lirrst2DF(outt)
        outt <- data.frame(outt)
        names(outt) <- Dates
        ## change back to NAs
        outt[outt < -999] <- NA



        outt

        stop()

        ## create proper structure
        store <- data.table(x_long = x_long,
                            y_lat  = y_lat,
                            name   = locations$Name,
                            outt)

        gather[[avvv]] <- store
    }
    saveRDS(object   = gather,
            file     = sfile,
            compress = "xz")
    close.nc(anc)
}
# image(x_inx, y_inx, flip_matrix_v(data$t2m[,,1]), col = heat.colors(9))





####    ERA20c exporter    #####################################################
nc_folder <- "/home/athan/DATA/ERA20C/"

filelist <- list.files(path        = nc_folder,
                       pattern     = "*.nc",
                       full.names  = T,
                       ignore.case = T,
                       recursive   = F
)
filelist <- sort(filelist)


for (afile in filelist) {
    sfile = sub("nc$","Rds",afile)
    cat(paste(basename(afile), "\r"))

    ## don't overwrite existing exports
    if (file.exists(sfile)) { next() }

    # stop()

    ## load file and read data
    anc  <- open.nc(afile,write = F)
    data <- read.nc(anc, unpack=TRUE)
    ## store data for saving
    gather          <- list()
    gather$filename <- basename(afile)

    ## variables in file
    variables <- grep("time|longitude|latitude|tp|expver" ,names(data), ignore.case = T, invert = T,value = T)

    #### get variables info #####
    var_info <- data.table()
    for (avar in variables) {
        var_info <- rbind(var_info,
                          data.table(sort_name = avar,
                                     long_name = att.get.nc(anc,avar,"long_name"),
                                     units     = att.get.nc(anc,avar,"units"))
        )
    }
    gather$var_info <- var_info

    #### get data from nc ####

    ## create proper dates from nc file
    time_desc <- att.get.nc(anc, "time", "units")
    Dates     <- utcal.nc(time_desc, data$time, type = "s")

    ## match indexes with cordinates
    x_inx <- data$longitude
    ## we have to reverse the y axis values and matrix
    y_inx <- rev(data$latitude)

    for (avvv in variables) {
        tempd <- data[[avvv]]

        ## Change NA to -9999
        tempd[is.na(tempd)] <- -99999

        ## interpolate temperature data to location
        out <- apply(tempd, MARGIN = c(3), function(z) bilinear(x = x_inx, y = y_inx, flip_matrix_v(z), poisen$x, poisen$y))

        ## get data coordinates
        x_long <- out[[1]]$x
        y_lat  <- out[[1]]$y

        ## prepare data
        outt <- lapply( out , "[[" , "z" )
        outt <- data.frame(outt)
        names(outt) <- Dates
        ## change back to NAs
        outt[outt < -999] <- NA

        ## create proper structure
        store <- data.table(x_long = x_long,
                            y_lat  = y_lat,
                            name   = poisen$Name,
                            outt)

        gather[[avvv]] <- store
    }
    saveRDS(object   = gather,
            file     = sfile,
            compress = "xz")
    close.nc(anc)
}


cat(paste("\n\n    DONE \n\n"))
