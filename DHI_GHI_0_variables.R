
## Variables for project


#### dictionary ####
dict <- list(DIR_att    = 'Direct Beam Irradiance',
             HOR_att    = 'Direct Irradiance on horizontal plane',
             DIR_transp = 'Tranparency for Direct Irradiance',
             GLB_att    = "Global Irradiance",
             ALL        = "All sky conditions",
             CLEAR      = "Clear sky conditions")
## function to translate objects names
translate <- function(...) as.vector(unlist(dict[c(...) == names(dict)]))


#### paths ####
CLEARdir                 <- "~/DATA/Broad_Band/CS_id"
tag                      <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))
CS_file                  <- "~/DATA/Common_application/Clear_Sky.Rds"
common_data              <- "~/MANUSCRIPTS/2022_sdr_trends/data/common_data.Rds"
variables_fl             <- "~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_variables.R"
data_procsess_fl         <- "~/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_0_data_input.R"

#### colors ####
col_DIR_att              <- "#2166ac"
col_HOR_att              <- "#4244ac"
col_DIR_transp           <- "#9970ab"
col_GLB_att              <- "#1a9850"


#### parameters ####

## https://www.rapidtables.com/calc/time/days-in-year.html
# Days_of_year             <- 365.25   ## Mean Julian year
Days_of_year             <- 365.2425 ## Mean Gregorian calendar year
pch_am                   <-  1
pch_pm                   <-  4
pch_ampm                 <- 13 ## try 10
pch_daily                <- 19

MIN_ELEVA                <-  5  ##  global low elevation limit
SZA_BIN                  <-  1
MIN_N                    <-  4
SEAS_MIN_N               <-  3

Daily_confidence_limit   <-  0.99
SZA_confidence_limit     <-  0.99
Monthly_confidence_limit <-  0.99

Daily_aggregation_N_lim  <- 50         ## was SUM_THRES
Monthly_aggegation_N_lim <- 20
SZA_aggregation_N_lim    <-  4         ## was SZA_THRES


