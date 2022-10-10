
## Variables


CLEARdir                 <- "~/DATA/Broad_Band/CS_id"
tag                      <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))
CS_file                  <- "~/DATA/Common_application/Clear_Sky.Rds"
common_data              <- "~/MANUSCRIPTS/2022_sdr_trends/data/common_data.Rds"

col_DIR_att              <- "#2166ac"
col_HOR_att              <- "#4244ac"
col_DIR_transp           <- "#9970ab"
col_GLB_att              <- "#1a9850"

## https://www.rapidtables.com/calc/time/days-in-year.html
# Days_of_year             <- 365.25   ## Mean Julian year
Days_of_year             <- 365.2425 ## Mean Gregorian calendar year

pch_am                   <-  1
pch_pm                   <-  4
pch_ampm                 <- 13 ## try 10
pch_daily                <- 19

MIN_ELEVA                <-  5  ## use data when sun above that
SZA_BIN                  <-  1
MIN_N                    <-  4
SEAS_MIN_N               <-  3

Daily_aggregation_N_lim  <- 50         ## was SUM_THRES
Daily_confidence_limit   <-  0.99
SZA_confidence_limit     <-  0.99
SZA_aggregation_N_lim    <-  5         ## was SZA_THRES
