
#' Pull wind and pm10 data from the GBUAPCD m-files. 
#' 
#' Pull wind speed and direction from teoms.teom_summary_data in AirSci 
#' PostgreSQL database. This function pulls only the T7 station (required as 
#' part of the TwB2 paired TEOM analysis.
#' *Note: The PM10 data in this table is the analog averaged data transmitted 
#' via LoggerNet. Although the digitial 5 minute data is preferrable for 
#' reporting, this data is used as the long turn-around for District collected 
#' PM10 data makes it unavailable for monthly reports. 
#' 
#' @param date1, date2 Text string. Date range for which to pull data.
#' @return Data frame.
#' @examples
#' pull_mfile_wind("2016-02-01", "2016-03-01")
pull_mfile_data<- function(date1, date2){
  print("pulling wind and pm10 data from archive.mfile_data...")
  mfile_df <- 
    query_owenslake(paste0("SELECT did, datetime, deployment_id, dir, aspd, 
                           teom, qaqc_level_id 
                           FROM archive.mfile_data 
                           WHERE datetime > timestamp '", date1, 
                           "' AND datetime < timestamp '", date2, "';"))
  if (sum(!is.na(mfile_df$qaqc_level_id))>0) print("QA/QC failures in data!")
  mfile_df <- mfile_df[is.na(mfile_df$qaqc_level_id), ]
  mfile_df <- select(mfile_df, data.id = did, datetime, 
                     deployment.id = deployment_id, 
                     wd = dir, ws = aspd, pm10.avg=teom)
  # remove duplicated data lines (problem in database)
  mfile_df <- mfile_df[!duplicated(mfile_df[ , -1]), ]
  mfile_df
}

#' Pull instrument location data.
#' 
#' pull instrument UTM coordinates and deployment description from 
#' instruments.deployments in AirSci PostgreSQL database, base on deployment_id 
#' number. 
#' 
#' @param deploys Numerical. Vector of deployment ids.
#' @return Data frame.
#' @examples
#' pull_locations(c(1719, 1718, 1849))
pull_locations <- function(deploys){
  deploys <- paste0("(", paste(deploys, collapse=", "), ")")
  station_locs <- 
    query_owenslake(paste0("SELECT deployment_id, deployment, northing_utm, 
                           easting_utm, description 
                           FROM instruments.deployments 
                           WHERE deployment_id IN ", deploys))
  colnames(station_locs) <- gsub("_", ".", colnames(station_locs))
  station_locs <- select(station_locs, -description)
  station_locs <- rename(station_locs, x=easting.utm, y=northing.utm)
  station_locs
}

#' Pull report-quality PM10 data
#'
#' Pull PM10 data for reporting from AirSci PostgreSQL database.
#' 
#' @param x A number.
#' @param deploys Numerical. Vector of deployment ids.
#' @return Data frame.
#' @examples
#' pull_pm10(c(1719, 1718, 1849))
pull_pm10 <- function(date1, date2, deploys){
  print("pulling PM10 data from teoms.deployment_data...")
  deploys <- paste0("(", paste(deploys, collapse=", "), ")")
  pm10_df <- 
    query_owenslake(paste0("SELECT d.deployment, 
                           file_uploads.date_trunc_hour(datetime) 
                           AS datetime_hour, 
                           AVG(dd.teomamc) 
                           FROM teoms.deployment_data dd 
                           JOIN instruments.deployments d 
                           ON dd.deployment_id=d.deployment_id 
                           WHERE datetime > timestamp '", date1,  
                           "' AND datetime < timestamp '", date2, 
                           "' AND dd.deployment_id IN ", deploys, 
                           " GROUP BY d.deployment, 
                           file_uploads.date_trunc_hour(datetime) 
                           ORDER BY d.deployment, datetime_hour"))
  pm10_df <- rename(pm10_df, pm10.avg=avg)
  print(paste0("removing ", nrow(filter(pm10_df, pm10.avg < -35)),
               " hours with pm10.avg < -35"))
  pm10_df <- filter(pm10_df, pm10.avg > -35)
  # remove duplicated data lines (problem in database)
  pm10_df <- pm10_df[!duplicated(pm10_df[ , -1]), ]
  pm10_df
}
