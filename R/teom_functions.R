
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
    query_owenslake(paste0("SELECT i.deployment, d.datetime, d.dir, d.aspd, 
                           d.teom, d.qaqc_level_id, i.easting_utm, 
                           i.northing_utm
                           FROM archive.mfile_data d
                           JOIN instruments.deployments i
                           ON d.deployment_id=i.deployment_id
                           WHERE datetime > timestamp '", date1, 
                           "' AND datetime < timestamp '", date2, "';"))
  if (sum(!is.na(mfile_df$qaqc_level_id))>0) print("QA/QC failures in data!")
  mfile_df <- mfile_df[is.na(mfile_df$qaqc_level_id), ]
  mfile_df <- select(mfile_df, deployment, datetime,  wd = dir, ws = aspd, 
                     pm10.avg=teom, x=easting_utm, y=northing_utm)
  # remove duplicated data lines (problem in database)
  mfile_df <- mfile_df[!duplicated(mfile_df[ , -1]), ]
  mfile_df
}

#' strip legend from ggplot object
#' 
#' @param a.gplot ggplot object.
#' @return A grob of the plot legend
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 
