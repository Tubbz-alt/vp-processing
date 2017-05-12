#' Get select metadata and data from a vertical bird profile as a data.frame
#' @description This function creates a data.frame from a vertical bird profile 
#' (vp). The data.frame contains:
#' * `vp$attributes$what`: NOD:radar_id, date_time (repeated for every HGHT)
#' * `vp$data`: HGHT, u, v, dens, sd_vpp
#' 
#' For documentation on vp attributes, see: 
#' https://github.com/adokter/vol2bird/wiki/ODIM-bird-profile-format-specification
#' 
#' @param vp bioRad 'vp' object (vertical profile)
#' @return data.frame with select metadata and data of that vp (records = HGHTs)
#' @examples
#' {
#' vp_to_df(vp)
#' }
vp_to_df <- function(vp) {
  # Select data from the vp$data
  df <- subset(vp$data, select = 
    c("HGHT", "u", "v", "dens", "sd_vvp")
  )
  
  # Select metadata from vp$attributes/what/
  # Get radar_id "seang" from "NOD:seang", which is the first element in 
  # attributes/what/source
  source <- unlist(strsplit(vp$attributes$what$source, ","))
  radar_id <- unlist(strsplit(source[1], ":"))[2]
  
  # Get and combine attributes/what/date & attributes/what/time
  date_time_string <- paste0(vp$attributes$what$date, vp$attributes$what$time)
  # Express as an ISO datetime
  date_time <- format(
    as.POSIXct(date_time_string, format = "%Y%m%d%H%M%S"),
    "%Y-%m-%dT%H:%M:%SZ"
  )
  
  # Prepend metadata to data.frame
  df <- cbind(
    "radar_id" = radar_id,
    "date_time" = date_time,
    df
  )
  return(df)
}
