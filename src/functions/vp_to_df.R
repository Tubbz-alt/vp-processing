#' Get select metadata and data from a vertical bird profile as a data frame
#' @description This function creates a data frame from a vertical bird profile
#' (vp). The data frame contains:
#' * `vp$attributes$what`: NOD:radar_id, date_time (repeated for every HGHT)
#' * `vp$data`: select variables like HGHT, u, v, dens
#'
#' For documentation on vp attributes, see:
#' https://github.com/adokter/vol2bird/wiki/ODIM-bird-profile-format-specification
#'
#' @param vp bioRad 'vp' object (vertical profile)
#' @param variables vector of variables that need to be extracted from vp$data
#' @return data frame with select metadata and data of that vp (records = HGHTs)
#' @examples
#' {
#' vp_to_df(vp)
#' }
vp_to_df <- function(vp, variables) {
  # Select data from the vp$data
  df <- subset(vp$data, select = variables)

  # Select metadata from vp$attributes/what/
  # Get radar_id "seang" from "NOD:seang", which is the first element in
  # attributes/what/source
  source <- unlist(strsplit(vp$attributes$what$source, ","))
  radar_id <- unlist(strsplit(source[1], ":"))[2]

  # Get and combine attributes/what/date & attributes/what/time
  datetime_string <- paste0(vp$attributes$what$date, vp$attributes$what$time)
  # Cast to POSIXct with UTC timezone
  datetime <- as.POSIXct(datetime_string, format = "%Y%m%d%H%M%S", tz = "UTC")

  # Prepend metadata to data frame
  df <- cbind(
    "radar_id" = radar_id,
    "datetime" = datetime,
    df
  )
  return(df)
}
