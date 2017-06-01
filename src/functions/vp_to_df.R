#' Get select metadata and data from a vertical bird profile as a data frame
#' @description This function creates a data frame from a vertical bird profile
#' (vp). The data frame contains:
#' * `vp$attributes$what`: NOD:radar_id, date_time (repeated for every HGHT)
#' * `vp$data`: select variables like HGHT (added by default), u, v, dens
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
  # Add HGHT as default variable
  variables <- c("HGHT", variables)

  # Select data from the vp$data
  df <- subset(vp$data, select = variables)

  # Select metadata from vp$attributes/what/
  source <- vp$attributes$what$source
  # Extract NOD code (e.g. "NOD:seang") from attributes/what/source
  nod_code <- regmatches(source, regexpr("NOD:[a-z]{5}", source))
  # Extract last 5 characters (e.g. "seang")
  radar_id <- substring(nod_code, 5, nchar(nod_code))
  # Throw an error if no nod_code was found
  if (identical(nod_code, character(0))) stop(paste0("No NOD code found in vp file in what$attributes$source: ", source))

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
