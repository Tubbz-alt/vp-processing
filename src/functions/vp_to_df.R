#' Get select metadata and data from a vertical bird profile as a data frame
#' @description This function creates a data frame from a vertical bird profile
#' (vp). The data frame contains:
#' * `vp$filename`: radar_id, datetime (repeated for every HGHT)
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
#' vp_to_df(vp, variables)
#' }
vp_to_df <- function(vp, variables) {
  # Add HGHT as default variable
  variables <- c("HGHT", variables)

  # Select data from the vp$data
  df <- subset(vp$data, select = variables)

  # Get filename
  filename <- basename(vp$filename)

  # Extract radar_id (e.g. "seang") from start of filename
  radar_id <- regmatches(filename, regexpr("^[a-z]{5}", filename))
  # Throw an error if no radar_id was found
  if (identical(radar_id, character(0))) stop(paste0("Cannot extract a five letter radar_id (e.g. \"seang\") from start of filename: ", filename))

  # Extract datetime from filename
  datetime_string <- regmatches(filename, regexpr("[0-9]{4}[01][0-9][0-3][0-9]T[0-2][0-9][0-5][0-9]", filename))
  # Throw an error if no correctly formatted datetime_string was found
  if (identical(datetime_string, character(0))) stop(paste0("Cannot extract a correctly formatted datetime (e.g. \"20160919T0000\") from filename: ", filename))
  # Cast to POSIXct with UTC timezone
  datetime <- as.POSIXct(datetime_string, format = "%Y%m%dT%H%M", tz = "UTC")

  # Prepend metadata to data frame
  df <- cbind(
    "radar_id" = radar_id,
    "datetime" = datetime,
    df
  )
  return(df)
}
