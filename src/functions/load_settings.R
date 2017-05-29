#' TODO: Need to add documentation
load_settings <- function(settings_file) {
  library(yaml)
  yaml_settings <- yaml.load_file(settings_file)
  settings <- list()

  # Copy original settings
  # settings[["raw_settings"]] <- yaml_settings

  # Get general settings
  general_start_date <- yaml_settings$general$include_dates[1]
  general_end_date <- yaml_settings$general$include_dates[2]
  general_min_height <- yaml_settings$general$include_heights[1]
  general_max_height <- yaml_settings$general$include_heights[2]

  # Save general settings
  settings$general$start_date <- general_start_date
  settings$general$end_date <- general_end_date
  settings$general$min_height <- general_min_height
  settings$general$max_height <- general_max_height

  # Get radar settings
  radars <- yaml_settings$radars
  radar_ids <- names(radars)

  for (radar_id in radar_ids) {
    # Throw an error if radar ID does not contain 5 characters
    # Expected is e.g "searl" (countrycode + odim code)
    if (nchar(radar_id) != 5) stop(paste0("Radar ID should have 5 characters: ", radar_id))
  }

  # Save radar ids
  settings$general$radar_ids_5char <- radar_ids
  settings$general$radar_ids_3char <- substr(radar_ids,3,5) # e.g. "arl" from "searl"
  settings$general$countries <- unique(substr(radar_ids,1,2)) # e.g. "se" from "searl" (unique values only)

  return(settings)
}
