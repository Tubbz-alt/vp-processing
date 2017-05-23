#' TODO: Need to add documentation
load_settings <- function(settings_file) {
  library(yaml)
  yaml_settings <- yaml.load_file(settings_file)
  settings <- list()

  # Copy original settings
  # settings[["yaml_settings"]] <- yaml_settings

  # Get general settings
  settings[["start_date"]] <- yaml_settings$general$include_dates[1]
  settings[["end_date"]] <- yaml_settings$general$include_dates[2]
  settings[["min_height"]] <- yaml_settings$general$include_heights[1]
  settings[["max_height"]] <- yaml_settings$general$include_heights[2]

  # Get radar ids
  radar_ids <- names(yaml_settings$radars)
  # Throw error if radar id does not contain 5 characters
  # Expected is e.g "searl" (countrycode + odim code)
  for (radar_id in radar_ids) {
    if (nchar(radar_id) != 5)
      stop(paste0("Radar ID should have 5 characters: ", radar_id))
  }
  settings[["radar_ids_5char"]] <- radar_ids
  settings[["radar_ids_3char"]] <- substr(radar_ids,3,5) # e.g. "arl" from "searl"
  settings[["countries"]] <- unique(substr(radar_ids,1,2)) # e.g. "se" from "searl" (unique values only)

  return(settings)
}
