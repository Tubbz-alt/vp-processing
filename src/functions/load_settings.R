#' TODO: Need to add documentation
load_settings <- function(settings_file, radars_metadata_file) {
  library(yaml)
  # It seems that the yaml library will try to interpret lists in a "smart" way
  # E.g. lists with multiple elements are interpreted as lists, while lists with
  # single elements are interpreted as vectors:
  # $exclude_datetimes = (c("start", "end"))
  # Which is annoying for looping, as we want all lists to be interpreted as
  # lists. So, we use a handler, which seems to do the job:
  yaml_settings <- yaml.load_file(settings_file, handlers = list(seq = function(x) { x }))
  settings <- list()

  # Read metadata
  metadata <- read.csv(radars_metadata_file, encoding = "UTF8")

  # Get general settings
  general_start_date_string <- yaml_settings$general$include_dates[[1]]
  general_end_date_string <- yaml_settings$general$include_dates[[2]]
  general_min_height <- yaml_settings$general$include_heights[[1]]
  general_max_height <- yaml_settings$general$include_heights[[2]]

  # Save general settings
  settings$general$start_date <- general_start_date_string
  settings$general$end_date <- general_end_date_string
  settings$general$min_height <- general_min_height
  settings$general$max_height <- general_max_height

  # Get radar IDs
  radar_ids <- names(yaml_settings$radars)

  # Get settings for each radar
  for (radar_id in radar_ids) {
    # Throw an error if radar ID does not contain 5 characters
    # Expected is e.g "searl" (countrycode + odim code)
    if (nchar(radar_id) != 5) stop(paste0("Radar ID should have 5 characters: ", radar_id))

    # Save radar id
    # Although we already have that as the key for a radar, it's easier to
    # retrieve later if it's also a value
    settings$radars[[radar_id]]$radar_id <- radar_id

    # Lookup radar metadata
    metadata %>% filter(odim_code == radar_id) -> radar_metadata
    # Throw error if radar ID cannot be found in metadata
    if (nrow(radar_metadata) == 0) stop(paste0("Radar \"", radar_id, "\" not listed in: ", radars_metadata_file))

    # Get latitude & longitude
    latitude <- radar_metadata$latitude
    longitude <- radar_metadata$longitude

    # Throw error if latitude or longitude are empty
    if(is.na(latitude) | is.na(longitude)) stop(paste0("Latitude or longitude not populated for \"", radar_id, "\" in: ", radars_metadata_file))

    # Save latitude & longitude
    settings$radars[[radar_id]]$latitude <- latitude
    settings$radars[[radar_id]]$longitude <- longitude

    # Get radar height settings
    radar_min_height <- yaml_settings$radars[[radar_id]]$include_heights[[1]]
    radar_max_height <- yaml_settings$radars[[radar_id]]$include_heights[[2]]

    # Save radar height settings
    # If the value is anything other than an integer (e.g. when no heights were
    # defined for that radar = NULL), use the general height settings
    settings$radars[[radar_id]]$min_height <- if (is.integer(radar_min_height))
      radar_min_height else general_min_height
    settings$radars[[radar_id]]$max_height <- if (is.integer(radar_max_height))
      radar_max_height else general_max_height

    # Get radar datetime settings
    radar_exclude_datetimes <- yaml_settings$radars[[radar_id]][["exclude_datetimes"]]

    exclude_datetime_from_list <- list()
    exclude_datetime_to_list <- list()

    for (i in seq_along(radar_exclude_datetimes)) {
      exclude_datetime_from <- radar_exclude_datetimes[[i]][[1]]
      exclude_datetime_to <- radar_exclude_datetimes[[i]][[2]]

      # If exclude_datetimes from/to is "NA" then replace with general
      # start_date/end_date respectively. Note that for multiple
      # exclude_datetimes, "NA" only makes sense for the very first "to" and
      # very last "from".
      if (exclude_datetime_from == "NA")
        exclude_datetime_from <- general_start_date_string
      if (exclude_datetime_to == "NA")
        exclude_datetime_to <- general_end_date_string

      # Cast to POSIXct with UTC timezone. There is no pretty error handling if
      # none datetime formats are used in the settings
      exclude_datetime_from <- as.POSIXct(exclude_datetime_from, tz = "UTC")
      exclude_datetime_to <- as.POSIXct(exclude_datetime_to, tz = "UTC")

      # If datetime_to has no time component (e.g. 2016-03-01), extend it to
      # 23:59:59, so we cover until the end of the day
      if (format(exclude_datetime_to, format = "%H:%M:%S") == "00:00:00")
        exclude_datetime_to <- exclude_datetime_to + hours(23) + minutes(59) + seconds(59)

      exclude_datetime_from_list[[i]] <- exclude_datetime_from
      exclude_datetime_to_list[[i]] <- exclude_datetime_to
    }

    # Save radar datatime settings (if there were any exclude_datetimes defined)
    if (length(exclude_datetime_from_list) > 0)
      settings$radars[[radar_id]]$exclude_datetime_from <- exclude_datetime_from_list
    if (length(exclude_datetime_to_list) > 0)
      settings$radars[[radar_id]]$exclude_datetime_to <- exclude_datetime_to_list
  }

  # Save radar IDs (after they've been evaluated to be 5 characters)
  settings$general$radar_ids_5char <- radar_ids
  settings$general$radar_ids_3char <- substr(radar_ids,3,5) # e.g. "arl" from "searl"
  settings$general$countries <- unique(substr(radar_ids,1,2)) # e.g. "se" from "searl" (unique values only)

  # Create output filename
  settings$general$vp_output_file <- paste0(
    "vp_data_",
    length(radar_ids), # Number of radars
    "_radars_",
    gsub("-", "", general_start_date_string), # YYYYMMDD
    "_",
    gsub("-", "", general_end_date_string), # YYYYMMDD
    ".csv")

  return(settings)
}
