is.LatitudeLongitude <- function(check.DF, check.latitude, check.longitude) {
  # **********************************Header***********************************
  # FUNCTION NAME: is.latitudelongitude
  # DESCRIPTION: This function accepts a latitude and longitude and will return
  # whether the values contained are legal latitude and longitudes
  # 
  #
  # Args:
  #   check.DF: URL Data Frame to be checked
  #   check.latitude: Latitude column name
  #   check.longitude: Longitude column name
  # Returns:
  #   check.DF: Corrected Data Frame minus any illegal Lat and Longs 
  #     
  #     
  # **********************************Header***********************************
  
  # -----------------------------------------------------------------------------
  # -- CONSTANT SET SECTION
  # -----------------------------------------------------------------------------

  # Regular Expression patterns for both Latitude and Longitude
  # slightly modified version found at this link. 
  # http://stackoverflow.com/questions/3518504/regular-expression-for-matching-latitude-longitude-coordinate  
  kLatitudePattern  <- "^(\\+|-)?(?:90(?:(?:\\.0{1,9})?)|(?:[0-9]|[1-8][0-9])(?:(?:\\.[0-9]{1,9})?))$"
  kLongitudePattern <- "^(\\+|-)?(?:180(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-9][0-9]|1[0-7][0-9])(?:(?:\\.[0-9]{1,6})?))$"
  
  
  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------  
  
  # Check both Latitude and Longitude against the patterns and if both are 
  # legal Latitude and Longitudes return row otherwise remove

  if (sum(grepl(kLatitudePattern, check.DF[[check.latitude]], perl = TRUE) &
          grepl(kLongitudePattern, check.DF[[check.longitude]], perl = TRUE)) < nrow(check.DF)){
    print("Illegal latitude/longitude combinations present and have been removed")
  }

  
  check.DF <- check.DF[grepl(kLatitudePattern, check.DF[[check.latitude]], perl = TRUE) &
                       grepl(kLongitudePattern, check.DF[[check.longitude]], perl = TRUE), ]

  
  return(check.DF)
} ### END is.LatitudeLongitude