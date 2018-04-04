UpdateSiteHistoryURLPath <- function(update.DF,
                                     API,
                                     API.Location,
                                     URL.col.name,
                                     language, #ignored in this function
                                     units, #ignored in this function
                                     api.key, #ignored in this function
                                     start.date.col.name, #ignored in this function
                                     end.date.col.name, #ignored in this function
                                     latitude.col.name,
                                     longitude.col.name,
                                     postal.code.col.name,
                                     station.id.col.name,
                                     country.code) {
  # **********************************Header***********************************
  # FUNCTION NAME: UpdateSiteHistoryURLPath
  # DESCRIPTION: This function accepts a data frame and will update the URL
  # as necessary in order to prepare the URL for Historical Site so that the
  # path with be correct. This differs from the URL Parameters function as 
  # URL Tools, HTTR, and other R packages only encapsulate parameter changes,
  # not path changes. For consistency, not all columns will be used, but all
  # are requested.
  #
  # Args:
  #   update.DF: The data frame to be processed.
  #   API: The API currently called.
  #   API.Location: What type of location is being requested. Latitude/Longitude
  #   postal code, or station ID.
  #   URL.col.name: Name of the column that contains the URL string that needs 
  #   updating
  #   language: The return language requested by the UI.
  #   units: The type of units measurements should be returned in: English (US)
  #   hybrid (UK), metric or metrix - SI units.
  #   api.key: TWCo API Key sent by the user.
  #   start.date.col.name: Name of the column with start date.
  #   end.date.col.name: Name of the column with end date
  #   latitude.col.name: Name of the column with latitude
  #   longitude.col.name: Name of the column with longitude
  #   postal.code.col.name: Name of the column with postal code
  #   station.id.col.name: Name of the column with Station ID
  #   country.code: The country code that was populated in the UI if postal code
  #   is checked.
  # Returns:
  #   check.DF: Updated Data Frame 
  #     
  # **********************************Header***********************************

  # -----------------------------------------------------------------------------
  # -- CONSTANT SET SECTION
  # -----------------------------------------------------------------------------



  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------  
  Base.URL.Temp <- url_parse(update.DF[[URL.col.name]])
  
  if (API == "History-Site" && API.Location == "geocode") {
    Base.URL.Temp$path <- paste("v1/geocode/", update.DF[[latitude.col.name]], 
  		       "/", update.DF[[longitude.col.name]], 
  		       "/observations/historical.json",
  		       sep="")
  
  } else if (location.type == "postalcode") {
    Base.URL.Temp$path <- paste("v1/location/", 
  		       update.DF[[postal.code.col.name]], ":4:", 
  		       country.code, 
  		       "/observations/historical.json",
  		       sep="")
  
  } else if (location.type == "stationID") {
    Base.URL.Temp$path <- paste("v1/location/", 
  		       update.DF[[kmodDStationIDColNameConstant]], 
  		       ":1:", 
  		       country.code, 
  		       "/observations/historical.json", 
  		       sep="")
  
  } else {
    Base.URL.Temp$path <- Base.URL.Temp$path
  }
  
  update.DF[[URL.col.name]] <- url_compose(Base.URL.Temp)



  return(update.DF)

} ### END UpdateSiteHistoryURLPath
