UpdateTWCoURLParameters <- function(update.DF,
                                    API,
                                    API.Location,
                                    URL.col.name,
                                    language,
                                    units,
                                    api.key,
                                    start.date.col.name,
                                    end.date.col.name,
                                    latitude.col.name, #ignored in this function
                                    longitude.col.name, #ignored in this function
                                    postal.code.col.name, #ignored in this function
                                    station.id.col.name, #ignored in this function
                                    country.code) {
  # **********************************Header***********************************
  # FUNCTION NAME: UpdateTWCoURLParameters
  # DESCRIPTION: This function accepts a data frame and will update all the 
  # parameters for the URL based on the specification laid out by TWC. For 
  # consistency, not all columns will be used, but all are requested.
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
  # -- UPDATE PARAMETERS SECTION
  # -----------------------------------------------------------------------------  
  if (API == "History-Site") {
    # Note: there is no difference in the parameters between geocode, station,  
    # and postal code versions of the historical site URL scheme.
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "language", language)
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "units", units)
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "apiKey", api.key)
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "startDate", 
                                 format(update.DF[[start.date.col.name]], "%Y%m%d"))
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "endDate", 
                                 format(update.DF[[end.date.col.name]], "%Y%m%d"))
    
  } else if (API == "Location-Point" && API.Location == "geocode") {
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "geocode", 
                                 paste(latitude.col.name, "," , longitude.col.name, sep = ""))
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "language", language)
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "apiKey", api.key)

    
  } else if (API == "Location-Point" && API.Location == "postalcode")  {
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "postalKey", 
                                 paste(update.DF[[postal.code.col.name]], ":" , country.code, sep=""))
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "language", language)
    update.DF[[URL.col.name]] <- param_set(update.DF[[URL.col.name]], "apiKey", api.key)
    
  } else {
    update.DF <- update.DF
  }

  return(update.DF)

} ### END UpdateTWCoURLParameters