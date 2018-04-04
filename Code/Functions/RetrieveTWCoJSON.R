RetrieveTWCoJSON <- function(update.DF,
                             API,
                             API.Location,
                             URL.col.name,
                             language,
                             units,
                             api.key,
                             start.date.col.name,
                             end.date.col.name,
                             latitude.col.name,
                             longitude.col.name,
                             postal.code.col.name,
                             station.id.col.name,
                             country.code,
                             tz.type) {
  # **********************************Header***********************************
  # FUNCTION NAME: RetrieveTWCoJSON
  # DESCRIPTION: This function accepts a data frame and based upon a completed
  # URL contained in the column that is named in the URL.col.name field 
  
  
  # For consistency, not all columns will be used, but all are requested.
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
  #     
  # http://stackoverflow.com/questions/29997325/successfully-coercing-paginated-json-object-to-r-dataframe    
  # **********************************Header***********************************

  # -----------------------------------------------------------------------------
  # -- CONSTANT SET SECTION
  # -----------------------------------------------------------------------------
  # resultData <- data.frame()
  Counter.JSONURLs <- NROW(update.DF)
  
  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------  

  for(i in 1:Counter.JSONURLs) {
    url <- update.DF[[URL.col.name]][i]
    
    Retrieve.Data <- RETRY("GET", url)
    
    status.code <- Retrieve.Data$status_code
    
    Content.Data <- content(Retrieve.Data, as="text")

    if (status.code != 200){
      print(paste("Location Data Not Found for: ", url))
    
    } else {
      resultData_new <- as.data.frame(fromJSON(Content.Data , 
                    simplifyDataFrame = TRUE, simplifyVector = FALSE, 
                    flatten = TRUE))
      
              
        
        if (API.Location == "geocode" && API == "History-Site") {
            latitude.temp      <- update.DF[[latitude.col.name]][i]
            longitude.temp     <- update.DF[[longitude.col.name]][i]
            start.date.temp    <- update.DF[[start.date.col.name]][i]
            end.date.temp      <- update.DF[[end.date.col.name]][i]
            
            resultData_new     <- CreateAppendDFColumnData(latitude.temp, 
                                      latitude.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(longitude.temp, 
                                      longitude.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(start.date.temp, 
                                      start.date.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(end.date.temp, 
                                      end.date.col.name, resultData_new)
            addcolnum <- 4
  
  
       
        } else if (API.Location == "postalcode" && API == "History-Site") {
            postal.code.temp   <- update.DF[[postal.code.col.name]][i]
            start.date.temp    <- update.DF[[start.date.col.name]][i]
            end.date.temp      <- update.DF[[end.date.col.name]][i]
   
            resultData_new     <- CreateAppendDFColumnData(postal.code.temp, 
                                      postal.code.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(start.date.temp, 
                                      start.date.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(end.date.temp, 
                                      end.date.col.name, resultData_new) 
            addcolnum <- 3
      
        } else if (API.Location == "stationid" && API == "History-Site") {
            station.id.temp    <- update.DF[[station.id.col.name]][i]
            start.date.temp    <- update.DF[[start.date.col.name]][i]
            end.date.temp      <- update.DF[[end.date.col.name]][i]
   
            resultData_new     <- CreateAppendDFColumnData(station.id.temp, 
                                      station.id.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(start.date.temp, 
                                      start.date.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(end.date.temp, 
                                      end.date.col.name, resultData_new) 
            addcolnum <- 3
            
        } else if (API.Location == "geocode" && API == "Location-Point") {
            latitude.temp      <- update.DF[[latitude.col.name]][i]
            longitude.temp     <- update.DF[[longitude.col.name]][i]
            
            resultData_new     <- CreateAppendDFColumnData(latitude.temp, 
                                      latitude.col.name, resultData_new)
            resultData_new     <- CreateAppendDFColumnData(longitude.temp, 
                                      longitude.col.name, resultData_new)
            addcolnum <- 2
            
        } else if (API.Location == "postalcode" && API == "Location-Point") {
            postal.code.temp   <- update.DF[[postal.code.col.name]][i]
   
            resultData_new     <- CreateAppendDFColumnData(postal.code.temp, 
                                      postal.code.col.name, resultData_new)  
            addcolnum <- 1
            
        } else {
          resultData_new = resultData_new
        }
 
    colnum <- NCOL(resultData_new)
    resultData_new <- cbind(resultData_new[, (colnum - addcolnum + 1): colnum],
                            resultData_new[, 1: (colnum - addcolnum)])
 
    if (exists("resultData")) {
    
    resultData <- rbind(resultData, resultData_new)
    
    } else {
    
    resultData <- resultData_new
    
    }
   
 
    } 
      

    i <- i + 1
    colnum    <- NULL
    addcolnum <- NULL
  }  

  return(resultData)

} ### END RetrieveTWCoJSON