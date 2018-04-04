# **********************************Header*************************************
# *********GENERAL*********
# OBJECT NAME: TWCoHistoryOnDemand 
# VERSION: 3.0
# OBJECT TYPE: R Custom Node
# CATEGORY: Utility
# SUBCATEGORY: Weather
# CREATED BY:   YU WENPEI	
# DATE:         11/15/2016
# MODIFIED BY:  GRANT CASE
# DATE:         03/19/2017
# DESCRIPTION:
# SPSS Modeler Predictive Extension to call the Historical-Site
# based TWCo API and return results.
#
# TWCo API Documentation
# https://docs.google.com/document/d/1HVKgGRdO4nPViF3YFC3e6MVd1nJvoYw202rGzaiOwbc/edit
# 
# https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&geocode=<geocode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey
# https://api.weather.com/v3/wx/hod/conditions/historical/point?pointType=<pointType>&geocode=<geocode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey
# 
# https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&iataCode=<iataCode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey
# 
# https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&icaoCode=<icaoCode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey
# 
# https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&placeid=<placeid>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey 
# 
# https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&postalKey=<postalKey>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey
# 
# https://api.weather.com/v3/wx/hod/reanalysis/historical/boundary?boundaryType=<boundaryType>&[bbox=<bboxCorners>|vertices=<vertices>]&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey
#
# 
# 
# *********DEPENDENCIES*********
# PACKAGES:
# httr
# plyr
# dplyr
# jsonline
# lubridate
# mefa
# 
# 
# *********MODIFICATION LOG*********
# DATE        INITIALS MODIFICATION
# 11/15/2016  YW
# Created  
# 01/29/2017  GSC      
# Forked to ensure correct results by aligning measures datatypes also
# add comments, sections, and headers for easier understanding. Now returning
# metadata and allows for different return languages, units, and no longer
# do postal codes need to be prefaced with the correct country code. This is
# now handled in the extension. Also updated so columns with spaces will no
# longer be an issue.
# 03/19/2017 GSC
# Added code so that users can pass more than 30 days of data
#
# *********TODO LOG*********
# TODO(Grant Case): Add parallelism code
#
#
#
#
# *********HEADER CONVENTIONS*********
# DO NOT GO PAST 80 CHARACTERS
# TO DEBUG, REPLACE ALL "# DEBUG "
# TO TURN DEBUG OFF, FIND ALL LINES WITH "  # DB" AND "# DEBUG " AT BEGINNING
# **********************************Header*************************************
# Careful below, this line of code one will completely wipe out the variables 
# (your SPSS console), will have nothing in it. If you are doing development work and 
# are creating variables and such, you may have issues when you reload 
# the .RData file. This line of code will specifically eliminate anything BUT 
# whats being sent by Modeler. Think of it as a reset. Its commented out with a 
# RESET tag even though its helpful for Debugging purposes because of what it 
# does. 
# http://r.789695.n4.nabble.com/How-to-remove-all-objects-except-a-few-specified-objects-td2335651.html
# RESET rm(list= ls()[!(ls() %in% c('modelerData','modelerDataModel','ibmspsscfpkg.connections',
#                           'ibmspsscfpkg.fileNamesList','ibmspsscfpkg.htmlFilesCount',
#                           'ibmspsscfpkg.oldwd','ibmspsscfpkg.zipFileNames'))]) 
# -----------------------------------------------------------------------------
# -- PACKAGE DECLARATION SECTION
# -----------------------------------------------------------------------------
# ********************Create test for package existence************************
list.of.packages <- c("httr","plyr","dplyr","lubridate","urltools","jsonlite","mefa","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# ****************************Packages to Use**********************************
library(httr)
library(plyr)
library(dplyr)
library(lubridate)
library(urltools)
library(jsonlite)
library(mefa)
library(stringr)
# DEBUG print("End Install Package") # DB

# -----------------------------------------------------------------------------
# -- CUSTOM DIALOG VARIABLE DECLARATION SECTION
# -----------------------------------------------------------------------------

# TWCo API Options Tab
# Left Panel
input_apikey            <- "%%item_apikey%%"
dialog.column.apikey    <- input_apikey

input_api               <- %%item_api%%
dialog.column.api       <- input_api

input_units             <- "%%item_unit%%"
  # e = English units
  # m = Metric units
  # s = Metric SI units (not available for all APIs)
dialog.column.units     <- input_units




input_startDate               <- "%%item_startdate%%"
dialog.column.startdate.name  <- make.names(input_startDate)

input_endDate                 <- "%%item_enddate%%" 
dialog.column.enddate.name    <- make.names(input_endDate)

input_geospatial_query        <- %%item_geospatial_query_type%%
dialog.option.geospatialtype  <- input_geospatial_query  

input_radius_km               <- "%%item_radius_km%%"
dialog.option.radius_km       <- input_radius_km  


# Center Panel
input_requestType                <- %%item_locationtype%%
dialog.option.locationtype       <- input_requestType

input_latitude                   <- "%%item_lat%%"
dialog.column.latitude.name      <- make.names(input_latitude)
input_longitude                  <- "%%item_lon%%"
dialog.column.longitude.name     <- make.names(input_longitude)
input_latitude_nw                <- "%%item_lat_nw%%"
dialog.column.latitude_nw.name   <- make.names(input_latitude_nw)
input_longitude_nw               <- "%%item_lon_nw%%"
dialog.column.longitude_nw.name  <- make.names(input_longitude_nw)

input_countrycode                <- "%%item_countrycode%%"
dialog.column.countrycode        <- input_countrycode

input_postalcode                 <- "%%item_postalcode%%"
dialog.column.postalcode.name    <- make.names(input_postalcode)

input_icaocode                   <- "%%item_icao%%"
dialog.column.icaocode.name      <- make.names(input_icaocode)

input_iatacode                   <- "%%item_iata%%"
dialog.column.iatacode.name      <- make.names(input_iatacode)

input_placeidcode                <- "%%item_place_id%%"
dialog.column.placeidcode        <- make.names(input_placeidcode)


# Customize Selection of Intraday Tab
# Left Panel
input_adjust_intraday            <- "%%item_adjust_intraday%%"
dialog.checkbox.intraday.custom  <- input_adjust_intraday

if (dialog.checkbox.intraday.custom == "true") {
  dialog.checkbox.intraday.custom <- "true"
} else {
  dialog.checkbox.intraday.custom <- "false"
}

dialog.checkbox.intraday.custom  <- as.logical(dialog.checkbox.intraday.custom)


if  (dialog.checkbox.intraday.custom) {

  input_start_hh            <- str_sub(paste("0","%%item_start_hh%%",sep=""), start= -2)   
  input_start_mm            <- str_sub(paste("0","%%item_start_mm%%",sep=""), start= -2)
  input_intraday_starttime  <-  paste(input_start_hh, input_start_mm, sep="")
  dialog.textbox.starttime  <- input_intraday_starttime

  input_end_hh              <- str_sub(paste("0","%%item_end_hh%%",sep=""), start= -2)
  input_end_mm              <- str_sub(paste("0","%%item_end_mm%%",sep=""), start= -2)
  input_intraday_endtime    <-  paste(input_end_hh, input_end_mm, sep="")
  dialog.textbox.endtime    <- input_intraday_endtime 

  
} else {
  dialog.textbox.starttime  <- "0000"
  dialog.textbox.endtime    <- "2359"
}









# Added for Future Use
# TODO(Grant Case): Add in Timezone so as to allow the user to switch between 
# GMT or Local Time when showing date/time
dialog.column.timezone        <- "GMT" 
location.type <- dialog.option.locationtype
timezone.type <- dialog.column.timezone



# -----------------------------------------------------------------------------
# -- CONSTANT SET SECTION
# -----------------------------------------------------------------------------
kErrorOutputPrefix               <- "TWCoHistoryOnDemand Node Error: "
kDebugImageLocation              <- "C:/Users/GrantCASE/Box Sync/My Work Folder/IBM SPSS Predictive Extensions/TWCo_History_on_Demand/Code/Debug/TWCoHistoryOnDemand.RData"
kDebugImageLocation2             <- "C:/Users/GrantCASE/Box Sync/My Work Folder/IBM SPSS Predictive Extensions/TWCo_History_on_Demand/Code/Debug/modelerDebug.RData"
kmodDLatitudeColNameConstant     <- "latitude"
kmodDLongitudeColNameConstant    <- "longitude"
kmodDPostalCodeColNameConstant   <- "postal.code"
kmodDStationIDColNameConstant    <- "station.id"
kmodDStartDateColNameConstant    <- "start.date"
kmodDEndDateColNameConstant      <- "end.date"
kmodDLocationURLColNameConstant  <- "location.type"
kmodDLatCommaLongColNameConstant <- "lat.comma.long"
kmodDPostalKeyColNameConstant    <- "postal.key"
kAPIHistorySite                  <- "History-Site"
kAPILocation                     <- "Location-Point"
kBaseURLColNameConstant          <- "Base.URL"
kBaseLocationURLColNameConstant  <- "Location.URL"
save.image(file=kDebugImageLocation, safe=FALSE) # DB
print("End Custom Dialog") # DB



RetrieveTWCoBaseURL <- function(API.Location, API) {
  # **********************************Header***********************************
  # FUNCTION NAME: RetrieveTWCoBaseURL
  # DESCRIPTION: This function accepts a data frame in the style of
  # modelerDataModel and will return a data frame will legal R field names that
  # in theory should match modelerData.
  #
  # Args:
  #   API.Location: (string) Equivalent to whether being searched by lat/long 
  #   or being searched by postal code.
  #   API: The actual version of the The Weather Channel API that is being
  #   called. Depending on the information that needs to be returned.
  #
  # Returns:
  # The base URL that will be updated later on by other functions and script.
  # 
  # TODO(Grant Case): Add Current and Forecast URL strings
  # **********************************Header***********************************
  if (API.Location == "geocode" && API == "History-Site") {
  
  
  https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&geocode=<geocode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=<api.key>
  https://api.weather.com/v3/wx/hod/conditions/historical/point?pointType=<pointType>&geocode=<geocode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=<api.key>
  
  https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&iataCode=<iataCode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=<api.key>
  
  https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&icaoCode=<icaoCode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=<api.key>
  
  https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&placeid=<placeid>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=<api.key> 
  
  https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&postalKey=<postalKey>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=<api.key>
  
https://api.weather.com/v3/wx/hod/reanalysis/historical/boundary?boundaryType=<boundaryType>&[bbox=<bboxCorners>|vertices=<vertices>]&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=<api.key>
  
  
  
  
  
  
    "https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&geocode=<geocode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey"
    "https://api.weather.com/v3/wx/hod/reanalysis/historical/point?pointType=<pointType>&geocode=<geocode>&distance=<distance>&startDateTime=<startDateTime>&endDateTime=<endDateTime>&units=<units>&format=<format>&apiKey=yourApiKey"
    
    
    
    BaseURL   <- "https://api.weather.com/v1/geocode/<latitude>/<longitude>/observations/historical.json?language=<language>&units=<units>&apiKey=<api.key>&startDate=<start.date>&endDate=<end.date>"
  } else if (API.Location == "postalcode" && API == "History-Site") {
    BaseURL   <- "https://api.weather.com/v1/location/<postal.code>:4:<country>/observations/historical.json?language=<language>&units=<units>&apiKey=<api.key>&startDate=<start.date>&endDate=<end.date>"
  } else if (API.Location == "stationid" && API == "History-Site") {
    BaseURL <- "https://api.weather.com/v1/location/<station.id>:4:<country>/observations/historical.json?language=<language>&units=<units>&apiKey=<api.key>&startDate=<start.date>&endDate=<end.date>" 
  } else if (API.Location == "geocode" && API == "Location-Point") {
    BaseURL <- "https://api.weather.com/v3/location/point?geocode=<latitudecommalongitude>&language=<language>&format=json&apiKey=<api.key>"
    
  } else if (API.Location == "postalcode" && API == "Location-Point") {
    BaseURL <- "https://api.weather.com/v3/location/point?postalKey=<postal.key>&language=<language>&format=json&apiKey=<api.key>"
    
  } else {
    BaseURL = ""
  }
  
  return(BaseURL)
} ### END RetrieveTWCoBaseURL




CreateAppendDFColumnData <- function(column, new.column.name, targetDF, sourceDF) {
  # **********************************Header***********************************
  # FUNCTION NAME: CreateAppendDFColumnData
  # DESCRIPTION: This function seeks to create a data frame or append a 
  # column to it dataframe based on data sent. 
  # 
  #
  # Args:
  #   column: a list or data frame column that needs to be part of the data.frame
  #
  #   new.column.name: The name to be given to that column when returned in DF
  #
  #   targetDF: The data frame where the data can be attached (note this can
  #   sent without data in the case of a new data frame to be built)
  #
  #   sourceDF: If information is being appended from one data frame to another
  #   the function will take the column, grab it from sourceDF, update the
  #   the column name and attach it to targetDF
  #
  # Returns:
  #   targetDF: the data frame either created or updated with new the information
  #   passed to it.     
  #     
  # **********************************Header***********************************
  # -----------------------------------------------------------------------------
  # -- CONSTANT SET SECTION
  # -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------  
  quotedcolumn = quote(column)
 
  if (missing(targetDF)) {
    targetDF <- data.frame(column)
  } else if (missing(sourceDF)) {
    targetDF <- data.frame(targetDF, column)
  } else {
    targetDF <- data.frame(targetDF, sourceDF[,column])
  }
  
  
  names(targetDF)[ncol(targetDF)] <- new.column.name
  return(targetDF)
} ### END CreateAppendDFColumnData
# DEBUG modelerData  # DB
# DEBUG print("End Constants and Variables")  # DB
# -----------------------------------------------------------------------------
# -- ERROR HANDLING FUNCTIONS SECTION
# -----------------------------------------------------------------------------
# Modeler doesnt appear to handle custom messages, warnings, and conditions,
# gracefully. For now, we will try to catch them before they occur and print
# the error in the console log.
# condition <- function(subclass, message, call = sys.call(-1), ...) {
#   structure(
#     class = c(subclass, "condition"),
#     list(message = message, call = call, ...)
#   )
# }
# 
# # Defines a custom stop function so that issues can be raised to the end user
# CustomMessageF <- function(subclass, message, call = sys.call(-1), ...) {
#   kErrorOutputPrefix          <- "TWCoHistoryOnDemand Node Error: "
#   
#   c <- condition(c(subclass, "message"), message, call = call, ...)
# 
# # DEBUG   save.image(file=kDebugImageLocation, safe=FALSE) # DB
# # DEBUG   print("DEBUG Image Save End") # DB
# 
#   paste(kErrorOutputPrefix, c)
# #  print(c)
# 
#   message(c)
# }
# _____________________________________________________________________________
is.TWCoDate  <- function(check.DF, check.start, check.end, check.historyonly = TRUE) {
  # **********************************Header***********************************
  # FUNCTION NAME: is.TWCoDate
  # DESCRIPTION: This function accepts a data frame and will filter it so as to 
  # determine what dates are legal dates. If it finds any dates that are not
  # legal it will print an error in the console log.
  # 
  #
  # Args:
  #   check.start: Name of the field that is the Start Date
  #   check.end: Name of the field that is End Date
  #   check.historyonly: (Boolean: TRUE) - if TRUE will only check for historic 
  #   dates. For weather forecast dates in the future you would choose FALSE.
  #
  # Returns:
  #   check.DF: Data Frame of only rows with valid dates.
  # **********************************Header***********************************
  
  # -----------------------------------------------------------------------------
  # -- TEST SECTION
  # -----------------------------------------------------------------------------
  # check.DF          <- URL.Data
  # check.start       <- kmodDStartDateColNameConstant
  # check.end         <- kmodDEndDateColNameConstant
  # check.historyonly <- TRUE
  
  # -----------------------------------------------------------------------------
  # -- VARIABLE SET SECTION
  # -----------------------------------------------------------------------------
  # Earliest date of The Weather Companys observations - January 1931
  # Earliest date of The Weather Companys observations - January 1931
  
  
  
  
  earliest.date <- ymd(19310101)
  
  # If check.historyonly is true it will do the latest day + 1 otherwise + 7 days
  if (check.historyonly) {
    latest.date <- today() + 1
  } else {
    latest.date <- today() + 7
  }
  
  # Convert Start and End dates to date elements. If they are not legal elements
  # ymd will return NA
  check.DF[[check.start]] <- ymd(check.DF[[check.start]])
  check.DF[[check.end]] <- ymd(check.DF[[check.end]])
  # -----------------------------------------------------------------------------
  # -- EXCEPTION HANDLING SECTION
  # -----------------------------------------------------------------------------
  if (sum(is.na(check.DF[[check.start]]) || is.na(check.DF[[check.end]]), na.rm = TRUE) > 0) {
    print("Extension Error: Data Frame contained rows with a either the Start or End Dates that are not a legal date")
  }
  
  if (sum(check.DF[[check.start]] > check.DF[[check.end]], na.rm = TRUE) > 0) {
    print("Extension Error: Data Frame contained rows with an input of Start Date must Be the Same or less than End Date")
  }
  
  if (sum(check.DF[[check.start]] < earliest.date || check.DF[[check.end]] < earliest.date, na.rm = TRUE) > 0) {
    print("Extension Error: Data Frame contained rows with a input of Start or End Date before weather records began (Jan 1931)")
  }
  
  if (sum(check.DF[[check.start]] > latest.date | check.DF[[check.end]] > latest.date, na.rm = TRUE) > 0) {
    print("Extension Error: Data Frame contained rows with a Future Date, No weather records yet")
  }
  
  # Remove rows that have errors in the date, see exception handling above,
  # where the start date or end date is NA, the start date, pre-dates end
  # date and where earliest date is before recording or after the latest 
  # date in the system.
  check.DF <-    check.DF[!(is.na(check.DF[[check.start]]) 
                       | is.na(check.DF[[check.end]])
                       | check.DF[[check.start]] > check.DF[[check.end]]
                       | check.DF[[check.start]] < earliest.date
                       | check.DF[[check.end]] < earliest.date
                       | check.DF[[check.start]] > latest.date
                       | check.DF[[check.end]] > latest.date)
                       , ]
                       
  
  # Split the data frame in two so that spans of greater than 30 days, TWC
  # counts the dates as such Jan 31st - Jan 1st would be 31 days as the 31st
  # returns information, are in the Above30Day, which those that are below
  # go directly to landing.
  Above30Day <- check.DF[check.DF[[check.end]] - check.DF[[check.start]]  > 29, ]
  check.DF   <- check.DF[check.DF[[check.end]] - check.DF[[check.start]]  <= 29, ]
  
  if (NROW(Above30Day) > 0) {
  
    for(i in 1:NROW(Above30Day)) {
      # Pull a row out that is above 30 days and create a sequence
      # so that we have a set of start and end dates that span
      # 30 days. You now replicate the rows to match the new sequence
      # and combine back the new start and end dates.
      To_Test <- Above30Day[i, ]
      new.start.date <- seq.Date(To_Test[[check.start]],To_Test[[check.end]],by = "30 day")
      new.end.date <- (new.start.date + 29)
      Clean30Day <- rep(To_Test, NROW(new.start.date))
      Clean30Day$start.date <- new.start.date
      Clean30Day$end.date <- if_else(Clean30Day$end.date < new.end.date, Clean30Day$end.date, new.end.date)
      check.DF <- rbind(check.DF, Clean30Day)
    }
  }  
  
  
  
  
  return(check.DF)
} ### END is.TWCoDate
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
  # -- EXCEPTION HANDLING SECTION
  # -----------------------------------------------------------------------------  
  # Check both Latitude and Longitude against the patterns and if both are 
  # legal Latitude and Longitudes return row otherwise remove
  if (sum(grepl(kLatitudePattern, check.DF[[check.latitude]], perl = TRUE) &
          grepl(kLongitudePattern, check.DF[[check.longitude]], perl = TRUE)) < nrow(check.DF)){
    print("Extension Error: Illegal latitude/longitude combinations present and have been removed")
  }
  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------   
  check.DF <- check.DF[grepl(kLatitudePattern, check.DF[[check.latitude]], perl = TRUE) &
                       grepl(kLongitudePattern, check.DF[[check.longitude]], perl = TRUE), ]
  
  return(check.DF)
} ### END is.LatitudeLongitude
# DEBUG save.image(file=kDebugImageLocation, safe=FALSE) # DB
# DEBUG print("End Error Handling Functions") # DB
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
  # How many rows are in the data frame.
  Counter.JSONURLs <- NROW(update.DF)
  
  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------  
  # This loop goes through each JSON URL, uses the RETRY function in HTTR
  # and coerces it to JSON for JSONLITE to pick up, flatten, combined, and make 
  # the ending data set.
  for(i in 1:Counter.JSONURLs) {
  
    url <- update.DF[[URL.col.name]][i]
    
    Retrieve.Data <- RETRY("GET", url)
    
    status.code <- Retrieve.Data$status_code
    
    Content.Data <- content(Retrieve.Data, as="text")
  # If the API returns anything other than a 200 (Status OK), print the value
  # Location Data Not Found and return the URL.
  # TODO(Grant Case): The status codes have data with them on specific errors
  # update it later with this information.
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
# TODO(Grant Case): Code for the timezone shift to be created later
# Code for future function to move time values to Local time from GMT
#   if (tz.type  == "GMT") {
#     resultData$metadata.expire_time_gmt <- as_datetime(origin + as.integer(resultData$observations.valid_time_gmt))
#     resultData$observations.valid_time_gmt <- as_datetime(origin + as.integer(resultData$observations.valid_time_gmt))
#     resultData$observations.expire_time_gmt <- as_datetime(origin + as.integer(resultData$observations.valid_time_gmt))
#   } else {
#     resultData$metadata.expire_time_gmt <- as_datetime(origin + as.integer(resultData$observations.valid_time_gmt))
#     resultData$observations.valid_time_gmt <- as_datetime(origin + as.integer(resultData$observations.valid_time_gmt))
#     resultData$observations.expire_time_gmt <- as_datetime(origin + as.integer(resultData$observations.valid_time_gmt))
#   }
  
  
BuildmodelerDataModel <- function(source.DF, API, API.Location) {
  ########Currently not being Used due to issue with Modeler not liking########
  ########setting the modelerdataModel in a function.########
  
  # **********************************Header***********************************
  # FUNCTION NAME: BuildmodelerDataModel
  # DESCRIPTION: This function accepts a dataframe, API and API.Location. It 
  # will then Build the modelerDataModel 
  # 
  #
  # Args:
  #   check.DF: Data Frame to be checked
  #   check.x: Latitude column name
  #   check.y: Longitude column name
  # Returns:
  #   check.DF: Updated Data Frame 
  #     
  #     
  # **********************************Header***********************************
  # -----------------------------------------------------------------------------
  # -- CONSTANT SET SECTION
  # -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------
  # -- EXECUTE SECTION
  # -----------------------------------------------------------------------------  
  check.DF <- data.frame(c(fieldName="dummy", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole=""))
  names(check.DF)[ncol(check.DF)] <- "dummy"
  
  
    if (API.Location == "geocode" && API == "History-Site") {
      check.DF$latitude     <- c(fieldName="latitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
      check.DF$longitude    <- c(fieldName="longitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
      check.DF$start.date   <- c(fieldName="start.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      check.DF$end.date     <- c(fieldName="end.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
     
    } else if (API.Location == "postalcode" && API == "History-Site") {
      check.DF$postal.code  <- c(fieldName="postal.code", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
      check.DF$start.date   <- c(fieldName="start.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      check.DF$end.date     <- c(fieldName="end.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      
    } else if (API.Location == "stationid" && API == "History-Site") {
      check.DF$station.id   <- c(fieldName="station.id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
      check.DF$start.date   <- c(fieldName="start.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      check.DF$end.date     <- c(fieldName="end.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
    
    } else {
      check.DF = check.DF
  }
  
  check.DF <- check.DF[2: NCOL(check.DF)]
  check.DF$metadata.language                      <- c(fieldName="metadata.language", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$metadata.transaction_id                <- c(fieldName="metadata.transaction_id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$metadata.version                       <- c(fieldName="metadata.version", fieldLabel="", fieldStorage="integer", fieldMeasure="", fieldFormat="",   fieldRole="")
  
  
  if (API.Location == "geocode") {
    check.DF$metadata.latitude     <- c(fieldName="metadata.latitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
    check.DF$metadata.longitude    <- c(fieldName="metadata.longitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="") 
  } else {
    check.DF$metadata.location_id                   <- c(fieldName="metadata.location_id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  }
  
  
  
  check.DF$metadata.units                         <- c(fieldName="metadata.units", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$metadata.expire_time_gmt               <- c(fieldName="metadata.expire_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$metadata.status_code                   <- c(fieldName="metadata.status_code", fieldLabel="", fieldStorage="integer", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.key                       <- c(fieldName="observations.key", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.class                     <- c(fieldName="observations.class", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.expire_time_gmt           <- c(fieldName="observations.expire_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.obs_id                    <- c(fieldName="observations.obs_id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.obs_name                  <- c(fieldName="observations.obs_name", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.valid_time_gmt            <- c(fieldName="observations.valid_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.day_ind                   <- c(fieldName="observations.day_ind", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.temp                      <- c(fieldName="observations.temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.wx_icon                   <- c(fieldName="observations.wx_icon", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.icon_extd                 <- c(fieldName="observations.icon_extd", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.wx_phrase                 <- c(fieldName="observations.wx_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.pressure_tend             <- c(fieldName="observations.pressure_tend", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.pressure_desc             <- c(fieldName="observations.pressure_desc", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.dewPt                     <- c(fieldName="observations.dewPt", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.heat_index                <- c(fieldName="observations.heat_index", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.rh                        <- c(fieldName="observations.rh", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.pressure                  <- c(fieldName="observations.pressure", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.vis                       <- c(fieldName="observations.vis", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.wc                        <- c(fieldName="observations.wc", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.wdir                      <- c(fieldName="observations.wdir", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.wdir_cardinal             <- c(fieldName="observations.wdir_cardinal", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.gust                      <- c(fieldName="observations.gust", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.wspd                      <- c(fieldName="observations.wspd", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.max_temp                  <- c(fieldName="observations.max_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.min_temp                  <- c(fieldName="observations.min_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.precip_total              <- c(fieldName="observations.precip_total", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.precip_hrly               <- c(fieldName="observations.precip_hrly", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.snow_hrly                 <- c(fieldName="observations.snow_hrly", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.uv_desc                   <- c(fieldName="observations.uv_desc", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.feels_like                <- c(fieldName="observations.feels_like", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.uv_index                  <- c(fieldName="observations.uv_index", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.qualifier                 <- c(fieldName="observations.qualifier", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.qualifier_svrty           <- c(fieldName="observations.qualifier_svrty", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.blunt_phrase              <- c(fieldName="observations.blunt_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.terse_phrase              <- c(fieldName="observations.terse_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  check.DF$observations.clds                      <- c(fieldName="observations.clds", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.water_temp                <- c(fieldName="observations.water_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.primary_wave_period       <- c(fieldName="observations.primary_wave_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.primary_wave_height       <- c(fieldName="observations.primary_wave_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.primary_swell_period      <- c(fieldName="observations.primary_swell_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.pprimary_swell_height     <- c(fieldName="observations.primary_swell_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.primary_swell_direction   <- c(fieldName="observations.primary_swell_direction", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.secondary_swell_period    <- c(fieldName="observations.secondary_swell_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.secondary_swell_height    <- c(fieldName="observations.secondary_swell_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  check.DF$observations.secondary_swell_direction <- c(fieldName="observations.secondary_swell_direction", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  Source.Column.Cnt <- NCOL(source.DF)
  MDM.Column.Cnt <- NCOL(check.DF)
  
  if (Source.Column.Cnt != MDM.Column.Cnt) {
    print("Extension Error: The total number of columns between the modelerDataModel and modelerData do not match.")
    print("Extension Error: This import will fail as a result.")
    print("Extension Error: Compare to see which column(s) are out as a result")
  
  }
  
#     T_MD  <- t(head(source.DF,2))
#     T_MDM <- t(head(check.DF,6))
#     print(T_MD)
#     print(T_MDM)
# T_MD_MDM <- cbind(T_MDM,T_MD)    
    
  return(check.DF)
} ### END BuildmodelerDataModel
# DEBUG save.image(file=kDebugImageLocation, safe=FALSE) # DB
# DEBUG print("End Node-Specific Functions") # DB
# -----------------------------------------------------------------------------
# -- BUILD URL DATA FRAME
# -----------------------------------------------------------------------------
# Based on types chosen in UI, grab the approriate URL template and start
# dataset
URL.Data <- CreateAppendDFColumnData(RetrieveTWCoBaseURL(location.type,kAPIHistorySite),kBaseURLColNameConstant)
# Based on what the location type is Lat/Long, Postal Code, or Station ID(unused)
# add that to the URL
if (location.type == "geocode") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.latitude.name, 
                                         kmodDLatitudeColNameConstant, 
                                         URL.Data, modelerData)
  URL.Data <- CreateAppendDFColumnData(dialog.column.longitude.name, 
                                         kmodDLongitudeColNameConstant, 
                                         URL.Data, modelerData)
                                                                              
                                         
} else if (location.type == "postalcode") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.postalcode.name, 
                                        kmodDPostalCodeColNameConstant, 
                                        URL.Data, modelerData)
  
                                        
} else if (location.type == "stationID") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.postalcode.name, 
                                         kmodDPostalCodeColNameConstant, 
                                         URL.Data, modelerData)  
} else {
  URL.Data <- URL.Data
}
# Add the date column or date inputs
if (date.input.type == "is_variable") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.startdate.name, 
                                       kmodDStartDateColNameConstant, 
                                       URL.Data, 
                                       modelerData)
                                       
  URL.Data <- CreateAppendDFColumnData(dialog.column.enddate.name, 
                                       kmodDEndDateColNameConstant, 
                                       URL.Data, 
                                       modelerData)
                                       
} else if (date.input.type == "is_textinput") {
  URL.Data <- CreateAppendDFColumnData(dialog.column.startdate.value, 
                                       kmodDStartDateColNameConstant, 
                                       URL.Data)
                                       
  URL.Data <- CreateAppendDFColumnData(dialog.column.enddate.value, 
                                       kmodDEndDateColNameConstant, 
                                       URL.Data)
} else {
  URL.Data <- URL.Data
}
# Grab the location URL
# TODO(Grant Case): Add a way for the UI to select to place times in local time and use code to grab the URL.
URL.Data <- CreateAppendDFColumnData(RetrieveTWCoBaseURL(location.type,
                                                         kAPILocation),
                                     kBaseLocationURLColNameConstant, URL.Data)
# Depending on dataset you could have many URLs that would the same and give duplicate date, filter only unique records
URL.Data <- unique(URL.Data)
# Run the function to determine if there are any dates that need to be weeded 
# out (doesnt return conditions as Modeler apparently hates conditions so 
# we are using PRINT into the log to find the error)
tryCatch(
URL.Data <- is.TWCoDate (URL.Data, kmodDStartDateColNameConstant, kmodDEndDateColNameConstant, check.historyonly = TRUE),
                         Invalid_Date = function(c) paste(kErrorOutputPrefix, "Either the Start or End Dates are not legal"),
                         Invalid_Start_End = function(c) paste(kErrorOutputPrefix, "Input of Start Date must be the same or less than End Date"),
                         Invalid_Too_Early = function(c) paste(kErrorOutputPrefix, "Input of Start or End Date before weather records began - Jan 1931"),
                         Invalid_Future_Date = function(c) paste(kErrorOutputPrefix, "Input of Future Date, No weather records yet")
)
# Run the function to determine if there are any lat/longs that need to be 
# weeded out (doesnt return conditions as Modeler apparently hates 
# conditions so we are using PRINT into the log to find the error)
if (location.type == "geocode") {
  tryCatch(
    URL.Data <- is.LatitudeLongitude (URL.Data, kmodDLatitudeColNameConstant, kmodDLongitudeColNameConstant),
                                     Invalid_Latitude_Longitude = function(c) paste(kErrorOutputPrefix, "Data Frame contained rows with an illegal lattitude or longitude")
  )
}
# -----------------------------------------------------------------------------
# -- BUILD BASE URL STRINGS
# -----------------------------------------------------------------------------
# Update the URL path for the calls as URLTools does not include a parameter
# to set those to our specification
URL.Data <- UpdateSiteHistoryURLPath (URL.Data,
                                      kAPIHistorySite,
                                      location.type,
                                      kBaseURLColNameConstant,
                                      dialog.column.language,
                                      dialog.column.units,
                                      dialog.column.apikey,
                                      kmodDStartDateColNameConstant,
                                      kmodDEndDateColNameConstant,
                                      kmodDLatitudeColNameConstant,
                                      kmodDLongitudeColNameConstant,
                                      kmodDPostalCodeColNameConstant,
                                      kmodDStationIDColNameConstant,
                                      dialog.column.countrycode)
                          
# Update the URL parameters with URLTools package
URL.Data <- UpdateTWCoURLParameters  (URL.Data,
                                      kAPIHistorySite,
                                      location.type,
                                      kBaseURLColNameConstant,
                                      dialog.column.language,
                                      dialog.column.units,
                                      dialog.column.apikey,
                                      kmodDStartDateColNameConstant,
                                      kmodDEndDateColNameConstant,
                                      kmodDLatitudeColNameConstant,
                                      kmodDLongitudeColNameConstant,
                                      kmodDPostalCodeColNameConstant,
                                      kmodDStationIDColNameConstant,
                                      dialog.column.countrycode)
# Update the URL parameters for Location-based API.
# Future: Invoked but not used in the final product                                    
URL.Data <- UpdateTWCoURLParameters  (URL.Data,
                                      kAPILocation,
                                      location.type,
                                      kBaseLocationURLColNameConstant,
                                      dialog.column.language,
                                      dialog.column.units,
                                      dialog.column.apikey,
                                      kmodDStartDateColNameConstant,
                                      kmodDEndDateColNameConstant,
                                      kmodDLatitudeColNameConstant,
                                      kmodDLongitudeColNameConstant,
                                      kmodDPostalCodeColNameConstant,
                                      kmodDStationIDColNameConstant,
                                      dialog.column.countrycode)
# Check Uniqueness One Last Time
URL.Data <- unique(URL.Data)
# Retrieve and create the modelerData that will be returned to Modeler
modelerData <- RetrieveTWCoJSON (URL.Data,
                                      kAPIHistorySite,
                                      location.type,
                                      kBaseURLColNameConstant,
                                      dialog.column.language,
                                      dialog.column.units,
                                      dialog.column.apikey,
                                      kmodDStartDateColNameConstant,
                                      kmodDEndDateColNameConstant,
                                      kmodDLatitudeColNameConstant,
                                      kmodDLongitudeColNameConstant,
                                      kmodDPostalCodeColNameConstant,
                                      kmodDStationIDColNameConstant,
                                      dialog.column.countrycode,
                                      timezone.type)
# ****************************************************************************
# ****************************************************************************
# Build modelerDataModel
# ****************************************************************************
  modelerDataModel <- data.frame(c(fieldName="dummy", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole=""))
  names(modelerDataModel)[ncol(modelerDataModel)] <- "dummy"
  
  
    if (location.type == "geocode") {
      modelerDataModel$latitude     <- c(fieldName="latitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
      modelerDataModel$longitude    <- c(fieldName="longitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
      modelerDataModel$start.date   <- c(fieldName="start.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      modelerDataModel$end.date     <- c(fieldName="end.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
     
    } else if (location.type == "postalcode") {
      modelerDataModel$postal.code  <- c(fieldName="postal.code", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
      modelerDataModel$start.date   <- c(fieldName="start.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      modelerDataModel$end.date     <- c(fieldName="end.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      
    } else if (location.type == "stationid") {
      modelerDataModel$station.id   <- c(fieldName="station.id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
      modelerDataModel$start.date   <- c(fieldName="start.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
      modelerDataModel$end.date     <- c(fieldName="end.date", fieldLabel="", fieldStorage="date", fieldMeasure="", fieldFormat="",   fieldRole="")
    
    } else {
      modelerDataModel = modelerDataModel
  }
  
  modelerDataModel <- modelerDataModel[2: NCOL(modelerDataModel)]
  modelerDataModel$metadata.language                      <- c(fieldName="metadata.language", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$metadata.transaction_id                <- c(fieldName="metadata.transaction_id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$metadata.version                       <- c(fieldName="metadata.version", fieldLabel="", fieldStorage="integer", fieldMeasure="", fieldFormat="",   fieldRole="")
  
  
  if (location.type == "geocode") {
    modelerDataModel$metadata.latitude     <- c(fieldName="metadata.latitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
    modelerDataModel$metadata.longitude    <- c(fieldName="metadata.longitude", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="") 
  } else {
    modelerDataModel$metadata.location_id                   <- c(fieldName="metadata.location_id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  }
  
  
  
  modelerDataModel$metadata.units                         <- c(fieldName="metadata.units", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$metadata.expire_time_gmt               <- c(fieldName="metadata.expire_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$metadata.status_code                   <- c(fieldName="metadata.status_code", fieldLabel="", fieldStorage="integer", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.key                       <- c(fieldName="observations.key", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.class                     <- c(fieldName="observations.class", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.expire_time_gmt           <- c(fieldName="observations.expire_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.obs_id                    <- c(fieldName="observations.obs_id", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.obs_name                  <- c(fieldName="observations.obs_name", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.valid_time_gmt            <- c(fieldName="observations.valid_time_gmt", fieldLabel="", fieldStorage="timestamp", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.day_ind                   <- c(fieldName="observations.day_ind", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.temp                      <- c(fieldName="observations.temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.wx_icon                   <- c(fieldName="observations.wx_icon", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.icon_extd                 <- c(fieldName="observations.icon_extd", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.wx_phrase                 <- c(fieldName="observations.wx_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.pressure_tend             <- c(fieldName="observations.pressure_tend", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.pressure_desc             <- c(fieldName="observations.pressure_desc", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.dewPt                     <- c(fieldName="observations.dewPt", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.heat_index                <- c(fieldName="observations.heat_index", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.rh                        <- c(fieldName="observations.rh", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.pressure                  <- c(fieldName="observations.pressure", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.vis                       <- c(fieldName="observations.vis", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.wc                        <- c(fieldName="observations.wc", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.wdir                      <- c(fieldName="observations.wdir", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.wdir_cardinal             <- c(fieldName="observations.wdir_cardinal", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.gust                      <- c(fieldName="observations.gust", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.wspd                      <- c(fieldName="observations.wspd", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.max_temp                  <- c(fieldName="observations.max_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.min_temp                  <- c(fieldName="observations.min_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.precip_total              <- c(fieldName="observations.precip_total", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.precip_hrly               <- c(fieldName="observations.precip_hrly", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.snow_hrly                 <- c(fieldName="observations.snow_hrly", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.uv_desc                   <- c(fieldName="observations.uv_desc", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.feels_like                <- c(fieldName="observations.feels_like", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.uv_index                  <- c(fieldName="observations.uv_index", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.qualifier                 <- c(fieldName="observations.qualifier", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.qualifier_svrty           <- c(fieldName="observations.qualifier_svrty", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.blunt_phrase              <- c(fieldName="observations.blunt_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.terse_phrase              <- c(fieldName="observations.terse_phrase", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="",   fieldRole="")
  modelerDataModel$observations.clds                      <- c(fieldName="observations.clds", fieldLabel="", fieldStorage="string", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.water_temp                <- c(fieldName="observations.water_temp", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.primary_wave_period       <- c(fieldName="observations.primary_wave_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.primary_wave_height       <- c(fieldName="observations.primary_wave_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.primary_swell_period      <- c(fieldName="observations.primary_swell_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.pprimary_swell_height     <- c(fieldName="observations.primary_swell_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.primary_swell_direction   <- c(fieldName="observations.primary_swell_direction", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.secondary_swell_period    <- c(fieldName="observations.secondary_swell_period", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.secondary_swell_height    <- c(fieldName="observations.secondary_swell_height", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  modelerDataModel$observations.secondary_swell_direction <- c(fieldName="observations.secondary_swell_direction", fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
  Source.Column.Cnt <- NCOL(modelerData)
  MDM.Column.Cnt <- NCOL(modelerDataModel)
  
  if (Source.Column.Cnt != MDM.Column.Cnt) {
    print("Extension Error: The total number of columns between the modelerDataModel and modelerData do not match.")
    print("Extension Error: This import will fail as a result.")
    print("Extension Error: Compare to see which column(s) are out as a result")
  
  }
# ****************************************************************************
# ****************************************************************************
# ****************************************************************************
# 
# modelerDataModel <- as.data.frame(BuildmodelerDataModel(modelerData_Temp, 
#                                           kAPIHistorySite, 
#                                           location.type))
# modelerData <- as.data.frame(modelerData_Temp)
# rm(list= ls()[!(ls() %in% c('modelerData','modelerDataModel','ibmspsscfpkg.connections',
#                            'ibmspsscfpkg.fileNamesList','ibmspsscfpkg.htmlFilesCount',
#                            'ibmspsscfpkg.oldwd','ibmspsscfpkg.zipFileNames','kDebugImageLocation'))]) 
save.image(file=kDebugImageLocation, safe=FALSE) # DB