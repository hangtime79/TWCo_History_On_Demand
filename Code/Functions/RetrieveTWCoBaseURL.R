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