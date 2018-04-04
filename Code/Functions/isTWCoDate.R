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
  # -- VARIABLE SET SECTION
  # -----------------------------------------------------------------------------
  # Earliest date of The Weather Company's observations - January 1931
  # Earliest date of The Weather Company's observations - January 1931
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

  if (sum(is.na(check.DF[[check.start]]) || is.na(check.DF[[check.end]]), na.rm = TRUE) > 0) {
    print("Data Frame contained rows with a either the Start or End Dates that are not a legal date")
  }
  
  if (sum(check.DF[[check.start]] > check.DF[[check.end]], na.rm = TRUE) > 0) {
    print("Data Frame contained rows with an input of Start Date must Be the Same or less than End Date")
  }
  
  if (sum(check.DF[[check.start]] < earliest.date || check.DF[[check.end]] < earliest.date, na.rm = TRUE) > 0) {
    print("Data Frame contained rows with a input of Start or End Date before weather records began (Jan 1931)")
  }
  
  if (sum(check.DF[[check.start]] > latest.date | check.DF[[check.end]] > latest.date, na.rm = TRUE) > 0) {
    print("Data Frame contained rows with a Future Date, No weather records yet")
  }
  
  check.DF <-    check.DF[!(is.na(check.DF[[check.start]]) 
                       | is.na(check.DF[[check.end]])
                       | check.DF[[check.start]] > check.DF[[check.end]]
                       | check.DF[[check.start]] < earliest.date
                       | check.DF[[check.end]] < earliest.date
                       | check.DF[[check.start]] > latest.date
                       | check.DF[[check.end]] > latest.date)
                       , ]
                       
  return(check.DF)
} ### END is.TWCoDate