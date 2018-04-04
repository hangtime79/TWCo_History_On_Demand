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