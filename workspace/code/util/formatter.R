# Format a desired row (row_number), starting from a column (start_column) 
# rounding values to "decimals" and adding a literal "suffix" from a data frame.
format.row <- function(df, row_number, start_column, decimals, suffix = NULL) {
  
  totalCols <- dim(df)[[2]]
  
  tmpDecimals <- as.numeric(df[row_number, start_column:totalCols],digits=decimals)
  
  tmpRow <- format(
    round( 
      tmpDecimals,digits = decimals
    ),  
    big.mark=".", 
    decimal.mark = ",",
    nsmall=decimals
  )
  
  if( ! is.null( suffix ))  {
    tmpRow <- paste(tmpRow, suffix,sep="")
  }
  
  df[row_number,start_column:totalCols] <- tmpRow
  
  return (df)
}


