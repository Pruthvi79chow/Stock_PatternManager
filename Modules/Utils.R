## Utility Function

## Function for converting string to data format
convert_strtodate <- function(data){
  dates <- as.Date(as.character(data), format='%Y%m%d')
  return(dates)
}