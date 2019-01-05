## function to read NSE data

readNSEBhavCopy.EQ<-function(path,
                          pattern = NULL,
                          type = NULL){
  if (is.null(pattern)) {
    pattern <- "txt"
  }
  files.list <-
    list.files(
      path = path,
      pattern = paste0(".", pattern),
      full.names = T
    )
  df <- rbindlist(lapply(files.list, fread , sep2 = "auto" , header = F))
  combine_df <-setNames(df,c("Stock", "Date", "open", "high", "low", "close", "volume"))
  # combine_df <- combine_df %>% filter(!grepl("(-II|-III)", Future))
  combine_df<-combine_df[,':='(Date=convert_strtodate(Date),
                               open=round(as.numeric(open),2),
                               high=round(as.numeric(high),2),
                               low=round(as.numeric(low),2),
                               close=round(as.numeric(close),2),
                               volume=as.integer(volume)
  )]
  return(combine_df)
  
}
