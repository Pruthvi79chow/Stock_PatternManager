
## Function to read and update NSE folder with EOD data
updateNSEfolder <-  function(path,
                           pattern = NULL,
                           type = NULL) {
  if (is.null(pattern)) {
    pattern <- "txt"
  }
  files.list <- list.files(path = path,pattern = paste0(".", pattern),full.names = F)
  
  lastday<-max(sort(as.Date(substr(files.list, 1, 10), format="%Y-%m-%d")))
  todaysDay<-Sys.Date()
  if(todaysDay!=lastday){
    i<-todaysDay
    while (i!=lastday) {
      print(i)
      nserawdata<-GetNSEDatabyDate(i)
      if(!is.null(nserawdata)){
        nseEQ<-nserawdata[SERIES=="EQ",c("SYMBOL","TIMESTAMP","OPEN","HIGH","LOW","CLOSE","TOTTRDQTY")]
        nseEQ$TIMESTAMP<-format(i,"%Y%m%d")
        fwrite(nseEQ,file.path(path,paste0(i,"-NSE-EQ.txt")),sep=",",col.names = F,quote = F)
      }
      i=i-1
    }
  }
return(NULL)
}
