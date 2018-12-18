###

options(scipen = 15)

GetNSEData<-function(days=0){
  i<-days
  d<-1
  while(d!=0){
    Date<-toupper(format(Sys.Date()-i,"%d%b%Y"))
    month<-toupper(gsub('[[:digit:]]+', '', Date))
    year<-year(Sys.Date()-i)
    BuildLink<-paste0("https://www.nseindia.com/content/historical/EQUITIES/",year,"/",month,"/cm",Date,"bhav.csv.zip")
    print(BuildLink)
    if(!http_error(BuildLink)){
      temp <- tempfile()
      download.file(BuildLink,temp,quiet=TRUE)
      d<-0
    }
    i<-i+1
  }
  data <- as.data.table(read.csv(unz(temp, paste0("cm",Date,"bhav.csv")),stringsAsFactors = F))
  # data$Date<-Date
  unlink(temp)
  #write.csv(data,"./cm28DEC2017bhav.csv")
  return(data)
}

GetLastActiveTradeDay<-function(){
  i<-0
  d<-1
  while(d!=0){
    Date<-toupper(format(Sys.Date()-i,"%d%b%Y"))
    month<-toupper(gsub('[[:digit:]]+', '', Date))
    year<-year(Sys.Date()-i)
    BuildLink<-paste0("https://www.nseindia.com/content/historical/EQUITIES/",year,"/",month,"/cm",Date,"bhav.csv.zip")
    if(!http_error(BuildLink)){
     d<-0
    }
    i<-i+1
  }
  return(i)
}

GetBSEData<-function(days=0){
  
  i<-days
  d<-1
  while(d!=0){
    Date<-format(Sys.Date()-i,"%d%m%y")
    Date1<-toupper(format(Sys.Date()-i,"%d-%b-%Y"))
    BuildLink<-paste0("http://www.bseindia.com/download/BhavCopy/Equity/EQ",Date,"_CSV.ZIP")
    print(BuildLink)
    if(!http_error(BuildLink)){
      temp <- tempfile()
      download.file(BuildLink,temp,quiet=TRUE)
      d<-0
    }
    i<-i+1
  }
  data <- as.data.table(read.csv(unz(temp, paste0("EQ",Date,".CSV")),stringsAsFactors = F))
  unlink(temp)
  data$TIMESTAMP<-Date1
  return(data)
}

Get52WeekStats<-function(){
  url<-"https://www.nseindia.com/content/CM_52_wk_High_low.csv"
  return(fread(url,stringsAsFactors = F,showProgress=F)[SERIES=="EQ"])
}
