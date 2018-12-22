### Code to Find patterns in Stock


require(data.table)
require(crayon)
library(httr)


lapply(list.files("./Modules/",full.names = T), source,echo=F)
lapply(list.files("./Classes/",full.names = T), source,echo=F)

StockMix<-fread(file.path("./MetaData/Master_List_Equity.csv"),stringsAsFactors = F,na.strings=c(""," ","NA"))
StockMix<-StockMix[!is.na(Category)]


NSE<-GetNSEData()
NSE$Exchange<-"NSE"
NSE<-NSE[(SERIES=="EQ" | SYMBOL %in% c("GAYAPROJ","GTLINFRA")),
                                 c("SYMBOL","SERIES","OPEN","HIGH","LOW","CLOSE","LAST","PREVCLOSE","TOTTRDQTY","TIMESTAMP")][,-c("SERIES")]
GetLastActiveTradeDay()
NSE_PreviousDay<-GetNSEData(GetLastActiveTradeDay())
NSE_PreviousDay$Exchange<-"NSE"
NSE_PreviousDay<-NSE_PreviousDay[(SERIES=="EQ" | SYMBOL %in% c("GAYAPROJ","GTLINFRA")),
                                 c("SYMBOL","SERIES","OPEN","HIGH","LOW","CLOSE","LAST","PREVCLOSE","TOTTRDQTY","TIMESTAMP")][,-c("SERIES")]

comb_data<-merge(NSE,NSE_PreviousDay,by.x=c("SYMBOL"),by.y=c("SYMBOL"),all=F,suffixes = c(".today",".LastDay"))

StockMix<-fread(file.path("./MetaData/Master_List_Equity.csv"),stringsAsFactors = F,na.strings=c(""," ","NA"))
StockMix<-StockMix[!is.na(Category)]
holdings<-merge(comb_data,StockMix,by.x=c("SYMBOL"),by.y=c("SYMBOL"),all=F)

# test<-comb_data[SYMBOL=="YESBANK"]
# data<-merge(holdings,holdings_PDay,by="SYMBOL")
# 


nrow <- 50
df_out <- data.table(
  "Stock_Symbol" = as.character(),
  "Pattern" = as.character(),
  "Date" = as.character(),
  "Buy_sell" = as.numeric(),
  "Price" = as.numeric(),
  "Stoploss" = as.numeric(),
  "Target" = as.numeric(),
  "Target_1" = as.numeric(),
  "Target_2" = as.numeric(),
  "Remarks" = as.numeric(),
  stringsAsFactors = FALSE
)[1:nrow]

##Bearish Engulfing
rbindlist(lapply(1:nrow(holdings), function(i){
  test<-holdings[i]
  #  print(test$SYMBOL)
  twoCandles<-new("TwoCandleSticks",
                  day1=new("StockProperties",open=test$OPEN.LastDay,close=test$CLOSE.LastDay,high=test$HIGH.LastDay,low=test$LOW.LastDay,volume=test$TOTTRDQTY.LastDay),
                  day2=new("StockProperties",open=test$OPEN.today,close=test$CLOSE.today,high=test$HIGH.today,low=test$LOW.today,volume=test$TOTTRDQTY.today)
  )
  #twoCandles->Trade
   if(Bearish_Engulfing(twoCandles)) {
    target<- getTarget(twoCandles,"Bearish_Engulfing")
    return(data.table(cbind("stock"=test$SYMBOL,"tradeDate"=test$TIMESTAMP.today,"pattern"="Bearish_Engulfing",target)))
   }else if(Bearish_Harami(twoCandles)){
     target<- getTarget(twoCandles,"Bearish_Harami")
     return(data.table(cbind("stock"=test$SYMBOL,"tradeDate"=test$TIMESTAMP.today,"pattern"="Bearish_Harami",target)))
  }
}))




