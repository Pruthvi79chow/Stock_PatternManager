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
lapply(1:nrow(holdings), function(i){
  test<-holdings[i]
  #  print(test$SYMBOL)
  twoCandles<-new("TwoCandleSticks",
                  day1=new("StockProperties",open=test$OPEN.LastDay,close=test$CLOSE.LastDay,high=test$HIGH.LastDay,low=test$LOW.LastDay,volume=test$TOTTRDQTY.LastDay),
                  day2=new("StockProperties",open=test$OPEN.today,close=test$CLOSE.today,high=test$HIGH.today,low=test$LOW.today,volume=test$TOTTRDQTY.today)
  )
  #twoCandles->Trade
  if(Bearish_Engulfing(twoCandles))
  df_out$Stock_Symbol[i] <- test$SYMBOL
  df_out$Pattern[i] <- "Bearish Engulfing"
  df_out$Date[i] <- test$TIMESTAMP.today
  df_out$Buy_sell[i] <- "Sell"
  df_out$Price[i] <- test$CLOSE.today
  df_out$Stoploss[i] <- max(test$HIGH.today,test$HIGH.LastDay)
  df_out$Target[i] <- abs(df_out$Price[i] - df_out$Stoploss[i])
  df_out$Target_1[i] <- test$CLOSE.today - (df_out$Target[i])
  df_out$Target_2[i] <- test$CLOSE.today - (2*df_out$Target[i])
  #list((paste0("Pattern Confirmed-Bearish Engulfing for ",test$SYMBOL)))
  return(df_out)
}
)




# rbindlist(lapply(1:nrow(holdings), function(i){
#   test<-holdings[i]
#   debug=F
#   if(test$CLOSE.LastDay>test$OPEN.LastDay){
#     if(debug)print("Day1 is Green")
#     if(test$OPEN.today>test$CLOSE.LastDay){
#       if(debug)  print("Day2 has a gap up opening")
#     #  if(abs(test$OPEN.today-test$CLOSE.today)>abs(test$CLOSE.LastDay-test$OPEN.LastDay)){
#       if(test$CLOSE.today<test$OPEN.LastDay){
#         if(debug) print("Day2 closed below Day1")
#         if(test$TOTTRDQTY.today>test$TOTTRDQTY.LastDay){
#           if(debug) print("Day2 Volume is higher the Day1")
#          list((paste0("Pattern Confirmed-Bearish Engulfing for ",test$SYMBOL)))
#         
#       }
#     }
#     }
#   }
# }))



