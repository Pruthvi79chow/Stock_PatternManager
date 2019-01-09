# stockdata<-stock_data[Stock=="AXISBANK"]
# TargetDays<-8
# Debug=T

BullishHarami.BackTest<-function(stockdata,TargetDays,Debug=F){
  
  data<-stockdata[order(Date)]
  patterns<-rbindlist(lapply(2:(nrow(data)), function(j){  
    day1<-data[j-1]
    day2<-data[j]
    twoCandles<-new("TwoCandleSticks",
                    day1=new("StockProperties",open=day1$open,close=day1$close,high=day1$high,low=day1$low,volume=day1$volume),
                    day2=new("StockProperties",open=day2$open,close=day2$close,high=day2$high,low=day2$low,volume=day2$volume)
    )
    return(list(Bullish_Harami(twoCandles)))
  }))
  data<-cbind(data,"isPatternDay"=unlist(c(NA,t(patterns)))) ## NA to keep in sync with data count
  
}