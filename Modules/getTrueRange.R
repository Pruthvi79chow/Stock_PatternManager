## Calculate True range
# Max(1. high-low
#     2. abs(High - previous close) 
#     3. abs(Low - previous close)
#     )
# Stockdata<-data
getTrueRange<-function(Stockdata){
  Stockdata$trueRange=
    apply(data.table(Stockdata$high-Stockdata$low,
                                abs(Stockdata$high-shift(Stockdata$close,1,"lag")),
                                abs(Stockdata$low-shift(Stockdata$close,1,"lag"))
                                ),1,max)
  Stockdata$trueRange[1]= Stockdata$range[1]
  return(Stockdata)
}
# Stockdata<-data
# periods<-8
getAverageTrueRange<-function(Stockdata,periods){
  period<-periods-1
  Segment1<-Stockdata[1:period]
  Segment2<-Stockdata[-(1:period), ,drop = FALSE]
  for(i in 1:nrow(Segment2)){
    if(i==1){
      Segment2$ATR[i]<-round(mean(c(Segment1$trueRange,Segment2$trueRange[i])),2)
    }else{
      Segment2$ATR[i]<-round((Segment2$ATR[i-1]*period+Segment2$trueRange[i])/(periods),2)
    }
  }
  return(rbind(Segment1,Segment2,fill=T))
}
