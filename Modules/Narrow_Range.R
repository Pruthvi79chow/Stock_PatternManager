
#
# 
# stockdata<-stock_data[Stock=="AXISBANK"] 
# NRDays<-4
# ATRDays<-8
# Debug=T
# backTesting=T

NarrowRange<-function(stockdata,NRDays,ATRDays,backTesting=F,Debug=F){
  stockdata$range<-stockdata$high-stockdata$low
  data<-stockdata[order(Date)]
  NRPeriods<-NRDays-1
  data<-cbind(data,as.data.table(shift(data$range, 1:NRPeriods, type="lag")))
  data<-data[,"isNR":=ifelse(apply(data[ ,(ncol(data)-NRPeriods):ncol(data)], MARGIN=1, min)==range,T,F)]
  if(!Debug)data<-data[,  -grep("V", colnames(data)),with=F]
  
  ## Calculate TrueRange
  data<-getTrueRange(data)
  data<-getAverageTrueRange(data,ATRDays)
  
  ## setting ATR = trueRange for NA
  data[is.na(ATR)]$ATR<-data[is.na(ATR)]$trueRange
  data[is.na(isNR)]$isNR<-FALSE
  
   
  
    if(backTesting){
      ## Define Defaults
      data$tradecall<-NA
      data$targetReached<-NA
        ## Decide trade Call and calculate achived Targers
        for(i in 1:(nrow(data)-1)){
          today<-data[i]
          nextDay<-data[i+1]
      
          if(today$isNR){
            tradecall<-if(nextDay$high>today$high) "BUY" else if(nextDay$low<today$low) "SELL" else "NOTRIGGER"
            targetReached<-if(tradecall=="BUY"){
              if(nextDay$low<today$low){
                -(today$high-nextDay$low) # Stop loss hit
              }else{
                abs(nextDay$high-today$high) # Achived Traget
              }
            }else if(tradecall=="SELL"){
              if(nextDay$high>today$high){
                nextDay$low-today$high # Stop loss hit
              }else{
                abs(nextDay$low-today$low)  # Achived Traget
              }
            }else{NA}
            data$tradecall[i]<-tradecall
            data$targetReached[i]<-targetReached
            
          }
        }
        
        ## Calculate traget ratio
         data$targetRatio<-data$targetReached*100/data$ATR
        
        ## Adding Row Index
         data$rowIndex<-1:nrow(data)
        ## Check if target is reached or not
         
        ##Stats
         calls<- data[!is.na(tradecall),.N, by = tradecall]
         hits<- data[!is.na(tradecall), c(sum(targetRatio > 0)), by = tradecall]
         fails<- data[!is.na(tradecall), c(sum(targetRatio<0)), by = tradecall]
         
         
        stats<- data.table("Stock"=data$Stock[1],"totalNRSignals"=sum(calls$N),
                    "BuySignals"=sum(calls[tradecall=="BUY"]$N),"BuyTargetReached"=hits[tradecall=="BUY"]$V1,
                    "BuysuccessPct"=round(hits[tradecall=="BUY"]$V1*100/calls[tradecall=="BUY"]$N,2),
                    "BuysuccessRatio"=round(calls[tradecall=="BUY"]$N/sum(hits[tradecall=="BUY"]$V1),2),
                    "SellSignals"=sum(calls[tradecall=="SELL"]$N),"SellTargetReached"=hits[tradecall=="SELL"]$V1,
                    "SellSuccessPct"=round(hits[tradecall=="SELL"]$V1*100/calls[tradecall=="SELL"]$N,2),
                    "SellSuccessRatio"=round(calls[tradecall=="SELL"]$N/hits[tradecall=="SELL"]$V1,2)
        )
        # hits<-data[isNR==T, hits := rleid(targetRatio),by="tradecall"]
        if(Debug)
          return(list(data,stats))
        else
          return(stats)
      }else{
        isNRday<-tail(data$isNR,1)
          if(isNRday){
           # if(!Debug) data<-data[,  -grep("V", colnames(data)),with=F]
            NRrow<-tail(data,1)
            stats<-data.table(NRrow,
                              "BuyTrigger"=NRrow$high,"BuySL"=NRrow$low,
                              "SellTrigger"=NRrow$low,"SellSL"=NRrow$high
                              )
            return(stats)
          }else{
            return(NULL)
          }
        
        
      }
   
}


