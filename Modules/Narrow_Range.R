
#
# 
NarrowRange_1DayTraget(stockdata<-stock_data[Stock=="ICICIBANK"],
NRDays<-4,
ATRDays<-8,
Debug=T,
backTesting=F)

NarrowRange<-function(stockdata,NRDays,ATRDays,backTesting=F,Debug=F){
  require(data.table)
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
      count<-0
      sellcount<-0
        ## Decide trade Call and calculate achived Targers
        for(i in 1:(nrow(data)-2)){
          NRDay<-data[i]
          tradedDay<-data[i+1]
          carryFwdDay<-data[i+2]
      
          if(NRDay$isNR){
            tradecall<-if(tradedDay$high>NRDay$high){
              #if(tradedDay$high==tradedDay$open )"GAP UP,NOTRADE" else "BUY" 
              "BUY"
            }else if(tradedDay$low<NRDay$low){ 
             # if(tradedDay$low==tradedDay$open )"GAP DOWN,NOTRADE" else "SELL" 
              "SELL"
            }else{ 
              "NOTRIGGER"
            }
            
            targetReached<-if(tradecall=="BUY"){
              if(tradedDay$low<NRDay$low){
                -(NRDay$high-NRDay$low) # Stop loss hit
              }else{
                if(carryFwdDay$low<NRDay$low){
                  tradedDay$high-NRDay$high # CarryFwd Day Stop loss hit
                }else{
                  if(any((NRDay$high+NRDay$ATR)>c(tradedDay$high,carryFwdDay$high))){
                    NRDay$ATR
                  }else{
                    if(carryFwdDay$close<NRDay$high){
                      cat("\nCount")
                      count<-count+1
                    }else{
                      max(abs(tradedDay$high-NRDay$high),abs(carryFwdDay$high-NRDay$high)) # Achived Traget- max(traded day,carryFwd Day)
                    }
                  }
                }
              }
            }else if(tradecall=="SELL"){
              if(tradedDay$high>NRDay$high){
                cat("\nSell triggered and SL hit on 1st day",NRDay$Stock,(NRDay$low-NRDay$high))
                  NRDay$low-NRDay$high # Stop loss hit
              }else{
                if(carryFwdDay$high>NRDay$high){
                  cat("\nSell triggered and SL hit on 2nd day",NRDay$Stock,(NRDay$low-NRDay$high))
                  
                  NRDay$low-NRDay$high # CarryFwd Day Stop loss hit
                }else{
                  if(any((NRDay$low-NRDay$ATR)>c(tradedDay$low,carryFwdDay$low))){
                    NRDay$ATR
                  }else{
                    if(carryFwdDay$close<NRDay$low){
                      cat("\nSellCount")
                      sellcount<-sellcount+1
                    }else{
                      max(abs(tradedDay$low-NRDay$low),abs(carryFwdDay$close-NRDay$low))  # Achived Traget
                    }  
                  }
                }  
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
                    "BuySignals"=sum(calls[tradecall=="BUY"]$N),
                    "BuyTargetReached"=hits[tradecall=="BUY"]$V1,
                    "NonProfitableBuyCall"=count,
                    "BuysuccessPct"=round(hits[tradecall=="BUY"]$V1*100/calls[tradecall=="BUY"]$N,2),
                   # "BuysuccessRatio"=round(calls[tradecall=="BUY"]$N/sum(hits[tradecall=="BUY"]$V1),2),
                    "SellSignals"=sum(calls[tradecall=="SELL"]$N),"SellTargetReached"=hits[tradecall=="SELL"]$V1,
                    "SellSuccessPct"=round(hits[tradecall=="SELL"]$V1*100/calls[tradecall=="SELL"]$N,2),
                    "NonProfitableSellCall"=sellcount
                  #  "SellSuccessRatio"=round(calls[tradecall=="SELL"]$N/hits[tradecall=="SELL"]$V1,2)
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

NarrowRange_1DayTraget<-function(stockdata,NRDays,ATRDays,backTesting=F,Debug=F){
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
    
    buysuccessCount<-0
    buyfailedCount<-0
    
    sellsuccessCount<-0
    sellfailedCount<-0
    ## Decide trade Call and calculate achived Targers
    for(i in 2:(nrow(data)-1)){
      previousDay<-data[i-1]
      today<-data[i]
      nextDay<-data[i+1]
      
        reward = today$ATR
        risk = today$high - today$low
        RR = reward/risk
        
        harami<-if(today$high<previousDay$high & today$low>previousDay$low) T else F
      
      if(today$isNR & RR>1.6 & harami){
        tradecall<-if(nextDay$high>today$high) "BUY" else if(nextDay$low<today$low) "SELL" else "NOTRIGGER"
        targetReached<-if(tradecall=="BUY"){
          if(nextDay$low<today$low){ ##1 # Stop loss hit
            #cat("")
            buyfailedCount<-buyfailedCount+1
          }else{
            if((nextDay$high-today$high)>=today$ATR){
              buysuccessCount<-buysuccessCount+1 # Achived Traget
            }else if(nextDay$close>today$high){
              buysuccessCount<-buysuccessCount+1 # Achived Traget
            }else{
              buyfailedCount<-buyfailedCount+1
            }
          }
        }else if(tradecall=="SELL"){
          if(nextDay$high>today$high){
          #  cat("\nSell triggered and SL hit",today$Stock,(today$low-today$high))
            sellfailedCount<-sellfailedCount+1# Stop loss hit
          }else{
            if((today$low-nextDay$low)>=today$ATR){
              sellsuccessCount<-sellsuccessCount+1
            }else if(nextDay$close<today$low){
              sellsuccessCount<-sellsuccessCount+1 # Achived Traget
            }else{
              sellfailedCount<-sellfailedCount+1
            }
          }
        }else{NA}
        data$tradecall[i]<-tradecall
        data$targetReached[i]<-targetReached
        
      }
       # cat(buysuccessCount,buyfailedCount,sellfailedCount,sellsuccessCount)
    }
    
    ## Calculate traget ratio
    # data$targetRatio<-data$targetReached*100/data$ATR
    # 
    # ## Adding Row Index
    # data$rowIndex<-1:nrow(data)
    # ## Check if target is reached or not
    # 
    # ##Stats
     calls<- data[!is.na(tradecall),.N, by = tradecall]
    # hits<- data[!is.na(tradecall), c(sum(targetRatio > 0)), by = tradecall]
    # fails<- data[!is.na(tradecall), c(sum(targetRatio<0)), by = tradecall]
    
    
    stats<- data.table("Stock"=data$Stock[1],"totalNRSignals"=sum(calls$N),
                       "BuySignals"=sum(calls[tradecall=="BUY"]$N),
                       "BuySuccess"=buysuccessCount,
                       "Buyfails"=buyfailedCount,
                       "BuySuccessPct"=round(buysuccessCount*100/sum(calls[tradecall=="BUY"]$N),2),
                       "SellSignals"=sum(calls[tradecall=="SELL"]$N),
                       "SellSuccess"=sellsuccessCount,
                       "Sellfails"=sellfailedCount,
                       "BuySuccessPct"=round(sellsuccessCount*100/sum(calls[tradecall=="SELL"]$N),2)
    )
    
    if(Debug)
      return(list(data,stats))
    else
      return(stats)
  }else{
    isNRday<-tail(data$isNR,1)
    
    if(isNRday){
      NRrow<-tail(data,1)
      previousDay<-tail(data,2)[1]
      
      reward = NRrow$ATR
      risk = NRrow$high - NRrow$low
      RR = reward/risk
      
      harami<-if(NRrow$high<previousDay$high & NRrow$low>previousDay$low) T else F
      
      if(harami & RR>1.6){
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
    }else{
      return(NULL)
    }
  }
}
