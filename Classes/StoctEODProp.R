### Stock Class
## Slots - OPEN,CLOSE,HIGH,LOW.
## Methods - Isbullish,IsBearish
###

setClass("StockProperties",
         representation(open ="numeric",
                        close="numeric",
                        high="numeric",
                        low="numeric",
                        volume="integer"))

setMethod("show", "StockProperties",
           function(object){
             if(object@open>object@close){
                 cat("OpenPrice=",green(object@open))
                 cat(" ClosePrice=",green(object@close))
                 cat(" HighPrice=",green(object@high))
                 cat(" LowPrice=",green(object@low))
               }else{
                 cat("OpenPrice=",red(object@open))
                 cat(" ClosePrice=",red(object@close))
                 cat(" HighPrice=",red(object@high))
                 cat(" LowPrice=",red(object@low))
             }
           }
          )

setGeneric("IsBullish",
            def=function(object){standardGeneric("IsBullish")}
          )


setMethod("IsBullish","StockProperties",
          function(object){
            if(object@close>object@open)T else F
          }
        )

setGeneric("IsBearish",
           def=function(object){standardGeneric("IsBearish")}
)

setMethod("IsBearish","StockProperties",
          function(object){
           if(object@close>object@open)F else T
            }
        )

#day<-new("StockProperties",open=test$OPEN.today,close=test$CLOSE.today,high=test$HIGH.today,low=test$LOW.today,volume=test$TOTTRDQTY.today)

