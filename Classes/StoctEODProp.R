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
                 cat(" \nLowerWix=",green(round(LowerWix(object),2)))
                 cat(" UpperWix=",green(round(UpperWix(object),2)))
                 cat(" RealBody=",green(round(RealBody(object),2)))
                 cat("\nUpperWixToRealBodyRatio=",green(round(UpperWixToRealBodyRatio(object),2)))
                 cat(" LowerWixToRealBodyRatio=",green(round(LowerWixToRealBodyRatio(object),2)))
               }else{
                 cat("OpenPrice=",red(object@open))
                 cat(" ClosePrice=",red(object@close))
                 cat(" HighPrice=",red(object@high))
                 cat(" LowPrice=",red(object@low))
                 cat(" \nLowerWix=",red(round(LowerWix(object),2)))
                 cat(" UpperWix=",red(round(UpperWix(object),2)))
                 cat(" RealBody=",red(round(RealBody(object),2)))
                 cat("\nUpperWixToRealBodyRatio=",red(round(UpperWixToRealBodyRatio(object),2)))
                 cat(" LowerWixToRealBodyRatio=",red(round(LowerWixToRealBodyRatio(object),2)))
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

setGeneric("LowerWix",
           def=function(object){standardGeneric("LowerWix")}
)

setMethod("LowerWix","StockProperties",
          function(object){
            min(object@open,object@close) - object@low
          }
)

setGeneric("UpperWix",
           def=function(object){standardGeneric("UpperWix")}
)

setMethod("UpperWix","StockProperties",
          function(object){
            object@high - max(object@open,object@close) 
          }
)

setGeneric("RealBody",
           def=function(object){standardGeneric("RealBody")}
)

setMethod("RealBody","StockProperties",
          function(object){
            if(object@open==object@close){
              0.05
            }else{
              abs(object@open-object@close)
            } 
          }
)

setGeneric("UpperWixToRealBodyRatio",
           def=function(object){standardGeneric("UpperWixToRealBodyRatio")}
)

setMethod("UpperWixToRealBodyRatio","StockProperties",
          function(object){
           UpperWix(object)/RealBody(object)
          }
)

setGeneric("LowerWixToRealBodyRatio",
           def=function(object){standardGeneric("LowerWixToRealBodyRatio")}
)

setMethod("LowerWixToRealBodyRatio","StockProperties",
          function(object){
            LowerWix(object)/RealBody(object)
          }
)

#day<-new("StockProperties",open=test$OPEN.today,close=test$CLOSE.today,high=test$HIGH.today,low=test$LOW.today,volume=test$TOTTRDQTY.today)

