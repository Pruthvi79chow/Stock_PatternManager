setClass("TwoCandleSticks",
         representation(day1 ="StockProperties",
                        day2="StockProperties"),contains = "StockProperties"
         )

setMethod("show", "TwoCandleSticks",
          function(object){
            if(object@day1@open>object@day1@close){
              cat("OpenPrice=",green(object@day1@open))
            }else{
              cat("OpenPrice=",red(object@day2@open))
            }
          }
)


setGeneric("IsGapUp",
           def=function(object){standardGeneric("IsGapUp")}
)


setMethod("IsGapUp","TwoCandleSticks",
          function(object){
            if(object@day1@close<object@day2@open)T else F
          }
)

setGeneric("IsGapDown",
           def=function(object){standardGeneric("IsGapDown")}
)


setMethod("IsGapDown","TwoCandleSticks",
          function(object){
            if(object@day1@close>object@day2@open)T else F
          }
)


setGeneric("hasVolumeUp",
           def=function(object){standardGeneric("hasVolumeUp")}
)


setMethod("hasVolumeUp","TwoCandleSticks",
          function(object){
            if(object@day2@volume>object@day1@volume)T else F
          }
)

setGeneric("hasVolumeDown",
           def=function(object){standardGeneric("hasVolumeDown")}
)


setMethod("hasVolumeDown","TwoCandleSticks",
          function(object){
            if(object@day2@volume<object@day1@volume)T else F
          }
)

setGeneric("hasClosedBelow",
           def=function(object){standardGeneric("hasClosedBelow")}
)


setMethod("hasClosedBelow","TwoCandleSticks",
          function(object){
            if(object@day2@close<object@day1@open)T else F
          }
)

setGeneric("hasClosedAbove",
           def=function(object){standardGeneric("hasClosedAbove")}
)


setMethod("hasClosedAbove","TwoCandleSticks",
          function(object){
            if(object@day2@close>object@day1@open)T else F
          }
)



# Double<-new("TwoCandleSticks",
#             day1=new("StockProperties",open=test$OPEN.LastDay,close=test$CLOSE.LastDay,high=test$HIGH.LastDay,low=test$LOW.LastDay,volume=test$TOTTRDQTY.LastDay),
#             day2=new("StockProperties",open=test$OPEN.today,close=test$CLOSE.today,high=test$HIGH.today,low=test$LOW.today,volume=test$TOTTRDQTY.today)
# )
# 
# 
# 
# IsGapUp(Double)
# IsGapDown(Double)
# hasVolumeUp(Double)
# hasVolumeDown(Double)
# hasClosedAbove(Double)
# hasClosedBelow(Double)


