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
            if(IsBullish(object@day1)){
              if(IsBullish(object@day2) & object@day2@open<object@day1@open) T
               else if (IsBearish(object@day2) & object@day2@close<object@day1@open) T 
                else F
            }else if(IsBearish(object@day1)){
              if(IsBullish(object@day2) & object@day2@open<object@day1@close) T
              else if (IsBearish(object@day2) & object@day2@close<object@day1@close) T 
              else F
            }else{
              F
            } 
          }
)

setGeneric("hasClosedAbove",
           def=function(object){standardGeneric("hasClosedAbove")}
)


setMethod("hasClosedAbove","TwoCandleSticks",
          function(object){
            if(IsBullish(object@day1)){
              if(IsBullish(object@day2) & object@day2@close>object@day1@close) T
              else if (IsBearish(object@day2) & object@day2@open>object@day1@close) T 
              else F
            }else if(IsBearish(object@day1)){
              if(IsBullish(object@day2) & object@day2@close>object@day1@open) T
              else if (IsBearish(object@day2) & object@day2@open>object@day1@open) T 
              else F
            }else{
              F
            } 
          }
)

setGeneric("hasClosedBelow50",
           def=function(object){standardGeneric("hasClosedBelow50")}
)


setMethod("hasClosedBelow50","TwoCandleSticks",
          function(object){
            if(object@day2@close < 
               (object@day1@open + (0.5*(object@day1@close-object@day1@open))))T else F
          }
)

setGeneric("Tweezertop",
           def=function(object){standardGeneric("Tweezertop")}
)


setMethod("Tweezertop","TwoCandleSticks",
          function(object){
            if((max(object@day1@open,object@day1@high,object@day1@close) == max(object@day2@open,object@day2@high,object@day2@close) |
                max(object@day2@open,object@day2@high,object@day2@close) == max(object@day1@open,object@day1@high,object@day1@close)))T else F
          }
)

setGeneric("hasClosedAbove50",
           def=function(object){standardGeneric("hasClosedAbove50")}
)


setMethod("hasClosedAbove50","TwoCandleSticks",
          function(object){
            if(object@day2@close > 
               (object@day1@close + (0.5*(object@day1@open-object@day1@close))))T else F
          }
)

setGeneric("Tweezerbottom",
           def=function(object){standardGeneric("Tweezerbottom")}
)


setMethod("Tweezerbottom","TwoCandleSticks",
          function(object){
            if((min(object@day1@open,object@day1@low,object@day1@close) == min(object@day2@open,object@day2@low,object@day2@close) |
                min(object@day2@open,object@day2@low,object@day2@close) == min(object@day1@open,object@day1@low,object@day1@close)))T else F
          }
)

setGeneric("lowwixlength",
           def=function(object){standardGeneric("lowwixlength")}
)

setMethod("lowwixlength","TwoCandleSticks",
          function(object){
            if((min(object@day1@open,object@day1@close) - object@day1@low) 
               > (2 * abs(object@day1@open - object@day1@close))) T else F
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


