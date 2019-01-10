getTarget <- function(Trade, Pattern) {
  # test<-holdings[SYMBOL=="BAJAJ-AUTO"]
  # Trade<-new("TwoCandleSticks",
  #                 day1=new("StockProperties",open=test$OPEN.LastDay,close=test$CLOSE.LastDay,high=test$HIGH.LastDay,low=test$LOW.LastDay,volume=test$TOTTRDQTY.LastDay),
  #                 day2=new("StockProperties",open=test$OPEN.today,close=test$CLOSE.today,high=test$HIGH.today,low=test$LOW.today,volume=test$TOTTRDQTY.today)
  # )
  if (Pattern == "Bearish_Engulfing" |
      Pattern == "Bearish_Harami" |
      Pattern == "Dark_Cloud" |
      Pattern == "Tweezer_Top" |
      Pattern == "Hangingman" |
      Pattern == "Shooting_star") {
    #  Date <- Trade@day2TIMESTAMP.today
    tradeCall <- "Sell"
    Price <- Trade@day2@close
    Stoploss <- max(Trade@day2@high, Trade@day1@high)
    Target <- abs(Price - Stoploss)
    Target_1 <- Trade@day2@close - Target
    Target_2 <- Trade@day2@close - (2 * Target)
    Remarks <- "Check at Resistance only"
  }
  if(Pattern == "Bullish_Engulfing" |
     Pattern == "Bullish_Harami" |
     Pattern == "Bullish_Piercing" |
     Pattern == "Tweezer_Bottom" |
     Pattern == "Hammer"|
     Pattern == "Inverted_Hammer"){
    #  Date <- Trade@day2TIMESTAMP.today
    tradeCall <- "Buy"
    Price <- Trade@day2@close
    Stoploss <- min(Trade@day2@low,Trade@day1@low)
    Target <- abs(Price - Stoploss)
    Target_1 <- Trade@day2@close + Target
    Target_2 <- Trade@day2@close + (2*Target)
    Remarks <- "Check at Support only"
  }
  return(data.table(
    tradeCall,
    Price,
    Stoploss,
    Target,
    Target_1,
    Target_2,
    Remarks
  ))
}