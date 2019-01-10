Bearish_Engulfing <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (IsBullish(Trade@day1) &
      IsBearish(Trade@day2) &
      IsGapUp(Trade) &
      hasClosedBelow(Trade))T else F
}

Bearish_Harami <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (IsBullish(Trade@day1) &
      IsBearish(Trade@day2) &
      IsGapDown(Trade) &
      !hasClosedAbove(Trade) &
      !hasClosedBelow(Trade))T else F
} 

Dark_Cloud <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (IsBullish(Trade@day1) &
      IsBearish(Trade@day2) &
      IsGapUp(Trade) &
      hasClosedBelow50(Trade) &
      hasClosedAbove(Trade))T else F
} 

Tweezer_Top <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (Tweezertop(Trade))T else F
} 

Bullish_Engulfing <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (IsBearish(Trade@day1) &
      IsBullish(Trade@day2) &
      IsGapDown(Trade) &
      hasClosedAbove(Trade))T else F
}

Bullish_Harami <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (IsBearish(Trade@day1) &
      IsBullish(Trade@day2) &
      IsGapUp(Trade) &
      !hasClosedBelow(Trade) &
      !hasClosedAbove(Trade))T else F
}

Bullish_Piercing <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (IsBearish(Trade@day1) &
      IsBullish(Trade@day2) &
      IsGapDown(Trade) &
      hasClosedAbove50(Trade) &
      hasClosedBelow(Trade))T else F
}

Tweezer_Bottom <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (Tweezerbottom(Trade))T else F
}

Hammer <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (between(LowerWixToRealBodyRatio(Trade@day1), 2 ,3) &
      between(UpperWixToRealBodyRatio(Trade@day1), 0 ,0.2) &
      IsBullish(Trade@day2) &
      hasClosedAbove(Trade)
      )T else F
}

Hangingman <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (between(LowerWixToRealBodyRatio(Trade@day1), 2 ,3) &
      between(UpperWixToRealBodyRatio(Trade@day1), 0 ,0.2) &
      IsBearish(Trade@day2) &
      hasClosedBelow(Trade)
  )T else F
}

Inverted_Hammer <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (between(UpperWixToRealBodyRatio(Trade@day1), 2 ,3) &
      between(LowerWixToRealBodyRatio(Trade@day1), 0 ,0.2) &
      IsBullish(Trade@day2) &
      hasClosedAbove(Trade)
  )T else F
}

Shooting_star <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Candle StickClass")
  if (between(UpperWixToRealBodyRatio(Trade@day1), 2 ,3) &
      between(LowerWixToRealBodyRatio(Trade@day1), 0 ,0.2) &
      IsBearish(Trade@day2) &
      hasClosedBelow(Trade)
  )T else F
}