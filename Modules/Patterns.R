Bearish_Engulfing <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (IsBullish(Trade@day1) &
      IsBearish(Trade@day2) &
      IsGapUp(Trade) &
      hasClosedBelow(Trade))T else F
}

Bearish_Harami <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (IsBullish(Trade@day1) &
      IsBearish(Trade@day2) &
      IsGapDown(Trade) &
      hasClosedAbove(Trade))T else F
} 

Dark_Cloud <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (IsBullish(Trade@day1) &
      IsBearish(Trade@day2) &
      IsGapUp(Trade) &
      hasClosedBelow50(Trade) &
      hasClosedAbove(Trade))T else F
} 

Tweezer_Top <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (Tweezertop(Trade))T else F
} 

Bullish_Engulfing <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (IsBearish(Trade@day1) &
      IsBullish(Trade@day2) &
      IsGapDown(Trade) &
      hasClosedAbove(Trade))T else F
}

Bullish_Harami <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (IsBearish(Trade@day1) &
      IsBullish(Trade@day2) &
      IsGapUp(Trade) &
      hasClosedBelow(Trade))T else F
}

Bullish_Piercing <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (IsBearish(Trade@day1) &
      IsBullish(Trade@day2) &
      IsGapDown(Trade) &
      hasClosedAbove50(Trade) &
      hasClosedBelow(Trade))T else F
}

Tweezer_Bottom <- function(Trade) {
  if (!isS4(Trade))
    return("Please check the input. It is not a Two Clandle StickClass")
  if (Tweezerbottom(Trade))T else F
}