Bearish_Engulfing<-function(Trade){
  if(!isS4(Trade)) return("Please check the input. It is not a Two Clandle StickClass")
  if(IsBullish(Trade@day1) & IsBearish(Trade@day2) & IsGapUp(Trade) & hasClosedBelow(Trade) & hasVolumeUp(Trade)) T else F
}


Bearish_Harami<-function(Trade){
  if(!isS4(Trade)) return("Please check the input. It is not a Two Clandle StickClass")
  if(IsBullish(Trade@day1) & IsBearish(Trade@day2) & IsGapDown(Trade) & hasClosedAbove(Trade) & hasVolumeUp(Trade) ) T else F
} 
