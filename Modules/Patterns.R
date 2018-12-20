Bearish_Engulfing<-function(Trade){
  if(!isS4(Trade)) return("Please check the input. It is not a Two Clandle StickClass")
  if(IsBullish(Trade@day1) & IsGapUp(Trade) & hasClosedBelow(Trade) & hasVolumeUp(Trade)) T else F
}
