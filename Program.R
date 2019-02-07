### Code to Find patterns in Stock

PackageList<-c("data.table","crayon","httr","parallel")
# Install CRAN packages (if not already installed)
.inst <- PackageList %in% installed.packages()
if(length(PackageList[!.inst]) > 0) install.packages(PackageList[!.inst])

# Load packages into session 
sapply(PackageList, require, character.only=TRUE)

lapply(list.files("./Modules/",full.names = T), source,echo=F)
lapply(list.files("./Classes/",full.names = T), source,echo=F)


NSEEODpath <- "./MetaData/NSE/"
stock_data <- readNSEBhavCopy.EQ(NSEEODpath)

## Pattern finder
todaysCall<-getPatternList()

FNOtrade <- todaysCall[stock %in% FNOlist]

FNOlist<-c("TataSteel", "Tatamotors", "SBIN", "ITC", "ICICIBANK", "AXISBANK", "HDFCBANK", "YESBANK", "INFY", "PCJEWELLER", "DHFL", 
           "RELIANCE", "ONGC", "DABUR", "HINDUNILVR", "VEDL", "HINDALCO", "BHARTIARTL", "ADANIENT", "SUNPHARMA", "MARUTI", "BANKBARODA")


completeFNOlist<-c("BANKNIFTY","NIFTY","NIFTYIT","NIFTYMID50","ACC","ADANIENT","ADANIPORTS","ADANIPOWER","AJANTPHARM","ALBK","AMARAJABAT","AMBUJACEM","APOLLOHOSP",
             "APOLLOTYRE","ARVIND","ASHOKLEY","ASIANPAINT","AUROPHARMA","AXISBANK","BAJAJ-AUTO","BAJAJFINSV","BAJFINANCE","BALKRISIND","BANKBARODA","BANKINDIA",
             "BATAINDIA","BEL","BEML","BERGEPAINT","BHARATFIN","BHARATFORG","BHARTIARTL","BHEL","BIOCON","BOSCHLTD","BPCL","BRITANNIA","CADILAHC","CANBK",
             "CANFINHOME","CAPF","CASTROLIND","CEATLTD","CENTURYTEX","CESC","CGPOWER","CHENNPETRO","CHOLAFIN","CIPLA","COALINDIA","COLPAL","CONCOR","CUMMINSIND",
             "DABUR","DCBBANK","DHFL","DISHTV","DIVISLAB","DLF","DRREDDY","EICHERMOT","ENGINERSIN","EQUITAS","ESCORTS","EXIDEIND","FEDERALBNK","GAIL","GLENMARK",
             "GMRINFRA","GODFRYPHLP","GODREJCP","GODREJIND","GRASIM","GSFC","HAVELLS","HCLTECH","HDFC","HDFCBANK","HEROMOTOCO","HEXAWARE","HINDALCO","HINDPETRO",
             "HINDUNILVR","HINDZINC","IBULHSGFIN","ICICIBANK","ICICIPRULI","IDBI","IDEA","IDFC","IDFCBANK","IFCI","IGL","INDIACEM","INDIANB","INDIGO","INDUSINDBK",
             "INFIBEAM","INFRATEL","INFY","IOC","IRB","ITC","JETAIRWAYS","JINDALSTEL","JISLJALEQS","JPASSOCIAT","JSWSTEEL","JUBLFOOD","JUSTDIAL","KAJARIACER",
             "KOTAKBANK","KPIT","KSCL","KTKBANK","L&TFH","LICHSGFIN","LT","LUPIN","M&M","M&MFIN","MANAPPURAM","MARICO","MARUTI","MCDOWELL","MCX","MFSL","MGL",
             "MINDTREE","MOTHERSUMI","MRF","MRPL","MUTHOOTFIN","NATIONALUM","NBCC","NCC","NESTLEIND","NHPC","NIITTECH","NMDC","NTPC","OFSS","OIL","ONGC","ORIENTBANK",
             "PAGEIND","PCJEWELLER","PEL","PETRONET","PFC","PIDILITIND","PNB","POWERGRID","PTC","PVR","RAMCOCEM","RAYMOND","RBLBANK","RCOM","RECLTD","RELCAPITAL",
             "RELIANCE","RELINFRA","REPCOHOME","RPOWER","SAIL","SBIN","SHREECEM","SIEMENS","SOUTHBANK","SREINFRA","SRF","SRTRANSFIN","STAR","SUNPHARMA","SUNTV",
             "SUZLON","SYNDIBANK","TATACHEM","TATACOMM","TATAELXSI","TATAGLOBAL","TATAMOTORS","TATAMTRDVR","TATAPOWER","TATASTEEL","TCS","TECHM","TITAN",
             "TORNTPHARM","TORNTPOWER","TV18BRDCST","TVSMOTOR","UBL","UJJIVAN","ULTRACEMCO","UNIONBANK","UPL","VEDL","VGUARD","VOLTAS","WIPRO","WOCKPHARMA",
             "YESBANK","ZEEL")

scripts<-sort(toupper(completeFNOlist))
uniqueStockdata<-unique(stock_data$Stock)
numWorkers=detectCores(all.tests = T, logical = T)
cl<-makeCluster(getOption("cl.cores", numWorkers-1))
clusterExport(cl=cl, varlist=c("uniqueStockdata","NarrowRange","stock_data","getTrueRange","getAverageTrueRange"), envir=environment())
i<-"AXISBANK"
NR_wOtherPatterns<-rbindlist(parLapply(cl,scripts, function(i){
  
  if(i %in% uniqueStockdata){
   scriptData<-stock_data[Stock==i]
    NRdata<-NarrowRange(scriptData,4,8,backTesting = T,T)[[1]]
    
    patterns<-rbindlist(lapply(2:(nrow(scriptData)), function(j){
      day1<-scriptData[j-1]
      day2<-scriptData[j]
      twoCandles<-new("TwoCandleSticks",
                      day1=new("StockProperties",open=day1$open,close=day1$close,high=day1$high,low=day1$low,volume=day1$volume),
                      day2=new("StockProperties",open=day2$open,close=day2$close,high=day2$high,low=day2$low,volume=day2$volume)
      )
      data.table("Stock"=day2$Stock,
                 "Date"=day2$Date,
                 "Bearish_Engulfing"  =Bearish_Engulfing(twoCandles),
                 "Bearish_Harami"     =Bearish_Harami(twoCandles),
                 "Bullish_Engulfing"  =Bullish_Engulfing(twoCandles),
                 "Bullish_Harami"     =Bullish_Harami(twoCandles),
                 "Bullish_Piercing"   =Bullish_Piercing(twoCandles),
                 "Dark_Cloud"         =Dark_Cloud(twoCandles),
                 "Tweezer_Bottom"     =Tweezer_Bottom(twoCandles),
                 "Tweezer_Top"        =Tweezer_Top(twoCandles)
                 )
    }))
    
    combinedData<-merge(NRdata,patterns,by=c("Stock","Date"),all=F)
    
  }else{
    return(NULL)
  }
}))
stopCluster(cl)

updateNSEfolder("./MetaData/NSE/")
numWorkers=detectCores(all.tests = T, logical = T)
cl<-makeCluster(getOption("cl.cores", numWorkers-1))
clusterExport(cl=cl, varlist=c("uniqueStockdata","NarrowRange_1DayTraget","stock_data","getTrueRange","getAverageTrueRange"), envir=environment())
NR_BackTesting<-rbindlist(parLapply(cl,scripts, function(i){
  require(data.table)
  if(i %in% uniqueStockdata){
    scriptData<-stock_data[Stock==i]
    NRdata<-NarrowRange_1DayTraget(scriptData,4,8,backTesting = F,F)
  }else{
    return(NULL)
  }
}))
stopCluster(cl)

barplot(dd[,-c("successRatio")],beside = TRUE)

fwrite(combinedData,"./NR4_axis_wPatterns.csv")
fwrite(NR_BackTesting,"./NR4_Backtesting_till_07_02_2019.csv")

scripts[scripts %in% unique(stock_data$Stock)]



