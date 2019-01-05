### Code to Find patterns in Stock

PackageList<-c("data.table","crayon","httr")
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

FNOlist<-c("TataSteel", "Tatamotors", "SBIN", "ITC", "ICICIBANK", "AXISBANK", "HDFCBANK", "YESBANK", "INFY", "PCJEWELLER", "DHFL", 
           "RELIANCE", "ONGC", "DABUR", "HINDUNILVR", "VEDL", "HINDALCO", "BHARTIARTL", "ADANIENT", "SUNPHARMA", "MARUTI")


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



scripts<-sort(toupper(FNOlist))
uniqueStockdata<-unique(stock_data$Stock)
dd<-rbindlist(lapply(scripts, function(i){
  if(i %in% uniqueStockdata){
    print(i)
    return(NarrowRange(stock_data[Stock==i],4,8,backTesting = T,F))
  }else{
    return(NULL)
  }
}))

barplot(dd[,-c("successRatio")],beside = TRUE)

fwrite(data,"./NR4_axis_Calc.csv")
fwrite(dd,"./tommorowNR7_calls_FNOList_31122018.csv")

scripts[scripts %in% unique(stock_data$Stock)]



