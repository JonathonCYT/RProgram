library(quantmod)

funds <- c("GLD", "IAU", "AGG", "BND", "SPY",
           "VOO", "VWO", "EEM", "QQQ", "XLK",
           "VNQ", "IYR", "DIA", "IEF","VTI",
           "VXUS", "SCHF", "VIG", "VGT","SCHH",
           "IJR", "SOXX", "IVV","IJH" )
fundExample <- c("GLD", "IAU", "AGG", "BND")

getSymbols(funds, from="2012-01-01", to="2022-12-31", periodicity = "monthly", auto.assign = TRUE)
FFData <- read.csv("data/F-F_Research_Data_Factors.CSV", skip=3)
#mergeData <- merge.xts(monthlyReturn(GLD$GLD.Adjusted)*100,monthlyReturn(IAU$IAU.Adjusted)*100,
#                      monthlyReturn(AGG$AGG.Adjusted)*100,monthlyReturn(BND$BND.Adjusted)*100)
mergeData <- merge.xts(
  monthlyReturn(GLD$GLD.Adjusted)*100,  # monthly returns of GLD
  monthlyReturn(IAU$IAU.Adjusted)*100,  # monthly returns of IAU
  monthlyReturn(AGG$AGG.Adjusted)*100,  # monthly returns of AGG
  monthlyReturn(BND$BND.Adjusted)*100,  # monthly returns of BND
  monthlyReturn(SPY$SPY.Adjusted)*100,  # monthly returns of SPY
  monthlyReturn(VOO$VOO.Adjusted)*100,  # monthly returns of VOO
  monthlyReturn(VWO$VWO.Adjusted)*100,  # monthly returns of VWO
  monthlyReturn(EEM$EEM.Adjusted)*100,  # monthly returns of EEM
  monthlyReturn(QQQ$QQQ.Adjusted)*100,  # monthly returns of QQQ
  monthlyReturn(XLK$XLK.Adjusted)*100,  # monthly returns of XLK
  monthlyReturn(VNQ$VNQ.Adjusted)*100,  # monthly returns of VNQ
  monthlyReturn(IYR$IYR.Adjusted)*100,  # monthly returns of IYR
  monthlyReturn(DIA$DIA.Adjusted)*100,  # monthly returns of DIA
  monthlyReturn(IEF$IEF.Adjusted)*100,  # monthly returns of IEF
  monthlyReturn(VTI$VTI.Adjusted)*100,  # monthly returns of VTI
  monthlyReturn(VXUS$VXUS.Adjusted)*100,  # monthly returns of VXUS
  monthlyReturn(SCHF$SCHF.Adjusted)*100,  # monthly returns of SCHF
  monthlyReturn(VIG$VIG.Adjusted)*100,  # monthly returns of VIG
  monthlyReturn(VGT$VGT.Adjusted)*100,  # monthly returns of VGT
  monthlyReturn(SCHH$SCHH.Adjusted)*100,  # monthly returns of SCHH
  monthlyReturn(IJR$IJR.Adjusted)*100,  # monthly returns of IJR
  monthlyReturn(SOXX$SOXX.Adjusted)*100,  # monthly returns of SOXX
  monthlyReturn(IVV$IVV.Adjusted)*100,  # monthly returns of IVV
  monthlyReturn(IJH$IJH.Adjusted)*100 # monthly returns of IJH
)

FFData <- FFData[1:1158,]
date.index <- as.Date(paste0(substr(FFData$X, 1, 4), "-", substr(FFData$X, 5, 6), "-", "01"))
FF.xts <- xts(FFData,date.index)
FF.xts <- FF.xts["2012-01-01/2022-12-31"]
mergeData2 <- merge.xts(FF.xts,mergeData)
colnames(mergeData2) <- c('date','Mkt.RF','SMB','HML','RF',funds)

RExcessData <- mergeData2
for(x in 1:length(funds)) {
  for(u in 0:dim(RExcessData[,x])[1]) {
    RExcessData[,x+5][u] <- mergeData2[,x+5][u]-mergeData2[,5][u]
  }
}

for(x in 1:length(funds)) {
  re <- lm(RExcessData[,x+5] ~ RExcessData$Mkt.RF + RExcessData$SMB + RExcessData$HML)
  print(funds[x])
  print(re)
}

qqnorm(resid(lm(RExcessData[,6] ~ RExcessData$Mkt.RF + RExcessData$SMB + RExcessData$HML)))
qqline(resid(lm(RExcessData[,6] ~ RExcessData$Mkt.RF + RExcessData$SMB + RExcessData$HML)))
lm(RExcessData[,28] ~ RExcessData$Mkt.RF + RExcessData$SMB + RExcessData$HML)