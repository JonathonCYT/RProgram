library(quantmod)

apple <- getSymbols("AAPL", from="2011-12-01", to="2022-12-31", periodicity = "monthly", auto.assign = FALSE)
ffData <- read.csv("data/F-F_Research_Data_Factors.CSV", skip=3)

ffData <- ffData[ffData$X %in% 202212:201201,]
dateIndex <- as.Date(paste0(substr(ffData$X, 1, 4), "-", substr(ffData$X, 5, 6), "-", "01"))
histReturn <- monthlyReturn(apple$AAPL.Adjusted)*100
colnames(histReturn) <- "AAPL.Returns"
ffxts <- xts(ffData,dateIndex)
mergeData <- merge.xts(ffxts,apple,histReturn)
mergeData <- mergeData[-1]

mergeData$AAPL.rExcess <- mergeData$AAPL.Returns - mergeData$RF

plot.new()
plot(x=coredata(mergeData$Mkt.RF), y=coredata(mergeData$AAPL.rExcess), type="p",
      main="Market vs. Apple risk premium", xlab="Market", ylab="Apple")

print(lm(mergeData$AAPL.rExcess ~ mergeData$Mkt.RF + mergeData$SMB + mergeData$HML))

qqnorm(resid(lm(mergeData$AAPL.rExcess ~ mergeData$Mkt.RF + mergeData$SMB + mergeData$HML)))
qqline(resid(lm(mergeData$AAPL.rExcess ~ mergeData$Mkt.RF + mergeData$SMB + mergeData$HML)))

