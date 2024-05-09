DT <- read.csv("data/DIS.csv")
FedFund <- read.csv("data/FEDFUNDS.csv")
DT[0,]
head(DT$Date,5)
dt1 <- DT$Date[1]
date1 <- as.Date(dt1, format = "%Y-%m-%d")
dim(DT)
price.t1 <- DT$Adj.Close[-dim(DT)[1]]
price.t2 <- DT$Adj.Close[-1]
daily.return <- (price.t2-price.t1)/price.t1

dim(DT)[1]
length(daily.return)
DT$daily.return <- c(NaN, daily.return)
DT$cumulative.return <- rep( NaN , dim(DT)[1])

for (i in 2:dim(DT)[1]) {
 DT$cumulative.return[i] <- prod(1+DT$daily.return[2:i])-1
}

plot(DT$cumulative.return[-1], main="Cumulative Return", xlab ="date", ylab = "cumulative")

#plot(DT$date.format[-1], DT$cumulative.return[-1], main="Cumulative Return", xlab ="date", ylab = "cumulative")

sp500 <- read.csv("data/SP500.csv")
head(sp500, 2)
sp500$annualReturn <- c(NaN, (sp500$SP500level[-1]-sp500$SP500level[-dim(sp500)[1]])/sp500$SP500level[-dim(sp500)[1]])

par(mar = c(5, 5, 3, 5))
plot(sp500$year[-1], sp500$annualReturn[-1], type="l" ,col="blue", main="SP500 dividend yield & annual return" ,xlab ="year", ylab ="Annual")

par(new=TRUE)
plot(sp500$dividendYield[-dim(sp500)[1]], type="l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)

par(new=TRUE)
plot(FedFund$FEDFUNDS[-dim(FedFund)[1]], type="l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "black")

axis(side = 4)
mtext("Dividend yield", side = 4, line = 3)
legend("topleft", c("annual return", "dividend yield"), col = c("blue", "red"), lty = c(1, 2))

cor(sp500$annualReturn[-1], sp500$dividendYield[-dim(sp500)[1]])
cor(sp500$annualReturn[-1], sp500$Peratio[-dim(sp500)[1]])
cor(sp500$annualReturn[-1], sp500$ShillerPEratio[-dim(sp500)[1]])
cor(sp500$annualReturn[-1], sp500$X10yearTyield[-1])

save(sp500, file = "data/SP500.RData")

getSymbols.yahoo("AAPL",env = globalenv())
getSymbols.yahoo(c("AAPL","MSFT","AMZN","NVDA","GOGL","BRK-B","GOOG","TSLA","UNH","META"),env = globalenv())
adjustedClose <- merge.xts(AAPL$AAPL.Adjusted,MSFT$MSFT.Adjusted,
                           AMZN$AMZN.Adjusted,NVDA$NVDA.Adjusted,
                           GOGL$GOGL.Adjusted,`BRK-B`$`BRK-B.Adjusted`,
                           GOOG$GOOG.Adjusted,TSLA$TSLA.Adjusted,
                           UNH$UNH.Adjusted,META$META.Adjusted)

plot(adjustedClose, subset = "2012::2022", legend.loc = "topleft")
plot(adjustedClose, subset = "2019::2022", legend.loc = "topleft")

load(file = "data/SP500.RData")
sp500$port <- 0.6*sp500$annualReturn + 0.4*sp500$X10yearTyield
sp500$indicator <- sp500$dividendYield > 0.03
sp500$strat <- rep(0, dim(sp500)[1])
for (i in 1:(dim(sp500)[1]-1)) {
 if (sp500$dividendYield[i] > 0.03) {
  sp500$strat[i+1] <- 0.8*sp500$annualReturn[i+1]+0.2*sp500$X10yearTyield[i+1]
 } else {
  sp500$strat[i+1] <- 0.6*sp500$annualReturn[i+1]+0.4*sp500$X10yearTyield[i+1]
 }
}
summary(sp500[,c("port","strat")])

sp500$port.cr <- rep(NaN, dim(sp500)[1])
sp500$strat.cr <- rep(NaN , dim(sp500)[1])

for (i in 2:dim(sp500)[1]) {
 sp500$port.cr[i] <- prod(1+sp500$port[2:i])-1
 sp500$strat.cr[i] <- prod(1+sp500$strat[2:i])-1
}

plot(sp500$year[-1], sp500$port.cr[-1], type="l", col ="blue", main="Cumulative Return", xlab ="date", ylab = "cumulative")
lines(sp500$year[-1], sp500$strat.cr[-1], type="l", col="red")

# Lesson 18/04/20223
ff_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"

temp_file <- tempfile()
download.file(ff_url,temp_file)

raw_data <- unzip(temp_file)
raw_data <- read.csv(raw_data, skip=3)

raw_data <- read.csv("data/F-F_Research_Data_Factors.CSV", skip=3)

head(raw_data)
tail(raw_data)
dim(raw_data)

FF <- raw_data[1:1122,]
colnames(FF)
colnames(FF)[1] <- "YearMonth"
is(FF$YearMonth)

date.index <- as.Date(paste0(substr(FF$YearMonth, 1, 4), "-", substr(FF$YearMonth, 5, 6), "-", "01"))
FF.xts <- xts(FF,date.index)

tickers <- c("IVV","IDEV","IUSB","IEMG","IAGG","IJH","IJR")
getSymbols(tickers, from="2015-01-01", to="2019-12-31",periodicity="daily")

get.adPrices <- function(x){Ad(get(x))}
AdClosePrices <- do.call(merge, lapply(tickers, get.adPrices))
head(AdClosePrices)

# class 20/4/2023
get.adReturns <- function(x){dailyReturn(Ad(get(x)))}
AdCloseReturns <- do.call(merge, lapply(tickers, get.adReturns))
colnames(AdCloseReturns) <- tickers
head(AdCloseReturns)

is.na(AdCloseReturns)
hist.return <- AdCloseReturns[rowSums(is.na(AdCloseReturns))==0,]
hist.return.ts <- as.timeSeries(hist.return)

ef <- portfolioFrontier(hist.return.ts, constraints ="LongOnly")

par(mfrow = c(1,1))
plot(ef,1)

return.risk.frontier <- frontierPoints(ef)
colnames(return.risk.frontier)
annualized.return.risk.frontier <- data.frame(target.risk = return.risk.frontier[,"targetRisk"]*sqrt(250),
                                              target.return = return.risk.frontier[,"targetReturn"]*250)
par(mfrow =c(1,1))
plot(annualized.return.risk.frontier, xlab ="Risk", ylab ="Return", main="Annualized Return")

riskfree <- rep(0,dim(annualized.return.risk.frontier)[1])
annualized.sharpe <- (annualized.return.risk.frontier$target.return-riskfree)/
                      annualized.return.risk.frontier$target.risk

max.sharpe.return.risk <- annualized.return.risk.frontier[annualized.sharpe==max(annualized.sharpe),]

plot(annualized.return.risk.frontier$target.risk , annualized.sharpe,xlab="Risk", ylab="Sharpe ratio", main="Sharpe ratio")
points(max.sharpe.return.risk[,1], max(annualized.sharpe), col="red")

weightsPlot(ef)
weightedReturnsPlot(ef)

write.csv(annualized.return.risk.frontier, file="output/frontier.csv")

getSymbols.yahoo("DIS", env=globalenv())
DIS <- read.csv("data/AAPL.csv")
as.Date(DIS$Date, format="%Y-%m-%d")
mean(DIS$Adj.Close[DIS$Date <= "2010-12-01" & DIS$Date > "2010-03-01"])
DIS$dateformat <- as.Date(DIS$Date, format="%Y-%m-%d")
DT <- DIS[DIS$dateformat >= "2010-01-01" & DIS$dateformat <= "2010-12-01",]
price.t1 <- DT$Adj.Close[-dim(DT)[1]]
price.t2 <- DT$Adj.Close[-1]
daily.return <- (price.t2-price.t1)/price.t1
DT$daily.return <- c(NaN, daily.return)
DT$cumulative.return <- rep( NaN , dim(DT)[1])
for (i in 2:dim(DT)[1]) {
 DT$cumulative.return[i] <- prod(1+DT$daily.return[2:i])-1
}

DT$bool <- DT$Volume >= 10000000000
SP500 <- read.csv("data/SP500.csv")
par(mar = c(5, 5, 3, 5))
plot(SP500$year[-1], SP500$annualReturn[-1], type="l" ,col="blue", main="SP500 peratio & annual return" ,xlab ="year", ylab ="Annual & Peratio")

par(new=TRUE)
plot(SP500$Peratio[-dim(SP500)[1]], type="l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)
legend("topleft", c("annual return", "peratio"), col = c("blue", "red"), lty = c(1, 2))
result = cor(SP500$Peratio,SP500$dividendYield)
print(result)