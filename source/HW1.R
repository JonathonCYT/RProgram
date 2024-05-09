sp500stock <- c("AAPL","MSFT","AMZN","NVDA","GOGL","BRKB","GOOG","TSLA","UNH","META")
AAPL <- read.csv("data/AAPL.csv")
MSFT <- read.csv("data/MSFT.csv")
AMZN <- read.csv("data/AMZN.csv")
NVDA <- read.csv("data/NVDA.csv")
GOGL <- read.csv("data/GOOGL.csv")
BRKB <- read.csv("data/BRK-B.csv")
GOOG <- read.csv("data/GOOG.csv")
TSLA <- read.csv("data/TSLA.csv")
UNH <- read.csv("data/UNH.csv")
META <- read.csv("data/META.csv")
stocklist <-list(AAPL,MSFT,AMZN,NVDA,GOGL,BRKB,GOOG,TSLA,UNH,META)

stockGraph <- function(obj,num){
  price1 <- obj$Adj.Close[-dim(obj)[1]]
  price2 <- obj$Adj.Close[-1]
  daily <- (price2-price1)/price1

  obj$daily <- c(NaN, daily)
  obj$cumulative <- rep(NaN, dim(obj)[1])

  for(i in 2:length(obj$Adj.Close)) {
    obj$cumulative[i] <- prod(1+obj$daily[2:i])-1
  }

  obj$dateformat <- as.Date(obj$Date, "%Y-%m-%d")
  plot(obj$cumulative[-1] ~ obj$dateformat[-1], main=paste("Cumulative Return of ",sp500stock[num]), xlab="date", ylab = "cumulative", type = "l")
  #axis(1, obj$dateformat, format(obj$dateformat, "%b %d"), cex.axis = .7)
}

stock2019 <- function(obj,num){
  obj$dateformat <- as.Date(obj$Date, format="%Y-%m-%d")
  obj <- obj[obj$dateformat >= "2019-01-01",]

  stockGraph(obj,num)
}

pdf(file="output/stock.pdf")
par(mfrow = c(2,2))

stockGraph(AAPL,1)
stock2019(AAPL,1)

stockGraph(MSFT,2)
stock2019(MSFT,2)

stockGraph(AMZN,3)
stock2019(AMZN,3)

stockGraph(NVDA,4)
stock2019(NVDA,4)

stockGraph(GOGL,5)
stock2019(GOGL,5)

stockGraph(BRKB,6)
stock2019(BRKB,6)

stockGraph(GOOG,7)
stock2019(GOOG,7)

stockGraph(TSLA,8)
stock2019(TSLA,8)

stockGraph(UNH,9)
stock2019(UNH,9)

stockGraph(META,10)
stock2019(META,10)

plot(NA, xlim=c(0,10), ylim=c(0,10), bty='n',xaxt='n', yaxt='n', xlab='', ylab='')
par(mfrow = c(1,1))
a <- "From the plot we can see that from 2012 to 2022,"
b <- "TSLA has highest cumulative returns with more than 200"
c <- "Lowest is BRKB with only around 3"
d <- "Then from the plot we can see that from 2019 to 2022,"
e <- "TSLA has highest cumulative returns with more than 15"
f <- "Lowest is BRKB with only around 0.6"
text(1,6,a, pos=4)
text(1,5,b, pos=4)
text(1,4,c, pos=4)
text(1,3,d, pos=4)
text(1,2,e, pos=4)
text(1,1,f, pos=4)
points(rep(1,6),1:6, pch=15)

dev.off()