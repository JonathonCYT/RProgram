SP500CSV <- read.csv("data/SP500.csv")
FedFund <- read.csv("data/FEDFUNDS.csv")

head(SP500CSV, 2)
SP500CSV$annualReturn <- c(NaN, (SP500CSV$SP500level[-1]-SP500CSV$SP500level[-dim(SP500CSV)[1]])/SP500CSV$SP500level[-dim(SP500CSV)[1]])

par(mar = c(5, 5, 3, 5))
plot(SP500CSV$year[-1], SP500CSV$annualReturn[-1], type="l" ,col="blue", main="SP500CSV dividend yield & annual return" ,xlab ="year", ylab ="Annual")

par(new=TRUE)
plot(SP500CSV$dividendYield[-dim(SP500CSV)[1]], type="l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 2)

par(new=TRUE)
plot(FedFund$FEDFUNDS[-dim(FedFund)[1]], type="l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "black")

axis(side = 4)
mtext("Dividend yield", side = 4, line = 3)
legend("topleft", c("annual return", "dividend yield", "FedFundRate"), col = c("blue", "red", "black"), lty = c(1, 2))

cor(SP500CSV$annualReturn[-1], SP500CSV$dividendYield[-dim(SP500CSV)[1]])
cor(SP500CSV$annualReturn[-1], SP500CSV$Peratio[-dim(SP500CSV)[1]])
cor(SP500CSV$annualReturn[-1], SP500CSV$ShillerPEratio[-dim(SP500CSV)[1]])
cor(SP500CSV$annualReturn[-1], SP500CSV$X10yearTyield[-1])