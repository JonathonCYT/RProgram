# library("arules")
# data(Groceries,package = "arules")
#
# inspect(head(Groceries))
# # Inspect the transactions with only 1 item
# inspect (Groceries [size(Groceries) == 1])
# # Plot the frequency of the basket size
# hist (size(Groceries))
# itemFrequencyPlot (Groceries, topN=50, type='relative')
# rules <- apriori(Groceries, parameter=list(supp=0.001, conf=0.8))
# inspect(rules[1:5])
#
# # Inspect any rules related to yogurt
# inspect(subset(rules, items %in% "yogurt"))

# Final Exam
library(rfm)
DT <- read.csv("data/UKGift.csv")

DT[, "Date"] <- as.Date(as.character(DT[, "Date"]),"%m/%d/%Y")
DT[, "Amount"] <- DT$UnitPrice*DT$Quantity
#clean data
DT <- subset(DT, Cancel == 0)

History_endDate <-  as.Date("2011-09-09")
Outcome_endDate <-  as.Date("2011-12-09")

HistDT <- DT[DT$Date <= History_endDate, ]
OutcomeDT <- DT[DT$Date > History_endDate, ]

Hist_RFM <- rfm_table_order(data = HistDT,
                           customer_id = CustomerID,
                           order_date = Date,
                           revenue = Amount,
                           analysis_date = History_endDate)

rfm_heatmap(Hist_RFM)

RFMDT <- Hist_RFM$rfm
OutcomeID <-OutcomeDT$CustomerID[!duplicated(OutcomeDT$CustomerID)]

RFMDT$Buy = ifelse(RFMDT$customer_id %in% OutcomeID, 1, 0)

library(caret)
set.seed(42)

RFMDT$Buy = as.factor(RFMDT$Buy)
inTrain = createDataPartition(RFMDT$Buy, p = 2/3, list = FALSE)
inTrain = as.vector(inTrain)
Train = RFMDT[inTrain, ]
Test = RFMDT[-inTrain, ]

LogitReg = glm(Buy~recency_days + transaction_count + amount,
            family=quasibinomial(link='logit'),
            data=Train)
summary(LogitReg)

BuyProb <- predict(LogitReg, Test, type = "response")

# If p exceeds threshold of 0.5, 1 else 0
PredBuy <- ifelse(BuyProb > 0.5, 1, 0)

# Convert to factor: p_class
PredBuy <- factor(PredBuy, levels = levels(Test$Buy))

# Create confusion matrix
LogitConfuse = confusionMatrix(PredBuy, as.factor(Test$Buy), positive = "1")
LogitConfuse

TN<-292
FN<-164
FP<-191
TP<-475
Precision = TP/(TP+FP)
print(Precision*100)
Recall = TP/(TP+FN)
print(Recall*100)

BuyProb <- predict(LogitReg, Test, type = "response")
PredBuy <- ifelse(BuyProb > 0.2, 1, 0)
PredBuy <- factor(PredBuy, levels = levels(Test$Buy))
LogitConfuse = confusionMatrix(PredBuy, as.factor(Test$Buy), positive = "1")
LogitConfuse

BuyProb <- predict(LogitReg, Test, type = "response")
PredBuy <- ifelse(BuyProb > 0.8, 1, 0)
PredBuy <- factor(PredBuy, levels = levels(Test$Buy))
LogitConfuse = confusionMatrix(PredBuy, as.factor(Test$Buy), positive = "1")
LogitConfuse

print()
print(639/(639+483)*200)
print(475/(475+191)*200)
print(145/(23+145)*200)