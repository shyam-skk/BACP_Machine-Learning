setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project - 4/code")
getwd()

mydata<-read.csv("PL_XSELL.csv",header = TRUE)
str(mydata)


############# spliting the data set

###install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(mydata$TARGET, SplitRatio = 0.70)
RFDF.dev = subset(mydata, split == TRUE)
RFDF.holdout = subset(mydata, split == FALSE)


#install.packages("randomForest")
library(randomForest)
RF <- randomForest(as.factor(TARGET) ~ ., data = RFDF.dev[,-c(1,11)], 
                   ntree=501, mtry = 7, nodesize = 140,
                   importance=TRUE)
print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")


RF$err.rate

impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

tRF <- tuneRF(x = RFDF.dev[,-c(1,2,11)], 
              y=as.factor(RFDF.dev$TARGET),
              mtryStart = 7, 
              ntreeTry=21, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace = TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 140, 
              importance= TRUE
)

tRF$importance
#View(RFDF.dev)


RFDF.dev$predict.class <- predict(tRF, RFDF.dev, type="class")
RFDF.dev$predict.score <- predict(tRF, RFDF.dev, type="prob")
head(RFDF.dev)
class(RFDF.dev$predict.score)

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


RFDF.dev$deciles <- decile(RFDF.dev$predict.score[,2])


library(data.table)
tmp_DT = data.table(RFDF.dev)
rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

sum(RFDF.dev$TARGET) / nrow(RFDF.dev)


library(ROCR)
pred <- prediction(RFDF.dev$predict.score[,2], RFDF.dev$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(RFDF.dev$predict.score[,2], type="Gini")
gini

## Classification Error
with(RFDF.dev, table(TARGET, predict.class))


## Scoring syntax
RFDF.holdout$predict.class <- predict(tRF, RFDF.holdout, type="class")
RFDF.holdout$predict.score <- predict(tRF, RFDF.holdout, type="prob")

RFDF.holdout$deciles <- decile(RFDF.holdout$predict.score[,2])

tmp_DT = data.table(RFDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)




sum(RFDF.holdout$TARGET) / nrow(RFDF.holdout)


library(ROCR)
pred <- prediction(RFDF.holdout$predict.score[,2], RFDF.holdout$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(RFDF.holdout$predict.score[,2], type="Gini")
gini

## Classification Error
with(RFDF.holdout, table(TARGET, predict.class))


