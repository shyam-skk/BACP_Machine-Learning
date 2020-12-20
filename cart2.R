setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project - 4/code")
getwd()

mydata<-read.csv("PL_XSELL.csv",header = TRUE)


############# spliting the data set

###install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(mydata$TARGET, SplitRatio = 0.70)
CTDF.dev = subset(mydata, split == TRUE)
CTDF.holdout = subset(mydata, split == FALSE)
dim(CTDF.dev)
dim(CTDF.holdout)
c(nrow(CTDF.dev), nrow(CTDF.holdout))



##############3 CART model

#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")
#install.packages("RColorBrewer")

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)



################# model 3 ##########################################################
r.ctrl = rpart.control(minsplit=100, minbucket =10, cp = 0, xval = 10)
m3 <- rpart(formula = TARGET ~ ., 
            data = CTDF.dev[,-c(1)], method = "class", 
            control = r.ctrl)
m3

#fancyRpartPlot(m3)

printcp(m3)
plotcp(m3)



#fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")
ptree<-m3

CTDF.dev$predict.class <- predict(ptree, CTDF.dev, type="class")
CTDF.dev$predict.score <- predict(ptree, CTDF.dev, type="prob")
#View(CTDF.dev)
#head(CTDF.dev)

#new_dev<-CTDF.dev[c(1,2,41,42)]
#View(new_dev)
#head(new_dev)

## deciling code
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

class(CTDF.dev$predict.score)
## deciling
CTDF.dev$deciles <- decile(CTDF.dev$predict.score[,2])
#View(CTDF.dev)

## Ranking code
#install.packages("data.table")
#install.packages("scales")
library(data.table)
library(scales)
tmp_DT = data.table(CTDF.dev)
rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),4);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp) * 100;
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)



##install.packages("ROCR")
##install.packages("ineq")
library(ROCR)
library(ineq)
pred <- prediction(CTDF.dev$predict.score[,2], CTDF.dev$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(CTDF.dev$predict.score[,2], type="Gini")

with(CTDF.dev, table(TARGET, predict.class))
auc
KS
gini

View(rank)
## Syntax to get the node path
tree.path <- path.rpart(ptree, node = c(2, 12))
nrow(CTDF.holdout)

##################################### Scoring Holdout sample ##############################
CTDF.holdout$predict.class <- predict(ptree, CTDF.holdout, type="class")
CTDF.holdout$predict.score <- predict(ptree, CTDF.holdout, type="prob")


CTDF.holdout$deciles <- decile(CTDF.holdout$predict.score[,2])
#View(CTDF.holdout)

## Ranking code
tmp_DT = data.table(CTDF.holdout)
h_rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp / h_rank$cnt,4);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),4);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),4);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp)*100;
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)

pred <- prediction(CTDF.holdout$predict.score[,2], CTDF.holdout$TARGET)
perf <- performance(pred, "tpr", "fpr")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(CTDF.holdout$predict.score[,2], type="Gini")

with(CTDF.holdout, table(TARGET, predict.class))
auc
KS
gini


