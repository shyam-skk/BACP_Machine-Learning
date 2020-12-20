setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project - 4/code")
getwd()

train<-read.csv("PL_XSELL.csv",header = TRUE)
cbind(colSums(is.na(train)))

library(caTools)
set.seed(123)
split = sample.split(train$TARGET, SplitRatio = 0.70)
training_set = subset(train, split == TRUE)
test_set = subset(train, split == FALSE)

dim(training_set)
dim(test_set)

occ.matrix <- model.matrix(~ OCCUPATION - 1, data = training_set)
training_set <- data.frame(training_set, occ.matrix)

Gender.matrix <- model.matrix(~ GENDER - 1, data = training_set)
training_set <- data.frame(training_set, Gender.matrix)

acc.matrix <- model.matrix(~ ACC_TYPE - 1, data = training_set)
training_set <- data.frame(training_set, acc.matrix)


occ.matrix <- model.matrix(~ OCCUPATION - 1, data = test_set)
test_set <- data.frame(test_set, occ.matrix)

Gender.matrix <- model.matrix(~ GENDER - 1, data = test_set)
test_set <- data.frame(test_set, Gender.matrix)

acc.matrix <- model.matrix(~ ACC_TYPE - 1, data = test_set)
test_set <- data.frame(test_set, acc.matrix)

dim(training_set)
dim(test_set)

library(neuralnet,quietly = TRUE)

train1 <- training_set[-c(1,2,4,6,7,10,11,40)]
cat("Training set dimention",dim(training_set))
cat("\nDimention after trim",dim(train1))

trainscaled <- scale(train1)
trainscaled <- cbind(training_set[2],trainscaled)
dim(trainscaled)

formu <- as.formula(paste('TARGET', '~', paste(colnames(trainscaled)[2:42],collapse = '+')))

nn_1 <- neuralnet(formu ,
                  data = trainscaled, 
                  hidden = 6,
                  err.fct = "sse",
                  linear.output = FALSE,
                  lifesign = "full",
                  lifesign.step = 10,
                  threshold = 0.1,
                  stepmax = 2000)
training_set$Prob = nn_1$net.result[[1]] 
quantile(training_set$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)

y <- subset(test_set[-c(1,2,4,6,7,10,11,40)])


y.scaled <- scale(y)
testcaled <- cbind(test_set[1], y.scaled)
compute.output = compute(nn_1, y.scaled)
test_set$Predict.score = compute.output$net.result


quantile(test_set$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)

hist(test_set$Predict.score)

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

## deciling
test_set$deciles <- decile(as.numeric(test_set$Predict.score))
library(data.table)
tmp_DT = data.table(test_set)
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
View(rank)

## Assgining 0 / 1 class based on certain threshold
test_set$Class = ifelse(test_set$Predict.score>0.5,1,0)
with( test_set, table(TARGET, as.factor(Class)  ))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
library(caret)
confusionMatrix(test_set$TARGET, test_set$Class)

library(ROCR)
pred <- prediction(test_set$Predict.score, test_set$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(test_set$Predict.score, type="Gini")


auc
KS
gini



