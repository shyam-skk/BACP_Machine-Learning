setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project - 4/code")
getwd()

mydata<-read.csv("PL_XSELL.csv",header = TRUE)
str(mydata)


occ.matrix <- model.matrix(~ OCCUPATION - 1, data = mydata)
mydata <- data.frame(mydata, occ.matrix)

Gender.matrix <- model.matrix(~  GENDER - 1, data = mydata)
mydata <- data.frame(mydata, Gender.matrix)

#AGE_BKT.matrix <- model.matrix(~ AGE_BKT - 1, data = mydata)
#mydata <- data.frame(mydata, AGE_BKT.matrix)

ACC_TYPE.matrix <- model.matrix(~ ACC_TYPE - 1, data = mydata)
mydata <- data.frame(mydata, ACC_TYPE.matrix)

dim(mydata)
str(mydata)

#mydata<-subset(mydata[-c(4,6,7,10,11)])
#dim(mydata)
#str(mydata)

library(caTools)
set.seed(123)
split = sample.split(mydata$TARGET, SplitRatio = 0.70)
nn.dev = subset(mydata, split == TRUE)
nn.holdout = subset(mydata, split == FALSE)
dim(nn.dev)
dim(nn.holdout)
c(nrow(nn.dev), nrow(nn.holdout))

########### VARIABLE SCALING
train1 <- nn.dev[-c(1,2,4,6,7,10,11)]
dim(train1)
trainscaled <- scale(train1)
trainscaled <- cbind(nn.dev[2],trainscaled)
dim(trainscaled)

train2 <- nn.holdout[-c(1,2,4,6,7,10,11)]
dim(train2)
testscaled <- scale(train2)
testscaled <- cbind(nn.holdout[2],testscaled)
dim(trainscaled1)
######### TARGET variable chech on data set
#View(nn.holdout)
c(nrow(nn.dev), nrow(nn.holdout))
str(nn.dev)
summary(nn.dev)
sum(nn.dev$TARGET) / nrow(nn.dev)
sum(nn.holdout$TARGET) / nrow(nn.holdout)

prop.table((table(mydata$TARGET)))

prop.table((table(mydata$TARGET)))

names(nn.dev)
#install.packages("neuralnet")

dev <- nn.dev[-c(1,4,6,7,10,11)]
dim(dev)
str(dev)
library(neuralnet)

formu <- as.formula(paste('TARGET', '~', paste(colnames(dev)[2:43],collapse = '+')))

nn1 <- neuralnet(formu ,
                  data = dev, 
                  hidden = 3,
                  err.fct = "sse",
                  linear.output = FALSE,
                  lifesign = "full",
                  lifesign.step = 10,
                  threshold = 0.1,
                  stepmax = 2000)
plot (nn1)

## Assigning the Probabilities to Dev Sample
nn.dev$Prob = nn1$net.result[[1]] 

## The distribution of the estimated probabilities
quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,98,99,100)/100)
hist(nn.dev$Prob)



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
nn.dev$deciles <- decile(nn.dev$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
library(scales)

tmp_DT = data.table(nn.dev)
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
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)



formu <- as.formula(paste('TARGET', '~', paste(colnames(trainscaled)[2:43],collapse = '+')))

nn2 <- neuralnet(formu ,
                 data = trainscaled, 
                 hidden = 6,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000)
plot (nn2)

weights1 <- nn2$weights
weights
plot(nn2)


## Assigning the Probabilities to Dev Sample
nn.dev$Prob = nn2$net.result[[1]] 


## The distribution of the estimated probabilities
quantile(nn.dev$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(nn.dev$Prob)


## deciling
nn.dev$deciles <- decile(nn.dev$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(nn.dev)
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





## Assgining 0 / 1 class based on certain threshold
nn.dev$Class = ifelse(nn.dev$Prob>0.5,1,0)
with( nn.dev, table(TARGET, as.factor(Class)  ))

## We can use the confusionMatrix function of the caret package 


library(caret)
confusionMatrix(nn.dev$TARGET, nn.dev$Class)

table(nn.dev$TARGET, nn.dev$Class)


u = union( nn.dev$TARGET,nn.dev$Class)
t = table(factor(nn.dev$TARGET, u),factor(nn.dev$Class, u))
confusionMatrix(t)
## Error Computation
sum((nn.dev$TARGET - nn.dev$Prob)^2)/2




## Other Model Performance Measures

library(ROCR)
pred <- prediction(nn.dev$Prob, nn.dev$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(nn.dev$Prob, type="Gini")


auc
KS
gini



## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute
x <- subset(nn.holdout, 
            select = c("Age","Balance", "SCR", "No_OF_CR_TXNS", "Holding_Period")
)
x.scaled <- scale(x)

compute.output = compute(nn2, x.scaled)
compute.output
nn.holdout$Predict.score = compute.output$net.result
View(nn.holdout)

quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
nn.holdout$deciles <- decile(nn.holdout$Predict.score)

library(data.table)
tmp_DT = data.table(nn.holdout)
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
View(rank)

plot(nn2)

