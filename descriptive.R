######## Descriptive statistics 
setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project - 4/code")
getwd()

mydata<-read.csv("PL_XSELL.csv",header = TRUE)
my_dta<-mydata

View(mydata)
colSums(is.na(mydata))

dim(mydata)
names(mydata)
str(mydata)
head(mydata)
tail(mydata)

View(mydata)
summary(mydata)
View(summary(mydata))
attach(mydata)

table(FLG_HAS_CC)
table(ACC_TYPE)
table(FLG_HAS_ANY_CHGS)
table(NO_OF_IW_CHQ_BNC_TXNS)
table(NO_OF_OW_CHQ_BNC_TXNS)
table(FLG_HAS_NOMINEE)

prop.table(table(TARGET))
prop.table(table(GENDER, TARGET),1)
prop.table(table(OCCUPATION, TARGET),1)
prop.table(table(AGE_BKT, TARGET),1)
prop.table(table(ACC_TYPE, TARGET),1)
prop.table(table(NO_OF_IW_CHQ_BNC_TXNS, TARGET),1)
prop.table(table(NO_OF_OW_CHQ_BNC_TXNS, TARGET),1)
prop.table(table(FLG_HAS_NOMINEE, TARGET),1)
prop.table(table(FLG_HAS_ANY_CHGS, TARGET),1)
prop.table(table(FLG_HAS_CC, TARGET),1)

boxplot(mydata)

attach(mydata)
par(mfrow=c(2, 2))

plot(mydata$BALANCE ~ mydata$TARGET,xlab='TARGET', ylab='BALANCE',col='blue')
plot(mydata$SCR~ mydata$TARGET,xlab='TARGET', ylab='SCR',col='yellow')
plot(mydata$AGE ~ mydata$TARGET,xlab='TARGET', ylab='AGE',col='brown')
plot(mydata$HOLDING_PERIOD ~ mydata$TARGET,xlab='TARGET', ylab='HOLDING_PERIOD',col='red')
dev.off()
par(mfrow=c(2, 2))
barplot(table(TARGET,GENDER),main = 'GENDER')
barplot(table(TARGET,AGE_BKT),main = 'AGE_BKT')
barplot(table(TARGET,OCCUPATION),main = 'OCCUPATION')
barplot(table(TARGET,ACC_TYPE),main = 'ACC_TYPE')
dev.off()
par(mfrow=c(2, 2))
mosaicplot(table(NO_OF_IW_CHQ_BNC_TXNS,TARGET), color=T)
mosaicplot(table(FLG_HAS_CC,TARGET), color=T)
mosaicplot(table(FLG_HAS_NOMINEE,TARGET), color=T)
mosaicplot(table(FLG_HAS_ANY_CHGS,TARGET), color=T)
dev.off()
par(mfrow=c(2, 2))
hist(mydata$HOLDING_PERIOD,main="HOLDING_PERIOD", xlab=NA, ylab='HOLDING_PERIOD',col='orange')
hist(mydata$NO_OF_L_DR_TXNS,main="NO_OF_L_DR_TXNS", xlab=NA, ylab='NO_OF_L_DR_TXNS',col='green' )
hist(mydata$AMT_L_DR,main="AMT_L_DR", xlab=NA, ylab='AMT_L_DR',col='gold')
hist(mydata$AMT_BR_CSH_WDL_DR ,main="AMT_BR_CSH_WDL_DR ", xlab=NA, ylab='AMT_BR_CSH_WDL_DR',col='blue')
dev.off()
par(mfrow=c(2, 2))
hist(mydata$BALANCE ,main="BALANCE ", xlab=NA, ylab='BALANCE',col='orange')
hist(mydata$AGE ,main="AGE ", xlab=NA, ylab='AGE',col='blue')
hist(mydata$SCR ,main="SCR ", xlab=NA, ylab='SCR',col='red')
hist(mydata$AVG_AMT_PER_NET_TXN ,main="AVG_AMT_PER_NET_TXN ", xlab=NA, ylab='AVG_AMT_PER_NET_TXN',col='brown')
dev.off()
par(mfrow=c(2, 2))
############ correlation plot ###############
cordata <- subset(mydata[-c(2,4,6,7,10,11,21,28,38,39)])
corr <- cor(cordata[,-1])
View(corr)
install.packages('corrplot')
library(corrplot)
opar2 <- par(no.readonly = TRUE)
corrplot(corr,method = "circle",tl.cex = 0.5,tl.col = "black",number.cex = 0.55,bg = "grey14",
         addgrid.col = "gray50", tl.offset = 2,col = colorRampPalette(c("blue1","ivory2","firebrick2"))(100))

############# data set scaling ##############
d1<-mydata[,-c(1,4,6,7,10,11)]
d1[,2:34]<-scale(d1[,2:34])
summary(d1)
boxplot(d1)

########### variable  transformations
mydata1<-mydata[,-c(1)]
names(mydata1)
View(mydata1)
mydata1$ACC_OP_DATE <- as.character(mydata1$ACC_OP_DATE)
mydata1$ACC_OP_DATE <- as.Date(mydata1$ACC_OP_DATE, format="%m/%d/%Y")
mydata1$FLG_HAS_CC <- as.factor(mydata1$FLG_HAS_CC)
mydata1$FLG_HAS_ANY_CHGS <- as.factor(mydata1$FLG_HAS_ANY_CHGS)
mydata1$FLG_HAS_NOMINEE <- as.factor(mydata1$FLG_HAS_NOMINEE)
mydata1$FLG_HAS_OLD_LOAN <- as.factor(mydata1$FLG_HAS_OLD_LOAN)

names(mydata1)
View(mydata1)

############### sampling the data #################
#install.packages("ROSE")
#myadta <- ovun.sample(TARGET ~ ., data = mydata, method = "both", p=0.5,N=1000, seed = 1)$data
#table(myadta$TARGET)

str(mydata)
#View(mydata)
library(mlbench)
library(caret)
cordata <- subset(mydata[-c(2,4,6,7,10,11,21,28,38,39)])
#View(cordata)
# load the data
#data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(cordata[,-1])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)


#install.packages('GGally')
library('GGally')
ggcorr(cordata)


cor_data <- subset(cordata[-c(7,11,8,10,12,18,26,17,15,29)])
ggcorr(cor_data)
dim(cor_data)
str(cor_data)
w_data<-cor_data
w_data$TARGET<-mydata$TARGET
w_data$GENDER<-mydata$GENDER
w_data$OCCUPATION<-mydata$OCCUPATION
w_data$AGE_BKT<-mydata$AGE_BKT
w_data$ACC_TYPE<-mydata$ACC_TYPE
w_data$ACC_OP_DATE<-mydata$ACC_OP_DATE
w_data$FLG_HAS_CC<-mydata$FLG_HAS_CC
w_data$FLG_HAS_ANY_CHGS<-mydata$FLG_HAS_ANY_CHGS
w_data$FLG_HAS_NOMINEE<-mydata$FLG_HAS_NOMINEE
w_data$FLG_HAS_OLD_LOAN<-mydata$FLG_HAS_OLD_LOAN

dim(w_data)
str(w_data)
View(w_data)
