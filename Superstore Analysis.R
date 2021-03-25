library(dplyr)
library(e1071)
library(ggplot2)
library(corrplot)

ds = read.csv("superstore.csv")
head(ds, n=10) 

# check levels for each categorical variables
str(ds)
levels(ds$Ship.Mode)
levels(ds$Segment)
levels(ds$City)
levels(ds$State)
levels(ds$Region)
levels(ds$Category)
levels(ds$Sub.Category)

# barplot
barplot(prop.table(table(ds$Region)))
barplot(prop.table(table(ds$Ship.Mode)))
barplot(prop.table(table(ds$Category)))

levels(ds$Sub.Category)
barplot(prop.table(table(ds$Sub.Category)))

barplot(prop.table(table(ds$State)))
barplot(prop.table(table(ds$City)))

range(ds$Profit)
ds$logProfit = sign(ds$Profit) * log(abs(ds$Profit)+1)
hist(ds$logProfit)
range(ds$logProfit)

# create numeric dataset
dsNumeric = ds[ , 18:22]
dsNumeric$Profit = NULL
head(dsNumeric)
plot(dsNumeric)
cor.full = cor(dsNumeric)
corrplot(cor.full)

# fit regression 
fullfit= lm(logProfit ~ ., data = dsNumeric)
summary(fullfit)
plot(fullfit)

cor.full = cor(dsNumeric)
corrplot(cor.full)


############### log sales #########
dsNumeric$logSales = log(dsNumeric$Sales)
dsNumeric$Sales = NULL
fullfit= lm(logProfit ~ ., data = dsNumeric)
summary(fullfit)
plot(fullfit)

cor.full = cor(dsNumeric)
corrplot(cor.full)
################ add dummies #########
# lm 2
#create dummies -- 52.18%
library(dummies)
dsDummy = dsNumeric
#dsDummy$profit.type = NULL
#dsDummy$Quantity = NULL
dsDummy$Region = ds$Region
dsDummy$Category = ds$Category
dsDummy$Segment = ds$Segment
dsDummy$Ship.Mode = ds$Ship.Mode
dsDummy = dummy.data.frame(dsDummy, names = c("Category","Segment","Region","Ship.Mode"),sep = ".")

fitRegion= lm(logProfit ~ ., data = dsDummy)
summary(fitRegion)
plot(fitRegion)


# run lasso regression 
# install.packages("glmnet") 
library(glmnet)
n = nrow(dsNumeric)
n
s = sample(n,3000)
salesTrain = dsNumeric[-s, ]
salesTest = dsNumeric[s,]

xTrain = as.matrix(salesTrain[, 1:3])  
yTrain = as.matrix(salesTrain[, 4])   

xTest = as.matrix(salesTest[, 1:3])  
yTest = as.matrix(salesTest[, 4]) 

fitLasso = glmnet(xTrain, yTrain, alpha=1) #, lambda=lRange fitLasso
# plot(fitLasso, xvar="lambda")
fitLasso
fitLasso = cv.glmnet(xTrain, yTrain, alpha=1, nfolds=7) 
fitLasso$lambda.min
fitLasso$lambda.1se
plot(fitLasso)

# To predict with this model, we need to tell it the lambda.
# we do so with the "s" parameter ... go figure :)
lassoPred = predict(fitLasso, xTest, s="lambda.1se")
rmseLasso = sqrt(mean((lassoPred - yTest)^2))
rmseLasso  

minPred = predict(fitLasso, xTest, s="lambda.min")
rmsemin = sqrt(mean((lassoPred - yTest)^2))
rmsemin  

coef(fitLasso,s="lambda.1se")
coef(fitLasso,s="lambda.min")

p = prcomp(dsNumeric)
summary(p)

library(psych)
fit = principal(dsNumeric[,-4], nfactors=2)
summary(fit)
print(fit$loadings, cutoff=.4, sort=T)


#####  milestone 4  ## 
#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library(ggplot2)

newds = ds[ ,c(5,8,13,15)]
head(newds)
cats = apply(newds, 2, function(x) nlevels(as.factor(x)))
cats
mca1 = MCA(newds, graph = FALSE)
mca1$eig
# column coordinates
head(mca1$var$coord)
# row coordinates
head(mca1$ind$coord)
# data frames for ggplot
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats),cats))
mca1_obs_df = data.frame(mca1$ind$coord)
# plot of variable categories
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + 
geom_text(aes(colour = Variable)) + ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", 
alpha = 0.7) + geom_density2d(colour = "gray80") + geom_text(data = mca1_vars_df, 
aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) + 
ggtitle("MCA plot of variables using R package FactoMineR") + scale_colour_discrete(name = "Variable")

plot(mca1)


library(corrplot)
library(ggplot2)
#install.packages('carData')
library(carData)
library(car)
library(MASS)
library(dummies)
#install.packages('aod')
#install.packages('pROC')
library(aod)
library(pROC)


ds$cost = ds$Sales - ds$Profit
ds$logcost = log(ds$cost)
ds$sellingPrice = ds$Sales / ds$Discount
ds$diffSpCost = ds$sellingPrice - ds$cost
ds$logpersale = log(ds$Sales / ds$Quantity)
ds$logpercost = log(ds$cost / ds$Quantity)
ds$Margin = (ds$cost - ds$Sales) / ds$Sales

#transform sales
Sales = ds$Sales
hist(Sales)
hist(log(Sales))
ds$logSales = log(Sales)


#subset when profit = 0 to see missing values
newdata = subset(ds, ds$Profit == 0)
head(newdata)

#create dummies(we do not need create dummies here)
#FullDatasetnew = dummy.data.frame(FullDataset, names = #c("Category","Segment","Region","Ship.Mode"),sep = ".")
#head(FullDatasetnew)
#summary(FullDatasetnew)

#select the useful variable
data1 = c("Segment","Region","Category","Quantity", "Discount","profit.type" )
newdata1 = ds[data1]

#set the training set
set.seed(900)
s = sample(nrow(ds), nrow(ds) * .7)
dsTrain = ds[s, ]
dsTest = ds[-s, ]

###########################

posProfit = subset(ds, ds$Profit >= 0)
set.seed(900)
s = sample(nrow(ds), nrow(ds) * .7)
dsTrain = ds[s, ]
dsTest = ds[-s, ]

#############################
#LDA for category(model works, but is that meaningful to predict the category for the product)
library(MASS)
trainLDA = lda(Category ~ Sales + Quantity + Profit + Discount, data=dsTrain)
print(trainLDA)
names(trainLDA)
summary(trainLDA)

trainLDA$prior
trainLDA$counts
trainLDA$means
trainLDA$scaling
trainLDA$svd

prop = trainLDA$svd^2/sum(trainLDA$svd^2)
prop#amount of the between-group variance that is explained by each linear discrimiant
#our first linear discriminant explains more than 99%  varince

p = predict(trainLDA, dsTest)
table(dsTest$Category, p$class)
poster = as.data.frame(p$posterior)

#LDA for segment
segmentLDA = lda(Segment ~ Sales + Quantity + Profit + Discount, data=dsTrain)
summary(segmentLDA)
print(segmentLDA)

p2 = predict(segmentLDA, dsTest)
table(dsTest$Segment, p2$class)

#LDA for Region
regionLDA = lda(Region ~ Sales + Quantity + Profit + Discount, data=dsTrain)
summary(regionLDA)
print(regionLDA)

p3 = predict(regionLDA,dsTest)
table(dsTest$Region, p3$class)

#try logistic regression with selected obs.
ds$profit.type = ds$Profit
ds$profit.type[ds$profit.type >= 0] = 0  # positive profit
ds$profit.type[ds$profit.type < 0] = 1   # negative profit
ds$logSales = log(ds$Sales)

set.seed(900)
s = sample(nrow(ds), nrow(ds) * .7)
dsTrain = ds[s, ]
dsTest = ds[-s, ]


logfull = glm(profit.type ~ Segment + Category + Region + Discount + logSales + Quantity, 
              data=dsTrain, family=binomial(link = "logit"), control = list(maxit = 50))

summary(logfull)
plot(logfull)
probabilities = predict(logfull,type = "response")
predicted.classes = ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

#test 
pred = predict(logfull, newdata=dsTest, type='response')
print(pred)
pred = ifelse(pred > 0.5, 1, 0)
table(pred, dsTest$profit.type) 
misClasificError = mean(pred != dsTest$profit.type)
print(paste('Accuracy',1-misClasificError))

#train
pred = predict(logfull, newdata=dsTrain, type='response')
print(pred)
pred = ifelse(pred > 0.5, 1, 0)
table(pred,dsTrain$profit.type) 
misClasificError = mean(pred != dsTrain$profit.type)
print(paste('Accuracy',1-misClasificError))

########## cluster Analysis
ds$profitMargin = ds$Profit / ds$Sales
ds$cost = ds$Sales - ds$Profit

library(MASS)
posProfit = subset(ds, ds$Profit >= 0)
negProfit = subset(ds,ds$Profit < 0)
negNumeric = negProfit[,18:26]
posNumeric = posProfit[,18:26]

head(negNumeric)
negNumeric$logSales = NULL
negNumeric$logProfit = NULL
negNumeric$profit.type = NULL
negNumeric$profitMargin = NULL
negNumeric$cost = NULL
#neg = negNumeric[negNumeric$Profit >= 1000]

negClust = kmeans(negNumeric, 3)
plot(negNumeric$Sales,negNumeric$Profit,  col=negClust$cluster)
plot(negNumeric$Sales,negNumeric$Profit, col=ds$Category)
#xlim = c(0,50), ylim = c(-50,0)


negNumeric$Profit = abs(negNumeric$Profit)
d = dist(negNumeric) # euclidean distances between the rows
fit = cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results
x = fit$points[, 1]
y = fit$points[, 2]
plot(x, y, col=negNumeric$Sales)
plot(x, y, col=negNumeric$Profit)


neg = as.matrix(negProfit)
fit = isoMDS(neg)
fit = cmdscale(negProfit, eig=TRUE, k=2) # k is the number of dim

# plot solution
x = fit$points[,1]
y = fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(ds), cex=.7)


##################### delete outliers ################
# sales > 10000: 2698, 166, 684
# profit > -3000: 7773, 684, 9775, 3012
# max cost: 2698, 684, 7773, 166, 6827

ds1 = read.csv("superstore(deleted outliers).csv")
ds1$logProfit = sign(ds1$Profit) * log(abs(ds1$Profit)+1)
ds1Numeric = ds1[ , 18:22]
ds1Numeric$Profit = NULL
cor.full = cor(ds1Numeric)
corrplot(cor.full)

# fit regression 
fullfit= lm(logProfit ~ ., data = ds1Numeric)
summary(fullfit)

############### log sales #########
ds1Numeric$logSales = log(ds1Numeric$Sales)
ds1Numeric$Sales = NULL
fullfit= lm(logProfit ~ ., data = ds1Numeric)
summary(fullfit)
plot(fullfit)

cor.full = cor(dsNumeric)
corrplot(cor.full)




