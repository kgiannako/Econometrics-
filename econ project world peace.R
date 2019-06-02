library(corrplot)

data<-read.csv("C:/Users/PARIS/Desktop/econ/worldpeace1.csv")
is.data.frame(data)
summary(data)
str(data)

corpl<-corrplot.mixed(data)
par(mfrow=c(1, 1))
num.cols <- sapply(data, is.numeric)
num.cols
corrPLOT<-corrplot(cor(data[,num.cols]), method='ellipse',order="AOE")
corrPLOT<-corrplot(cor(data[,num.cols]), method='number',order="AOE")

attach(data)
cor(Export,Import)
cor(Export,Population)
cor(Export,GPI)
cor(Export,GCI)
cor(Export, Muslim)
cor(Export, Mu2)

#stepwise regression
library(SignifReg)
lmf=lm(Export~GPI+GCI+Import+Muslim+Mu2+Population)
step=step(lmf, direction = "both")

#Scatterplot
plot(Export, Import)
abline(lm(Export~Import))
pairs(Export~Import+Population+Muslim+Mu2, main = "Scatter Matrix")


lmimp=lm(Export~Import+Population+Muslim+Mu2)
summary(lmimp)

mean(lmimp$residuals^2)

ssr = sum((fitted(lmimp) - mean(Export))^2)
ssr
sse=sum(lmimp$residuals^2)
sse

sst=ssr+sse
sst

par(mfrow=c(1, 2))
boxplot(Export, main="Export")
boxplot(Import, main="Import")
boxplot(Population,main="Population")
boxplot(Muslim,main="Muslim")

#Histogram
hist(Export, breaks = 32,col = "Royal Blue")
hist(Import, breaks = 32,col = "Navy Blue")
hist(Population, breaks = 32,col = "Yellow")
hist(Muslim, breaks = 32,col = "red")

#density plot
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(Export), main="Density Plot: Export", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Export), 2)))  # density plot for 'speed'
polygon(density(Export), col="red")
plot(density(Import), main="Density Plot: Import", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Import), 2)))  # density plot for 'dist'
polygon(density(Import), col="red")

#Scatter plot: Visualize the linear relationship between the predictor and response
#Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect the predictions as they can easily affect the direction/slope of the line of best fit.
#Density plot: To see the distribution of the predictor variable. Ideally, a close to normal distribution (a bell shaped curve), without being skewed to the left or right is preferred. Let us see how to make each one of them.



modelSummary <- summary(lmimp)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Import", "Estimate"]  # get beta estimate for Import
std.error <- modelCoeffs["Import", "Std. Error"]  # get std.error for import
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(data)-ncol(data))  # calc p Value
f_statistic <- lmimp$fstatistic[1]  # fstatistic
f <- summary(lmimp)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
t_value
p_value
f
model_p
modelSummary
attach(data)
par(mfrow=c(1, 1))
#residuals
res<-residuals(lmimp)
pred<-predict(lmimp)
s<-summary(lmimp)$sigma
h<-lm.influence(lmimp)$hat
res<-res/(s*sqrt(1-h))
plot(Export, res, xlab="Export", ylab="Std. Residuals")
abline(h=0, lty=2)
title("Std. Residuals versus Export")



plot(pred, res, xlab="Predicted Export", ylab="Std. Residuals")
abline(h=0, lty=2)
title("Std. Residuals vs Fitted Values")

qqnorm(res, ylab="Std. Residuals", main="Normal Plot of Std. Residuals")
qqline(res)




library(boot)
#KFold
#best model linear
glm.fit1=glm(Export~Import+Population+Muslim+Mu2)
summary(glm.fit1)
cv.err0=cv.glm(data,glm.fit1,K=10)
cv.err0$delta


# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data
# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary (lmMod)  # model summary
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 48.38%, mean absolute percentage deviation
str(data)
#kfold
library(DAAG)
cvResults <- suppressWarnings(CVlm( df=data ,form.lm=formula(Export~Import+Population+Muslim+Mu2) , m=10, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'mse')  # => 251.2783 mean squared err




#Autocorrelation
library(lmtest)

bgtest(lmimp)
dwtest(lmimp)
bgtest(lmimp,order=2)

#Heteroskesticity
library(het.test)
library(vars)


u2 <- lmimp$residuals^2
y <- fitted(lmimp)
Ru2<- summary(lm(u2 ~ y + I(y^2)))$r.squared
LM <- nrow(data)*Ru2
p.value <- 1-pchisq(LM, 2)
p.value
#p<0.05 hence no heteroskedasticity

#residuals
res2<-residuals(lmimp)^2
pred<-predict(lmimp)
s<-summary(lmimp)$sigma
h<-lm.influence(lmimp)$hat
res2<-res2/(s*sqrt(1-h))
plot(Export, res2, xlab="Export", ylab="Std. Residuals squared")
abline(h=0, lty=2)
title("Std. Residuals versus Export squared")

res2=residuals(lmimp)^2
plot(Export, res2, xlab="Export", ylab="Std. Residuals squared")
abline(h=0, lty=2)
title("Std. Residuals squared versus Export")

#multicolinearity
library(car)
vif(lmimp) #everything ok


#chow
library(strucchange)
sctest(lmimp, type="Chow", point=20)
#n=20 is not a break point

