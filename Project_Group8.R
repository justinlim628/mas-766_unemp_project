rm(list = ls())
setwd('~/Desktop/SyracuseUniv/Fall2020/MAS766/')

data<- read.table("UNRATE.csv",header=TRUE, sep=",") 
# describe data
dim(data)      # 874 by 2
names(data)    # "DATE"    "UNRATE"

## View data
View(data)
head(data) # starts from 1948-01-01, Unemployment Rate in %
tail(data) # ends at 2020-10-01rm(list = ls())

#change the class to be date
date<-as.Date(data$DATE, "%m/%d/%Y")
unrate <- data$UNRATE

png('unemploymentRate.png',width=720,height = 640)
plot(unrate ~ date, main='Unemployment rate', type = "b")
dev.off()
# Slight upward trend


# create a lag variable
#lnRate <- log(unrate)
nr <- nrow(data)
unrateLag <- c(NA, unrate[-nr])
# autoregressive? - plot shows the linear relationship
png('unemp_rate_vs_lag.png', width=720, height=640)
plot(unrate~unrateLag, main='Unemployment Rate vs Unemployment Rate Lag(1)')
dev.off()
# trend variable: 1 to 874
trend <- c(1:nr)
# cut 3 observations for the evaluation
cut <- 3

# Models
## 1. Seasonal Autoregressive model (Month): Observe if the data depends on the previous month
numlag <- 1
tp_sar1 <- c((numlag+1): (nr-cut)) # 2...... 871, leave 872,873,874 out for evaluation
y_sar1 <- unrate[tp_sar1]
ylag_sar1 <- unrateLag[tp_sar1]
model_sar1 <- lm(y_sar1~ylag_sar1)
summary(model_sar1)
sar1_r2 <- summary(model_sar1)$adj.r.squared
# SAR(1): Adjusted R^2 = 0.9407, Unemployment Rate = 0.165187 + 0.972643 * ylag_sar1
# Intercept: when unemp rate = 0 on the previous month, the unemp rate of the following month is 0.165187 % on average
# Slope: Unemployment rate is expected to increase 0.972643 % when the previous month's rate increase by 1%

# residual analysis
png('SAR(1)_residualAnalysis.png', width=720, height=640)
residual_sar1 <- resid(model_sar1)
plot(residual_sar1~ylag_sar1, axes=F, main='SAR(1) Residual Analysis')
axis(side=1, pos=0)
axis(side=2)
dev.off()
# Find outliers
stdres <- residual_sar1/sd(residual_sar1)
resmat <- data.frame(residual_sar1, stdres)
outlier <- which(abs(stdres)>3)
resmat[outlier,]

# Prediction
ftp <- c((nr-cut+1):nr) # 872 873 874
real <- unrate[ftp]
ylag_sar1_v <- unrate[ftp-numlag]
PV_sar1 <- predict(model_sar1, data.frame(ylag_sar1=ylag_sar1_v))
# compute mse and mae
e_sar1 <- real - PV_sar1
mse_sar1 <- mean(e_sar1^2)
mae_sar1 <- mean(abs(e_sar1))
FV_sar1 <- c(mse_sar1, mae_sar1, sar1_r2)

# Estimated regression line
model_fit <- fitted(model_sar1)
fit <- c(model_fit)
cut_date <- date[tp_sar1]

png('Estimated Regression Line.png', width=720, height = 640)
plot(unrate~date, type='b')
lines(fit~cut_date, col='red', lty=1)
dev.off()

## 2. Seasonal Autoregression model (month) + Trend
t <- trend[tp_sar1]
model_sar1_trend <- lm(y_sar1~t+ylag_sar1)
summary(model_sar1_trend)
sar1_trend_r2 <- summary(model_sar1_trend)$adj.r.squared
# p-value of time trend variable is 0.377 (Not significant) and adjusted R-squared did not increase after adding time trend
# therefore, adding time does not improve the model

# Residual Analysis
png('SAR(1)_trend_residualAnalysis.png', width=720, height = 640)
residual_sar1_trend <- resid(model_sar1_trend)
plot(residual_sar1_trend~t, axes=F, main='SAR(1)+Trend Residual Analysis')
axis(side=1, pos=0)
axis(side=2)
dev.off()
# Find Outliers
stdres2 <- residual_sar1_trend/sd(residual_sar1_trend)
resmat2 <- data.frame(residual_sar1_trend, stdres2)
outlier2 <- which(abs(stdres2)>3)
resmat2[outlier2,]

# Prediction
PV_sar1_trend <- predict(model_sar1_trend, data.frame(t=ftp,ylag_sar1=ylag_sar1_v))
# compute mse and mae
e_sar1_trend <- real - PV_sar1_trend
mse_sar1_trend <- mean(e_sar1_trend^2)
mae_sar1_trend <- mean(abs(e_sar1_trend))
FV_sar1_trend <- c(mse_sar1_trend, mae_sar1_trend, sar1_trend_r2)


## 3. Seasonal Autoregressive model (year): observe if the data depends on the previous year
numlag_sar12 <- 12
tp_sar12 <- c((numlag_sar12+1):(nr-cut))
rmv_sar12 <- (nr-numlag_sar12+1):nr
unrateLag12 <- c(rep(NA, numlag_sar12), unrate[-rmv_sar12])
# plot
png('unempRate_unempRateLag12.png', width = 720, height = 640)
plot(unrate~unrateLag12, main='Unemployment Rate vs Unemployment Rate Lag 12')
dev.off()
# still linear but more deviation

y_sar12 <- unrate[tp_sar12]
ylag_sar12 <- unrateLag12[tp_sar12]

# build model
model_sar12 <- lm(y_sar12~ylag_sar12)
summary(model_sar12)
sar12_r2 <- summary(model_sar12)$adj.r.squared
# SAR(12): Adjusted R^2 = 0.479, LN(Rate) = 0.51243 + 0.70313 * ylag_sar12
# Intercept: when unemp rate = 1 on the previous month, the unemp rate of the following month is ln(0.51243) on average
# Slope: Unemployment rate is expected to increase 0.70313% when the previous month's rate increase by 1%

# Residual Analysis
png('SAR(12)_residualAnalysis.png', width = 720, height = 640)
residual_sar12 <- resid(model_sar12)
plot(residual_sar12~ylag_sar12, axes=F, main='SAR(12) Residual Analysis')
axis(side=1, pos=0)
axis(side=2)
dev.off()
# Find outliers
stdres3 <- residual_sar12/sd(residual_sar12)
resmat3 <- data.frame(residual_sar12, stdres3)
outlier3 <- which(abs(stdres3)>3)
resmat3[outlier3,]

# Prediction
ylag_sar12_v <- unrate[ftp-numlag_sar12]
PV_sar12 <- predict(model_sar12, data.frame(ylag_sar12=ylag_sar12_v))
# compute mse and mae
e_sar12 <- real - PV_sar12
mse_sar12 <- mean(e_sar12^2)
mae_sar12 <- mean(abs(e_sar12))
FV_sar12 <- c(mse_sar12, mae_sar12, sar12_r2)

## 4. Seasonal Autoregression Model (year) + trend
t_sar12 <- trend[tp_sar12]
model_sar12_trend <- lm(y_sar12~t_sar12+ylag_sar12)
summary(model_sar12_trend)
sar_12_trend_r2 <- summary(model_sar12_trend)$adj.r.squared
# adjusted R^2 slightly improved. Better model than model 3

# Residual Analysis
png('SAR(12)+trend_residualAnalysis.png', width=720, height = 640)
residual_sar12_trend <- resid(model_sar12_trend)
plot(residual_sar12_trend~t_sar12, axes=F)
axis(side=1, pos=0)
axis(side=2)
dev.off()
# Find outliers
stdres4 <- residual_sar12_trend/sd(residual_sar12_trend)
resmat4 <- data.frame(residual_sar12_trend, stdres4)
outlier4 <- which(abs(stdres4)>3)
resmat4[outlier4,]

# Prediction
PV_sar12_trend <- predict(model_sar12_trend, data.frame(t_sar12=ftp, ylag_sar12=ylag_sar12_v))
e_sar12_trend <- real - PV_sar12_trend
mse_sar12_trend <- mean(e_sar12_trend^2)
mae_sar12_trend <- mean(abs(e_sar12_trend))
FV_sar12_trend <- c(mse_sar12_trend, mae_sar12_trend, sar_12_trend_r2)

## 5. Trend Only
tp_trend <- c(1:(nr-cut))
y_trend <- unrate[tp_trend]
model_trend <- lm(y_trend~tp_trend)
summary(model_trend)
# adjusted R^2 = 0.06883

# Residual Analysis
png('Trend_residualAnalysis.png', width=720, height = 640)
residual_trend <- resid(model_trend)
plot(residual_trend~tp_trend, axes=F)
axis(side=1, pos=0)
axis(side=2)
dev.off()
# Find outliers
stdres5 <- residual_trend/sd(residual_trend)
resmat5 <- data.frame(residual_trend, stdres5)
outlier5 <- which(abs(stdres5)>3)
resmat5[outlier5,]

# Unequal Variance Detected: Apply LN to y
lny_trend <- log(y_trend)
model_trend_adj <- lm(lny_trend~tp_trend)
summary(model_trend_adj)
trend_r2 <- summary(model_trend_adj)$adj.r.squared

# plot it again
png('Trend_residualAnalysis_adjusted.png', width=720, height = 640)
residual_trend <- resid(model_trend_adj)
plot(residual_trend~tp_trend, axes=F)
axis(side=1, pos=0)
axis(side=2)
dev.off()

# outliers
stdres6 <- residual_trend/sd(residual_trend)
resmat6 <- data.frame(residual_trend, stdres6)
outlier6 <- which(abs(stdres6)>3)
resmat6[outlier6,]

# compute mse and mae
PV_trend <- predict(model_trend_adj, data.frame(tp_trend=ftp))
e_trend <- real-PV_trend
mse_trend <- mean(e_trend^2)
mae_trend <- mean(abs(e_trend))
FV_trend <- c(mse_trend, mae_trend, trend_r2)

# create a comparison table
rw <- c('MSE', 'MAE', 'Adj.R^2')
comparison_table <- data.frame(FV_sar1, FV_sar1_trend, FV_sar12, FV_sar12_trend, FV_trend, row.names = rw)
# rename column names
colnames(comparison_table) <- c('SAR(1)', 'SAR(1)+Trend', 'SAR(12)', 'SAR(12)+Trend', 'Trend')
comparison_table

