library(dplyr)
library(MASS)
library(car)

load('train_v2.RData')
load('test_v2.RData')

df = train# %>% rbind(test)

df %>% names() %>% list()

# removing outliers
#  95     103    226    303    526    543    649    777    998    1094   1164   1194   1209   1240   1268   1297 
#  1403   1477   1490   1576   1607   1718   1720   1799   1809   1971   1974   2043   2125   2140   2235 

df <- df[-c(95 ,103,226 , 303  ,  526 ,   543 ,   649   , 777   , 998  ,  1094 ,  1164 ,  1194  , 1209 ,  1240 ,  1268  , 1297 , 1403  , 1477  , 1490  , 1576  , 1607 ,  1718 ,  1720  , 1799  , 1809 ,  1971  , 1974 ,  2043 ,  2125 ,  2140  , 2235 ),]

# 10 features selected from variable selection
features = c("PercentMarried","PctHS25_Over","PctBachDeg25_Over","PctEmployed16_Over","PctUnemployed16_Over","PctPrivateCoverage","PctPublicCoverage","PctPublicCoverageAlone","PctMarriedHouseholds","BirthRate" )

for(i in 1:length(features)){
  f = features[[i]]
  
  # scatter diagrams of target by regressors
  plot(df$TARGET_deathRate, df[[f]], pch=16, cex=1, xlab=f, ylab="TARGET_deathRate", main = sprintf("Scatter diagram of TARGET_deathRate by %s",f))
  
  # simple linear regression model on each regressor
  single.regressor.model <- lm(df$TARGET_deathRate ~ df[[f]])

  # reading fitted values
  y.hat <- fitted(single.regressor.model)
  
  # reading student residuals
  R.stud.res <- rstudent(single.regressor.model)
  
  # plotting student residuals by fitted values
  plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab=sprintf("R-student residual - %s",f), main = sprintf("Residual-by-fitted-value plot %s",f))
  abline(h=0, lty=1, lwd=3)
}


"
########################## Observations

# PercentMarried
PercentMarried has an approximate linear relationship with negative slope
Residual-by-fitted displays double bow pattern

PctHS25_Over
Scatter plot shows linear relationship wiuth positive slope
Residual-by-fitted displays double bow pattern

PctBachDeg25_Over
Very slight exponential curved relationship
Clear outward opening funnel pattern displayed

PctEmployed16_Over
linear relationship with negative slope
clear double-bow pattern

PctUnemployed16_Over
approximate linear relationship with positive slope
slight double bow pattern

PctPrivateCoverage
linear with negative slope
slight outward funnel

PctPublicCoverage
linear with positive slope
very slight outward funnel

PctPublicCoverageAlone
slight exponential
uniform

PctMarriedHouseholds
slight exponential
clear double bow

BirthRate
0 slope linear relationship
a cluster of points with no pattern may be slightly double bow

##########################
"

"
Based on above observations variance stabilization seems to be required on PctBachDeg25_Over, PctPrivateCoverage, PctPublicCoverage with square-root transformation
and on PercentMarried, PctHS25_Over, PctEmployed16_Over, PctUnemployed16_Over, PctMarriedHouseholds with arcsin transformation
"

############ Variance stabilization transformation of response - SQRT

y.transformed.sqrt = df$TARGET_deathRate %>% sqrt()

sqrt.features = c("PctBachDeg25_Over", "PctPrivateCoverage", "PctPublicCoverage")

for(i in 1:length(sqrt.features)){
  f = sqrt.features[[i]]
  
  # scatter diagrams of target by regressors
  plot(y.transformed.sqrt, df[[f]], pch=16, cex=1, xlab=f, ylab="TARGET_deathRate", main = sprintf("Scatter diagram of TARGET_deathRate by %s",f))
  
  # simple linear regression model on each regressor
  single.regressor.model <- lm(y.transformed.sqrt ~ df[[f]])
  
  # reading fitted values
  y.hat <- fitted(single.regressor.model)
  
  # reading student residuals
  R.stud.res <- rstudent(single.regressor.model)
  
  # plotting student residuals by fitted values
  #plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab=sprintf("R-student residual - %s",f), main = sprintf("Residual-by-fitted-value plot %s",f))
  #abline(h=0, lty=1, lwd=3)
}

"
########################## Observations
No improvement observed for any of the features

Using Box-Cox to identify transformation on response variable
##########################
"

box.cox.power.trans <- function(lambda, resp) {
  y.dot <- exp(mean(log(resp)))
  if (lambda == 0) {
    resp.trans <- y.dot*log(resp)
  } else {
    resp.trans <- (resp^lambda - 1) / (lambda*y.dot^(lambda-1))
  }
  return(resp.trans)
}

# BirthRate is removed as it has no relationship with response variable as per above plots

lambda.list <- seq(from=0, to=1, by=0.01)
n.lambda <- length(lambda.list)

SS.Res.list <- numeric(length=n.lambda)
for (i.lambda in 1:n.lambda) {
  lambda <- lambda.list[i.lambda]
  y.trans <-  box.cox.power.trans(lambda, resp=df$TARGET_deathRate)
  df.new <- data.frame(df, y.trans)
  new.lm <- lm(y.trans ~ PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds, data=df.new)
  SS.Res.list[i.lambda] <- anova(new.lm)[2,2]
}
cbind(lambda.list, SS.Res.list)

# selecting SS.Res minimizing value
SS.Res.min <- min(SS.Res.list)
i.lambda <- which(SS.Res.list == SS.Res.min)
lambda.hat <- lambda.list[i.lambda]

n = df %>% nrow()

# computing CI for lambda
alpha <- 0.05
chisq.crit <- qchisq(alpha, df=1, lower.tail=FALSE)
SS.Res.crit <- SS.Res.min*exp(chisq.crit/n)

# lambda values within CI

lambda.list[which(SS.Res.list <= SS.Res.crit)]
# [1] 0.94 0.95 0.96 0.97 0.98 0.99 1.00

plot(lambda.list, SS.Res.list, , type="l", lty=1, lwd=3, xlab="lambda", ylab="residual sum-of-square", main = "Selecting a Box-Cox transformation")
abline(h=SS.Res.crit, lty=1, lwd=1)

"
########################## Observations
Transformation of lambda = 1 is recommended
##########################
"

# without transformation
before.trans.lm = lm(TARGET_deathRate ~ PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds, data=df)
summary(before.trans.lm)

# All features are significant but with a poor Rsquare value of 0.3586

y.hat = fitted(before.trans.lm)
R.stud.res <- rstudent(before.trans.lm)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")

# after transformation
df.new = data.frame(df, box.cox.power.trans(df$TARGET_deathRate,1))
before.trans.lm = lm(y.trans ~ PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds, data=df.new)
summary(before.trans.lm)

# All features are significant but with a poor Rsquare value of 0.3586

y.hat = fitted(before.trans.lm)
R.stud.res <- rstudent(before.trans.lm)
plot(y.hat, R.stud.res, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")

"
########################## Observations
No major changes observed. All x values shifted by 1.
##########################
"


