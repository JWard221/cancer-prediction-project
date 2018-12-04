
# Read in cleaned data
train = read.csv("train_v2.csv", sep = " ")
train = train[, -which(names(train) %in% c('Data', 'studyPerCap','binnedInc'))]
train$PctEmpPrivCoverage.greater.than.mean = as.numeric(train$PctEmpPrivCoverage.greater.than.mean)

test = read.csv("test_v2.csv", sep = " ")
test = test[, -which(names(test) %in% c('Data', 'studyPerCap','binnedInc'))]
test$PctEmpPrivCoverage.greater.than.mean = as.numeric(test$PctEmpPrivCoverage.greater.than.mean)


# Model 1: In Variable Selection analysis, we found that models with 13, 11, 8, and 7 
# variable-combinations were good starting points.  Here we will build a baseline linear
# model.

# these are the features we will begin with
names13
# [1] "avgAnnCount"            "avgDeathsPerYear"       "incidenceRate"         
# [4] "popEst2015"             "MedianAgeMale"          "PercentMarried"        
# [7] "PctHS25_Over"           "PctBachDeg25_Over"      "PctEmployed16_Over"    
# [10] "PctPublicCoverage"      "PctPublicCoverageAlone" "PctMarriedHouseholds"  
# [13] "BirthRate"

names8
# [1] "incidenceRate"          "PercentMarried"         "PctHS25_Over"          
# [4] "PctBachDeg25_Over"      "PctEmployed16_Over"     "PctPublicCoverage"     
# [7] "PctPublicCoverageAlone" "PctMarriedHouseholds"

# fit linear model with 13 above features 
model13 = lm(TARGET_deathRate ~ avgAnnCount + popEst2015 + PctHS25_Over + PctPublicCoverage + 
               BirthRate + avgDeathsPerYear + MedianAgeMale + PctBachDeg25_Over + 
               PctPublicCoverageAlone + incidenceRate + PercentMarried + PctEmployed16_Over
             + PctMarriedHouseholds, data = train)
summary(model13)

# fit linear model with 8 above features 
model8 = lm(TARGET_deathRate ~ PctHS25_Over + PctPublicCoverage + 
                PctBachDeg25_Over + 
               PctPublicCoverageAlone + incidenceRate + PercentMarried + PctEmployed16_Over
             + PctMarriedHouseholds, data = train)
summary(model8)


# For both of these models, the R^2 is hovering around .5, while all of the variables 
# appear significant based on their t and p-values.

# We will check model adequacy for the model with 8 features
# get residuals 
res8 = resid(model8)
# get hat values
hat8 = hatvalues(model8)
# get studentized residuals
stud8 = rstandard(model8)
# get r student residuals
rstud8 = rstudent(model8)

# create a normal probability plot of residuals
qqnorm(rstud8, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", main = "Normal probability plot of residuals")
qqline(rstud8)
# slightly heavy-tailed distribution 

# residual-by-fitted-value-plot
yhat = predict(model8, train)
plot(yhat, rstud8, pch=16, cex=1, xlab="fitted value", ylab="R-student residual", main = "Residual-by-fitted-value plot")
abline(h=0, lty=1, lwd=3)
# Apart from a few outliers, it appears the residuals can be contained in a horizontal band

# Due to the presence of outliers suggested in the previous two residual plots, we will now 
# remove outliers from the data, and refit the model.
# Based on running the "Outlier Exploration" script with the above 8 variables input, 
# these are suggested to be outliers for this model: 
# "473"  "782"  "996"  "1490" "1537" "1607" "1638" "1728" "2021" "2043" "2273"

# create new training set without these observations 
# train2 = train[-c(473, 782, 996, 1490, 1537, 1607, 1638, 1728, 2021, 2043, 2273), ]
# 
# # fit linear model with 8 above features 
# model8 = lm(TARGET_deathRate ~ PctHS25_Over + PctPublicCoverage + 
#               PctBachDeg25_Over + 
#               PctPublicCoverageAlone + incidenceRate + PercentMarried + PctEmployed16_Over
#             + PctMarriedHouseholds, data = train2)
# summary(model8)

# residual-by-regressor plots
par(mfrow = c(2, 2))
plot(train$incidenceRate, rstud8, pch=16, cex=1, xlab="incidenceRate", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
plot(train$PctHS25_Over, rstud8, pch=16, cex=1, xlab="PctHS25_Over", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
plot(train$PctPublicCoverage, rstud8, pch=16, cex=1, xlab="PctPublicCoverage", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
plot(train$PctBachDeg25_Over, rstud8, pch=16, cex=1, xlab="PctBachDeg25_Over", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
## All of these display a few outliers, but the only other concerning is the fanning in of the 
# PctBachDeg25_Over, implying that variance is nonconstant.
par(mfrow = c(2, 2))
plot(train$PctPublicCoverageAlone, rstud8, pch=16, cex=1, xlab="PctPublicCoverageAlone", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
plot(train$PercentMarried, rstud8, pch=16, cex=1, xlab="PercentMarried", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
plot(train$PctEmployed16_Over, rstud8, pch=16, cex=1, xlab="PctEmployed16_Over", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
plot(train$PctMarriedHouseholds, rstud8, pch=16, cex=1, xlab="PctMarriedHouseholds", ylab="R-student residual", main = "Residual-by-regressor plot")
abline(h=0, lty=1, lwd=3)
# These all look good aside from a few outliers.
par(mfrow = c(1, 1))

# The PRESS residuals are                                
PRESS.res <- stud8 / (1-hat8)

PRESS.stat <- sum(PRESS.res^2)
PRESS.stat
# [1] 2318.857

# percentage of each point's contribution to the PRESS statistic can identify high-influence points
inf.mat <- as.matrix(100*PRESS.res^2 / PRESS.stat)

# high influence rows 
high.inf.rows <- inf.mat[inf.mat >= .65,]
high.inf.rows
# 89       302       707       996      1268      1483      1720 
# 0.6858250 2.1842365 0.8714494 1.6315601 0.6648515 0.6822118 0.7598063  
# the PRESS residual associated with observation i = 302 contributes 2.18% 
# of the PRESS statistic

