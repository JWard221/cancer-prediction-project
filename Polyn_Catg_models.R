
load("D:/Courses/STAT 6021 Fall 2018/Project/Repository/cancer-prediction-project/train_v2.RData")
summary(train)
train <- train[, names(train) != 'Data']

# Removing outlier and influential observations from training set for the 4 different models from variable selection-

train_1 = train[-c(53,441,740,757,1024,1098,1164,1268,1390,1403,1490,1532,1607,1720,1728,1783,1789,1799,1850,1961
,1971,2043,2094,2230),]
train_2 = train[-c(34,53,460,688,788,1268,1390,1403,1596,1679,1728,1737,1786,1961,1974,1976,1979,2002,2043,2053,2230,2235),]
train_3 = train[-c(53,460,683,788,916,957,1169,1268,1390,1403,1596,1728,1737,1786,1961,1974,1979,2002,2043,2053,2220,2230,
2235),]
train_4 = train[-c(67,200,460,504,683,757,788,916,957,1164,1169,1268,1390,1462,1464,1596,1728,1737,1799,1979,2002,2043,
                   2053),]

# Trying out linear models using variables from vaiable selection -

names10
# [1] "PercentMarried"         "PctHS25_Over"           "PctBachDeg25_Over"      "PctEmployed16_Over"     "PctUnemployed16_Over"  
# [6] "PctPrivateCoverage"     "PctPublicCoverage"      "PctPublicCoverageAlone" "PctMarriedHouseholds"   "BirthRate"  

names6
# [1] "MedianAgeFemale"      "PctHS25_Over"         "PctBachDeg25_Over"    "PctUnemployed16_Over" "PctMarriedHouseholds"
# [6] "BirthRate"  


names5
# [1] "PctHS25_Over"         "PctBachDeg25_Over"    "PctUnemployed16_Over" "PctMarriedHouseholds" "BirthRate"    

names4
# [1] "PctHS25_Over"         "PctBachDeg25_Over"    "PctUnemployed16_Over" "PctMarriedHouseholds" 

# names.forlm1 = paste(names10, collapse="+")
# names.forlm2 = paste(names6, collapse="+")
# names.forlm3 = paste(names5, collapse="+")
# names.forlm4 = paste(names4, collapse="+")
# 
# # Linear model 1
# 
# lm1 = lm(paste("TARGET_deathRate ~ ", names.forlm1, sep=""), data = train_new)
# summary(lm1)
# 
# 
# lm2 = lm(paste("TARGET_deathRate ~ ", names.forlm2, sep=""), data = train_new)
# summary(lm2)
# 
# 
# lm3 = lm(paste("TARGET_deathRate ~ ", names.forlm3, sep=""), data = train_new)
# summary(lm3)
# 
# 
# lm4 = lm(paste("TARGET_deathRate ~ ", names.forlm4, sep=""), data = train_new)
# summary(lm4)

lm1 = lm(TARGET_deathRate ~ PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+
           PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm1)
# Call:
#   lm(formula = TARGET_deathRate ~ PercentMarried + PctHS25_Over + 
#        PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + 
#        PctPrivateCoverage + PctPublicCoverage + PctPublicCoverageAlone + 
#        PctMarriedHouseholds + BirthRate, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -83.344 -12.287   0.628  12.304 144.577 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            211.4890    12.8023  16.520  < 2e-16 ***
#   PercentMarried           1.3038     0.1919   6.794 1.39e-11 ***
#   PctHS25_Over             0.8056     0.1096   7.351 2.73e-13 ***
#   PctBachDeg25_Over       -1.2562     0.1782  -7.049 2.38e-12 ***
#   PctEmployed16_Over      -0.7603     0.1203  -6.319 3.16e-10 ***
#   PctUnemployed16_Over     0.8571     0.2091   4.099 4.29e-05 ***
#   PctPrivateCoverage       0.5611     0.1075   5.217 1.98e-07 ***
#   PctPublicCoverage       -1.6521     0.1850  -8.930  < 2e-16 ***
#   PctPublicCoverageAlone   2.5018     0.2698   9.274  < 2e-16 ***
#   PctMarriedHouseholds    -1.8011     0.1799 -10.011  < 2e-16 ***
#   BirthRate               -1.6000     0.2502  -6.395 1.94e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.51 on 2250 degrees of freedom
# Multiple R-squared:  0.3691,	Adjusted R-squared:  0.3663 
# F-statistic: 131.6 on 10 and 2250 DF,  p-value: < 2.2e-16

lm2 = lm(TARGET_deathRate ~ MedianAgeFemale+PctHS25_Over+PctBachDeg25_Over+PctUnemployed16_Over+PctMarriedHouseholds+
           BirthRate, data = train_2)
summary(lm2)
# Call:
#   lm(formula = TARGET_deathRate ~ MedianAgeFemale + PctHS25_Over + 
#        PctBachDeg25_Over + PctUnemployed16_Over + PctMarriedHouseholds + 
#        BirthRate, data = train_2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -97.476 -12.543   0.541  12.395 159.944 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          217.05283    8.36901  25.935  < 2e-16 ***
#   MedianAgeFemale       -0.48083    0.09801  -4.906 9.98e-07 ***
#   PctHS25_Over           0.96235    0.11041   8.716  < 2e-16 ***
#   PctBachDeg25_Over     -1.26560    0.15262  -8.293  < 2e-16 ***
#   PctUnemployed16_Over   1.37153    0.18099   7.578 5.09e-14 ***
#   PctMarriedHouseholds  -0.72328    0.08269  -8.747  < 2e-16 ***
#   BirthRate             -1.43306    0.25578  -5.603 2.37e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 22.34 on 2256 degrees of freedom
# Multiple R-squared:  0.3399,	Adjusted R-squared:  0.3381 
# F-statistic: 193.6 on 6 and 2256 DF,  p-value: < 2.2e-16


lm3 = lm(TARGET_deathRate ~ PctHS25_Over+PctBachDeg25_Over+PctUnemployed16_Over+BirthRate, data = train_3)
summary(lm3)
# Call:
#   lm(formula = TARGET_deathRate ~ PctHS25_Over + PctBachDeg25_Over + 
#        PctUnemployed16_Over + BirthRate, data = train_3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -101.651  -13.305    0.881   12.981  160.649 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          164.0886     6.7043  24.475  < 2e-16 ***
#   PctHS25_Over           0.7254     0.1094   6.632 4.13e-11 ***
#   PctBachDeg25_Over     -1.3398     0.1572  -8.521  < 2e-16 ***
#   PctUnemployed16_Over   2.0497     0.1703  12.036  < 2e-16 ***
#   BirthRate             -1.5192     0.2589  -5.869 5.03e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 22.9 on 2257 degrees of freedom
# Multiple R-squared:  0.3047,	Adjusted R-squared:  0.3034 
# F-statistic: 247.2 on 4 and 2257 DF,  p-value: < 2.2e-16

lm4 = lm(TARGET_deathRate ~ PctHS25_Over+PctBachDeg25_Over+PctUnemployed16_Over+PctMarriedHouseholds, data = train_4)
summary(lm4)
# Call:
#   lm(formula = TARGET_deathRate ~ PctHS25_Over + PctBachDeg25_Over + 
#        PctUnemployed16_Over + PctMarriedHouseholds, data = train_4)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -82.780 -12.730   0.532  12.829 165.018 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          197.5580     7.6197  25.927  < 2e-16 ***
#   PctHS25_Over           0.8605     0.1071   8.037 1.47e-15 ***
#   PctBachDeg25_Over     -1.2393     0.1516  -8.174 4.91e-16 ***
#   PctUnemployed16_Over   1.4884     0.1801   8.263 2.40e-16 ***
#   PctMarriedHouseholds  -0.8509     0.0830 -10.252  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 22.45 on 2257 degrees of freedom
# Multiple R-squared:  0.3321,	Adjusted R-squared:  0.3309 
# F-statistic: 280.5 on 4 and 2257 DF,  p-value: < 2.2e-16


yhat1 <- predict(lm1,newdata=test)
mse1 <- mean((yhat1-test$TARGET_deathRate)^2)
mse1
# [1] 468.3813

yhat2 <- predict(lm2,newdata=test)
mse2 <- mean((yhat2-test$TARGET_deathRate)^2)
mse2
# [1] 476.9319

yhat3 <- predict(lm3,newdata=test)
mse3 <- mean((yhat3-test$TARGET_deathRate)^2)
mse3
# [1] 505.5494

yhat4 <- predict(lm4,newdata=test)
mse4 <- mean((yhat4-test$TARGET_deathRate)^2)
mse4
# [1] 477.0798

# Model 1 seems to be performing the best in explaining the data. 
# Let's also bring in insight from EDA and modify model 1

# From EDA we can keep -> PercentMarried + PctHS25_Over + PctBachDeg25_Over(try poly) + PctEmployed16_Over + PctUnemployed16_Over
# + PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds

# So Only difference from model1 is dropped Birthrate and trying out PctBachDeg25_Over with polynomials

# We can also add medIncome as polynomial model, and add binnedInc and PctEmpPrivCoverage.greater.than.mean as categorical
# variables.

# Another option is feature engineering a majority race categorical variable(which clubs asians with others) as seen from
# EDA

#------------------ 1st modification - adding polynomial term for PctBachDeg25_Over, removing birthrate -

lm1_v1 = lm(TARGET_deathRate ~ PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,2,raw=T)+PctEmployed16_Over+PctUnemployed16_Over+
               PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm1_v1)
# Call:
#   lm(formula = TARGET_deathRate ~ PercentMarried + PctHS25_Over + 
#        poly(PctBachDeg25_Over, 2) + PctEmployed16_Over + PctUnemployed16_Over + 
#        PctPrivateCoverage + PctPublicCoverage + PctPublicCoverageAlone + 
#        PctMarriedHouseholds + BirthRate, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -83.780 -12.233   0.645  12.268 143.601 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  193.0902    12.4959  15.452  < 2e-16 ***
#   PercentMarried                 1.2895     0.1922   6.710 2.45e-11 ***
#   PctHS25_Over                   0.7883     0.1103   7.148 1.19e-12 ***
#   poly(PctBachDeg25_Over, 2)1 -333.5249    46.4565  -7.179 9.48e-13 ***
#   poly(PctBachDeg25_Over, 2)2   31.4271    23.1808   1.356    0.175    
# PctEmployed16_Over            -0.7286     0.1226  -5.945 3.20e-09 ***
#   PctUnemployed16_Over           0.8485     0.2091   4.057 5.14e-05 ***
#   PctPrivateCoverage             0.5650     0.1076   5.253 1.64e-07 ***
#   PctPublicCoverage             -1.5956     0.1896  -8.416  < 2e-16 ***
#   PctPublicCoverageAlone         2.4362     0.2740   8.890  < 2e-16 ***
#   PctMarriedHouseholds          -1.7978     0.1799  -9.994  < 2e-16 ***
#   BirthRate                     -1.5829     0.2505  -6.320 3.15e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.5 on 2249 degrees of freedom
# Multiple R-squared:  0.3696,	Adjusted R-squared:  0.3665 
# F-statistic: 119.9 on 11 and 2249 DF,  p-value: < 2.2e-16

# There is no improvement in adjusted R2 on having polynomial term for PctBachDeg25_Over(which also has large p value)

# Let's try adding 3rd order polynomial
lm1_v2 = lm(TARGET_deathRate ~ PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,3,raw=T)+PctEmployed16_Over+PctUnemployed16_Over+
              PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm1_v2)
# Call:
#   lm(formula = TARGET_deathRate ~ PercentMarried + PctHS25_Over + 
#        poly(PctBachDeg25_Over, 3, raw = T) + PctEmployed16_Over + 
#        PctUnemployed16_Over + PctPrivateCoverage + PctPublicCoverage + 
#        PctPublicCoverageAlone + PctMarriedHouseholds + BirthRate, 
#      data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -86.162 -12.290   0.611  12.269 141.126 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          230.884242  13.521745  17.075  < 2e-16 ***
#   PercentMarried                         1.313553   0.191579   6.856 9.09e-12 ***
#   PctHS25_Over                           0.811780   0.110053   7.376 2.28e-13 ***
#   poly(PctBachDeg25_Over, 3, raw = T)1  -5.948265   1.099004  -5.412 6.88e-08 ***
#   poly(PctBachDeg25_Over, 3, raw = T)2   0.271737   0.063437   4.284 1.92e-05 ***
#   poly(PctBachDeg25_Over, 3, raw = T)3  -0.004658   0.001133  -4.111 4.08e-05 ***
#   PctEmployed16_Over                    -0.666251   0.123061  -5.414 6.82e-08 ***
#   PctUnemployed16_Over                   0.880587   0.208548   4.222 2.51e-05 ***
#   PctPrivateCoverage                     0.525796   0.107613   4.886 1.10e-06 ***
#   PctPublicCoverage                     -1.512875   0.189995  -7.963 2.65e-15 ***
#   PctPublicCoverageAlone                 2.288633   0.275424   8.309  < 2e-16 ***
#   PctMarriedHouseholds                  -1.817237   0.179318 -10.134  < 2e-16 ***
#   BirthRate                             -1.540471   0.249790  -6.167 8.22e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.43 on 2248 degrees of freedom
# Multiple R-squared:  0.3743,	Adjusted R-squared:  0.371 
# F-statistic: 112.1 on 12 and 2248 DF,  p-value: < 2.2e-16

# There is slight improvement in Adjusted R2 value and p-value is small as well.
# Increasing order of polynomial yields larger p-values. Therefore we will keep poly 3 term for PctBachDeg25_Over.

# Let's try dropping birthrate
lm1_v3 = lm(TARGET_deathRate ~ PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,3,raw=T)+PctEmployed16_Over+PctUnemployed16_Over+
              PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds, data = train_1)
summary(lm1_v3)
# Call:
#   lm(formula = TARGET_deathRate ~ PercentMarried + PctHS25_Over + 
#        poly(PctBachDeg25_Over, 3, raw = T) + PctEmployed16_Over + 
#        PctUnemployed16_Over + PctPrivateCoverage + PctPublicCoverage + 
#        PctPublicCoverageAlone + PctMarriedHouseholds, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -85.140 -12.274   0.776  12.350 141.553 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          220.082135  13.517754  16.281  < 2e-16 ***
#   PercentMarried                         1.188116   0.192058   6.186 7.30e-10 ***
#   PctHS25_Over                           0.880636   0.110383   7.978 2.35e-15 ***
#   poly(PctBachDeg25_Over, 3, raw = T)1  -6.146092   1.107544  -5.549 3.21e-08 ***
#   poly(PctBachDeg25_Over, 3, raw = T)2   0.291038   0.063879   4.556 5.49e-06 ***
#   poly(PctBachDeg25_Over, 3, raw = T)3  -0.004947   0.001141  -4.334 1.53e-05 ***
#   PctEmployed16_Over                    -0.696695   0.123970  -5.620 2.15e-08 ***
#   PctUnemployed16_Over                   0.950095   0.209950   4.525 6.34e-06 ***
#   PctPrivateCoverage                     0.560936   0.108344   5.177 2.45e-07 ***
#   PctPublicCoverage                     -1.425515   0.191020  -7.463 1.20e-13 ***
#   PctPublicCoverageAlone                 2.223133   0.277476   8.012 1.79e-15 ***
#   PctMarriedHouseholds                  -1.760102   0.180547  -9.749  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.6 on 2249 degrees of freedom
# Multiple R-squared:  0.3637,	Adjusted R-squared:  0.3606 
# F-statistic: 116.9 on 11 and 2249 DF,  p-value: < 2.2e-16

# Adjusted R2 decreases ; therefore we will keep birthrate

# Let's try dropping birthrate and keeping PctBachDeg25_Over with monomial term-
lm1_v4 = lm(TARGET_deathRate ~ PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+
              PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds, data = train_1)
summary(lm1_v4)
# Call:
#   lm(formula = TARGET_deathRate ~ PercentMarried + PctHS25_Over + 
#        PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + 
#        PctPrivateCoverage + PctPublicCoverage + PctPublicCoverageAlone + 
#        PctMarriedHouseholds, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -82.451 -12.466   0.734  12.613 145.416 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            198.5691    12.7534  15.570  < 2e-16 ***
#   PercentMarried           1.1748     0.1925   6.102 1.23e-09 ***
#   PctHS25_Over             0.8800     0.1099   8.006 1.89e-15 ***
#   PctBachDeg25_Over       -1.0685     0.1773  -6.026 1.96e-09 ***
#   PctEmployed16_Over      -0.8036     0.1212  -6.631 4.15e-11 ***
#   PctUnemployed16_Over     0.9296     0.2106   4.414 1.06e-05 ***
#   PctPrivateCoverage       0.5993     0.1083   5.532 3.52e-08 ***
#   PctPublicCoverage       -1.5797     0.1863  -8.481  < 2e-16 ***
#   PctPublicCoverageAlone   2.4587     0.2721   9.037  < 2e-16 ***
#   PctMarriedHouseholds    -1.7410     0.1812  -9.606  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.7 on 2251 degrees of freedom
# Multiple R-squared:  0.3576,	Adjusted R-squared:  0.3551 
# F-statistic: 139.2 on 9 and 2251 DF,  p-value: < 2.2e-16

# Adjusted R2 decreases on dropping birthrate from original lm1; therefore we keep it in model. 

###---- Final conclusion : Use PctBachDeg25_Over with 3rd order polynomial

# ---------------------2nd Modification - adding medIncome as polynomial term

lm1_v5 = lm(TARGET_deathRate ~ poly(medIncome,2,raw=T)+PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,3,raw=T)+PctEmployed16_Over+PctUnemployed16_Over+
           PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm1_v5)
# Call:
#   lm(formula = TARGET_deathRate ~ +poly(medIncome, 2, raw = T) + 
#        PercentMarried + PctHS25_Over + poly(PctBachDeg25_Over, 3, 
#                                             raw = T) + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + 
#        PctPublicCoverage + PctPublicCoverageAlone + PctMarriedHouseholds + 
#        BirthRate, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -86.481 -12.185   0.646  12.313 143.638 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           2.444e+02  1.402e+01  17.432  < 2e-16 ***
#   poly(medIncome, 2, raw = T)1         -7.711e-04  2.646e-04  -2.915 0.003595 ** 
#   poly(medIncome, 2, raw = T)2          6.889e-09  2.046e-09   3.368 0.000771 ***
#   PercentMarried                        1.284e+00  1.933e-01   6.644 3.82e-11 ***
#   PctHS25_Over                          8.366e-01  1.105e-01   7.572 5.32e-14 ***
#   poly(PctBachDeg25_Over, 3, raw = T)1 -5.526e+00  1.103e+00  -5.010 5.88e-07 ***
#   poly(PctBachDeg25_Over, 3, raw = T)2  2.633e-01  6.335e-02   4.157 3.35e-05 ***
#   poly(PctBachDeg25_Over, 3, raw = T)3 -4.884e-03  1.132e-03  -4.313 1.68e-05 ***
#   PctEmployed16_Over                   -6.183e-01  1.268e-01  -4.878 1.15e-06 ***
#   PctUnemployed16_Over                  8.446e-01  2.106e-01   4.009 6.28e-05 ***
#   PctPrivateCoverage                    5.451e-01  1.137e-01   4.793 1.75e-06 ***
#   PctPublicCoverage                    -1.508e+00  1.912e-01  -7.887 4.78e-15 ***
#   PctPublicCoverageAlone                2.262e+00  2.757e-01   8.203 3.90e-16 ***
#   PctMarriedHouseholds                 -1.800e+00  1.894e-01  -9.501  < 2e-16 ***
#   BirthRate                            -1.531e+00  2.492e-01  -6.141 9.67e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.38 on 2246 degrees of freedom
# Multiple R-squared:  0.3778,	Adjusted R-squared:  0.3739 
# F-statistic:  97.4 on 14 and 2246 DF,  p-value: < 2.2e-16

# There is some improvement in Adjusted R2. Let's try poly 3 term-

lm6 = lm(TARGET_deathRate ~ +poly(medIncome,3,raw=T)+PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,3,raw=T)+PctEmployed16_Over+PctUnemployed16_Over+
           PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm6)
# Call:
#   lm(formula = TARGET_deathRate ~ +poly(medIncome, 3, raw = T) + 
#        PercentMarried + PctHS25_Over + poly(PctBachDeg25_Over, 3, 
#                                             raw = T) + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + 
#        PctPublicCoverage + PctPublicCoverageAlone + PctMarriedHouseholds + 
#        BirthRate, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -87.441 -11.966   0.842  12.435 143.097 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           2.701e+02  1.825e+01  14.804  < 2e-16 ***
#   poly(medIncome, 3, raw = T)1         -2.396e-03  7.831e-04  -3.060 0.002241 ** 
#   poly(medIncome, 3, raw = T)2          3.384e-08  1.240e-08   2.730 0.006382 ** 
#   poly(medIncome, 3, raw = T)3         -1.387e-13  6.293e-14  -2.204 0.027591 *  
#   PercentMarried                        1.276e+00  1.931e-01   6.609 4.81e-11 ***
#   PctHS25_Over                          8.299e-01  1.104e-01   7.515 8.15e-14 ***
#   poly(PctBachDeg25_Over, 3, raw = T)1 -4.778e+00  1.153e+00  -4.143 3.56e-05 ***
#   poly(PctBachDeg25_Over, 3, raw = T)2  2.168e-01  6.671e-02   3.251 0.001169 ** 
#   poly(PctBachDeg25_Over, 3, raw = T)3 -4.061e-03  1.191e-03  -3.408 0.000665 ***
#   PctEmployed16_Over                   -6.046e-01  1.268e-01  -4.767 1.99e-06 ***
#   PctUnemployed16_Over                  8.067e-01  2.112e-01   3.820 0.000137 ***
#   PctPrivateCoverage                    5.639e-01  1.140e-01   4.948 8.05e-07 ***
#   PctPublicCoverage                    -1.490e+00  1.912e-01  -7.793 9.93e-15 ***
#   PctPublicCoverageAlone                2.230e+00  2.759e-01   8.084 1.02e-15 ***
#   PctMarriedHouseholds                 -1.794e+00  1.893e-01  -9.479  < 2e-16 ***
#   BirthRate                            -1.536e+00  2.490e-01  -6.168 8.19e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.36 on 2245 degrees of freedom
# Multiple R-squared:  0.3791,	Adjusted R-squared:  0.375 
# F-statistic: 91.39 on 15 and 2245 DF,  p-value: < 2.2e-16

# There is no improvement in Adjusted R2 and p-value increases.For higher orders, p-value becomes even larger.

###---- Final conclusion : Use medIncome with 2nd order polynomial.

# ---------------------3rd Modification - adding binnedInc categorical variable

lm1_v7 = lm(TARGET_deathRate ~ binnedInc+PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+
              PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm1_v7)
# Call:
#   lm(formula = TARGET_deathRate ~ binnedInc + PercentMarried + 
#        PctHS25_Over + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + 
#        PctPrivateCoverage + PctPublicCoverage + PctPublicCoverageAlone + 
#        PctMarriedHouseholds + BirthRate, data = train_1)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -86.79 -12.08   0.78  12.19 141.59 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 204.1680    13.5075  15.115  < 2e-16 ***
#   binnedInc(37413.8, 40362.7]   0.1347     2.0627   0.065 0.947950    
# binnedInc(40362.7, 42724.4]  -3.2499     2.0718  -1.569 0.116862    
# binnedInc(42724.4, 45201]    -2.4237     2.1721  -1.116 0.264621    
# binnedInc(45201, 48021.6]    -3.1040     2.2445  -1.383 0.166828    
# binnedInc(48021.6, 51046.4]  -5.4005     2.3702  -2.278 0.022792 *  
#   binnedInc(51046.4, 54545.6]  -4.1556     2.5019  -1.661 0.096852 .  
# binnedInc(54545.6, 61494.5]  -3.3759     2.6455  -1.276 0.202065    
# binnedInc(61494.5, 125635]    0.9052     3.1382   0.288 0.773027    
# binnedInc[22640, 34218.1]     3.8301     2.1061   1.819 0.069108 .  
# PercentMarried                1.2414     0.1930   6.432 1.53e-10 ***
#   PctHS25_Over                  0.7988     0.1098   7.273 4.84e-13 ***
#   PctBachDeg25_Over            -1.3578     0.1825  -7.440 1.43e-13 ***
#   PctEmployed16_Over           -0.6543     0.1248  -5.241 1.74e-07 ***
#   PctUnemployed16_Over          0.7770     0.2115   3.674 0.000244 ***
#   PctPrivateCoverage            0.6385     0.1138   5.609 2.29e-08 ***
#   PctPublicCoverage            -1.5579     0.1879  -8.291  < 2e-16 ***
#   PctPublicCoverageAlone        2.3958     0.2715   8.823  < 2e-16 ***
#   PctMarriedHouseholds         -1.7585     0.1870  -9.402  < 2e-16 ***
#   BirthRate                    -1.5623     0.2505  -6.237 5.33e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.43 on 2241 degrees of freedom
# Multiple R-squared:  0.376,	Adjusted R-squared:  0.3707 
# F-statistic: 71.08 on 19 and 2241 DF,  p-value: < 2.2e-16

# Including binnedInc increases adjusted R2 marginally, however the p-values are large and
# F statistic decreases a lot. 

###---- Final conclusion : Drop binnedInc from model.

# ---------------------4th Modification - adding PctEmpPrivCoverage.greater.than.mean categorical variable

lm1_v8 = lm(TARGET_deathRate ~ PctEmpPrivCoverage.greater.than.mean+PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over
            +PctUnemployed16_Over+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate,data=train_1)
summary(lm1_v8)
# Call:
#   lm(formula = TARGET_deathRate ~ PctEmpPrivCoverage.greater.than.mean + 
#        PercentMarried + PctHS25_Over + PctBachDeg25_Over + PctEmployed16_Over + 
#        PctUnemployed16_Over + PctPrivateCoverage + PctPublicCoverage + 
#        PctPublicCoverageAlone + PctMarriedHouseholds + BirthRate, 
#      data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -83.303 -12.257   0.614  12.311 144.507 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              212.2616    13.1415  16.152  < 2e-16 ***
#   PctEmpPrivCoverage.greater.than.meanTRUE   0.3654     1.3979   0.261    0.794    
# PercentMarried                             1.3100     0.1934   6.773 1.60e-11 ***
#   PctHS25_Over                               0.8023     0.1103   7.273 4.84e-13 ***
#   PctBachDeg25_Over                         -1.2544     0.1784  -7.033 2.68e-12 ***
#   PctEmployed16_Over                        -0.7636     0.1210  -6.311 3.33e-10 ***
#   PctUnemployed16_Over                       0.8531     0.2097   4.068 4.90e-05 ***
#   PctPrivateCoverage                         0.5465     0.1211   4.513 6.73e-06 ***
#   PctPublicCoverage                         -1.6400     0.1907  -8.600  < 2e-16 ***
#   PctPublicCoverageAlone                     2.4856     0.2769   8.978  < 2e-16 ***
#   PctMarriedHouseholds                      -1.8046     0.1805 -10.000  < 2e-16 ***
#   BirthRate                                 -1.5976     0.2504  -6.380 2.14e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.51 on 2249 degrees of freedom
# Multiple R-squared:  0.3691,	Adjusted R-squared:  0.366 
# F-statistic: 119.6 on 11 and 2249 DF,  p-value: < 2.2e-16

# Including PctEmpPrivCoverage.greater.than.mean increases adjusted R2 marginally, however the p-value is large. 

lm1_v9 = lm(TARGET_deathRate ~ poly(medIncome,2,raw=T)+PctEmpPrivCoverage.greater.than.mean+PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,3,raw=T)
            +PctEmployed16_Over+PctUnemployed16_Over+PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate,data=train_1)
summary(lm1_v9)
# Call:
#   lm(formula = TARGET_deathRate ~ poly(medIncome, 2, raw = T) + 
#        PctEmpPrivCoverage.greater.than.mean + PercentMarried + PctHS25_Over + 
#        poly(PctBachDeg25_Over, 3, raw = T) + PctEmployed16_Over + 
#        PctUnemployed16_Over + PctPrivateCoverage + PctPublicCoverage + 
#        PctPublicCoverageAlone + PctMarriedHouseholds + BirthRate, 
#      data = train_1)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -86.43 -12.14   0.59  12.41 143.53 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               2.459e+02  1.439e+01  17.088  < 2e-16 ***
#   poly(medIncome, 2, raw = T)1             -7.888e-04  2.671e-04  -2.954 0.003174 ** 
#   poly(medIncome, 2, raw = T)2              7.003e-09  2.059e-09   3.401 0.000683 ***
#   PctEmpPrivCoverage.greater.than.meanTRUE  6.900e-01  1.410e+00   0.489 0.624743    
# PercentMarried                            1.293e+00  1.942e-01   6.659 3.45e-11 ***
#   PctHS25_Over                              8.294e-01  1.115e-01   7.442 1.41e-13 ***
#   poly(PctBachDeg25_Over, 3, raw = T)1     -5.511e+00  1.104e+00  -4.994 6.38e-07 ***
#   poly(PctBachDeg25_Over, 3, raw = T)2      2.624e-01  6.338e-02   4.141 3.59e-05 ***
#   poly(PctBachDeg25_Over, 3, raw = T)3     -4.864e-03  1.133e-03  -4.292 1.85e-05 ***
#   PctEmployed16_Over                       -6.216e-01  1.270e-01  -4.896 1.05e-06 ***
#   PctUnemployed16_Over                      8.381e-01  2.111e-01   3.970 7.41e-05 ***
#   PctPrivateCoverage                        5.205e-01  1.244e-01   4.182 3.00e-05 ***
#   PctPublicCoverage                        -1.485e+00  1.967e-01  -7.551 6.26e-14 ***
#   PctPublicCoverageAlone                    2.231e+00  2.829e-01   7.887 4.80e-15 ***
#   PctMarriedHouseholds                     -1.802e+00  1.895e-01  -9.510  < 2e-16 ***
#   BirthRate                                -1.526e+00  2.495e-01  -6.115 1.14e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.38 on 2245 degrees of freedom
# Multiple R-squared:  0.3778,	Adjusted R-squared:  0.3737 
# F-statistic: 90.89 on 15 and 2245 DF,  p-value: < 2.2e-16

# Including with the earlier polynomial terms shows no improvement in R2 or p-value from that model.

###---- Final conclusion : Drop PctEmpPrivCoverage.greater.than.mean from model.

# ---------------------5th Modification - adding majority.race categorical variable

# we feature engineer a categorical variable for majority race of county

train_1$maj_race<-NA
train_1[which(train_1$PctWhite>=train_1$PctBlack&train_1$PctWhite>=(train_1$PctAsian+train_1$PctOtherRace)),'maj_race']<-'White'
train_1[which(train_1$PctBlack>=train_1$PctWhite&train_1$PctBlack>=(train_1$PctAsian+train_1$PctOtherRace)),'maj_race']<-'Black'
train_1[which((train_1$PctAsian+train_1$PctOtherRace)>=train_1$PctWhite&(train_1$PctAsian+train_1$PctOtherRace)>=train_1$PctBlack),'maj_race']<-'Other'

train_1$maj_race<-as.factor(train_1$maj_race)

# Including maj_race in original lm1 model

lm10 = lm(TARGET_deathRate ~ maj_race+PercentMarried+PctHS25_Over+PctBachDeg25_Over+PctEmployed16_Over+PctUnemployed16_Over+
           PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm10)
# Call:
#   lm(formula = TARGET_deathRate ~ maj_race + PercentMarried + PctHS25_Over + 
#        PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + 
#        PctPrivateCoverage + PctPublicCoverage + PctPublicCoverageAlone + 
#        PctMarriedHouseholds + BirthRate, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -81.841 -12.256   0.601  12.328 144.448 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            209.7971    12.8643  16.308  < 2e-16 ***
#   maj_raceOther          -25.2929    12.7149  -1.989   0.0468 *  
#   maj_raceWhite            1.6983     2.7927   0.608   0.5432    
# PercentMarried           1.2859     0.1922   6.692 2.77e-11 ***
#   PctHS25_Over             0.8014     0.1095   7.317 3.52e-13 ***
#   PctBachDeg25_Over       -1.2573     0.1782  -7.057 2.26e-12 ***
#   PctEmployed16_Over      -0.7559     0.1203  -6.286 3.90e-10 ***
#   PctUnemployed16_Over     0.8660     0.2106   4.111 4.08e-05 ***
#   PctPrivateCoverage       0.5711     0.1076   5.309 1.21e-07 ***
#   PctPublicCoverage       -1.6500     0.1849  -8.925  < 2e-16 ***
#   PctPublicCoverageAlone   2.5220     0.2698   9.346  < 2e-16 ***
#   PctMarriedHouseholds    -1.8071     0.1809  -9.989  < 2e-16 ***
#   BirthRate               -1.5874     0.2503  -6.343 2.72e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.49 on 2248 degrees of freedom
# Multiple R-squared:  0.3705,	Adjusted R-squared:  0.3671 
# F-statistic: 110.3 on 12 and 2248 DF,  p-value: < 2.2e-16

# Adjusted R2 shows marginal increase, however the p-value of White race is insignificant.

# Including maj_race in modified model with polynomials

lm11 = lm(TARGET_deathRate ~ maj_race+poly(medIncome,2,raw=T)+PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,3,raw=T)+PctEmployed16_Over+PctUnemployed16_Over+
           PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm11)

# Adjusted R2 shows marginal increase, however the p-values race labels are insignificant.

###---- Final conclusion : Drop maj_race from model.

### Final model -

lm1_v5 = lm(TARGET_deathRate ~ poly(medIncome,2,raw=T)+PercentMarried+PctHS25_Over+poly(PctBachDeg25_Over,3,raw=T)+PctEmployed16_Over+PctUnemployed16_Over+
           PctPrivateCoverage+PctPublicCoverage+PctPublicCoverageAlone+PctMarriedHouseholds+BirthRate , data = train_1)
summary(lm1_v5)
# Call:
#   lm(formula = TARGET_deathRate ~ poly(medIncome, 2, raw = T) + 
#        PercentMarried + PctHS25_Over + poly(PctBachDeg25_Over, 3, 
#                                             raw = T) + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + 
#        PctPublicCoverage + PctPublicCoverageAlone + PctMarriedHouseholds + 
#        BirthRate, data = train_1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -86.481 -12.185   0.646  12.313 143.638 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           2.444e+02  1.402e+01  17.432  < 2e-16 ***
#   poly(medIncome, 2, raw = T)1         -7.711e-04  2.646e-04  -2.915 0.003595 ** 
#   poly(medIncome, 2, raw = T)2          6.889e-09  2.046e-09   3.368 0.000771 ***
#   PercentMarried                        1.284e+00  1.933e-01   6.644 3.82e-11 ***
#   PctHS25_Over                          8.366e-01  1.105e-01   7.572 5.32e-14 ***
#   poly(PctBachDeg25_Over, 3, raw = T)1 -5.526e+00  1.103e+00  -5.010 5.88e-07 ***
#   poly(PctBachDeg25_Over, 3, raw = T)2  2.633e-01  6.335e-02   4.157 3.35e-05 ***
#   poly(PctBachDeg25_Over, 3, raw = T)3 -4.884e-03  1.132e-03  -4.313 1.68e-05 ***
#   PctEmployed16_Over                   -6.183e-01  1.268e-01  -4.878 1.15e-06 ***
#   PctUnemployed16_Over                  8.446e-01  2.106e-01   4.009 6.28e-05 ***
#   PctPrivateCoverage                    5.451e-01  1.137e-01   4.793 1.75e-06 ***
#   PctPublicCoverage                    -1.508e+00  1.912e-01  -7.887 4.78e-15 ***
#   PctPublicCoverageAlone                2.262e+00  2.757e-01   8.203 3.90e-16 ***
#   PctMarriedHouseholds                 -1.800e+00  1.894e-01  -9.501  < 2e-16 ***
#   BirthRate                            -1.531e+00  2.492e-01  -6.141 9.67e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.38 on 2246 degrees of freedom
# Multiple R-squared:  0.3778,	Adjusted R-squared:  0.3739 
# F-statistic:  97.4 on 14 and 2246 DF,  p-value: < 2.2e-16

# Using this model we predict for test data-

yhat5 <- predict(lm1_v5,newdata=test)
mse5 <- mean((yhat5-test$TARGET_deathRate)^2)
mse5
# [1] 463.2646

# MSE has decreased marginally compared to linear model lm1.