load('train_v2.RData')
train <- train[, -which(names(train) %in% c('Data', "avgDeathsPerYear", 'avgAnnCount', 'incidenceRate')) ]

# First the data needs to be scaled. Applying unit-normal scaling:
categoricals <- train[, c('binnedInc', 'PctEmpPrivCoverage.greater.than.mean')]
train <- train[, -which(names(train) %in% c('binnedInc', 'PctEmpPrivCoverage.greater.than.mean'))]
n <- dim(train)[1]
k <- dim(train)[2]
tr.names <- names(train)

# Setting up a matrix to hold scaled values, column means, and column sum of squares
X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
x.bar.list <- numeric(length=k)
css.list <- numeric(length=k)

# Itterating through train data and scaling
for (j in 1:k) {
  x.bar.list[j] <- mean(train[,j])
  xj.cent <- train[,j] - x.bar.list[j]
  css.list[j] <- sum(xj.cent^2)
  X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
}

# Assembling scaled data and categoricals
scaled.train <- as.data.frame(X.mat.scl)
names(scaled.train) <- tr.names

# Running multiple linear regression on this data
fit <- lm(TARGET_deathRate ~. -1, data = scaled.train)
summary(fit)

# Now the outliers can be explored initially:

# Calculating Cook's distance
cook <- cooks.distance(fit)
names(cook[cook >= 1])
# No values appear in this list

# Grabbing the diffits
train.inf <- dffits(fit)

# Finding cutoff value
n <- dim(scaled.train)[1]
p <- length(coefficients(fit))
cut.inf <- 2*sqrt(p/n)

# Finding values over the cutoff
dif.ov <- cbind(train.inf, abs(train.inf) > cut.inf)
rows.ov <- row.names(dif.ov[which(dif.ov[,2] == 1),])
rows.ov

# Grabbing cov
train.inf <- covratio(fit)

# Finding the cutoff
n <- dim(scaled.train)[1]
p <- length(coefficients(fit))
cut.inf.lo <- 1 - 3*p/n
cut.inf.hi <- 1 + 3*p/n

# Tracking values over the cutoff
cov.inf <- cbind(train.inf, (train.inf < cut.inf.lo) | (train.inf > cut.inf.hi))
rows.cov <- row.names(cov.inf[which(cov.inf[,2] == 1),])
rows.cov

# In case we missed anything, grab other values:
# Hat values:
influence <- influence.measures(fit)
hat <- influence[["is.inf"]][,'hat']
hat.true <- names(hat[hat == TRUE])
hat.true

# Looking at dfbetas
df_vals <- influence[["is.inf"]][,1:27]
fin <- apply(df_vals, 1, function(r) any(r != FALSE))
fin.nam <- names(fin[fin == TRUE])
fin.nam
# Only returns one value

# Finding the values in all 3 lists:
final.influencers <- Reduce(intersect, list(rows.ov,rows.cov,hat.true))
final.influencers

# When we build models we can be sure to be on the lookout for these values
# "95"   "103"  "226"  "303"  "526"  "543"  "649"  "777"  "998"  "1094" "1164" "1194" "1209" "1240" "1268" "1297"
# "1403" "1477" "1490" "1576" "1607" "1718" "1720" "1799" "1809" "1971" "1974" "2043" "2125" "2140" "2235"

# Now Looking at the model with less variables
names10 <- c("PercentMarried", "PctHS25_Over", "PctBachDeg25_Over","PctEmployed16_Over", "PctUnemployed16_Over",
  "PctPrivateCoverage", "PctPublicCoverage", "PctPublicCoverageAlone", "PctMarriedHouseholds","BirthRate") 


names6 <- c("MedianAgeFemale","PctHS25_Over", "PctBachDeg25_Over", "PctUnemployed16_Over", "PctMarriedHouseholds","BirthRate")  


names5 <- c("PctHS25_Over", "PctBachDeg25_Over", "PctUnemployed16_Over", "PctMarriedHouseholds", "BirthRate")   

names4 <- c("PctHS25_Over", "PctBachDeg25_Over", "PctUnemployed16_Over",  "PctMarriedHouseholds")

y<- 0
final.influencers <- list()
fin.name <- list()
for (i in c(10,6,5,4)){
  y <- y +1
  name <- get(paste0('names', i))
  this.train <- train[, which(names(train) %in% c(name, "TARGET_deathRate"))]
  n <- dim(this.train)[1]
  k <- dim(this.train)[2]
  tr.names <- names(this.train)
  
  # Setting up a matrix to hold scaled values, column means, and column sum of squares
  X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
  x.bar.list <- numeric(length=k)
  css.list <- numeric(length=k)
  
  # Itterating through this.train data and scaling
  for (j in 1:k) {
    x.bar.list[j] <- mean(this.train[,j])
    xj.cent <- this.train[,j] - x.bar.list[j]
    css.list[j] <- sum(xj.cent^2)
    X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
  }
  
  # Assembling scaled data and categoricals
  scaled.this.train <- as.data.frame(X.mat.scl)
  names(scaled.this.train) <- tr.names
  
  # Running multiple linear regression on this data
  fit <- lm(TARGET_deathRate ~.-1, data = scaled.this.train)
  summary(fit)
  
  # Now the outliers can be explored initially:
  
  # Calculating Cook's distance
  cook <- cooks.distance(fit)
  print(names(cook[cook >= 1]))
  # No values appear in this list
  
  # Grabbing the diffits
  this.train.inf <- dffits(fit)
  
  # Finding cutoff value
  n <- dim(scaled.this.train)[1]
  p <- length(coefficients(fit))
  cut.inf <- 2*sqrt(p/n)
  
  # Finding values over the cutoff
  dif.ov <- cbind(this.train.inf, abs(this.train.inf) > cut.inf)
  rows.ov <- row.names(dif.ov[which(dif.ov[,2] == 1),])
  rows.ov
  
  # Grabbing cov
  this.train.inf <- covratio(fit)
  
  # Finding the cutoff
  n <- dim(scaled.this.train)[1]
  p <- length(coefficients(fit))
  cut.inf.lo <- 1 - 3*p/n
  cut.inf.hi <- 1 + 3*p/n
  
  # Tracking values over the cutoff
  cov.inf <- cbind(this.train.inf, (this.train.inf < cut.inf.lo) | (this.train.inf > cut.inf.hi))
  rows.cov <- row.names(cov.inf[which(cov.inf[,2] == 1),])
  rows.cov
  
  # In case we missed anything, grab other values:
  # Hat values:
  influence <- influence.measures(fit)
  hat <- influence[["is.inf"]][,'hat']
  hat.true <- names(hat[hat == TRUE])
  hat.true
  
  # Finally looking at dfbetas
  df_vals <- influence[["is.inf"]][,1:i]
  fin <- apply(df_vals, 1, function(r) any(r != FALSE))
  fin.nam.this <- names(fin[fin == TRUE])
  print(fin.nam.this)

  # Finding the values in all 3 lists:
  final.influencers[[y]] <- Reduce(intersect, list(rows.ov,rows.cov,hat.true))
}
# Printing out dfbetas and cooks d to verify there's no influence points as usual
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)

# names10
final.influencers[[1]]
# [1] "53"   "441"  "740"  "757"  "1024" "1098" "1164" "1268" "1390" "1403" "1490" "1532" "1607" "1720" "1728" "1783"
#[17] "1789" "1799" "1850" "1961" "1971" "2043" "2094" "2230"

# names6
final.influencers[[2]]
# [1] "34"   "53"   "460"  "688"  "788"  "1268" "1390" "1403" "1596" "1679" "1728" "1737" "1786" "1961" "1974" "1976"
# [17] "1979" "2002" "2043" "2053" "2230" "2235"

# names5
final.influencers[[3]]
# [1] "53"   "460"  "683"  "788"  "916"  "957"  "1169" "1268" "1390" "1403" "1596" "1728" "1737" "1786" "1961" "1974"
# [17] "1979" "2002" "2043" "2053" "2220" "2230" "2235"

# names4
final.influencers[[4]]
#  [1] "67"   "200"  "460"  "504"  "683"  "757"  "788"  "916"  "957"  "1164" "1169" "1268" "1390" "1462" "1464" "1596"
# [17] "1728" "1737" "1799" "1979" "2002" "2043" "2053"