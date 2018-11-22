load('train_v2.RData')
train <- train[, names(train) != 'Data']

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
fit <- lm(avgAnnCount ~., data = scaled.train)
summary(fit)

# Now the outliers can be explored initially:

# Calculating Cook's distance
library(car)
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

# Finally looking at dfbetas
df_vals <- influence[["is.inf"]][,1:31]
fin <- apply(df_vals, 1, function(r) any(r != FALSE))
fin.nam <- names(fin[fin == TRUE])
fin.nam
# This returned no influencers

# Finding the values in all 3 lists:
final.influencers <- Reduce(intersect, list(rows.ov,rows.cov,hat.true))
final.influencers

# When we build models we can be sure to be on the lookout for these values
