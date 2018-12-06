library(dplyr)
library(glmnet)


##################### CUSTOM FUNCTIONS

ridge.reg.coefficients <- function(y.vect, X0.mat, plot=TRUE, grid.size=25, grid.st=0.001, grid.fn=0.5) {
  # Collect parameters
  n <- dim(X0.mat)[1]
  k <- dim(X0.mat)[2]
  p <- k + 1
  # Unit-length scaling
  y.bar <- mean(y.vect)
  y.cent <- y.vect - y.bar
  SS.T <- sum(y.cent^2)
  y.vect.scl <- y.vect / sqrt(SS.T)
  X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
  x.bar.list <- numeric(length=k)
  css.list <- numeric(length=k)
  for (j in 1:k) {
    x.bar.list[j] <- mean(X0.mat[,j])
    xj.cent <- X0.mat[,j] - x.bar.list[j]
    css.list[j] <- sum(xj.cent^2)
    X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
  }
  # Calculate ridge trace diagram
  ridge.k.grid <- exp(seq(from=log(grid.st), to=log(grid.fn), length.out=grid.size))
  b.hat.R.scl.list <- matrix(data=0, nrow=k, ncol=grid.size)
  y.vect.aug <- rbind(y.vect.scl, matrix(data=0, nrow=k, ncol=1))
  for (iVAL in 1:grid.size) {
    ridge.k.val <- ridge.k.grid[iVAL]
    X.mat.aug <- rbind(X.mat.scl, sqrt(ridge.k.val)*diag(k))
    XpX.mat.aug <- t(X.mat.aug) %*% X.mat.aug
    Xpy.mat.aug <- t(X.mat.aug) %*% y.vect.aug
    XpX.inv.aug <- solve(XpX.mat.aug)
    b.hat.R.scl.list[,iVAL] <- XpX.inv.aug %*% Xpy.mat.aug
  }
  if (plot) {
    plot(ridge.k.grid, rep(x=0, times=grid.size), pch=3, cex=1, ylim=c(min(b.hat.R.scl.list), max(b.hat.R.scl.list)), xlab="ridge constant, k", ylab="fitted ridge regression coefficient", main = "Ridge trace diagram")
    abline(h=0, lty=1, lwd=1)
    for (j in 1:k) {
      lines(ridge.k.grid, b.hat.R.scl.list[j,], type="l", lty=1, lwd=3)
    }
  }
  # Convert to the original scale and calculate MS.Res and R2.
  X.mat <- as.matrix(cbind(rep(x=1, times=n), X0.mat))
  b.hat.R.list <- matrix(data=0, nrow=p, ncol=grid.size)
  SS.Res.list <- numeric(length=grid.size)
  R2.list <- numeric(length=grid.size)
  for (iVAL in 1:grid.size) {
    b.hat.R.list[1,iVAL] <- y.bar
    for (j in 1:k) {
      b.hat.R.list[j+1,iVAL] <- b.hat.R.scl.list[j,iVAL] / sqrt(css.list[j] / SS.T)
      b.hat.R.list[1,iVAL] <- b.hat.R.list[1,iVAL] - b.hat.R.list[j+1,iVAL]*x.bar.list[j]
    }
    SS.Res.list[iVAL] <- sum((y.vect - X.mat %*% b.hat.R.list[,iVAL])^2)
    R2.list[iVAL] <- 1 - SS.Res.list[iVAL] / SS.T
  }
  MS.Res.list <- SS.Res.list / (n-p)
  out.list <- list(ridge.k.grid=ridge.k.grid, b.hat.R.list=b.hat.R.list, MS.Res.list=MS.Res.list, R2.list=R2.list)
  return(out.list)
}

prin.comp.coefficients <- function(y.vect, X0.mat) {
  # Collect parameters
  n <- dim(X0.mat)[1]
  k <- dim(X0.mat)[2]
  p <- k + 1
  # Unit-length scaling
  y.bar <- mean(y.vect)
  y.cent <- y.vect - y.bar
  SS.T <- sum(y.cent^2)
  y.vect.scl <- y.vect / sqrt(SS.T)
  X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
  x.bar.list <- numeric(length=k)
  css.list <- numeric(length=k)
  for (j in 1:k) {
    x.bar.list[j] <- mean(X0.mat[,j])
    xj.cent <- X0.mat[,j] - x.bar.list[j]
    css.list[j] <- sum(xj.cent^2)
    X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
  }
  # Calculate principal components and coefficient estimates
  XpX.mat.scl <- t(X.mat.scl) %*% X.mat.scl
  eig.out <- eigen(XpX.mat.scl)
  Lambda.mat <- diag(eig.out$values)
  Lambda.inv <- diag(1/eig.out$values)
  T.mat <- eig.out$vectors
  Z.mat <- X.mat.scl %*% T.mat
  Zpy.mat <- t(Z.mat) %*% y.vect.scl
  a.hat.scl <- Lambda.inv %*% Zpy.mat[1:j,]
  a.hat.PC.scl.list <- matrix(data=0, nrow=k, ncol=k)
  b.hat.PC.scl.list <- matrix(data=0, nrow=k, ncol=k)
  for (j in 1:k) {
    a.hat.PC.scl.list[1:j,j] <- a.hat.scl[1:j,]
    b.hat.PC.scl.list[,j] <- T.mat %*% a.hat.PC.scl.list[,j]
  }
  # Convert to the original scale and calculate MS.Res and R2.
  X.mat <- as.matrix(cbind(rep(x=1, times=n), X0.mat))
  grid.size <- dim(b.hat.PC.scl.list)[2]
  b.hat.PC.list <- matrix(data=0, nrow=p, ncol=grid.size)
  SS.Res.list <- numeric(length=grid.size)
  R2.list <- numeric(length=grid.size)
  for (iVAL in 1:grid.size) {
    b.hat.PC.list[1,iVAL] <- y.bar
    for (j in 1:k) {
      b.hat.PC.list[j+1,iVAL] <- b.hat.PC.scl.list[j,iVAL] / sqrt(css.list[j] / SS.T)
      b.hat.PC.list[1,iVAL] <- b.hat.PC.list[1,iVAL] - b.hat.PC.list[j+1,iVAL]*x.bar.list[j]
    }
    SS.Res.list[iVAL] <- sum((y.vect - X.mat %*% b.hat.PC.list[,iVAL])^2)
    R2.list[iVAL] <- 1 - SS.Res.list[iVAL] / SS.T
  }
  MS.Res.list <- SS.Res.list / (n-p)
  out.list <- list(b.hat.PC.list=b.hat.PC.list, MS.Res.list=MS.Res.list, R2.list=R2.list)
  return(out.list)
}

#####################

load('train_v2.RData')
load('test_v2.RData')

# 9 features selected through variable selection
features = c("PercentMarried","PctHS25_Over","PctBachDeg25_Over","PctEmployed16_Over","PctUnemployed16_Over","PctPrivateCoverage","PctPublicCoverage","PctPublicCoverageAlone","PctMarriedHouseholds" )
all.features = train %>% names()
all.features = all.features[4:length(all.features)]
all.features = all.features[-which(all.features %in% c('Data'))]

# defining X matrix
X0.mat = train[features] %>% as.matrix()
n <- dim(X0.mat)[1]
k <- dim(X0.mat)[2]
p <- k + 1

y.vect <- train$TARGET_deathRate %>% as.matrix()

############## Using glmnet function

cancer.ridge <- glmnet(X0.mat, y.vect, alpha=0)
plot(cancer.ridge, xvar="lambda", label=TRUE)

"
########################## Observations
Estimates appear to stabilize around log lambda value of 5
##########################
"

######################### All Features

categorical.features <- c('PctEmpPrivCoverage.greater.than.mean','binnedInc')

train$PctEmpPrivCoverage.greater.than.mean <- train$PctEmpPrivCoverage.greater.than.mean %>% as.numeric()
train$binnedInc <- train$binnedInc %>% as.numeric()

# defining X matrix
X0.mat = train[all.features] %>% as.matrix()
n <- dim(X0.mat)[1]
k <- dim(X0.mat)[2]
p <- k + 1

y.vect <- train$TARGET_deathRate %>% as.matrix()





############## Using custom defined function

out.list <- ridge.reg.coefficients(y.vect, X0.mat,grid.fn = 10)
names(out.list)
out.list$ridge.k.grid
out.list$b.hat.R.list
out.list$MS.Res.list
# [1] 381.3595 381.3658 381.3784 381.4036 381.4534 381.5485 381.7233 382.0290 382.5339 383.3162 384.4536 386.0198 388.1100 390.9073
# [15] 394.7762 400.3404 408.4804 420.1914 436.2915 457.0878 482.2129 510.7908 541.8131 574.3669 607.5050
out.list$R2.list
# [1] 0.5138853 0.5138773 0.5138613 0.5138291 0.5137657 0.5136444 0.5134216 0.5130319 0.5123884 0.5113912 0.5099413 0.5079449
# [13] 0.5052805 0.5017149 0.4967832 0.4896906 0.4793146 0.4643867 0.4438641 0.4173553 0.3853287 0.3489007 0.3093569 0.2678610
# [25] 0.2256202

R2.max = out.list$R2.list %>% max()
MS.Res.min = out.list$MS.Res.list %>% min()

R2.max.index = out.list$R2.list %>% match(R2.max)
R2.max.index
# 1

MS.Res.min.index = out.list$MS.Res.list %>% match(MS.Res.min)
R2.max.index
# 1

X.mat <- cbind(rep(x=1, times=n), X0.mat)

# Test of significance
y.hat = X.mat %*% (out.list$b.hat.R.list[,1] %>% as.matrix())



df.Res <- n - p
df.R <- k

ypy.val <- as.numeric(t(y.vect) %*% y.vect)
yhatpy <- as.numeric(t(y.hat) %*% y.vect)

SS.R = yhatpy - sum(y.vect)^2 / n

MS.R <- SS.R / df.R

SS.Res <- ypy.val - yhatpy
MS.Res <- SS.Res / df.Res

F0 <- MS.R / MS.Res

alpha <- 0.01
F.crit <- qf(alpha, df1=df.R, df2=df.Res, lower.tail=FALSE)
F.crit
# [1] 1.705025

p.val <- pf(F0, df1=df.R, df2=df.Res, lower.tail=FALSE)
p.val


# Validation on test data

test$PctEmpPrivCoverage.greater.than.mean <- test$PctEmpPrivCoverage.greater.than.mean %>% as.numeric()
test$binnedInc <- test$binnedInc %>% as.numeric()

X0.test.mat = test[all.features] %>% as.matrix()
n <- dim(X0.test.mat)[1]
k <- dim(X0.test.mat)[2]
p <- k + 1
X.test.mat = cbind(rep(x=1, times=n), X0.test.mat)

y.test.hat = X.test.mat %*% (out.list$b.hat.R.list[,1] %>% as.matrix())

(y.test.hat - test$TARGET_deathRate) ^ 2 %>% mean()


############# Principal Component Regression


out.list <- prin.comp.coefficients(y.vect, X0.mat)
names(out.list)
out.list$b.hat.PC.list
out.list$MS.Res.list
out.list$R2.list


# Test of significance
y.hat = X.mat %*% (out.list$b.hat.PC.list[,30] %>% as.matrix())



df.Res <- n - p
df.R <- k

ypy.val <- as.numeric(t(y.vect) %*% y.vect)
yhatpy <- as.numeric(t(y.hat) %*% y.vect)

SS.R = yhatpy - sum(y.vect)^2 / n

MS.R <- SS.R / df.R

SS.Res <- ypy.val - yhatpy
MS.Res <- SS.Res / df.Res

F0 <- MS.R / MS.Res

alpha <- 0.01
F.crit <- qf(alpha, df1=df.R, df2=df.Res, lower.tail=FALSE)
F.crit
# [1] 1.705025

p.val <- pf(F0, df1=df.R, df2=df.Res, lower.tail=FALSE)
p.val