

################################################################
# Functions
################################################################

get.model.str <- function(var.in, resp.name, reg.names) {
  var.in.idx <- which(var.in)
  model.str <- paste(resp.name, "~")
  first.in <- TRUE
  for (iVAR in var.in.idx) {
    if (first.in) {
      model.str <- paste(model.str, reg.names[iVAR])
      first.in <- FALSE
    } else {
      model.str <- paste(model.str, "+", reg.names[iVAR])
    }
  }
  return(model.str)
}

eval.lm <- function(model.str, data.name) {
  lm.call.str <- paste("reg.lm <- lm(", model.str, ", data=", data.name, ")")
  eval(parse(text=lm.call.str))
  return(reg.lm)
}

forward.step <- function(curr.var.in, alpha.in, resp.name, reg.names, data.name) {
  curr.var.out.idx <- which(!curr.var.in)
  enter.idx <- NA
  if (length(curr.var.out.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=Inf, times=k)
    for (iVAR in curr.var.out.idx) {
      cand.var.in <- curr.var.in
      cand.var.in[iVAR] <- TRUE
      cand.model.str <- get.model.str(cand.var.in, resp.name, reg.names)
      cand.model.lm <- eval.lm(cand.model.str, data.name)
      iROW <- which(row.names(summary(cand.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(cand.model.lm)$coefficients[iROW,4]
    }
    enter.idx <- which.min(pval.seq)
    if (pval.seq[enter.idx] < alpha.in) {
      print(paste("Variable ", reg.names[enter.idx], " enters the model (pval=", sprintf("%6.4f", pval.seq[enter.idx]), ")", sep=""))
    } else {
      print("No variables enter the model")
      enter.idx <- NA
    }
  } else {
    print("No variables available to enter the model")
  }
  return(enter.idx)
}

backward.step <- function(curr.var.in, alpha.out, resp.name, reg.names, data.name) {
  curr.var.in.idx <- which(curr.var.in)
  leave.idx <- NA
  if (length(curr.var.in.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=-Inf, times=k)
    curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
    curr.model.lm <- eval.lm(curr.model.str, data.name)
    for (iVAR in curr.var.in.idx) {
      iROW <- which(row.names(summary(curr.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(curr.model.lm)$coefficients[iROW,4]
    }
    leave.idx <- which.max(pval.seq)
    if (pval.seq[leave.idx] >= alpha.out) {
      print(paste("Variable ", reg.names[leave.idx], " leaves the model (pval=", sprintf("%6.4f", pval.seq[leave.idx]), ")", sep=""))
    } else {
      print("No variables leave the model")
      leave.idx <- NA
    }
  } else {
    print("No variables available to leave the model")
  }
  return(leave.idx)
}

forward.selection <- function(alpha.in, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=FALSE, times=k)
  stop <- FALSE
  while(!stop) {
    enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
    if (is.na(enter.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[enter.idx] <- TRUE
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

backward.elimination <- function(alpha.out, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=TRUE, times=k)
  stop <- FALSE
  while(!stop) {
    leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
    if (is.na(leave.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[leave.idx] <- FALSE
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

stepwise.selection <- function(alpha.in, alpha.out, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=FALSE, times=k)
  stop <- FALSE
  while(!stop) {
    enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
    if (is.na(enter.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[enter.idx] <- TRUE
      leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
      if (!is.na(leave.idx)) {
        curr.var.in[leave.idx] <- FALSE
        if (leave.idx == enter.idx) {
          stop <- TRUE
        }
      }
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

####################################################################



# Read in cleaned data
df = read.csv("train_v2.csv", sep = " ")
df = df[, -which(names(df) %in% c('Data', 'studyPerCap','binnedInc'))]
df$PctEmpPrivCoverage.greater.than.mean = as.numeric(df$PctEmpPrivCoverage.greater.than.mean)


# Fit linear model to all regressors to get an initial idea
full.lm = lm(TARGET_deathRate ~ ., data = df)
summary(full.lm)

# Certain variables like MedianAge (which we would expect to have
# some effect on our response) shockingly have high p-values,
# so we need to investigate further.



#######################
# Backward elimination
#######################

resp.name <- "TARGET_deathRate"
reg.names <- names(df)[-which(names(df) %in% c('incidenceRate', 'avgAnnCount', 'avgDeathsPerYear', 'TARGET_deathRate'))]
data.name <- "df"
alpha.out <- 0.10

death.lm.back <- backward.elimination(alpha.out, resp.name, reg.names, data.name)
summary(death.lm.back)


#######################
# Forward selection
#######################

alpha.in = 0.25

death.lm.forw = forward.selection(alpha.in, resp.name, reg.names, data.name)
summary(death.lm.forw)


#######################
# Stepwise regression
#######################

death.lm.step = stepwise.selection(alpha.in, alpha.out, resp.name, reg.names, data.name)
summary(death.lm.forw)


#############################
# Compare significant variables from step
death.step.names = row.names(summary(death.lm.step)$coefficients)
death.back.names = row.names(summary(death.lm.back)$coefficients)
death.forw.names = row.names(summary(death.lm.forw)$coefficients)

# Check what overlaps between them
names.overlap = Reduce(intersect, list(death.back.names,death.forw.names,death.step.names))[-1]

# Run a quick lm on those variables
names.forlm = paste(names.overlap, collapse="+")
red.lm = lm(paste("TARGET_deathRate ~ ", names.forlm, sep=""), data = df)
summary(red.lm)

# Check on R-Squared values
summary(death.lm.back)$adj.r.squared
summary(death.lm.forw)$adj.r.squared
summary(death.lm.step)$adj.r.squared


# We choose to drop PctBlack and PctOtherRace from our list of
# variables because PctBlack didn't perform quite so well in 
# some of our stepwise models, and we think that basing a prediction
# on a regressor representing "otherness" is dangerous and likely
# to lead to errors on unseen data.
names.overlap = names.overlap[! names.overlap %in% c('PctBlack', 'PctOtherRace')]


##################################################
# Try regsubsets

# Get only variables from main dataframe which we found with stepwise models
df.red = df[,c(names.overlap, "TARGET_deathRate")]

# Import packages
library(leaps)

# Use regsubsets to get an initial idea of which variables are important
n <- dim(df.red)[1]
k <- dim(df.red)[2] - 1 
p <- k + 1
df.vs <- regsubsets(TARGET_deathRate ~ ., data=df.red, nbest=1, nvmax = 15)
df.subsets = summary(df.vs)


# Check on stats
R2.seq <- summary(df.vs)$rsq
adj.R2.seq <- summary(df.vs)$adjr2
Cp.seq <- summary(df.vs)$cp
BIC.seq <- summary(df.vs)$bic
SS.Res.seq <- summary(df.vs)$rss
k.seq <- as.numeric(row.names(summary(df.vs)$which))
p.seq <- k.seq + 1
MS.Res.seq <- SS.Res.seq / (n - p.seq)
AIC.seq <- n*log(SS.Res.seq / n) + 2*p.seq

# The results are displayed in tabular form as follows
disp.submod <- cbind(p.seq, SS.Res.seq, R2.seq, adj.R2.seq, MS.Res.seq, Cp.seq, AIC.seq, BIC.seq)
col.names <- c("p", "SS.Res", "R2", "adj.R2", "MS.Res", "Cp", "AIC", "BIC")
dimnames(disp.submod) <- list(NULL, col.names)
cbind(summary(df.vs)$coefficients, disp.submod)


# We look at the statistics and see that we prefer the models with
# 4, 5, 6, and 10 variables

# So we store the names of the variable for each model
# for ease of use in other scripts
names10.tf = df.subsets$which[10,-1]
names10 = names(which(names10.tf))
names10
# [1] "PercentMarried"         "PctHS25_Over"           "PctBachDeg25_Over"      "PctEmployed16_Over"     "PctUnemployed16_Over"  
# [6] "PctPrivateCoverage"     "PctPublicCoverage"      "PctPublicCoverageAlone" "PctMarriedHouseholds"   "BirthRate"  


names6.tf = df.subsets$which[6,-1]
names6 = names(which(names6.tf))
names6
# [1] "MedianAgeFemale"      "PctHS25_Over"         "PctBachDeg25_Over"    "PctUnemployed16_Over" "PctMarriedHouseholds"
# [6] "BirthRate"  


names5.tf = df.subsets$which[5,-1]
names5 = names(which(names5.tf))
names5
# [1] "PctHS25_Over"         "PctBachDeg25_Over"    "PctUnemployed16_Over" "PctMarriedHouseholds" "BirthRate"    

names4.tf =df.subsets$which[4,-1]
names4 = names(which(names4.tf))
names4
# [1] "PctHS25_Over"         "PctBachDeg25_Over"    "PctUnemployed16_Over" "PctMarriedHouseholds"