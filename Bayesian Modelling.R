# LOAD LIBRARY & DATA ----
library(rjags)
library(tidyverse)

crash = read.csv("./www/crash_sample_02nov.csv")

# covert all categorical vars to factors
for (i in 1:ncol(crash)) {
  if (class(crash[, i]) == "character") {
    crash[, i] = factor(crash[, i])
  }
}

# convert NA to 0 for column pedestrian
crash$pedestrian[is.na(crash$pedestrian)] = 0
crash$minorInjuryCount[is.na(crash$minorInjuryCount)] = 0

# remove Null or Unknown values in columns
crash = crash %>% 
  dplyr::filter(weatherA != "Null") %>% 
  dplyr::filter(light != "Unknown") %>% 
  dplyr::filter(roadType != "Unknown")

crash$weatherA = droplevels(crash$weatherA)
crash$light = droplevels(crash$light)
crash$roadType = droplevels(crash$roadType)

summary(crash)

# TRAIN & TEST ----
# generate random digits
set.seed(42)
which_train = sample(1:nrow(crash), size=nrow(crash)*.8, replace = F)

# slice data
train = crash %>% slice(which_train)
test = crash %>% slice(-which_train)

# MODEL DEV ----
# design matrix
X = model.matrix(
  severeOrFatal ~ weatherA + vehicleType + light + roadType + pedestrian,
  data = train)

X_test = model.matrix(
  severeOrFatal ~ weatherA + vehicleType + light + roadType + pedestrian,
  data = test)

colnames(X)
colnames(X_test)

# LIKELIHOOD data from train
Y = train$severeOrFatal

# STEP 1: define
test_model <- "model{

    # PRIOR
    
    for(j in 1:k){
      beta[j] ~ dnorm(0, 1/1000^2)
    }
    
    # LIKELIHOOD
    
    for (i in 1:N) {
      Y[i] ~ dbinom(p[i],n)
      
      logit(p[i]) <- inprod(X[i,], beta)
    }
}"

# STEP 2: compline

test_jags <- jags.model(
  textConnection(test_model),
  data = list(Y = Y, X = X, N = length(Y), n = 1, k = dim(X)[2]),
  # n.chains= 2,
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)

# STEP 3: simulate
test_sim <- coda.samples(model = test_jags, variable.names = "beta", thin = 5, n.iter = 5000)

# STEP 4: save posterior samples
# posterior <- data.frame(test_sim[[1]])
posterior1000 <- data.frame(test_sim[[1]])
write.csv(posterior1000, "./posterior_sample.csv")

# MODEL PERFORMANCE ----
# TRACE/DENSITY plot
plot(test_sim,trace=F)

# ESS
coda::effectiveSize(posterior1000)

# PROB Distribution 
beta_fun <- function(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, i) {
  prob_logit = 
    b1 * X_test[i, 1] + b2 * X_test[i, 2] + b3 * X_test[i, 3] + 
    b4 * X_test[i, 4] + b5 * X_test[i, 5] + b6 * X_test[i, 6] + 
    b7 * X_test[i, 7] + b8 * X_test[i, 8] + b9 * X_test[i, 9] + 
    b10 * X_test[i, 10] + b11 * X_test[i, 11] + b12 * X_test[i, 12] +
    b13 * X_test[i, 13] + b14 * X_test[i, 14] + b15 * X_test[i, 15] + 
    b16 * X_test[i, 16] + b17 * X_test[i, 17] 
  prob_exp = exp(prob_logit) / (1 + exp(prob_logit))
}

# MEDIAN OF PROB Dist of prob distribution for all test data

result <- vector("numeric", nrow(X_test)) # prepare a container

for (i in 1:nrow(X_test)) {
  median = median(mapply(beta_fun, posterior1000$beta.1., posterior1000$beta.2., posterior1000$beta.3., 
                         posterior1000$beta.4., posterior1000$beta.5., posterior1000$beta.6.,
                         posterior1000$beta.7., posterior1000$beta.8., posterior1000$beta.9., 
                         posterior1000$beta.10., posterior1000$beta.11., posterior1000$beta.12.,
                         posterior1000$beta.13., posterior1000$beta.14., posterior1000$beta.15.,
                         posterior1000$beta.16., posterior1000$beta.17., i))
  result[i] <- median         
}

head(result)

# ASSIGN PREDICTED PROB TO TEST DATA
test$prob_pred <- result

# ASSIGN PREDICTED CLASS TO TEST DATA / threshold 0.6
test = test %>% 
  mutate(class_pred = 
           case_when(prob_pred > 0.6 ~ 1, TRUE ~ 0))

# CONFUSION MATRIX
confusiton_matrix = table(test$class_pred, test$severeOrFatal)

TP = 8
TN = 12858
FP = 21
FN = 875

# Sensitivity
TP/(TP+FN) #  0.009060023

# Specificity
TN/(TN+FP) # 0.9983694

# Accuracy
(TP+TN)/(TP+TN+FP+FN) # 0.9348932
# mean(test$severeOrFatal==test$class_pred)

# 95% CREDIBLE INTERVAL ----

ci95 = as.data.frame(cbind(colnames(posterior1000), colnames(X),
                           round(t(
                             sapply(posterior1000, function(x)
                               quantile(x, probs = c(0.025, 0.5, 0.975)))
                           ), 3)), row.names = F)
colnames(ci95)[c(1, 2)] = c("coefficient", "variable")

# save csv
write.csv(ci95, "./ci95_1000.csv", row.names = F)