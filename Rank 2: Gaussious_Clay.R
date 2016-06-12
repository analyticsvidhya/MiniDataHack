library(Hmisc)
library(xgboost)
library(SuperLearner)
library(Matrix)
library(data.table)
library(ROCR)
library(h2o)
library(h2oEnsemble)
library(DMwR)
library(mlr)

train = read.csv("train.csv")
train=train[c(-587,-298),]
test = read.csv("test.csv")

train$Year = ifelse(train$Year==2007,0.5,0.75)
test$Year = 1

train$SaleMonth = ifelse(train$Month==12,1,0)
train$SaleMonth = ifelse(train$Month==11,1,0)
train$SaleMonth = ifelse(train$Month==10,1,0)

test$SaleMonth = ifelse(test$Month==12,1,0)
test$SaleMonth = ifelse(test$Month==11,1,0)
test$SaleMonth = ifelse(test$Month==10,1,0)
train$Date = as.factor(train$Date)
train$Month = as.factor(train$Month)

test$Date = as.factor(test$Date)
test$Month = as.factor(test$Month)
test$Number_SKU_Sold = 0

## create mlr task and convert factors to dummy features
trainTask = makeRegrTask(data = train, target = "Number_SKU_Sold")
trainTask = createDummyFeatures(trainTask)
testTask = makeRegrTask(data = test, target = "Number_SKU_Sold")
testTask = createDummyFeatures(testTask)

## create mlr learner
set.seed(123)
lrn = makeLearner("regr.xgboost")
lrn$par.vals = list(
  print.every.n       = 500,
  objective           = "reg:linear"
)


# 1) Define the set of parameters you want to tune
ps = makeParamSet(
  makeIntegerParam("nthread",lower=8,upper=15),
  makeIntegerParam("nrounds",lower=3000,upper=5000),
  makeIntegerParam("max_depth",lower=6,upper=12),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("lambda_bias",lower=0.70,upper=0.75),
  makeNumericParam("gamma",lower=0,upper=1),
  makeNumericParam("eta", lower = 0.010, upper = 0.015),
  makeNumericParam("colsample_bytree", lower = 0.65,upper=0.70),
  makeNumericParam("subsample", lower = 0.75, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=20)
)
# # 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 3L)
# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 1)
# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK
res = tuneParams(lrn, task = trainTask, resampling = rdesc, par.set = ps, control = ctrl, measures = rmse)
res
# # 5) set the optimal hyperparameter
lrn = setHyperPars(lrn, par.vals = res$x)

# perform crossvalidation in parallel
cv = crossval(lrn, trainTask, iter = 5, measures = rmse, show.info = TRUE)

## now train the model on all training data
tr = train(lrn, trainTask)

## predict using the optimal cut-points 
pred = predict(tr, testTask)


## create submission file
sub = 
imp = xgb.importance(feature_names = y,model=tr$learner.model)
