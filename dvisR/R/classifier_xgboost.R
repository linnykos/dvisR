# Need some cleanup to handle NAs
.xgboost_learn <- function(data, label){
  xgboost::xgboost(data = data, label = label, nround = 10, objective = "binary:logistic", 
                   verbose = 0)
}

# returns on the probability scale (between 0 and 1), and NOT discretized
.xgboost_predict <- function(classifier, newdata){
 stats::predict(classifier, newdata = newdata)
}

# make a function to generate and isvalid classifier objs
classifier_xgboost <- structure(
 list(learn = .xgboost_learn, 
      predict = .xgboost_predict),
 class = "dvisR_classifier"
)
