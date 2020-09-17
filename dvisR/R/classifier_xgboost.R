# Need some cleanup to handle NAs
classifier_xgboost_closure <- function(nround = 10, ...){
 function(data, label){
  xgboost::xgboost(data = data, label = label, nround = nround, objective = "binary:logistic", ...)
 }
}

# returns on the probability scale (between 0 and 1), and NOT discretized
predict_xgboost_wrapper <- function(classifier, newdata){
 stats::predict(classifier, newdata = newdata)
}