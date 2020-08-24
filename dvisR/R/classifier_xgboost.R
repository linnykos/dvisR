classifier_xgboost_closure <- function(nround = 10, ...){
 function(data, label){
  xgboost::xgboost(data = data, label = label, nround = nround, objective = "binary:logistic", ...)
 }
}