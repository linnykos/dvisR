print.dvisR <- function(x, verbose = 0, ...){
 if(verbose != 3){
  cat("dvisR system output object.\n----------\n")
  cat("Applied to dataset containing ", x$dim["n"], " samples and ", x$dim["p"], "dimensions after labeling ",
      ncol(x$df), "pairs.\n\n")
  cat(sprintf("Plots labeled with pattern of interest:         %d\n", length(which(x$df$Response == 1))))
  cat(sprintf("Plots labeled without pattern of interest: %d\n", length(which(x$df$Response == 0))))
  cat(sprintf("Plots unlabeled: %d\n", sum(is.na(x$df$Response))))
  cat("\n--------\n")
  cat("Names within object:\n")
  print(names(x))
  
  if(verbose == 1){
   cat("\n--------\n")
   cat("Partial head of df:\n")
   print(head(x$df[,1:min(ncol(x$df), 6)]))
  } else if(verbose == 2){
   cat("\n--------\n")
   cat("Full head of df:\n")
   print(head(x$df))
  }
 } else {
  sapply(x, print, verbose = verbose)
 }
}

plot.dvisR <- function(x, mode = "embedding", verbose = 1, ...){
 if(mode == "embedding"){
  if(verbose == 1) message("Use the dvisR_embedding function for more control")
  dvisR_embedding(x)
 }
 
 invisible()
}