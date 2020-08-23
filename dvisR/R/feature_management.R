# grab all the feaature functions
.grab_functions_in_package <- function(package_name = "dvisR",
                                       function_starter = "feature"){
 if(!isNamespaceLoaded(package_name)) stop(paste(package_name, "is not",
                                                 "currently loaded"))
 
 all.obj <- ls(paste0("package:", package_name))
 
 fun <- grep(paste0("^", function_starter, "_*"), all.obj, value = T)
 
 lis <- lapply(fun, function(x){
  eval(parse(text = paste0(package_name, "::", x)))
 })
 names(lis) <- fun
 
 lis
}

# extract pairs of columns in pairs, and then apply the features
.extract_features <- function(dat, pairs, feature_list){
  mat_new <- apply(pairs, 2, function(x){
    dat_2col <- cbind(dat[,x[1]], dat[,x[2]])
    
    .apply_feature_list(dat_2col, feature_list)
  })
  
  t(mat_new)
}

.apply_feature_list <- function(dat_2col, feature_list){
  res_mat <- sapply(feature_list, function(feature_func){
    feature_func(mat)
  })

  names(res) <- names(feature_list)
  res
}
