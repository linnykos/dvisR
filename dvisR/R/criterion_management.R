.grab_functions_in_package <- function(package_name = "dvisR",
                                       function_starter = "criterion"){
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