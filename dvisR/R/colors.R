col_palette_default <- function(input = NA){
 df <- .color_df()
 if(all(is.na(input))) input <- nrow(df)
 vec <- NA
 
 if(all(is.numeric(input)) & length(input) == 1){
  if(input > nrow(df)){
   vec <- rep(df$col, times = ceiling(input/nrow(df)))[1:input]
  } else {
   vec <- df$col[1:input]
  }
  
 } else if(all(is.character(input)) & all(input %in% df$name)){
   vec <- sapply(input, function(x){df$col[which(df$name == x)]})
 } else {
  stop()
 }
 
 vec
}

## colors from destiny package: https://github.com/theislab/destiny/blob/master/R/aaa.r
.color_df <- function(){
 col_vec <- c('#8DD3C7', '#FFED6F', '#BEBADA', '#FB8072', '#80B1D3', 
              '#FDB462', '#B3DE69', '#BC80BD', '#FCCDE5', '#D9D9D9', 
              '#CCEBC5', '#FFFFB3', '#F2EDDA', '#000000', '#FFFFFF')
 col_name <- c('teal', 'yellow', 'purple', 'red', 'blue', 
               'orange', 'green', 'magenta', 'pink', 'gray',
               'lightgreen', 'lightyellow', 'beige', 'black', 'white')
 
 df <- data.frame(col = col_vec, name = col_name)
 df
}

