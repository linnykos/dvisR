plotting_options_default <- function(color_palette = col_palette_default("black"), 
                                     axis_labels = T, first_phase_gridsize = c(3,3),
                                     first_mar = c(4,4,4,0.5), second_mar = c(4,4,4,0.5),
                                     first_subsampling_options = list(NA),
                                     second_subsampling_options = list(NA)){
 
 .check_plotting_options(color_palette, axis_labels, first_phase_gridsize,
                         first_mar, second_mar,
                         first_subsampling_options,
                         second_subsampling_options)
 
 list(color_palette = color_palette, axis_labels = axis_labels, first_phase_gridsize = first_phase_gridsize,
      first_mar = first_mar, second_mar = second_mar,
      first_subsampling_options = first_subsampling_options,
      second_subsampling_options = second_subsampling_options)
}

plotting_module_base <- function(x, y, xlab, ylab, main, col,
                                 ordering = NA,
                                 xlim = range(x), ylim = range(y),
                                 pch = 16, asp = T, 
                                 grid.num = 7,
                                 col.grid = col_palette_default("gray"), lwd.grid = 1, lty.grid = 2, ...){
   stopifnot(all(is.na(ordering)) || (length(ordering) == length(x) & ordering %% 1 == 0 & ordering > 0 & max(ordering) == length(unique(ordering))))
   stopifnot(length(y) == length(x))
   stopifnot(length(col) == 1 || length(col) == length(x))
   stopifnot(length(grid.num) == 1, is.na(grid.num) || (grid.num > 2 & grid.num %% 1 == 0))
   
   graphics::plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, asp = asp, ...)
   
   # compute bounds for the grid
   if(!is.na(grid.num)){
      bounds <- graphics::par("usr"); x_bounds <- bounds[1:2]; y_bounds <- bounds[3:4]
      if(asp){
         num_grid <- ceiling(grid.num/2)
         spacing <- min(diff(x_bounds), diff(y_bounds))/(2*num_grid-2)
         x_num <- ceiling(diff(x_bounds)/spacing)
         x_grid <- c(-x_num:x_num)*spacing + mean(x_bounds)
         y_num <- ceiling(diff(y_bounds)/spacing)
         y_grid <- c(-num_grid:num_grid)*spacing + mean(y_bounds)
      } else {
         x_grid <- seq(x_bounds[1], x_bounds[2], length.out = grid.num)
         y_grid <- seq(y_bounds[1], y_bounds[2], length.out = grid.num)
      }
      
      x_bounds_enlarge <- c(-2,2)*diff(x_bounds)+mean(x_bounds)
      y_bounds_enlarge <- c(-1,1)*diff(y_bounds)+mean(y_bounds)
      
      for(x_loc in x_grid){
         graphics::lines(rep(x_loc,2), y_bounds_enlarge, col = col.grid, lwd = lwd.grid, lty = lty.grid)
      }
      for(y_loc in y_grid){
         graphics::lines(x_bounds_enlarge, rep(y_loc,2), col = col.grid, lwd = lwd.grid, lty = lty.grid)
      }
   }
   
   if(!all(is.na(ordering)))
   for(i in sort(unique(ordering))){
      idx <- which(ordering == i)
      if(length(idx) > 0){
         graphics::points(x = x[idx], y = y[idx], col = col[idx], pch = pch, ...)
      }
   } else {
      graphics::points(x = x, y = y, col = col, pch = pch, ...)
   }
   
}

##############

.check_plotting_options <- function(color_palette, axis_labels, first_phase_gridsize,
                                    first_mar, second_mar,
                                    first_subsampling_options,
                                    second_subsampling_options){
 stopifnot(length(color_palette) >= 1 && all(!is.na(color_palette)))
 stopifnot(is.logical(axis_labels), length(axis_labels) == 1)
 
 stopifnot(length(first_phase_gridsize) == 2, all(first_phase_gridsize %% 1 == 0), all(first_phase_gridsize > 0))
 
 stopifnot((length(first_mar) == 1 && all(is.na(first_mar)) || 
             (length(first_mar) == 4) && all(first_mar >= 0) && all(is.numeric(first_mar))))
 stopifnot((length(second_mar) == 1 && all(is.na(second_mar)) || 
             (length(second_mar) == 4) && all(second_mar >= 0) && all(is.numeric(second_mar))))

 stopifnot(is.list(first_subsampling_options))
 stopifnot(is.list(second_subsampling_options))
}

# does nothing right now
.prepare_data_plotting <- function(dat_2col, subsampling_options){
 dat_2col
}