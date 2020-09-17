plotting_options_default <- function(color_vec = NA, axis_labels = T, first_phase_gridsize = c(3,3),
                                     first_subsampling_options = list(NA),
                                     second_subsampling_options = list(NA)){
 
 .check_plotting_options(color_vec, axis_labels, first_phase_gridsize,
                         first_subsampling_options,
                         second_subsampling_options)
 
 list(color_vec = color_vec, axis_labels = axis_labels, first_phase_gridsize = first_phase_gridsize,
      first_subsampling_options = first_subsampling_options,
      second_subsampling_options = second_subsampling_options)
}

plotting_module_base <- function(x, y, xlab, ylab, main, col, ...){
 graphics::plot(x = x, y = y, xlab = xlab, ylab = ylab, main = main, col = col, ...)
}

##############

.check_plotting_options <- function(color_vec, axis_labels, first_phase_gridsize,
                                    first_subsampling_options,
                                    second_subsampling_options){
 stopifnot((length(color_vec) == 1 && all(is.na(color_vec))) || (length(color_vec) > 1 && all(!is.na(color_vec))))
 stopifnot(is.logical(axis_labels), length(axis_labels) == 1)
 stopifnot(length(first_phase_gridsize) == 2, all(first_phase_gridsize %% 1 == 0), all(first_phase_gridsize > 0))
 stopifnot(is.list(first_subsampling_options))
 stopifnot(is.list(second_subsampling_options))
}

# does nothing right now
.prepare_data_plotting <- function(dat_2col, subsampling_options){
 dat_2col
}