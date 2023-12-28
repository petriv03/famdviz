#' Plot variables
#'
#' Calculate the square root of a numeric value
#' @param x the numeric value
#' @return the square root of x
#' @examples
#' sqroot(22);
#' @export
plot_variables <- function(famd, dim_x, dim_y, color_by) {

}

#' Get Variables
#'
#' Get variables of factoMineR::FAMD model as an input for plot_variables function
#' @param famd list produced by factoMineR::FAMD function
#' @param dim_x first dimension integer
#' @param dim_y second dimension integer
#' @param color_by "contrib" or "cos2" (see factoMineR::FAMD documentation)
#' @return a data frame of x, y coordinates and color intensities
#' @examples
#' famd <- factoMineR::FAMD(x, graph = F)
#' get_variables(famd, 1, 2, "cos2")
#' @export
get_variables <- function(famd, dim_x, dim_y, color_by) {
  coordinates <- rbind(as.data.frame(model$quali.var[["coord"]]),
                       as.data.frame(model$quanti.var[["coord"]])
                       )
  coordinates <- coordinates[, c(dim_x, dim_y)]
  colors <- rbind(as.data.frame(model$quali.var[[color_by]]),
                  as.data.frame(model$quanti.var[[color_by]])
                  )
  colors <- data.frame(colors[, 1] + colors[, 2])
  colnames(colors) <- c(color_by)
  variables <- cbind(coordinates, colors)
  return(variables)
}
