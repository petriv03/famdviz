#' Plot variables
#'
#' Calculate the square root of a numeric value
#' @param x the numeric value
#' @return the square root of x
#' @examples
#' sqroot(22);
#' @export
plot_variables <- function(famd, dim_x, dim_y, color_by) {
  variables <- get_variables(famd, dim_x, dim_y, color_by)
  mean_color <- mean(variables[, 3])
  figure <- ggplot2::ggplot(variables, aes(x = data_frame[, 1],
                                  y = data_frame[, 2],
                                  color = data_frame[, 3],
                                  label = rownames(variables))) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_point(shape = 17) +
    ggrepel::geom_text_repel(max.overlaps = Inf, size=3) +
    ggplot2::labs(x = NULL, y = NULL) +
    set_gradient(mean_color) +
    set_custom_theme(legend_position_x = 1,
                     legend_position_y = 1,
                     axis_text_x = T,
                     axis_text_y = T) +
    ggplot2::guides(color = guide_colourbar(title = toupper(color_by)))
  return(figure)
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
#' vars <- get_variables(famd, 1, 2, "cos2")
#' print(vars)
#' @export
get_variables <- function(famd, dim_x, dim_y, color_by) {
  coordinates <- rbind(as.data.frame(famd$quali.var[["coord"]]),
                       as.data.frame(famd$quanti.var[["coord"]])
                       )
  coordinates <- coordinates[, c(dim_x, dim_y)]
  colors <- rbind(as.data.frame(famd$quali.var[[color_by]]),
                  as.data.frame(famd$quanti.var[[color_by]])
                  )
  colors <- data.frame(colors[, 1] + colors[, 2])
  colnames(colors) <- c(color_by)
  variables <- cbind(coordinates, colors)
  return(variables)
}

#' Set Gradient
#'
#' Supporting function for ggplot generating 3-point scale_colour_gradient2
#' @param midpoint numeric value indicating mid color
#' @return ggplot2::scale_colour_gradient2()
#' @examples
#' data_frame <- data.frame(x = c(1, 2, 3),
#'                          y = c(1, 2, 3),
#'                          z = c(12.8, 6.5, 15.4))
#' mean_color <- mean(data_frame[, "z"])
#' ggplot(data_frame, aes(x = x, y = y, color = z) +
#'   geom_point(shape = 17) +
#'   set_gradient(mean_color)
#' @export
set_gradient <- function(midpoint) {
  ggplot2::scale_colour_gradient2(low="#00AFBB",
                                  mid="#E7B800",
                                  high="#FC4E07",
                                  midpoint = mean(midpoint))
}

#' Set Custom Theme
#'
#' Supporting function for ggplot generating customized theme
#' @param legend_position_x numeric between 0 and 1 indicating left-right position of legend
#' @param legend_position_y numeric between 0 and 1 indicating top-down position of legend
#' @param axis_text_x boolean indicating labels on horizontal axis tics
#' @param axis_text_y boolean indicating labels on vertical axis tics
#' @return ggplot2::theme()
#' @examples
#' data_frame <- data.frame(x = c(1, 2, 3),
#'                          y = c(1, 2, 3),
#'                          z = c(12.8, 6.5, 15.4))
#' mean_color <- mean(data_frame[, "z"])
#' ggplot(data_frame, aes(x = x, y = y, color = z) +
#'   geom_point(shape = 17) +
#'   set_gradient(mean_color)
#' @export
set_custom_theme <- function(legend_position_x,
                             legend_position_y,
                             axis_text_x = T,
                             axis_text_y = T) {

  if (axis_text_x) {
    x_text <- ggplot2::element_text()
  } else {
    x_text <- ggplot2::element_blank()
  }

  if (axis_text_y) {
    y_text <- ggplot2::element_text()
  } else {
    y_text <- ggplot2::element_blank()
  }

  theme_bw() +
    theme(
      axis.text.x = x_text,
      axis.text.y = y_text,
      text = ggplot2::element_text(size = 8),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.key = ggplot2::element_rect(fill = "transparent"),
      legend.justification = c(legend_position_x, legend_position_y),
      legend.position = c(legend_position_x, legend_position_y),
      legend.title = ggplot2::element_text(face = "bold")
    )
}
