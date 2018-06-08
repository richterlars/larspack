#' Creates grid from model applicable to plotly surface plot
#'
#' @param data data frame
#' @param x columnname tidyway
#' @param y columnname tidyway
#' @param model model object, e.g. lm object
#' @param n integer, grid space
#'
#' @return list of three objects
#' grid ... the predicted z variables
#' x_axis ... the x variables in the grid
#' y_axis ... the y_variables in the grid
#'
#' @examples
#' m <- lm(mpg ~ wt + hp, mtcars)
#' gr <- grid_plotly_surface(mtcars, wt, hp, model = m)
#' plotly::plot_ly(mtcars, x= ~wt, y = ~hp, z = ~mpg) %>%
#'      add_surface(x = ~gr$x_axis, y = ~gr$y_axis, z = ~gr$grid)
#'
#' @describeIn
#'
#' @export

grid_plotly_surface <- function(data, x, y, model, n = 100) {

  enquo_x <- rlang::enquo(x)
  enquo_y <- rlang::enquo(y)

  chr_x <- deparse(substitute(x))
  chr_y <- deparse(substitute(y))

  X <- data %>% dplyr::pull(!!enquo_x)
  Y <- data %>% dplyr::pull(!!enquo_y)

  axis_x <- X %>%
    modelr::seq_range(n = n)

  axis_y <- Y %>%
    modelr::seq_range(n = n)

  grid <- tibble::tibble(axis_x, axis_y)
  colnames(grid) <- c(chr_x, chr_y)

  grid <- grid %>%
    tidyr::expand_(c(chr_x, chr_y))

  preds <- grid %>%
    modelr::add_predictions(model = model, data = .) %>%
    reshape2::acast(str_c(chr_y, " ~ ", chr_x))

  list("grid" = preds, "x_axis" = axis_x, "y_axis" = axis_y)
}
