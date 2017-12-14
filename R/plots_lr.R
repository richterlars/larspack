#' Creates a point plot, with R and p labels
#'
#' Creates a point plot, with R and p labels
#'
#' @param data Input data frame
#' @param x column name, unquoted
#' @param y column name, unquoted
#' @param plot TRUE ... the function plots directly, FALSE ... the function returns a ggplot object for further processing.
#' @param color column name, unquoted
#' @param shape column name, unquoted
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
#' lr_corplot(mtcars, "hp", "mpg")
#'
corplot_lr <- function(data, x, y, color = NULL, shape = NULL, plot = TRUE, hjust = -.1, vjust = 2) {

  enquo_x <- enquo(x)
  enquo_y <- enquo(y)

  if (!is.null(color)) {color <- deparse(substitute(color))}
  if (!is.null(shape)) {shape <- deparse(substitute(shape))}

  #nice blog entry by Colin FAY: http://colinfay.me/tidyeval-1/
  X <- data %>% dplyr::select(!!enquo_x) %>% unlist()
  Y <- data %>% dplyr::select(!!enquo_y) %>% unlist()

  cor_obj <- cor.test(X, Y)

  r <- cor_obj$estimate %>%
    round(1)

  p <- cor_obj$p.value %>%
    round(3)
  p <-  ifelse(p < 0.001, 'P< 0.001', paste0('P== ' ,p))

  eqn <- paste0("R = ", r, "; ", p)

  g <- ggplot2::ggplot(data,
                       ggplot2::aes_string(x = quo_name(enquo_x),
                                           y = quo_name(enquo_y),
                                           color = color,
                                           shape = shape )
                       ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::annotate("label", -Inf, Inf, label = eqn, hjust = -.1, vjust = 2)

  ifelse(plot, plot(g), g)

}




