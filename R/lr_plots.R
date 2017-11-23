#' Title
#'
#' Creates a simple ggplot with R and p labels
#'
#' @param data
#' @param x
#' @param y
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
#' lr_corplot(mtcars, "hp", "mpg")
#'
lr_corplot <- function(data, x, y) {

  cor_obj <- cor.test(data[[x]], data[[y]])

  r <- cor_obj$estimate %>%
    round(1)

  p <- cor_obj$p.value %>%
    round(3)
  p <-  ifelse(p < 0.001, 'P< 0.001', paste0('P== ' ,p))

  eqn <- paste0("R = ", r, "; ", p)

  g <- ggplot(data, aes_string(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm") +
    annotate("label", -Inf, Inf, label = eqn, hjust = -.1, vjust = 2)

}
