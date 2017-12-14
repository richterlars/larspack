#' Title
#'
#' @param data 
#' @param x 
#' @param y 
#' @param method 
#' @param alternative 
#' @param exact 
#' @param conf.level 
#' @param continuity 
#'
#' @return
#' @export
#'
#' @examples
cor_lr <- function(data,x,y, method = "pearson", alternative ="two.sided", exact = NULL, conf.level =.95, continuity =FALSE) {
  enquo_x <- enquo(x)
  enquo_y <- enquo(y)
  
  X <- data %>% dplyr::select(!!enquo_x) %>% unlist()
  Y <- data %>% dplyr::select(!!enquo_y) %>% unlist()
  cor.test(X, Y, method = method,
           alternative = alternative,
           exact = exact,
           conf.level = conf.level,
           continuity = continuity) %>%
    broom::tidy()
}