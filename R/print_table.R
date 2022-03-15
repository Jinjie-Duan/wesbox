#' Print table with the style
#' 
#' Print the input table with following styles, bootstrap_options = c("striped", "hover", "condensed", "responsive")
#' 
#' @param table  A input table 
#' @param ... Arguments from kableExtra::kable
#'
#' @return A styling table
#' @export
#'
#' @examples
#' kable_with_style(iris)
kable_with_style <- function(table, ...) {
  
  kableExtra::kable(table, ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  
}
