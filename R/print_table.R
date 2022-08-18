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


#' Print table with scroll box 
#'
#' @param table A input table
#' @param scroll.height The height param in scroll_box()
#' @param scroll.width The width param in scroll_box()
#' @param scroll.fixed_thead The fixed_thead param in scroll_box()
#' @param ... Arguments from kableExtra::kable
#'
#' @return A styling table with scroll box
#' @export
#'
#' @examples
#' kable_with_scroll_box(iris)
#' 
kable_with_scroll_box <- function(table, scroll.height = "300px", scroll.width = "100%", scroll.fixed_thead = TRUE, ...) {
  
  kableExtra::kable(table, ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>% 
    kableExtra::scroll_box(height = scroll.height, width = scroll.width, fixed_thead = scroll.fixed_thead)

}





