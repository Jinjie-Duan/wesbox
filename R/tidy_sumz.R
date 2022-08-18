#' Run metap::sumz to meta two p values with weights stored in a tibble
#'
#' @param dt input data table contains 4 columns representing two p values and two weights
#' @param pval1 the column name of 1st p value
#' @param pval2 the column name of 2nd p value
#' @param wt1 the column name of weight of 1st p value
#' @param wt2 the column name of weight of 2nd p value
#'
#' @return It returns the original input data table with one additional column **sumz_pval** representing meta-ed p value from metap::sumz
#' @export
#'
#' @examples
#' dt <- tibble::tribble(
#' ~p1, ~p2, ~w1, ~w2,
#' 0.01, 0.0003, 1,1,
#' 0.0003, 0.01, 10,1,
#' 0.5, 0.0003, 2,5)
#' 
#' tidy_sumz(dt, p1, p2, w1, w2)
#' 
#' 
tidy_sumz <- function(dt, pval1, pval2, wt1, wt2){
  
  meta_dt <- dt %>%
    dplyr::rowwise() %>%
    dplyr::mutate(pval = list(c({{pval1}}, {{pval2}})),
                  weights = list(c({{wt1}}, {{wt2}}))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sumz_pval = purrr::map2_dbl(pval, weights, ~ metap::sumz(.x, .y)$p)) %>%
    dplyr::select(-pval, -weights)

  meta_dt
}