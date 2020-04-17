#' Make buckets from custom data frame
#'
#' Function makes bucket vector using buckets border data frame.
#'
#' @param feature_vec Feature vector which will be bucketed.
#' @param max_border_df Data Frame with id of buckets
#' and maximum value in each bucket.
#' You can use count_bucket_max_borders() function
#' on feature_vec to produce such data frame.
#'
#' @return Function returns a vaector with id of buckets.
#' @export
#'
#' @examples
#'
#' a <- c(1, 1, 1, 2, 2, 3, 4, 6, 7, 8, 9, 9, 9, NA)
#' b <- c(NA, NA, 12, 3, 4, 5, 6, 6, 2, 2, 1, 321, 512, NA)
#'
#' a_buckets <- make_buckets(a, 3)
#' a_borders <- count_bucket_max_borders(a, a_buckets)
#' make_buckets_from_border_df(b, a_borders)
#'
#' # Making buckets for multipe features
#' # library(tidyverse)
#'
#' test_tbl <- tibble(
#'   fct = c("d", "a", NA_character_, "a", "c", "d", NA_character_, "e", "a", "a", "d", "c", NA_character_, "b", "b", "d", "e", "b", "c", NA_character_) %>% as_factor(),
#'   chr = c("d", "a", NA_character_, "a", "c", "d", NA_character_, "e", "a", "a", "d", "c", NA_character_, "b", "b", "d", "e", "b", "c", NA_character_),
#'   num = c(1, 3, 91, 12, 3, 1, 2, 4, 8, 5, NA_real_, 78, 12, 51, 13, 68, 5, 10, 238, NA_real_),
#'   int = c(1L, 3L, 91L, 12L, 3L, 1L, 2L, 4L, 8L, 5L, NA_integer_, 78L, 12L, 51L, 13L, 68L, 5L, 10L, 238L, NA_integer_)
#' )
#'
#' # Please note that in test_tbl_2 in fct and chr features
#' # there is new value - "f", which does not occur in test_tbl.
#' # Due to that, new bucket will be made for that value.
#'
#' test_tbl_2 <- tibble(
#'   fct = c("d", "d", NA_character_, "d", "d", "d", NA_character_, "b", "c", "c", "c", "c", NA_character_, "b", "b", "d", "f", "f", "f", NA_character_) %>% as_factor(),
#'   chr = c("d", "d", NA_character_, "d", "d", "d", NA_character_, "b", "c", "c", "c", "c", NA_character_, "b", "b", "d", "f", "f", "f", NA_character_),
#'   num = c(131, 321, 95, 11, 53, 12, 32, 64, 86, 5123, NA_real_, 7, 32, 5121, 11233, 6538, 645, 1120, 23338, NA_real_),
#'   int = c(131, 321, 95, 11, 53, 12, 32, 64, 86, 5123, NA_integer_, 7, 32, 5121, 11233, 6538, 645, 1120, 23338, NA_integer_)
#' )
#'
#' test_tbl_2 %>%
#'   map2_df(
#'     test_tbl,
#'     ~ make_buckets_from_border_df(.x, count_bucket_max_borders(.y, make_buckets(.y, 3)))
#'   )
#'
#'
make_buckets_from_border_df <- function(feature_vec, max_border_df) {

    if(is.numeric(feature_vec)) {
      dplyr::tibble(feature = feature_vec) %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        tidyr::crossing(max_border_df) %>%
        dplyr::mutate(bucket_flag = dplyr::case_when(
          is.na(feature) ~ max(bucket),
          border >= feature ~ bucket
        )) %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(
          feature = min(feature),
          bucket = min(bucket_flag, na.rm = TRUE)
        ) %>%
        dplyr::pull(bucket)
    } else forcats::fct_c(max_border_df$border, as.factor(feature_vec))[length(max_border_df$border)+1:length(feature_vec)] %>% as.numeric()
}


