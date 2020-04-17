#' Count buckets maximum borders
#'
#' Function counts maximum value in each bucket.
#'
#' @param feature_vec Feature vector for which maximum
#' bucket borders will be computed.
#' @param bucket_vec Vector of buckets for feature_vec
#' (may be the result of make_buckets function).
#'
#' @return Function returns a tibble with id of buckets
#' and maximum value in each bucket.
#' In case of character or factor vectors,
#' the tibble consists of unique pairs of feature
#' and bucket number.
#' @export
#'
#' @examples
#'
#' num_vec <- c(1, 1, 1, 2, 2, 3, 4, 6, 7, 8, 9, 9, 9, NA)
#' chr_vec <- c("a", "a", "b", "c", NA_character_)
#' num_custom_buckets_vec <- c(1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4)
#'
#' count_bucket_max_borders(num_vec, make_buckets(num_vec, 3))
#' count_bucket_max_borders(num_vec, num_custom_buckets_vec)
#' count_bucket_max_borders(chr_vec, make_buckets(chr_vec))
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
#' test_tbl %>% map(~ count_bucket_max_borders(.x, make_buckets(.x, 3)))
#'
#'
count_bucket_max_borders <- function(feature_vec, bucket_vec) {

    if(is.numeric(feature_vec)) {
      data <- dplyr::tibble(
        feature = feature_vec,
        bucket = bucket_vec
      ) %>%
        dplyr::group_by(bucket) %>%
        dplyr::summarise(border = max(feature))

      if(any(is.na(feature_vec))) data %>% dplyr::mutate(border = ifelse(bucket == max(bucket) - 1, Inf, border))
      else data %>% dplyr::mutate(border = ifelse(bucket == max(bucket), Inf, border))

    } else {
      dplyr::tibble(
        bucket = bucket_vec,
        border = feature_vec %>% factor(exclude = NULL)
      ) %>%
        dplyr::distinct(bucket, border)
    }
}


