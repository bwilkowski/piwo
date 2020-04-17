#' Make Buckets
#'
#' Function creates vector of buckets for a given feature vector.
#'
#' @param feature_vec Feature vector which will be bucketed.
#' @param no_buckets Maximum number of buckets. NAs are always bucketed
#' separately in no_buckets + 1 bucket.
#' When it comes to character or factor vector,
#' function assign unique values to unique buckets
#' and no_buckets has no effect.
#'
#' @return Function returns numeric vector of buckets to which
#' each observation is assigned.
#' @export
#'
#' @examples
#'
#' num_vec <- c(1,1,1,2,2,3,4,6,7,8,9,9,9, NA)
#' chr_vec <- c("a", "a", "b", "c", NA_character_)
#'
#' make_buckets(num_vec, no_buckets = 3)
#' make_buckets(chr_vec)
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
#' test_tbl %>% map_df(make_buckets, 3)
#'
#'
make_buckets <- function(feature_vec, no_buckets = 8) {

    if(is.numeric(feature_vec)) {
      ifelse(!is.na(feature_vec),
             ceiling(dplyr::min_rank(feature_vec) * no_buckets / max(dplyr::min_rank(feature_vec), na.rm = TRUE)),
             no_buckets + 1
      )
    } else {
      feature_vec <- feature_vec %>% factor(exclude = NULL)
      feature_vec %>% as.numeric()
    }
}


