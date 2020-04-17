#' Count WoE
#'
#' Function counts Weight of Evidence for a given feature.
#'
#' @param feature_vec Feature vector for which WoE will be computed.
#' @param status_vec Vector of 2-class target variable.
#' It has to be in 0/1 format where 1 describes BAD population
#' and 0 is GOOD population.
#' @param start_with_buckets Boolean value which determines whether or not
#' feature_vec is already bucketed. Default is FALSE, which means that
#' feature_vec is not bucketed.
#' @param ... Argument no_buckets to pass to make_buckets function if
#' start_with_buckets is set to FALSE.
#' @param correction Correction parameter which ensures that even if in
#' bucket is no value of one type, WoE will still be computed.
#'
#' @return Function returns a data frame (tibble) with information of
#' WoE in each bucket, as well as number of observations
#' and share of each target type in all buckets.
#' @export
#'
#' @examples
#'
#' # library(tidyverse)
#' df <- tibble(
#'   feature = c(NA, 1, 1, 2, 3, 4, 4, 4, 6, 6, 7, 7, 7, 7, 8, 8, 8),
#'   feature2 = c("a", "a", "a", "a", "a", "b", "b", "b", "c", "c", "c", "c", "d", "d", "d", "d", NA_character_),
#'   target = c(0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1)
#' )
#'
#' feature_buckets <- make_buckets(df$feature, 3)
#' count_woe(feature_buckets, df$target, start_with_buckets = TRUE)
#'
#' count_woe(df$feature, df$target, no_buckets = 3)
#'
#' # Counting WoE for multipe features
#'
#' df %>% map(count_woe, df$target, no_buckets = 3)
#'
#'
count_woe <- function(feature_vec, status_vec, start_with_buckets = FALSE, ...,  correction = 0.5) {

    feature_bucketed <- feature_vec

    if(!start_with_buckets) {
      feature_bucketed <- feature_vec %>%
        make_buckets(...)

      feature_bucket_borders <- count_bucket_max_borders(feature_vec, feature_bucketed)

    }

    woe_tbl <- dplyr::tibble(bucket = feature_bucketed, status = status_vec) %>%
      dplyr::mutate(all = dplyr::n(), all_bad = sum(status), all_good = all - all_bad) %>%
      dplyr::group_by(bucket) %>%
      dplyr::summarise(
        no_all = dplyr::n(),
        no_bad = sum(status),
        no_good = no_all - no_bad,
        share_good = (no_good + correction)/ max(all_good + correction),
        share_bad = (no_bad + correction)/ max(all_bad + correction),
        woe = log(share_good / share_bad)
      )

    if(!start_with_buckets) woe_tbl %>% dplyr::inner_join(feature_bucket_borders, by = "bucket") %>% dplyr::select(bucket, border, no_all, no_bad, no_good, share_good, share_bad, woe)
    else woe_tbl
}


