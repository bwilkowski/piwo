#' Count PSI
#'
#' Function counts Population Stability Index
#' for a given feature.
#'
#' @param pop1_feature_vec Feature vector for first population.
#' @param pop2_feature_vec Feature vector for second population.
#' @param start_with_buckets Boolean value which determines whether or not
#' feature_vec is already bucketed. Default is FALSE, which means that
#' feature_vec is not bucketed.
#' @param ... Argument no_buckets to pass to make_buckets function if
#' start_with_buckets is set to FALSE.
#' @param info_for_all_buckets Boolean value which determines
#' whether or not PSI will be computed for whole feature or for each bucket.
#' Default value is set to FALSE, which means that single value
#' of feature's PSI will be returned.
#'
#' @return Function returns a data frame (tibble) which contains either
#' a single value of feature's PSI, or (if info_for_all_buckets argument
#' is set to TRUE) PSI values for each bucket with
#' information of maximum border in buckets, etc.
#' @export
#'
#' @examples
#'
#' a <- c(1, 1, 1, 2, 2, 3, 4, 6, 7, 8, 9, 9, 9, NA)
#' b <- c(NA, NA, 12, 3, 4, 5, 6, 6, 2, 2, 1, 321, 512, NA)
#'
#' count_psi(a,b, no_buckets = 3)
#' count_psi(a,b, no_buckets = 3, info_for_all_buckets = TRUE)
#'
#' # Counting PSI for multipe features
#' # library(tidyverse)
#' test_tbl %>% map2(test_tbl_2, count_psi)
#' test_tbl %>% map2(test_tbl_2, count_psi, no_buckets = 3, info_for_all_buckets = TRUE)
#'
count_psi <-
  function(pop1_feature_vec, pop2_feature_vec, start_with_buckets = FALSE, ..., info_for_all_buckets = FALSE) {

    if(!start_with_buckets) {

      pop1_bucket_borders <- count_bucket_max_borders(pop1_feature_vec, make_buckets(pop1_feature_vec, ...))

      pop2_feature_vec <- pop2_feature_vec %>%
        make_buckets_from_border_df(pop1_bucket_borders)

      pop1_feature_vec <- pop1_feature_vec %>%
        make_buckets(...)
    }

    bucket_share_pop1 <- pop1_feature_vec %>% table() %>% as_tibble() %>% mutate(n = n / sum(n)) %>% set_names(c("bucket", "n")) %>% mutate(bucket = as.numeric(bucket))
    bucket_share_pop2 <- pop2_feature_vec %>% table() %>% as_tibble() %>% mutate(n = n / sum(n)) %>% set_names(c("bucket", "n")) %>% mutate(bucket = as.numeric(bucket))

    psi_tbl <- bucket_share_pop1 %>%
      dplyr::full_join(bucket_share_pop2, by = "bucket") %>%
      dplyr::mutate_all(~ifelse(is.na(.),0.0005,.)) %>%
      dplyr::mutate(psi = (n.x - n.y) * (log(n.x/n.y)))

    if(!info_for_all_buckets) psi_tbl %>% dplyr::summarise(psi = sum(psi))
    else {
      if(!start_with_buckets) psi_tbl %>% dplyr::inner_join(pop1_bucket_borders, by = "bucket") %>% dplyr::select(bucket, border, pop_1_share = n.x, pop_2_share = n.y, psi)
      else psi_tbl %>% dplyr::select(bucket, pop_1_share = n.x, pop_2_share = n.y, psi)
    }
  }


