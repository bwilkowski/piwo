#' Plot WoE
#'
#' Function plots Weight of Evidence for
#' each bucket of feature.
#'
#' @param data TBD
#' @param feature_name TBD
#'
#' @return TBD
#' @export
#'
#' @examples
#'
#' df <- tibble(
#'   feature = c(NA, 1, 1, 2, 3, 4, 4, 4, 6, 6, 7, 7, 7, 7, 8, 8, 8),
#'   target = c(0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1)
#' )
#'
#' piwo::count_woe(df$feature, df$target, no_buckets = 3) %>% plot_woe("zmienna")
#'
#'
plot_woe <- function(data, feature_name) {

  data %>%
    dplyr::arrange(bucket) %>%
    dplyr::mutate(
      border_1 = dplyr::lag(border),
      x_axis = dplyr::case_when(
        is.na(border) ~ "Brak war.",
        border == Inf ~ stringr::str_glue("> { border_1 }") %>% as.character(),
        TRUE          ~ stringr::str_glue("<= { border }") %>% as.character()
      )
    ) %>%
    ggplot2::ggplot(aes(x = forcats::as_factor(x_axis), y = woe)) +
    ggplot2::geom_col(fill = ifelse(data$woe > 0, "#99E472", "#D82C20")) +
    ggplot2::labs(
      title = stringr::str_glue("WoE for feature { feature_name }"),
      x = "bucket",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(limits = c(-1.1*max(abs(data$woe)), 1.1*max(abs(data$woe))))
}


