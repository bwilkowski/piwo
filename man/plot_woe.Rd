% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_woe.R
\name{plot_woe}
\alias{plot_woe}
\title{Plot WoE}
\usage{
plot_woe(data, feature_name)
}
\arguments{
\item{data}{TBD}

\item{feature_name}{TBD}
}
\value{
TBD
}
\description{
Function plots Weight of Evidence for
each bucket of feature.
}
\examples{

df <- tibble(
  feature = c(NA, 1, 1, 2, 3, 4, 4, 4, 6, 6, 7, 7, 7, 7, 8, 8, 8),
  target = c(0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1)
)

piwo::count_woe(df$feature, df$target, no_buckets = 3) \%>\% plot_woe("zmienna")


}
