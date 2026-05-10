#' Chi-square Mosaic Plot of a Transition Network
#'
#' htna-named alias of [Nestimate::mosaic_plot()]. Renders a
#' chi-square mosaic where row x column area equals the joint share
#' of `(from, to)` transitions and fill encodes the standardized
#' residual (blue = over-represented, red = under-represented,
#' white = at-expected).
#'
#' Designed with htna in mind: Nestimate ships an explicit
#' `mosaic_plot.htna` S3 method that recognises the actor partition
#' on `htna` networks. Pass an htna network from [build_htna()]
#' directly. The underlying transition matrix must be integer-weighted
#' (build with `method = "frequency"`), since the chi-square test
#' needs counts.
#'
#' Suffixed `_htna` to avoid clashing with `Nestimate::mosaic_plot()`
#' when both packages are loaded.
#'
#' @inheritParams Nestimate::mosaic_plot
#'
#' @return A ggplot or gtable, depending on input shape. See
#'   [Nestimate::mosaic_plot()] for full details and the per-class S3
#'   methods.
#'
#' @seealso [plot_frequencies_htna()] for the marginal-frequency
#'   companion view.
#' @examples
#' \donttest{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long),
#'                   method = "frequency")
#' mosaic_plot_htna(net, n_perm = 50, seed = 1)
#' }
#' @export
mosaic_plot_htna <- Nestimate::mosaic_plot
