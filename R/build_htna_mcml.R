# Helpers for constructing one global HTNA from an MCML partition -----------

#' @keywords internal
.build_htna_from_mcml <- function(data, source_data, node_groups, actor,
                                  action, session, order, method,
                                  dots = list()) {
  clusters <- node_groups %||% data$cluster_members
  if (is.null(clusters)) {
    stop("The `mcml` object has no `cluster_members`; supply the node ",
         "partition with `node_groups`.", call. = FALSE)
  }

  args <- list(x = data, clusters = clusters, method = method)
  if (!is.null(source_data)) {
    args$data <- source_data

    # build_htna's long-data roles are useful when the caller explicitly
    # provides the original source. Retained MCML sources are already in the
    # build_network-ready wide form and must not receive these default names.
    if (!is.null(actor)) args$actor <- actor
    if (is.data.frame(source_data)) {
      if (action %in% names(source_data)) args$action <- action
      if (session %in% names(source_data)) args$session <- session
      if (order %in% names(source_data)) args$order <- order
    }
  }

  net <- do.call(Nestimate::as_htna, c(args, dots))
  if (!inherits(net, "htna")) { # nocov start
    stop("Expanding an `mcml` must produce one `htna`; got `",
         class(net)[1L], "`.", call. = FALSE)
  } # nocov end
  net
}
