#' Bootstrap an HTNA Network
#'
#' Thin wrapper around [Nestimate::bootstrap_network()] that runs the standard
#' state-level edge bootstrap on an htna network and tags the result so it is
#' identifiable as an htna bootstrap. The bootstrap inference itself is
#' Nestimate's: per-edge mean, sd, p-value, significance, and confidence /
#' credibility intervals across `iter` resamples. State-level granularity is
#' the correct level for edge-stability analysis - aggregating up to actor
#' pairs would smear over real edge-by-edge differences.
#'
#' What this wrapper adds is identity and downstream class continuity:
#' * The returned object has class `"htna_bootstrap"` ahead of
#'   `"net_bootstrap"`, so `inherits(boot, "htna_bootstrap")` works.
#' * The actor partition (`$nodes$groups`, `$node_groups`) is restored on
#'   `boot$model` from the input network so cograph's auto-detect still
#'   recognizes the heterogeneous schema downstream.
#' * `boot$model` is re-tagged with `"htna"` at the front of its class chain
#'   so the chain is `c("htna", "netobject", "cograph_network")` - identical
#'   to what [build_htna()] returns.
#'
#' @param x \[`htna`\]\cr A network built with [build_htna()].
#' @param ... Arguments forwarded to [Nestimate::bootstrap_network()] (e.g.
#'   `iter`, `ci_level`, `consistency_range`, `seed`).
#'
#' @return An object of class `c("htna_bootstrap", "net_bootstrap")`. All
#'   slots produced by [Nestimate::bootstrap_network()] are preserved
#'   verbatim (`$summary`, `$mean`, `$sd`, `$p_values`, `$significant`,
#'   `$ci_lower`/`$ci_upper`, `$cr_lower`/`$cr_upper`, `$model`, `$original`,
#'   etc.).
#'
#' @seealso [Nestimate::bootstrap_network()] for the underlying algorithm
#'   and slot documentation. [build_htna()] for constructing the input.
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net  <- build_htna(list(Human = human_long, AI = ai_long))
#' boot <- bootstrap_htna(net, iter = 200)
#' inherits(boot, "htna_bootstrap")            # TRUE
#' inherits(boot$model, "htna")                # TRUE
#' head(boot$summary)                          # state-level edge stability
#' }
#'
#' @export
#' @rdname bootstrap_htna
bootstrap_htna <- function(x, ...) {
  if (inherits(x, "htna_group") ||
      (is.list(x) && !inherits(x, "htna") && !is.data.frame(x))) {
    res <- lapply(x, bootstrap_htna, ...)
    names(res) <- names(x)
    class(res) <- c("htna_bootstrap_group", "list")
    return(res)
  }
  if (!inherits(x, "htna")) {
    stop("`x` must be an htna network produced by build_htna().",
         call. = FALSE)
  }

  boot <- Nestimate::bootstrap_network(x, ...)

  if (!is.null(boot$model)) {
    # nocov start - defensive: Nestimate currently preserves nodes$groups
    if (is.null(boot$model$nodes$groups) && !is.null(x$nodes$groups)) {
      boot$model$nodes$groups <- x$nodes$groups
    }
    # nocov end
    if (is.null(boot$model$node_groups) && !is.null(x$node_groups)) {
      boot$model$node_groups <- x$node_groups
    }
    if (is.null(boot$model$actor_levels) && !is.null(x$actor_levels)) {
      boot$model$actor_levels <- x$actor_levels
    }
    if (!inherits(boot$model, "htna")) {
      class(boot$model) <- c("htna", class(boot$model))
    }
  }

  class(boot) <- c("htna_bootstrap", class(boot))
  boot
}

#' Bootstrap generic
#'
#' S3 generic dispatched on the class of `x`. Provided so `bootstrap(net)`
#' works directly on an htna network (see [bootstrap_htna()]).
#'
#' @param x An object to bootstrap.
#' @param ... Arguments forwarded to the method.
#'
#' @return An object whose structure is method-defined.
#' @export
bootstrap <- function(x, ...) UseMethod("bootstrap")

#' @export
#' @rdname bootstrap_htna
bootstrap.htna <- function(x, ...) bootstrap_htna(x, ...)