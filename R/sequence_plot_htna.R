#' Plot the sessions of an htna network as sequences
#'
#' @description
#' `sequence_plot_htna()` plots the sessions of an htna network as
#' sequences: every session is a row of coloured cells, one per event, so
#' the reader sees how sessions begin and end, how long they run and
#' whether some codes cluster early or late. A transition network pools the
#' transitions of all sessions and so loses when each code occurs and how
#' long the sessions are; the sequence plot restores that view. In a
#' heterogeneous network the plot can separate the actor types, showing the
#' course of each actor's own events or which actor acted at each step.
#'
#' The plot is drawn with `Nestimate::sequence_plot()`. With `by = "state"`
#' (the default) each cell is coloured by its code, and every session
#' contributes one row per actor type holding that actor's events in order;
#' the legend is split into one block per actor type. With
#' `by = "group"` the sessions are kept whole and each cell is coloured by
#' the actor type that acted at that step. `type` selects the display: a
#' sequence index plot with one panel per actor type (`"index"`), a single
#' carpet with a separator between the actor types (`"heatmap"`), or the
#' share of each code at every position (`"distribution"`). A grouped
#' network is drawn as one plot per cohort. `plot_sequences()` on an htna
#' network calls this function. It is called for the plot and returns,
#' invisibly, the description of the drawn plot (a list), or `net` for a
#' grouped network.
#'
#' Rows are ordered by clustering the sessions, which can take some seconds
#' for several hundred sessions. For the network itself use [plot_htna()];
#' to test which sequence patterns differ between cohorts use
#' [sequence_compare_htna()].
#'
#' @param net,x An htna network from [build_htna()], or an `htna_group`.
#' @param by `"state"` (default) colours cells by code, `"group"` by actor
#'   type.
#' @param type `"index"` (default), `"heatmap"` or `"distribution"`.
#' @param grouped_legend With `by = "state"`, split the legend into one block
#'   per actor type. Default `TRUE`.
#' @param ... Further arguments of `Nestimate::sequence_plot()`, such as
#'   `sort`, `legend`, `state_colors` or `main`.
#'
#' @return Called for the plot. Invisibly, the description of the drawn plot
#'   returned by `Nestimate::sequence_plot()` (a list), or `net` for a
#'   grouped network.
#'
#' @section Conditions:
#' `htna_bad_input` when `net` is not an htna network with sequence data;
#' `htna_unassigned_node` when it has no actor partition;
#' `htna_empty_input` for a grouped network without cohorts.
#'
#' @seealso [plot_htna()], [sequence_compare_htna()],
#'   [extract_meta_paths()], [build_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(subset(human_ai, project == "Project_9"),
#'                   actor_type = "actor_type")
#' sequence_plot_htna(net)
#' sequence_plot_htna(net, type = "heatmap")
#' sequence_plot_htna(net, by = "group", type = "distribution")
#'
#' @export
sequence_plot_htna <- function(net,
                               by   = c("state", "group"),
                               type = c("index", "heatmap", "distribution"),
                               grouped_legend = TRUE,
                               ...) {
  if (inherits(net, "htna_group") ||
      (is.list(net) && !inherits(net, "htna") && !is.data.frame(net))) {
    if (length(net) == 0L) .htna_stop("Empty htna_group.",
                                      class = "htna_empty_input")
    nms <- names(net) %||% as.character(seq_along(net))
    by   <- match.arg(by)
    type <- match.arg(type)
    lapply(seq_along(net), function(i) {
      sequence_plot_htna(net[[i]], by = by, type = type,
                         grouped_legend = grouped_legend,
                         main = nms[i], ...)
    })
    return(invisible(net))
  }
  by   <- match.arg(by)
  type <- match.arg(type)

  if (!inherits(net, "htna")) {
    .htna_stop("`net` must be an htna network produced by build_htna().")
  }
  if (is.null(net$data) || !is.data.frame(net$data)) {
    .htna_stop("`net$data` is missing or not a data frame; rebuild with ",
               "`build_htna()`.")
  }
  if (is.null(net$node_groups) || !nrow(net$node_groups)) {
    .htna_stop("`net` has no actor partition (`$node_groups` is empty).",
               class = "htna_unassigned_node")
  }

  type_map  <- setNames(as.character(net$node_groups$group),
                        as.character(net$node_groups$node))
  # Canonical actor order from build_htna() (Human, AI, …) so colours
  # and stacking match plot_htna(). Falls back to encounter order if
  # the network was built without `$actor_levels`.
  actors    <- net$actor_levels %||%
               unique(as.character(net$node_groups$group))

  # An actor type without an event in the sessions (the start and end
  # states, or an actor absent from a cohort) has no sequence to draw; the
  # others keep the colours they have in the full partition.
  all_actors <- as.character(actors)
  occurring  <- unique(unname(type_map[as.character(unlist(
    net$data, use.names = FALSE))]))
  actors     <- actors[all_actors %in% occurring]
  if (!length(actors)) {
    .htna_stop("The sessions of `net` hold no code of any actor type; ",
               "there is nothing to plot.", class = "htna_empty_input")
  }

  if (by == "group") {
    actor_chr <- as.character(actors)
    color_map <- .htna_actor_colors(all_actors)

    # Nestimate's `.encode_states()` always alphabetises levels, so a
    # naive label of "Human" / "AI" would put AI as level 1 (the bottom
    # band). Prefix actors with leading spaces so the alphabetical sort
    # yields the canonical order (Human → " Human", AI → "AI", " " < "A"),
    # then mask Nestimate's drawn legend with our own clean labels.
    n_act    <- length(actor_chr)
    rename   <- setNames(
      paste0(strrep(" ", n_act - seq_len(n_act)), actor_chr),
      actor_chr)

    wide <- net$data
    wide[] <- lapply(wide, function(col) {
      a <- unname(type_map[as.character(col)])
      unname(rename[a])
    })

    user_args <- list(...)
    if (is.null(user_args$state_colors)) {
      # state_colors aligned with the canonical (and now alphabetical)
      # actor order so band 1 = first actor with its canonical colour.
      user_args$state_colors <- unname(color_map[actor_chr])
    }
    if (type == "distribution" && is.null(user_args$na)) {
      user_args$na <- FALSE
    }
    legend_pos <- user_args$legend %||% "right"
    result <- do.call(Nestimate::sequence_plot,
                      c(list(x = wide, type = type), user_args))
    overlay_pos <- .htna_normalize_overlay_pos(legend_pos)
    if (!is.null(overlay_pos)) {
      .htna_overlay_legend(actor_chr,
                           unname(color_map[actor_chr]),
                           position = overlay_pos)
    }
    return(invisible(result))
  }

  state_mat <- as.matrix(net$data)

  pieces <- lapply(actors, function(a) {
    rows <- lapply(seq_len(nrow(state_mat)), function(i) {
      r <- state_mat[i, ]
      r <- r[!is.na(r)]
      r[unname(type_map[r]) == a]
    })
    keep   <- lengths(rows) > 0L
    rows   <- rows[keep]
    if (!length(rows)) return(matrix(NA_character_, 0L, 0L))
    n_cols <- max(lengths(rows))
    out    <- t(vapply(rows, function(v) {
      length(v) <- n_cols
      v
    }, character(n_cols)))
    colnames(out) <- paste0("T", seq_len(n_cols))
    out
  })

  n_cols_all <- max(vapply(pieces, ncol, integer(1L)))
  pieces <- lapply(pieces, function(m) {
    if (ncol(m) >= n_cols_all) return(m)
    extra <- matrix(NA_character_, nrow(m), n_cols_all - ncol(m))
    colnames(extra) <- paste0("T", (ncol(m) + 1L):n_cols_all)
    cbind(m, extra)
  })
  stacked <- do.call(rbind, pieces)
  # Pass `group` as a factor with the canonical actor order so
  # Nestimate's `as.factor(group)` preserves it (instead of
  # alphabetising and putting AI before Human).
  group   <- factor(rep(actors, vapply(pieces, nrow, integer(1L))),
                    levels = as.character(actors))

  user_args <- list(...)

  # For disambiguated codes, assign colors based on base code name and clean labels
  disambiguated_mapping <- NULL
  if (is.null(user_args$state_colors)) {
    all_states <- unique(as.vector(stacked))
    all_states <- all_states[!is.na(all_states)]

    # Check if codes are disambiguated (contain ":")
    disambiguated <- grepl(":", all_states, fixed = TRUE)
    if (any(disambiguated)) {
      # Create base code mapping
      # Base code of a disambiguated code (e.g. "Human:Ask" -> "Ask").
      base_code_map <- stats::setNames(
        ifelse(disambiguated,
               vapply(strsplit(all_states, ":", fixed = TRUE), `[`,
                      character(1L), 2L),
               all_states),
        all_states
      )

      # Store mapping for later label cleaning
      disambiguated_mapping <- base_code_map

      # Get unique base codes and assign colors using an extended palette
      unique_base_codes <- unique(base_code_map)
      n_states <- length(unique_base_codes)

      # Use extended palette for many states
      if (n_states <= 6) {
        # Use nice htna colors for few states
        base_colors <- .htna_actor_colors(unique_base_codes)
      } else {
        # Generate more distinct colors for many states
        extended_palette <- c(
          htna_palette,  # Start with htna colors
          "#FF1493", "#2E8B57", "#8B4513", "#4169E1",
          "#FFD700", "#32CD32", "#FF4500", "#9400D3",
          "#DC143C", "#00CED1", "#FF69B4", "#228B22",
          "#B22222", "#4682B4", "#D2691E", "#6A5ACD",
          "#20B2AA", "#CD5C5C", "#8FBC8F", "#800080"
        )

        if (n_states <= length(extended_palette)) {
          base_colors <- setNames(extended_palette[seq_len(n_states)], unique_base_codes)
        } else {
          # Generate even more colors using rainbow with better spacing
          base_colors <- setNames(
            grDevices::rainbow(n_states, s = 0.8, v = 0.9),
            unique_base_codes
          )
        }
      }

      # Sort states alphabetically to match Nestimate's internal ordering
      all_states_sorted <- sort(all_states)

      # Create color vector in sorted order (unnamed vector for Nestimate)
      state_colors <- unname(base_colors[base_code_map[all_states_sorted]])

      user_args$state_colors <- state_colors
    }
  }

  use_grouped_legend <- isTRUE(grouped_legend)
  legend_position    <- user_args$legend %||% "right"
  overlay_position   <- .htna_normalize_overlay_pos(legend_position)
  if (is.null(overlay_position)) use_grouped_legend <- FALSE
  if (use_grouped_legend) user_args$legend <- legend_position

  if (type == "heatmap") {
    hc <- .htna_combined_hclust(pieces)
    if (is.null(user_args$tree))         user_args$tree         <- hc
    if (is.null(user_args$k))            user_args$k            <- length(actors)
    if (is.null(user_args$k_color))      user_args$k_color      <- "white"
    if (is.null(user_args$k_line_width)) user_args$k_line_width <- 12
    if (is.null(user_args$na_color))     user_args$na_color <- "white"
  } else {
    if (length(actors) >= 2L) user_args$group <- group
    if (is.null(user_args$na_color)) user_args$na_color <- "white"
    if (type == "index" &&
        is.null(user_args$ncol) && is.null(user_args$nrow)) {
      user_args$ncol <- 1L
    }
    if (type == "distribution" && is.null(user_args$na)) {
      user_args$na <- FALSE  # nocov
    }
  }

  result <- do.call(Nestimate::sequence_plot,
                    c(list(x = stacked, type = type), user_args))

  if (use_grouped_legend) {
    .htna_grouped_legend(result, type_map, actors,
                         position = overlay_position,
                         disambiguated_mapping = disambiguated_mapping)
  }

  invisible(result)
}

#' Plot sequences
#'
#' @description
#' `plot_sequences()` plots the sequences behind a model; it is a generic.
#' For an htna network it draws the sessions with [sequence_plot_htna()],
#' so `plot_sequences(net)` gives the same plot as `sequence_plot_htna(net)`.
#' The 'tna' package exports a generic of the same name; whichever package
#' is attached last provides `plot_sequences()`.
#'
#' @param x An object to plot; for an htna network, see
#'   [sequence_plot_htna()].
#' @param ... Arguments for the method.
#'
#' @return Called for the plot; the value is defined by the method (for an
#'   htna network, see [sequence_plot_htna()]).
#'
#' @seealso [sequence_plot_htna()]
#'
#' @examples
#' data(human_ai)
#' net <- build_htna(subset(human_ai, project == "Project_9"),
#'                   actor_type = "actor_type")
#' plot_sequences(net)
#' @export
plot_sequences <- function(x, ...) UseMethod("plot_sequences")

#' @export
#' @rdname sequence_plot_htna
plot_sequences.htna <- function(x, ...) sequence_plot_htna(x, ...)


# Map any base-graphics legend position spec to one of the two the gutter
# overlay supports ("right", "bottom"). Returns NULL for "none"/FALSE/NULL
# (caller should skip the overlay). Side-style positions (left/right
# variants) collapse to "right"; top/bottom variants collapse to "bottom".
# Anything else warns and falls back to "right".
.htna_normalize_overlay_pos <- function(position) {
  if (is.null(position) || isFALSE(position) ||
      identical(position, "none")) {
    return(NULL)
  }
  if (!is.character(position) || length(position) != 1L) {
    warning("htna: unsupported legend position; using 'right'.",
            call. = FALSE)
    return("right")
  }
  if (position %in% c("right", "bottom")) return(position)
  fallback <- if (grepl("bottom|top", position, fixed = FALSE)) {
    "bottom"
  } else if (grepl("right|left", position, fixed = FALSE)) {
    "right"
  } else {
    "right"
  }
  warning("htna: legend position '", position,
          "' is not supported by the actor-overlay legend; using '",
          fallback, "'.", call. = FALSE)
  fallback
}

# Replicates Nestimate:::.legend_oma_size and converts the resulting outer
# margin (in lines) to NDC fractions for par(fig=...) so the overlay aligns
# with the actual gutter Nestimate reserved.
.htna_legend_gutter_fig <- function(levels, position,
                                    legend_size = NULL,
                                    legend_ncol = NULL,
                                    legend_title = NULL) {
  if (is.null(legend_size)) {
    din_w <- graphics::par("din")[1]
    legend_size <- max(0.65, min(1.2, din_w / 10))
  }
  label_chars <- max(nchar(as.character(levels)))
  title_chars <- if (!is.null(legend_title)) nchar(legend_title) else 0
  wide_chars  <- max(label_chars, title_chars)
  cw_per_line <- graphics::par("cin")[1] / graphics::par("csi")
  rows <- if (!is.null(legend_ncol) && legend_ncol > 0) {
    ceiling(length(levels) / legend_ncol)
  } else if (position == "bottom") 1L else length(levels)
  cols <- if (!is.null(legend_ncol) && legend_ncol > 0) {
    legend_ncol
  } else if (position == "bottom") length(levels) else 1L
  h <- rows * legend_size * 1.6 +
       (if (!is.null(legend_title)) 1.2 else 0.6)
  w <- cols * ((wide_chars + 3) * cw_per_line * legend_size * 1.15) + 2.5

  csi <- graphics::par("csi")
  din <- graphics::par("din")
  if (position == "right") {
    right_frac <- 1 - (w * csi) / din[1]
    c(max(0, right_frac), 1, 0, 1)
  } else {
    bot_frac <- (h * csi) / din[2]
    c(0, 1, 0, min(1, bot_frac))
  }
}

# Overlay one legend block per actor on top of the gutter Nestimate already
# reserved for its (now-masked) single legend. Position must match the value
# Nestimate received so the gutter location is correct.
.htna_overlay_legend <- function(labels, colors,
                                 position = c("right", "bottom"),
                                 cex = 0.7) {
  position <- match.arg(position)
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)

  fig <- .htna_legend_gutter_fig(labels, position)
  graphics::par(fig = fig, new = TRUE, mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1),
                        xaxs = "i", yaxs = "i")
  graphics::rect(0, 0, 1, 1, col = "white", border = NA)

  if (position == "right") {
    graphics::legend(x = 0.05, y = 0.95,
                     legend = labels, fill = colors,
                     border = NA, bty = "n",
                     xjust = 0, yjust = 1, cex = cex)
  } else {
    graphics::legend(x = 0.5, y = 0.95,
                     legend = labels, fill = colors,
                     border = NA, bty = "n", horiz = TRUE,
                     xjust = 0.5, yjust = 1, cex = cex)
  }
  invisible(NULL)
}

.htna_grouped_legend <- function(res, type_map, actors,
                                 position = c("right", "bottom"),
                                 cex = 0.7,
                                 disambiguated_mapping = NULL) {
  position <- match.arg(position)
  if (is.null(res$levels) || is.null(res$palette)) return(invisible(NULL))

  state_to_actor <- unname(type_map[as.character(res$levels)])
  actor_fac      <- factor(state_to_actor, levels = as.character(actors))
  states_by      <- split(as.character(res$levels), actor_fac)
  colors_by      <- split(unname(res$palette),       actor_fac)
  keep           <- vapply(states_by, length, integer(1L)) > 0L
  if (!any(keep)) return(invisible(NULL))
  states_by <- states_by[keep]
  colors_by <- colors_by[keep]

  # Clean up labels if disambiguated mapping is provided
  if (!is.null(disambiguated_mapping)) {
    states_by <- lapply(states_by, function(states) {
      clean_states <- disambiguated_mapping[states]
      ifelse(is.na(clean_states), states, clean_states)
    })
  }

  n_blocks  <- length(states_by)

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)

  fig <- .htna_legend_gutter_fig(res$levels, position)
  graphics::par(fig = fig, new = TRUE, mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1),
                        xaxs = "i", yaxs = "i")
  graphics::rect(0, 0, 1, 1, col = "white", border = NA)

  lapply(seq_len(n_blocks), function(i) {
    a    <- names(states_by)[i]
    if (position == "right") {
      block_h <- 1 / n_blocks
      y_top   <- 1 - (i - 1) * block_h
      graphics::legend(x = 0.05, y = y_top - 0.01,
                       legend = states_by[[i]], fill = colors_by[[i]],
                       title  = a, border = NA, bty = "n",
                       xjust = 0, yjust = 1, cex = cex)
    } else {
      block_w <- 1 / n_blocks
      x_left  <- (i - 1) * block_w
      graphics::legend(x = x_left + 0.02, y = 0.95,
                       legend = states_by[[i]], fill = colors_by[[i]],
                       title  = a, border = NA, bty = "n",
                       xjust = 0, yjust = 1, cex = cex)
    }
  })
  invisible(NULL)
}

# Build a combined hclust that has one real per-actor subtree, joined at
# the root. Each per-actor hclust is computed independently on that
# actor's compressed sequences (Euclidean distance over integer-coded
# states, ward.D2 linkage). A single trivial actor (n < 2) becomes a
# synthetic single-leaf "tree" that still composes correctly.
.htna_combined_hclust <- function(pieces) {
  per_actor <- lapply(pieces, .htna_actor_hclust)
  Reduce(.htna_join_hclust, per_actor)
}

.htna_actor_hclust <- function(mat) {
  n <- nrow(mat)
  if (n == 0L) return(NULL)
  if (n == 1L) {
    return(list(merge = matrix(integer(0L), 0L, 2L),
                height = numeric(0L), order = 1L,
                labels = NULL, method = "ward.D2",
                call = sys.call(), dist.method = "euclidean",
                class = "hclust"))
  }
  vals  <- as.vector(mat)
  lvls  <- sort(unique(vals[!is.na(vals)]))
  codes <- matrix(match(mat, lvls), nrow = n)
  codes[is.na(codes)] <- 0L
  hc <- stats::hclust(stats::dist(codes, method = "euclidean"),
                      method = "ward.D2")
  hc
}

.htna_join_hclust <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  n_a  <- length(a$order); n_b <- length(b$order)
  m_a  <- nrow(a$merge);   m_b <- nrow(b$merge)

  b_merge <- b$merge
  b_merge[b_merge < 0] <- b_merge[b_merge < 0] - n_a
  b_merge[b_merge > 0] <- b_merge[b_merge > 0] + m_a

  if (n_a == 1L) a_root_id <- -1L else a_root_id <- m_a
  if (n_b == 1L) b_root_id <- -(n_a + 1L) else b_root_id <- m_a + m_b

  joined_merge  <- rbind(a$merge, b_merge,
                         matrix(c(a_root_id, b_root_id), 1L, 2L))
  max_h         <- max(c(a$height, b$height), 0)
  joined_height <- c(a$height, b$height, max_h * 1.5 + 1)
  joined_order  <- c(a$order, b$order + n_a)

  structure(list(merge = joined_merge, height = joined_height,
                 order = joined_order, labels = NULL,
                 method = "ward.D2", call = sys.call(),
                 dist.method = "euclidean"),
            class = "hclust")
}
