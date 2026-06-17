#' Plot Per-Actor Sequences From an htna Network
#'
#' Extracts each actor's events from the combined wide sequence in
#' `net$data` (using `net$node_groups` as the node-to-actor lookup),
#' compresses each session into the actor's own ordered events, pads to a
#' common width, and renders with [Nestimate::sequence_plot()] grouped by
#' actor. Each session contributes one row per actor that had at least one
#' event in it.
#'
#' @param net An htna network from [build_htna()]. Must have `$data` and
#'   `$node_groups` populated.
#' @param by `"state"` (default) keeps state-level colouring with one
#'   row per (session, actor) extracted from `net$data`; `"group"`
#'   re-colours the original combined session matrix by actor (each cell
#'   = the actor that acted at that time step).
#' @param type Sequence plot layout: `"index"` (default; one panel per
#'   actor, vertically stacked), `"heatmap"` (single carpet with a white
#'   separator at the actor boundary, controllable via `k_line_width`), or
#'   `"distribution"` (one stacked-area panel per actor).
#' @param grouped_legend Logical. If `TRUE` (default) and `by = "state"`,
#'   the per-state legend is split into one block per actor with the actor
#'   name as a sub-title.
#' @param x Same as `net`; used when calling via the `plot_sequences()`
#'   generic.
#' @param ... Forwarded to [Nestimate::sequence_plot()].
#'
#' @return Invisibly, the list returned by [Nestimate::sequence_plot()].
#'
#' @seealso [Nestimate::sequence_plot()], [build_htna()].
#'
#' @examples
#' \dontrun{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#'
#' sequence_plot_htna(net)                       # index, faceted
#' sequence_plot_htna(net, type = "heatmap")     # single carpet, white gulf
#' sequence_plot_htna(net, type = "distribution")
#' }
#'
#' @export
sequence_plot_htna <- function(net,
                               by   = c("state", "group"),
                               type = c("index", "heatmap", "distribution"),
                               grouped_legend = TRUE,
                               ...) {
  if (inherits(net, "htna_group") ||
      (is.list(net) && !inherits(net, "htna") && !is.data.frame(net))) {
    if (length(net) == 0L) stop("Empty htna_group.", call. = FALSE)
    nms <- names(net) %||% as.character(seq_along(net))
    for (i in seq_along(net)) {
      sequence_plot_htna(net[[i]], by = match.arg(by), type = match.arg(type),
                         grouped_legend = grouped_legend,
                         main = nms[i], ...)
    }
    return(invisible(net))
  }
  by   <- match.arg(by)
  type <- match.arg(type)

  if (!inherits(net, "htna")) {
    stop("`net` must be an htna network produced by build_htna().",
         call. = FALSE)
  }
  if (is.null(net$data) || !is.data.frame(net$data)) {
    stop("`net$data` is missing or not a data frame; rebuild with ",
         "`build_htna()`.", call. = FALSE)
  }
  if (is.null(net$node_groups) || !nrow(net$node_groups)) {
    stop("`net` has no actor partition (`$node_groups` is empty).",
         call. = FALSE)
  }

  type_map  <- setNames(as.character(net$node_groups$group),
                        as.character(net$node_groups$node))
  # Canonical actor order from build_htna() (Human, AI, …) so colours
  # and stacking match plot_htna(). Falls back to encounter order if
  # the network was built without `$actor_levels`.
  actors    <- net$actor_levels %||%
               unique(as.character(net$node_groups$group))

  if (by == "group") {
    actor_chr <- as.character(actors)
    color_map <- .htna_actor_colors(actor_chr)

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
      base_code_map <- character(length(all_states))
      names(base_code_map) <- all_states

      for (i in seq_along(all_states)) {
        state <- all_states[i]
        if (grepl(":", state, fixed = TRUE)) {
          # Extract base code from disambiguated code (e.g., "Human:Ask" -> "Ask")
          base_code_map[state] <- strsplit(state, ":", fixed = TRUE)[[1]][2]
        } else {
          base_code_map[state] <- state
        }
      }

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
      state_colors <- character(length(all_states_sorted))
      for (i in seq_along(all_states_sorted)) {
        state <- all_states_sorted[i]
        base_code <- base_code_map[state]
        state_colors[i] <- base_colors[base_code]
      }

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

#' Sequence plot generic
#'
#' S3 generic dispatched on `x`. Calling `plot_sequences(net)` on an htna
#' network forwards to [sequence_plot_htna()].
#'
#' @param x An object to plot.
#' @param ... Forwarded to the method.
#' @return Method-defined.
#' @examples
#' \donttest{
#' data(human_long, ai_long, package = "Nestimate")
#' net <- build_htna(list(Human = human_long, AI = ai_long))
#' plot_sequences(net)
#' }
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

  for (i in seq_len(n_blocks)) {
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
  }
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
