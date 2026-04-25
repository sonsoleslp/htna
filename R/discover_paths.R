# Internal core shared by extract_meta_paths() and find_patterns().
# Operates on integer-encoded sequences (each state mapped to 0..T-1) and an
# alphabet vector that maps the integer back to a label. extract_meta_paths
# uses alphabet = actor types; find_patterns uses alphabet = node labels.

.discover_paths <- function(int_seqs,
                            alphabet,
                            len         = 2:4,
                            gap         = 1L,
                            type        = c("contiguous", "gapped"),
                            pattern     = NULL,
                            min_freq    = 1L,
                            min_support = 0,
                            min_lift    = 0,
                            start       = NULL,
                            end         = NULL,
                            contain     = NULL) {

  type <- match.arg(type)
  T <- length(alphabet)
  n_seq <- length(int_seqs)

  if (type == "contiguous") {
    gap_vals <- 0L
  } else {
    stopifnot(
      is.numeric(gap), length(gap) >= 1L, all(gap >= 0L),
      all(gap == as.integer(gap))
    )
    gap_vals <- sort(unique(as.integer(gap)))
    if (identical(gap_vals, 0L)) gap_vals <- 1L  # nocov
  }

  marg_p <- .marginal_freq(int_seqs, T)

  if (!is.null(pattern)) {
    parts_idx <- .parse_pattern(pattern, alphabet)
    rows <- lapply(gap_vals, function(g)
      .search_pattern(int_seqs, parts_idx, T, g, n_seq, alphabet, marg_p))
  } else {
    stopifnot(
      is.numeric(len), length(len) >= 1L, all(len >= 2L),
      all(len == as.integer(len))
    )
    len_vals <- sort(unique(as.integer(len)))
    rows <- list()
    for (g in gap_vals) for (k in len_vals) {
      rows[[length(rows) + 1L]] <-
        .enumerate_paths(int_seqs, T, k, g, n_seq, alphabet, marg_p)
    }
  }

  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) {
    return(.empty_paths_df())
  }

  out$support   <- out$n_sequences / n_seq
  out$frequency <- stats::ave(
    out$count, out$length, out$gap,
    FUN = function(v) if (sum(v) > 0) v / sum(v) else v * 0
  )

  out <- .filter_paths(out, min_freq, min_support, min_lift,
                       start, end, contain)
  out <- out[order(out$length, out$gap, -out$count), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# ---- Encoding ---------------------------------------------------------------

.encode_int_seqs <- function(x, alphabet, label_to_alphabet) {
  state_cols <- grep("^T[0-9]+$", names(x$data), value = TRUE)
  if (!length(state_cols)) state_cols <- names(x$data)
  mat <- as.matrix(x$data[, state_cols, drop = FALSE])
  storage.mode(mat) <- "character"
  lookup <- match(label_to_alphabet, alphabet) - 1L
  names(lookup) <- names(label_to_alphabet)
  lapply(seq_len(nrow(mat)), function(i) {
    v <- mat[i, ]
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return(integer(0L))
    unname(lookup[v])
  })
}

.window_codes <- function(v, k, gap, T) {
  span <- (k - 1L) * (gap + 1L)
  n_valid <- length(v) - span
  if (n_valid < 1L) return(integer(0L))
  codes <- integer(n_valid)
  for (j in seq_len(k)) {
    start <- 1L + (j - 1L) * (gap + 1L)
    codes <- codes + (as.integer(T) ^ (j - 1L)) *
      v[start:(start + n_valid - 1L)]
  }
  codes
}

.marginal_freq <- function(int_seqs, T) {
  flat <- unlist(int_seqs, use.names = FALSE)
  if (!length(flat)) return(rep(1 / T, T))
  tabulate(flat + 1L, nbins = T) / length(flat)
}


# ---- Pattern parsing (with wildcards) --------------------------------------

.parse_pattern <- function(pattern, alphabet) {
  stopifnot(is.character(pattern), length(pattern) == 1L, nzchar(pattern))
  parts <- strsplit(gsub("\\s+", "", pattern), "->", fixed = TRUE)[[1]]
  if (length(parts) < 2L) {
    stop("`pattern` must have at least two elements separated by '->'.",
         call. = FALSE)
  }
  parts_idx <- vector("list", length(parts))
  for (i in seq_along(parts)) {
    if (parts[i] == "*") {
      parts_idx[[i]] <- seq_len(length(alphabet)) - 1L
    } else {
      m <- match(parts[i], alphabet)
      if (is.na(m)) {
        stop("Unknown alphabet element in pattern: '", parts[i], "'. ",
             "Known: ", paste(alphabet, collapse = ", "), ".",
             call. = FALSE)
      }
      parts_idx[[i]] <- m - 1L
    }
  }
  parts_idx
}


# ---- Total windows of length k with gap g across all sequences ------------

.total_windows <- function(int_seqs, k, gap) {
  span <- (k - 1L) * (gap + 1L)
  sum(pmax(0L, lengths(int_seqs) - span))
}


# ---- Enumerate all paths of given length ----------------------------------

.enumerate_paths <- function(int_seqs, T, k, gap, n_seq, alphabet, marg_p) {
  n_codes <- as.integer(T) ^ k
  total  <- integer(n_codes)
  in_seq <- integer(n_codes)
  for (s in int_seqs) {
    codes <- .window_codes(s, k, gap, T)
    if (!length(codes)) next
    tab <- tabulate(codes + 1L, nbins = n_codes)
    total  <- total + tab
    in_seq <- in_seq + (tab > 0L)
  }
  keep <- total > 0L
  if (!any(keep)) return(NULL)
  codes_kept <- which(keep) - 1L
  digits <- .decode_codes(codes_kept, k, T)
  schema <- apply(digits, 1L, function(d)
    paste(alphabet[d + 1L], collapse = "->"))
  expected <- apply(digits, 1L, function(d) prod(marg_p[d + 1L]))
  data.frame(
    schema        = schema,
    length        = k,
    gap           = gap,
    count         = total[keep],
    n_sequences   = in_seq[keep],
    expected_p    = expected,
    total_windows = .total_windows(int_seqs, k, gap),
    stringsAsFactors = FALSE
  )
}


# ---- Search a specific pattern (possibly with wildcards) ------------------

.search_pattern <- function(int_seqs, parts_idx, T, gap, n_seq,
                            alphabet, marg_p) {
  k <- length(parts_idx)
  expanded <- expand.grid(parts_idx, KEEP.OUT.ATTRS = FALSE)
  expanded <- as.matrix(expanded)
  weights <- as.integer(T) ^ (0L:(k - 1L))
  targets <- as.integer(expanded %*% weights)

  total  <- integer(length(targets))
  in_seq <- integer(length(targets))
  for (s in int_seqs) {
    codes <- .window_codes(s, k, gap, T)
    if (!length(codes)) next
    m <- match(codes, targets)
    m <- m[!is.na(m)]
    if (!length(m)) next
    tab <- tabulate(m, nbins = length(targets))
    total  <- total + tab
    in_seq <- in_seq + (tab > 0L)
  }
  keep <- total > 0L
  if (!any(keep)) return(NULL)
  schema <- apply(expanded[keep, , drop = FALSE], 1L,
                  function(d) paste(alphabet[d + 1L], collapse = "->"))
  expected <- apply(expanded[keep, , drop = FALSE], 1L,
                    function(d) prod(marg_p[d + 1L]))
  data.frame(
    schema        = schema,
    length        = k,
    gap           = gap,
    count         = total[keep],
    n_sequences   = in_seq[keep],
    expected_p    = expected,
    total_windows = .total_windows(int_seqs, k, gap),
    stringsAsFactors = FALSE
  )
}


# ---- Decode integer codes to base-T digit matrix --------------------------

.decode_codes <- function(codes, k, T) {
  digits <- matrix(0L, nrow = length(codes), ncol = k)
  rem <- codes
  for (j in seq_len(k)) {
    digits[, j] <- rem %% as.integer(T)
    rem <- rem %/% as.integer(T)
  }
  digits
}


# ---- Filtering --------------------------------------------------------------

.filter_paths <- function(out, min_freq, min_support, min_lift,
                          start, end, contain) {
  expected_count <- out$expected_p * out$total_windows
  out$lift <- ifelse(expected_count > 0,
                     out$count / expected_count, NA_real_)

  keep <- out$count >= min_freq & out$support >= min_support
  if (min_lift > 0) keep <- keep & !is.na(out$lift) & out$lift >= min_lift

  if (!is.null(start)) {
    first <- vapply(strsplit(out$schema, "->", fixed = TRUE),
                    `[`, character(1L), 1L)
    keep <- keep & first %in% start
  }
  if (!is.null(end)) {
    parts_list <- strsplit(out$schema, "->", fixed = TRUE)
    last <- vapply(parts_list, function(p) p[length(p)], character(1L))
    keep <- keep & last %in% end
  }
  if (!is.null(contain)) {
    parts_list <- strsplit(out$schema, "->", fixed = TRUE)
    has_all <- vapply(parts_list,
                      function(p) all(contain %in% p), logical(1L))
    keep <- keep & has_all
  }
  out$expected_p <- NULL
  out$total_windows <- NULL
  out[keep, , drop = FALSE]
}

.empty_paths_df <- function() {
  data.frame(
    schema = character(0L), length = integer(0L), gap = integer(0L),
    count = integer(0L), n_sequences = integer(0L),
    support = numeric(0L), frequency = numeric(0L), lift = numeric(0L),
    stringsAsFactors = FALSE
  )
}
