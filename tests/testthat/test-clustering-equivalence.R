# Cross-package, cell-level equivalence for clustered HTNA construction.
# These tests compare build_htna() with ordinary Nestimate results and manual
# per-assignment builds, so shared metadata plumbing cannot make a wrong
# transition matrix look correct.

.htna_cluster_equiv_data <- function(n_sessions, session_length,
                                     n_human, n_ai, seed) {
  set.seed(seed)
  human <- paste0("H", seq_len(n_human))
  ai <- paste0("A", seq_len(n_ai))
  states <- c(human, ai)
  transition <- matrix(
    stats::runif(length(states)^2, min = 0.05, max = 1),
    nrow = length(states),
    dimnames = list(states, states)
  )
  transition <- transition / rowSums(transition)

  rows <- lapply(seq_len(n_sessions), function(i) {
    sequence <- character(session_length)
    sequence[1L] <- sample(states, 1L)
    for (j in seq_len(session_length - 1L)) {
      sequence[j + 1L] <- sample(states, 1L, prob = transition[sequence[j], ])
    }
    data.frame(
      session_id = paste0("S", i),
      code = sequence,
      order_in_session = seq_len(session_length),
      stringsAsFactors = FALSE
    )
  })
  data <- do.call(rbind, rows)
  data$code[seq_along(states)] <- states
  rownames(data) <- NULL
  list(
    data = data,
    node_groups = list(Human = human, AI = ai)
  )
}

.htna_plain_copy <- function(net) {
  class(net) <- setdiff(class(net), "htna")
  net$node_groups <- NULL
  net$actor_levels <- NULL
  net$nodes$groups <- NULL
  net
}

.expect_htna_cluster_matrix <- function(x, y, tolerance = 1e-13) {
  expect_setequal(rownames(x), rownames(y))
  expect_setequal(colnames(x), colnames(y))
  y <- y[rownames(x), colnames(x), drop = FALSE]
  expect_equal(unname(x), unname(y), tolerance = tolerance)
}

.expect_htna_cluster_net_equiv <- function(x, y, actor_levels,
                                           info = NULL) {
  .expect_htna_cluster_matrix(x$weights, y$weights)
  if (is.null(x$initial) || is.null(y$initial)) {
    expect_identical(is.null(x$initial), is.null(y$initial), info = info)
  } else {
    expect_setequal(names(x$initial), names(y$initial))
    expect_equal(
      unname(x$initial), unname(y$initial[names(x$initial)]),
      tolerance = 1e-13, info = info
    )
  }
  expect_identical(x$method, y$method, info = info)
  expect_setequal(as.character(x$nodes$label), as.character(y$nodes$label))

  x_data <- x$data
  y_data <- y$data
  rownames(x_data) <- NULL
  rownames(y_data) <- NULL
  expect_equal(x_data, y_data, info = info)

  expect_s3_class(x, "htna")
  expect_identical(x$actor_levels, actor_levels, info = info)
  expect_identical(as.character(x$node_groups$node),
                   as.character(x$nodes$label), info = info)
  observed <- stats::setNames(
    as.character(x$node_groups$group),
    as.character(x$node_groups$node)
  )
  expected <- ifelse(startsWith(names(observed), "H"), "Human", "AI")
  expect_identical(unname(observed), expected, info = info)
  expect_identical(as.character(x$nodes$groups), unname(observed), info = info)
}

set.seed(20260721)
.htna_cluster_configs <- lapply(seq_len(10L), function(i) {
  list(
    n_sessions = sample(c(18L, 24L, 30L), 1L),
    session_length = sample(c(7L, 10L, 13L), 1L),
    n_human = sample(2:4, 1L),
    n_ai = sample(2:4, 1L),
    k = sample(2:3, 1L),
    cluster_method = sample(c("pam", "ward.D2", "complete"), 1L),
    seed = sample.int(100000L, 1L)
  )
})

test_that("build_htna distance-clustering paths equal manual networks across methods", {
  checked_cells <- 0L

  for (cfg in .htna_cluster_configs) {
    fixture <- .htna_cluster_equiv_data(
      cfg$n_sessions, cfg$session_length, cfg$n_human, cfg$n_ai, cfg$seed
    )
    base <- suppressWarnings(build_htna(
      fixture$data, node_groups = fixture$node_groups
    ))
    clustering <- Nestimate::build_clusters(
      base,
      k = cfg$k,
      method = cfg$cluster_method,
      seed = cfg$seed
    )
    plain_clustering <- clustering
    plain_clustering$htna_partition <- NULL

    for (network_method in c("relative", "frequency", "attention")) {
      from_clustering <- build_htna(
        clustering,
        node_groups = base$node_groups,
        method = network_method
      )
      plain_group <- Nestimate::build_network(
        plain_clustering, method = network_method
      )
      from_fitted_group <- build_htna(
        plain_group, node_groups = base$node_groups
      )

      expect_s3_class(from_clustering, "htna_group")
      expect_identical(names(from_clustering), names(plain_group))
      expect_identical(attr(from_clustering, "clustering")$assignments,
                       clustering$assignments)
      expect_equal(attr(from_clustering, "clustering")$distance,
                   clustering$distance)
      expect_equal(attr(from_clustering, "clustering")$silhouette,
                   clustering$silhouette)

      for (cluster_id in seq_len(cfg$k)) {
        subset <- clustering$data[
          clustering$assignments == cluster_id, , drop = FALSE
        ]
        manual <- Nestimate::build_network(
          subset, method = network_method
        )
        info <- paste0(
          "seed=", cfg$seed, " method=", network_method,
          " cluster=", cluster_id
        )
        .expect_htna_cluster_net_equiv(
          from_clustering[[cluster_id]], manual, base$actor_levels, info
        )
        .expect_htna_cluster_net_equiv(
          from_fitted_group[[cluster_id]], manual, base$actor_levels, info
        )
        checked_cells <- checked_cells + 2L * length(manual$weights)
      }
    }
  }

  expect_gte(checked_cells, 2500L)
})

test_that("Nestimate direct clustering and build_htna conversion are equivalent", {
  checked_cells <- 0L

  for (cfg in .htna_cluster_configs[seq_len(8L)]) {
    fixture <- .htna_cluster_equiv_data(
      cfg$n_sessions, cfg$session_length, cfg$n_human, cfg$n_ai, cfg$seed
    )
    base <- suppressWarnings(build_htna(
      fixture$data, node_groups = fixture$node_groups
    ))

    for (network_method in c("relative", "frequency", "attention")) {
      direct <- Nestimate::cluster_network(
        base,
        k = cfg$k,
        cluster_by = cfg$cluster_method,
        seed = cfg$seed,
        method = network_method
      )
      clustering <- Nestimate::build_clusters(
        base,
        k = cfg$k,
        method = cfg$cluster_method,
        seed = cfg$seed
      )
      via_constructor <- build_htna(
        clustering,
        node_groups = base$node_groups,
        method = network_method
      )
      direct_htna <- build_htna(direct, node_groups = base$node_groups)

      expect_identical(attr(direct, "clustering")$assignments,
                       clustering$assignments)
      expect_identical(attr(direct_htna, "clustering")$assignments,
                       attr(via_constructor, "clustering")$assignments)
      for (cluster_id in seq_len(cfg$k)) {
        .expect_htna_cluster_net_equiv(
          direct_htna[[cluster_id]], via_constructor[[cluster_id]],
          base$actor_levels,
          paste("direct", cfg$seed, network_method, cluster_id)
        )
        checked_cells <- checked_cells + length(direct[[cluster_id]]$weights)
      }
    }
  }

  expect_gte(checked_cells, 900L)
})

test_that("build_htna consumes cluster_mmm losslessly across repeated fits", {
  checked_cells <- 0L
  seeds <- c(101L, 211L, 307L, 401L, 503L, 601L)

  for (seed in seeds) {
    fixture <- .htna_cluster_equiv_data(30L, 10L, 3L, 3L, seed)
    base <- suppressWarnings(build_htna(
      fixture$data, node_groups = fixture$node_groups
    ))
    plain <- .htna_plain_copy(base)
    args <- list(k = 2L, n_starts = 2L, max_iter = 40L, seed = seed)

    plain_group <- do.call(
      Nestimate::cluster_mmm, c(list(data = plain), args)
    )
    direct_group <- do.call(
      Nestimate::cluster_mmm, c(list(data = base), args)
    )
    weights_before <- lapply(plain_group, function(net) net$weights)
    converted <- build_htna(plain_group, node_groups = base$node_groups)
    converted_direct <- build_htna(
      direct_group, node_groups = base$node_groups
    )

    fit_plain <- attr(plain_group, "clustering")
    fit_direct <- attr(direct_group, "clustering")
    fit_converted <- attr(converted, "clustering")

    expect_identical(fit_direct$assignments, fit_plain$assignments)
    expect_equal(fit_direct$posterior, fit_plain$posterior, tolerance = 1e-12)
    expect_equal(fit_direct$mixing, fit_plain$mixing, tolerance = 1e-12)
    expect_equal(c(fit_direct$AIC, fit_direct$BIC, fit_direct$ICL),
                 c(fit_plain$AIC, fit_plain$BIC, fit_plain$ICL),
                 tolerance = 1e-12)
    expect_identical(fit_converted$assignments, fit_plain$assignments)
    expect_equal(fit_converted$posterior, fit_plain$posterior,
                 tolerance = 1e-12)
    expect_equal(lapply(converted, function(net) net$weights), weights_before)

    for (cluster_id in seq_along(plain_group)) {
      .expect_htna_cluster_net_equiv(
        converted[[cluster_id]], plain_group[[cluster_id]],
        base$actor_levels, paste("MMM plain", seed, cluster_id)
      )
      .expect_htna_cluster_net_equiv(
        converted_direct[[cluster_id]], plain_group[[cluster_id]],
        base$actor_levels, paste("MMM direct", seed, cluster_id)
      )
      checked_cells <- checked_cells + 2L * length(plain_group[[cluster_id]]$weights)
    }
  }

  expect_gte(checked_cells, 700L)
})

test_that("legacy tna::cluster_sequences conversion equals manual assignment builds", {
  skip_if_not_installed("tna")
  checked_cells <- 0L

  for (cfg in .htna_cluster_configs[seq_len(6L)]) {
    fixture <- .htna_cluster_equiv_data(
      cfg$n_sessions, cfg$session_length, cfg$n_human, cfg$n_ai, cfg$seed
    )
    base <- suppressWarnings(build_htna(
      fixture$data, node_groups = fixture$node_groups
    ))
    sequence_data <- Nestimate::build_clusters(
      base, k = cfg$k, seed = cfg$seed
    )$data
    # tna 1.2.2 lower-cases "ward.D2" before passing it to hclust(), which
    # rejects "ward.d2". Use another supported legacy method for that case;
    # the equivalence target here is the assignment-to-network conversion.
    legacy_method <- if (identical(cfg$cluster_method, "ward.D2")) {
      "complete"
    } else {
      cfg$cluster_method
    }
    legacy <- tna::cluster_sequences(
      sequence_data,
      k = cfg$k,
      method = legacy_method
    )

    for (network_method in c("relative", "frequency", "attention")) {
      result <- build_htna(
        legacy,
        node_groups = base$node_groups,
        method = network_method
      )
      expect_identical(attr(result, "clustering")$assignments,
                       legacy$assignments)
      expect_equal(attr(result, "clustering")$distance, legacy$distance)

      for (cluster_id in seq_len(cfg$k)) {
        subset <- legacy$data[
          legacy$assignments == cluster_id, , drop = FALSE
        ]
        manual <- Nestimate::build_network(subset, method = network_method)
        .expect_htna_cluster_net_equiv(
          result[[cluster_id]], manual, base$actor_levels,
          paste("legacy", cfg$seed, network_method, cluster_id)
        )
        checked_cells <- checked_cells + length(manual$weights)
      }
    }
  }

  expect_gte(checked_cells, 600L)
})

test_that("enhanced Nestimate clustering is already a valid HTNA group", {
  fixture <- .htna_cluster_equiv_data(24L, 9L, 3L, 3L, 727L)
  base <- suppressWarnings(build_htna(
    fixture$data, node_groups = fixture$node_groups
  ))
  grouped <- Nestimate::cluster_network(base, k = 2, seed = 727L)

  if (!inherits(grouped, "htna_group")) {
    skip("Installed Nestimate predates actor-preserving HTNA clustering.")
  }

  expect_identical(attr(grouped, "actor_levels"), base$actor_levels)
  expect_s3_class(attr(grouped, "clustering"), "net_clustering")
  for (net in grouped) {
    expect_s3_class(net, "htna")
    expect_identical(net$actor_levels, base$actor_levels)
    expect_identical(as.character(net$node_groups$node),
                     as.character(net$nodes$label))
    expect_false(anyNA(net$node_groups$group))
  }

  round_trip <- build_htna(grouped)
  expect_equal(lapply(round_trip, function(net) net$weights),
               lapply(grouped, function(net) net$weights))
  expect_identical(attr(round_trip, "clustering")$assignments,
                   attr(grouped, "clustering")$assignments)
})
