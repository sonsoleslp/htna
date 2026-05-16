test_that("extract_meta_paths() default returns state-level patterns", {
  net <- make_htna()
  sp  <- extract_meta_paths(net, length = 2:3)

  expect_s3_class(sp, "htna_meta_paths")
  expect_s3_class(sp, "htna_paths")
  expect_s3_class(sp, "data.frame")
  expect_true(all(c("schema", "meta_schema", "length", "count", "support",
                    "frequency", "lift") %in% names(sp)))
  expect_identical(attr(sp, "level"), "state")
  # alphabet is the set of concrete states (node labels)
  expect_setequal(attr(sp, "alphabet"),
                  as.character(net$node_groups$node))
})

test_that("extract_meta_paths(level = 'type') returns type-level meta-paths", {
  net <- make_htna()
  mp  <- extract_meta_paths(net, level = "type", length = 2:3)

  expect_s3_class(mp, "htna_meta_paths")
  expect_identical(attr(mp, "level"), "type")
  expect_false("meta_schema" %in% names(mp))
  expect_setequal(attr(mp, "alphabet"), c("Human", "AI"))
})

test_that("schema returns concrete state instances of the meta-template", {
  net <- make_htna()
  type_map <- setNames(as.character(net$node_groups$group),
                       as.character(net$node_groups$node))

  mp <- extract_meta_paths(net, schema = "Human->AI->Human")
  expect_gt(nrow(mp), 0L)
  expect_true("meta_schema" %in% names(mp))
  expect_true(all(mp$meta_schema == "Human->AI->Human"))
  expect_true(all(mp$length == 3L))
  parts <- strsplit(mp$schema, "->", fixed = TRUE)
  expect_true(all(vapply(parts, function(p)
    identical(unname(type_map[p]), c("Human", "AI", "Human")),
    logical(1L))))
  # Concrete instances aggregate back to the type-level meta-path count.
  type_total <- sum(mp$count)
  agg <- extract_meta_paths(net, level = "type", length = 3L)
  expect_equal(type_total,
               agg$count[agg$schema == "Human->AI->Human"])
})

test_that("schema with wildcard and mixed parts expands correctly", {
  net <- make_htna()
  type_map <- setNames(as.character(net$node_groups$group),
                       as.character(net$node_groups$node))

  mp_w <- extract_meta_paths(net, schema = "Human->*->Human")
  expect_gt(nrow(mp_w), 0L)
  first <- vapply(strsplit(mp_w$schema, "->", fixed = TRUE),
                  `[`, character(1L), 1L)
  last  <- vapply(strsplit(mp_w$schema, "->", fixed = TRUE),
                  function(p) p[length(p)], character(1L))
  expect_true(all(type_map[first] == "Human"))
  expect_true(all(type_map[last]  == "Human"))
  expect_true(any(grepl("Human->Human->Human", mp_w$meta_schema, fixed = TRUE)))
  expect_true(any(grepl("Human->AI->Human",    mp_w$meta_schema, fixed = TRUE)))

  mp_m <- extract_meta_paths(net, schema = "Human->Ask->Human")
  expect_gt(nrow(mp_m), 0L)
  mids <- vapply(strsplit(mp_m$schema, "->", fixed = TRUE),
                 `[`, character(1L), 2L)
  expect_true(all(mids == "Ask"))
})

test_that("min_lift filter keeps only over-represented paths", {
  net <- make_htna()
  mp  <- extract_meta_paths(net, level = "type", length = 3, min_lift = 1.2)
  expect_gt(nrow(mp), 0L)
  expect_true(all(mp$lift >= 1.2))
})

test_that("extract_meta_paths() errors on non-htna input", {
  expect_error(extract_meta_paths(list()), regexp = "build_htna")
  expect_error(extract_meta_paths(list(), level = "type"), regexp = "build_htna")
})

test_that("type='gapped' rejects gap=0 with a targeted message", {
  net <- make_htna()
  expect_error(
    extract_meta_paths(net, level = "type", length = 2,
                       type = "gapped", gap = 0L),
    regexp = "contiguous"
  )
  expect_error(
    extract_meta_paths(net, length = 2, type = "gapped", gap = c(0L, 1L)),
    regexp = "contiguous"
  )
  expect_error(
    extract_meta_paths(net, schema = "Human->AI",
                       type = "gapped", gap = 0L),
    regexp = "contiguous"
  )
})

test_that("state-level schema honours gapped, multi-gap, and all filters", {
  net <- make_htna()

  r1 <- extract_meta_paths(net, schema = "Human->AI->Human",
                           type = "gapped", gap = 1L)
  expect_gt(nrow(r1), 0L)
  expect_true(all(r1$gap == 1L))
  expect_true(all(r1$meta_schema == "Human->AI->Human"))

  r2 <- extract_meta_paths(net, schema = "Human->AI->Human",
                           type = "gapped", gap = 1:2)
  expect_setequal(unique(r2$gap), c(1L, 2L))

  r3 <- extract_meta_paths(net, schema = "Human->AI->Human", min_count = 20)
  expect_true(all(r3$count >= 20L))
  r4 <- extract_meta_paths(net, schema = "Human->AI->Human",
                           min_support = 0.05)
  expect_true(all(r4$support >= 0.05))
  r5 <- extract_meta_paths(net, schema = "Human->AI->Human", min_lift = 2)
  expect_true(all(r5$lift >= 2))

  r6 <- extract_meta_paths(net, schema = "Human->AI->Human",
                           start = "Command")
  expect_gt(nrow(r6), 0L)
  expect_true(all(vapply(strsplit(r6$schema, "->", fixed = TRUE),
                         `[`, character(1L), 1L) == "Command"))
  r7 <- extract_meta_paths(net, schema = "Human->*->Human",
                           contain = "Execute")
  expect_gt(nrow(r7), 0L)
  expect_true(all(vapply(strsplit(r7$schema, "->", fixed = TRUE),
                         function(p) "Execute" %in% p, logical(1L))))
})

test_that("all-wildcard schema agrees with default state-level enumeration", {
  net <- make_htna()
  via_schema <- extract_meta_paths(net, schema = "*->*->*")
  via_state  <- extract_meta_paths(net, length = 3L)
  expect_equal(nrow(via_schema), nrow(via_state))
  m <- merge(via_schema[, c("schema", "count")],
             via_state[,  c("schema", "count")],
             by = "schema")
  expect_equal(nrow(m), nrow(via_schema))
  expect_equal(m$count.x, m$count.y)
})

test_that("schema branch handles short, long, and fully-concrete templates", {
  net <- make_htna()
  type_map <- setNames(as.character(net$node_groups$group),
                       as.character(net$node_groups$node))

  r2 <- extract_meta_paths(net, schema = "Human->AI")
  expect_gt(nrow(r2), 0L)
  expect_true(all(r2$length == 2L))
  expect_true(all(r2$meta_schema == "Human->AI"))

  r4 <- extract_meta_paths(net, schema = "Human->AI->Human->AI")
  expect_gt(nrow(r4), 0L)
  expect_true(all(r4$length == 4L))
  expect_true(all(r4$meta_schema == "Human->AI->Human->AI"))

  rc <- extract_meta_paths(net, schema = "Command->Ask->Correct")
  expect_equal(nrow(rc), 1L)
  expect_equal(rc$schema,      "Command->Ask->Correct")
  expect_equal(rc$meta_schema, "Human->AI->Human")

  rm <- extract_meta_paths(net, schema = "Human->Ask->*")
  expect_gt(nrow(rm), 0L)
  resolved <- vapply(strsplit(rm$schema, "->", fixed = TRUE),
                     function(p) paste(type_map[p], collapse = "->"),
                     character(1L))
  expect_equal(rm$meta_schema, resolved)
})

test_that("level='type' schema resolves concrete codes to their type", {
  net <- make_htna()
  # 'Ask' is an AI code, so this should resolve to Human->AI->Human.
  via_concrete <- extract_meta_paths(net, level = "type",
                                     schema = "Human->Ask->Human")
  via_types    <- extract_meta_paths(net, level = "type",
                                     schema = "Human->AI->Human")
  expect_equal(via_concrete$schema, via_types$schema)
  expect_equal(via_concrete$count,  via_types$count)
})

test_that("schema branch returns a well-formed empty frame on no matches", {
  net <- make_htna()
  empty <- extract_meta_paths(net, schema = "Command->Ask->Correct",
                              min_count = 1e9)
  expect_s3_class(empty, "htna_meta_paths")
  expect_equal(nrow(empty), 0L)
  expect_no_error(capture.output(print(empty)))
})
