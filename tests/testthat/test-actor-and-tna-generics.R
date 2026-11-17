# actor = (resampling whole actors, done by Nestimate and reached through
# ...) and the registration
# of the htna methods on tna's bootstrap() / plot_sequences() generics.

nestimate_takes_actor <- function(fn) {
  "actor" %in% names(formals(getExportedValue("Nestimate", fn)))
}

describe("actor =", {

  it("resamples whole projects in permutation and bootstrap", {
    grp <- build_htna(human_ai, actor_type = "actor_type", group = "phase",
                      actor = "project")
    perm <- permutation_htna(grp, iter = 20, seed = 1, actor = "project")
    expect_s3_class(perm, "net_permutation_group")
    one <- build_htna(human_ai, actor_type = "actor_type", actor = "project")
    boot <- bootstrap_htna(one, iter = 20, seed = 1, actor = "project")
    expect_s3_class(boot, "htna_bootstrap")
  })

  it("reaches casedrop and centrality stability through ...", {
    skip_if_not(nestimate_takes_actor("casedrop_reliability") &&
                  nestimate_takes_actor("centrality_stability"),
                "installed Nestimate has no actor argument")
    one <- build_htna(human_ai, actor_type = "actor_type", actor = "project")
    res <- casedrop_reliability_htna(one, iter = 5, seed = 1,
                                     actor = "project")
    expect_s3_class(res, "net_casedrop_reliability")
    cs <- centrality_stability_htna(one, iter = 5, seed = 1,
                                    actor = "project")
    expect_s3_class(cs, "htna_stability")
  })

  it("leaves calls without actor unchanged", {
    one <- build_htna(human_ai, actor_type = "actor_type")
    expect_s3_class(centrality_stability_htna(one, iter = 5, seed = 1),
                    "htna_stability")
  })
})

describe("tna's generics reach the htna methods", {

  it("dispatches tna::bootstrap() on an htna network", {
    skip_if_not_installed("tna")
    one <- build_htna(human_ai, actor_type = "actor_type")
    boot <- tna::bootstrap(one, iter = 20, seed = 1)
    expect_s3_class(boot, "htna_bootstrap")
  })

  it("maps tna's argument names onto the htna bootstrap", {
    skip_if_not_installed("tna")
    one <- build_htna(human_ai, actor_type = "actor_type")
    via_tna <- tna::bootstrap(one, iter = 20, level = 0.1, seed = 3)
    direct <- bootstrap_htna(one, iter = 20, ci_level = 0.1, seed = 3)
    expect_equal(summary(via_tna), summary(direct))
  })

  it("dispatches tna::plot_sequences() on an htna network", {
    skip_if_not_installed("tna")
    one <- build_htna(human_ai, actor_type = "actor_type")
    grDevices::pdf(nullfile())
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_no_error(tna::plot_sequences(one))
  })
})
