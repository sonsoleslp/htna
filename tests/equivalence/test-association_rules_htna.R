# association_rules_htna(): Apriori association rules (Agrawal and Srikant 1994) over the sessions of an htna
# network, each session the set of codes it contains; it forwards to Nestimate::association_rules(). Reference:
# arules::apriori() and arules::interestMeasure() for the rules with one consequent item (arules mines only those),
# as in Nestimate's suite. Re-expressed from htna's equivalence/test-equiv-delegation.R, which compared the wrapper
# with the Nestimate call it makes.

data(human_ai)

# One key per rule: "a, b => c" and "a => b, c" must not be one key.
ref_rule_key <- \(rules) data.frame(rule = paste(rules$antecedent, "=>", rules$consequent),
                                    subset(rules, select = -c(antecedent, consequent)))
ref_items <- \(items) vapply(items, \(set) paste(sort(set), collapse = ", "), character(1))

test_that("rules with one consequent and their measures equal arules::apriori() on the sessions", {
  skip_without_reference("arules")
  baskets <- lapply(ref_sequences(ref_wide(human_ai)), unique)
  transactions <- methods::as(baskets, "transactions")
  rules <- arules::apriori(transactions, parameter = list(supp = 0.2, conf = 0.5, minlen = 2L, maxlen = 3L,
                                                         target = "rules"), control = list(verbose = FALSE))
  quality <- arules::quality(rules)
  expected <- data.frame(antecedent = ref_items(methods::as(arules::lhs(rules), "list")),
                         consequent = ref_items(methods::as(arules::rhs(rules), "list")),
                         support = quality$support, confidence = quality$confidence, lift = quality$lift,
                         conviction = arules::interestMeasure(rules, "conviction", transactions = transactions))
  found <- association_rules_htna(build_htna(human_ai, actor_type = "actor_type"), min_support = 0.2,
                                  min_confidence = 0.5, min_lift = 0, max_length = 3L)
  one_consequent <- ref_rule_key(subset(summary(found), !grepl(",", consequent), select = -c(count, n_transactions)))
  expected <- ref_rule_key(expected)
  # Conviction is (1 - support of the consequent) / (1 - confidence): 0 / 0 for a consequent that is in every
  # session (confidence 1 then). It is not defined there (arules NaN, Nestimate Inf); every other number is compared.
  in_every_session <- names(which(vapply(arules::itemLabels(transactions), \(item)
    all(vapply(baskets, \(basket) item %in% basket, logical(1))), logical(1))))
  undefined <- sub("^.* => ", "", expected$rule) %in% in_every_session
  expect_reference(one_consequent, subset(expected, select = -conviction), reference = "arules",
                   covers = "association_rules_htna()", quantity = "human_ai sessions: rules with one consequent",
                   tolerance = 1e-10, key = "rule")
  expect_reference(subset(one_consequent, rule %in% expected$rule[!undefined], select = c(rule, conviction)),
                   subset(expected, !undefined, select = c(rule, conviction)), reference = "arules",
                   covers = "association_rules_htna()", quantity = "human_ai sessions: conviction", tolerance = 1e-10,
                   key = "rule")
  expect_not_comparable(subset(one_consequent, rule %in% expected$rule[undefined], select = c(rule, conviction)),
                        subset(expected, undefined, select = c(rule, conviction)), reference = "arules",
                        covers = "association_rules_htna()",
                        quantity = "human_ai sessions: conviction of a consequent in every session", key = "rule",
                        reason = "The consequent is in every session, so confidence is 1 and conviction is 0 / 0: not defined (arules NaN, htna through Nestimate Inf).")
})
