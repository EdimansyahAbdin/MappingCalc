# tests/testthat/test-utils.R

test_that("panss_utility_score computes correctly", {
  expected <- round(1.3103 - 0.0044*34 + 0.0025*17 - 0.0146*80 - 0.0029*40 + 0.0149*1, 3)
  expect_equal(panss_utility_score(34, 17, 80, 40, 1), expected)
})

test_that("panss_utility_score caps at 1.000", {
  expect_lte(panss_utility_score(7, 7, 16, 21, 1), 1.000)
})

test_that("panss_utility_comparison returns above or below", {
  expect_true(panss_utility_comparison(34, 17, 80, 40, 1) %in% c("above", "below"))
})

test_that("sqls_utility_score returns numeric", {
  expect_type(sqls_utility_score(50, 50, 50, 40, 1), "double")
})

test_that("sqls_utility_comparison returns above or below", {
  expect_true(sqls_utility_comparison(50, 50, 50, 40, 1) %in% c("above", "below"))
})

test_that("whodas_total_utility_score floors at -0.584", {
  expect_gte(whodas_total_utility_score(48), -0.584)
})

test_that("whodas_total_utility_score correct at zero", {
  expect_equal(whodas_total_utility_score(0), round(0.9564739, 3))
})

test_that("whodas_total_utility_comparison returns above or below", {
  expect_true(whodas_total_utility_comparison(10) %in% c("above", "below"))
})

test_that("eq5d5l_profile_string concatenates correctly", {
  expect_equal(eq5d5l_profile_string(1, 1, 1, 1, 1), "11111")
  expect_equal(eq5d5l_profile_string(3, 2, 1, 4, 5), "32145")
})

test_that("panss_utility_score vectorises", {
  result <- panss_utility_score(c(20, 34), c(15, 17), c(40, 80), c(30, 40), c(0, 1))
  expect_length(result, 2)
})

test_that("whodas_total_utility_score vectorises", {
  expect_length(whodas_total_utility_score(c(0, 10, 48)), 3)
})
