test_that("internal finharvest fetch uses runtime codigos attrs", {
  fetch_body <- paste(deparse(.bt_fetch_finharvest_data), collapse = "\n")

  expect_match(fetch_body, 'attrs_source = "codigos"', fixed = TRUE)
  expect_false(grepl('attrs_source = "fintickers"', fetch_body, fixed = TRUE))
})
