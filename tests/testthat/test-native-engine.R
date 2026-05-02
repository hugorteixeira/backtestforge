bt_test_ohlc <- function(n = 260) {
  set.seed(42)
  idx <- as.Date("2020-01-01") + seq_len(n) - 1L
  close <- cumsum(rnorm(n, mean = 0.08, sd = 1.1)) + 100
  open <- close + rnorm(n, sd = 0.2)
  high <- pmax(open, close) + runif(n, min = 0.05, max = 1.2)
  low <- pmin(open, close) - runif(n, min = 0.05, max = 1.2)
  out <- xts::xts(
    cbind(Open = open, High = high, Low = low, Close = close),
    order.by = idx
  )
  attr(out, "symbol") <- "BTTEST"
  attr(out, "fut_tick_size") <- 1
  attr(out, "fut_multiplier") <- 1
  attr(out, "identifiers") <- list(fees = 1, slippage = 1)
  out
}

test_that("native Donchian engine returns finite shaped results", {
  x <- bt_test_ohlc()

  res <- bt_eldoc(
    x,
    up = 20,
    down = 10,
    fee = "nofee",
    engine = "native",
    verbose = FALSE
  )

  expect_s3_class(res, "bt_native_result")
  expect_s3_class(res$rets, "xts")
  expect_true(all(c("Discrete", "Log") %in% colnames(res$rets)))
  expect_true(all(is.finite(as.numeric(res$rets))))
  expect_true(all(c("trades", "positions", "mktdata", "stats") %in% names(res)))
  expect_true(NROW(res$positions) == NROW(x))
})

test_that("native wrappers do not create quantstrat global state", {
  x <- bt_test_ohlc()
  had_blotter <- exists(".blotter", envir = .GlobalEnv, inherits = FALSE)
  had_strategy <- exists(".strategy", envir = .GlobalEnv, inherits = FALSE)

  invisible(bt_eldoc(x, up = 15, down = 8, fee = "nofee", engine = "native"))

  expect_identical(exists(".blotter", envir = .GlobalEnv, inherits = FALSE), had_blotter)
  expect_identical(exists(".strategy", envir = .GlobalEnv, inherits = FALSE), had_strategy)
})

test_that("native metadata falls back to one with a warning", {
  x <- bt_test_ohlc()
  attr(x, "fut_tick_size") <- NULL
  attr(x, "fut_multiplier") <- NULL
  attr(x, "identifiers") <- NULL

  expect_warning(
    res <- bt_eldoc(x, up = 20, down = 10, fee = "nofee", engine = "native"),
    "using 1 as fallback"
  )
  expect_equal(res$stats$Multiplier, 1)
  expect_equal(res$stats$TickSize, 1)
})

test_that("native moving-average wrappers run on synthetic data", {
  x <- bt_test_ohlc()

  ema <- bt_ema(x, fast = 10, slow = 30, fee = "nofee", engine = "native")
  sma <- bt_sma(x, fast = 10, slow = 30, fee = "nofee", engine = "native")

  expect_s3_class(ema, "bt_native_result")
  expect_s3_class(sma, "bt_native_result")
  expect_true(all(is.finite(as.numeric(ema$rets))))
  expect_true(all(is.finite(as.numeric(sma$rets))))
})

test_that("native search evaluates and sorts parameter grids", {
  x <- bt_test_ohlc()

  search <- bt_search_native(
    x,
    list(type = "donchian", up = c(10, 20), down = c(5, 10)),
    execution = bt_execution_spec(fee = "nofee"),
    objective = "total_return"
  )

  expect_s3_class(search, "data.frame")
  expect_equal(NROW(search), 4)
  expect_true(all(c("up", "down", "total_return", "num_trades") %in% names(search)))
  expect_true(all(diff(search$total_return) <= 0))
  expect_length(attr(search, "results"), 4)
})

test_that("bt_batch uses the native engine with exact-match preloaded data", {
  x <- bt_test_ohlc()
  assign("BT_NATIVE_BATCH", x, envir = .GlobalEnv)
  on.exit(rm("BT_NATIVE_BATCH", envir = .GlobalEnv), add = TRUE)

  out <- bt_batch(
    type = "eldoc",
    tickers = "BT_NATIVE_BATCH",
    exact_match = TRUE,
    mup = c(12, 20),
    mdown = c(8, 10),
    fee = "nofee",
    engine = "native",
    only_returns = TRUE,
    returns_type = "Log",
    verbose = FALSE
  )

  expect_s3_class(out, "xts")
  expect_equal(NCOL(out), 2)
  expect_true(all(is.finite(as.numeric(out))))
})
