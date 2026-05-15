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
  attr(out, "ps_value") <- 2
  attr(out, "ps_type") <- "eldoc"
  attr(out, "fee_value") <- 1
  attr(out, "fee_type") <- "contract"
  attr(out, "slip_value") <- 1
  attr(out, "slip_type") <- "cash"
  out
}

bt_test_eldoc_breakout_ohlc <- function() {
  idx <- as.Date("2024-01-01") + 0:9
  open <- c(10, 10, 10, 9.8, 11, 12, 13, 14, 15, 16)
  high <- c(10, 10, 10, 9.9, 11.6, 12.6, 13.6, 14.6, 15.6, 16.6)
  low <- c(9, 9, 9, 9, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5)
  close <- c(9.5, 9.5, 9.5, 9.8, 11, 12, 13, 14, 15, 16)
  out <- xts::xts(
    cbind(Open = open, High = high, Low = low, Close = close),
    order.by = idx
  )
  attr(out, "symbol") <- "BT_HAND"
  attr(out, "fut_tick_size") <- 1
  attr(out, "fut_multiplier") <- 1
  out
}

bt_test_di_ohlc <- function() {
  idx <- as.Date("2024-01-02") + 0:9
  maturity <- as.Date("2028-01-03")
  cal <- .generate_calendar("Brazil/ANBIMA")
  open <- c(10, 10, 10, 9.8, 11, 12, 13, 14, 15, 16)
  high <- c(10, 10, 10, 9.9, 11.6, 12.6, 13.6, 14.6, 15.6, 16.6)
  low <- c(9, 9, 9, 9, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5)
  close <- c(9.5, 9.5, 9.5, 9.8, 11, 12, 13, 14, 15, 16)
  pu_for <- function(rate, i) {
    .calculate_futures_di_notional(rate, maturity_date = maturity, basis_date = idx[i], cal = cal)$pu
  }
  tick_value_for <- function(rate, i) {
    .calculate_futures_di_notional(rate, maturity_date = maturity, basis_date = idx[i], cal = cal)$tick_value
  }
  pu_open <- mapply(pu_for, open, seq_along(open))
  pu_high <- mapply(pu_for, high, seq_along(high))
  pu_low <- mapply(pu_for, low, seq_along(low))
  pu_close <- mapply(pu_for, close, seq_along(close))
  tick_value <- -abs(mapply(tick_value_for, close, seq_along(close)))
  out <- xts::xts(
    cbind(
      Open = open,
      High = high,
      Low = low,
      Close = close,
      PU_Open = pu_open,
      PU_High = pu_high,
      PU_Low = pu_low,
      PU_Close = pu_close,
      TickSize = rep(0.01, length(close)),
      TickValue = tick_value
    ),
    order.by = idx
  )
  attr(out, "symbol") <- "DI1F28"
  attr(out, "maturity") <- maturity
  attr(out, "ticksize") <- 0.01
  attr(out, "tickvalue") <- tick_value[1]
  attr(out, "fee_value") <- 1
  attr(out, "fee_type") <- "contract"
  attr(out, "slip_value") <- 7
  attr(out, "slip_type") <- "bps"
  out
}

test_that("native Donchian engine returns finite shaped results", {
  x <- bt_test_ohlc()

  res <- bt_eldoc(
    x,
    up = 20,
    down = 10,
    fee = "nofee",
    verbose = FALSE,
    hide_details = TRUE
  )

  expect_s3_class(res, "bt_native_result")
  expect_s3_class(res$rets, "xts")
  expect_true(all(c("Discrete", "Log") %in% colnames(res$rets)))
  expect_true(all(is.finite(as.numeric(res$rets))))
  expect_true(all(c("trades", "positions", "mktdata", "stats") %in% names(res)))
  expect_true(all(c("trade_episodes", "trade_excursions", "pyramid_events", "info_blocks") %in% names(res)))
  expect_true(NROW(res$positions) == NROW(x))
  expect_s3_class(res$trade_episodes, "data.frame")
  expect_s3_class(res$trade_excursions, "data.frame")
  expect_true(all(c("entry_atr", "entry_atr_value_per_unit", "initial_stop_atr", "initial_risk_per_unit") %in% names(res$trade_episodes)))
  expect_true(all(c("mfe_pct", "mae_pct", "mfe_atr", "mae_atr", "final_R", "hit_5_0_atr", "hit_10_0_atr", "hit_20_0_atr", "hit_40_0_atr", "post_5_0_atr_R", "post_20_0_atr_R", "post_40_0_atr_R") %in% names(res$trade_excursions)))
  expect_true(all(is.finite(res$trade_excursions$mfe_pct)))
  expect_true(all(res$trade_excursions$mfe_pct >= 0))
  expect_true(any(vapply(res$info_blocks, function(block) identical(attr(block, "id"), "atr_stats"), logical(1))))
  expect_equal(attr(res$info_blocks$atr_stats, "contexts"), "research")
  expect_true("Mean Initial Stop ATR" %in% res$info_blocks$atr_stats$stat_name)
  expect_true(any(grepl("Mean Initial Stop Value /", res$info_blocks$atr_stats$stat_name, fixed = TRUE)))
  expect_true(any(grepl("Mean 20 ATR Value /", res$info_blocks$atr_stats$stat_name, fixed = TRUE)))
  expect_true(any(vapply(res$info_blocks, function(block) identical(attr(block, "id"), "excursions"), logical(1))))
  expect_true(any(vapply(res$info_blocks, function(block) identical(attr(block, "id"), "excursion_thresholds"), logical(1))))
  expect_true("P90 MFE ATR" %in% res$info_blocks$excursions$stat_name)
  expect_false(any(grepl("^Hit ", res$info_blocks$excursions$stat_name)))
  expect_false(any(grepl("^Post ", res$info_blocks$excursions$stat_name)))
  expect_true(all(c(
    "ATRT", "Hits", "Hit %", "Med FR", "Mean FR",
    "Med PR", "Mean PR", "PR Win %"
  ) %in% names(res$info_blocks$excursion_thresholds)))
  expect_true(any(res$info_blocks$excursion_thresholds$ATRT == "20"))
  expect_true(any(res$info_blocks$excursion_thresholds$ATRT == "40"))
  expect_true(res$spec$risk$reinvest)
  expect_equal(res$spec$risk$initial_equity, 100000)
  expect_equal(as.numeric(res$equity$Equity[1]), 100000)
  expect_equal(res$stats$InitialEquity, 100000)
})

test_that("native wrappers accept starting equity", {
  x <- bt_test_ohlc(160)

  eldoc <- bt_eldoc(x, up = 12, down = 8, initial_equity = 250000, fee = "nofee", hide_details = TRUE)
  ema <- bt_ema(x, fast = 8, slow = 20, initial_equity = 300000, fee = "nofee", hide_details = TRUE)
  sma <- bt_sma(x, fast = 8, slow = 20, initial_equity = 350000, fee = "nofee", hide_details = TRUE)

  expect_equal(eldoc$spec$risk$initial_equity, 250000)
  expect_equal(as.numeric(eldoc$equity$Equity[1]), 250000)
  expect_equal(eldoc$stats$InitialEquity, 250000)
  expect_equal(ema$spec$risk$initial_equity, 300000)
  expect_equal(as.numeric(ema$equity$Equity[1]), 300000)
  expect_equal(sma$spec$risk$initial_equity, 350000)
  expect_equal(as.numeric(sma$equity$Equity[1]), 350000)
})

test_that("native reinvest sizing uses current equity at entry", {
  metadata <- list(multiplier = 1, tick_size = 1)
  risk_reinvest <- bt_risk_spec(
    mode = "risk",
    initial_equity = 1000,
    risk_pct = 2,
    reinvest = TRUE,
    integer_qty = FALSE
  )
  risk_fixed_basis <- bt_risk_spec(
    mode = "risk",
    initial_equity = 1000,
    risk_pct = 2,
    reinvest = FALSE,
    integer_qty = FALSE
  )

  expect_equal(
    .bt_native_size("long", price = 100, stop_price = 90, equity = 2000, risk_reinvest, metadata),
    4
  )
  expect_equal(
    .bt_native_size("long", price = 100, stop_price = 90, equity = 2000, risk_fixed_basis, metadata),
    2
  )
  expect_error(
    .bt_native_size("long", price = 100, stop_price = 90, equity = NA_real_, risk_reinvest, metadata),
    "valid equity"
  )
})

test_that("native reinvest does not resize open positions between orders", {
  idx <- as.Date("2020-01-01") + 0:5
  close <- c(100, 100, 120, 140, 110, 130)
  data <- xts::xts(
    cbind(Open = close, High = close, Low = close, Close = close),
    order.by = idx
  )
  prices <- list(open = close, high = close, low = close, close = close, index = idx)
  indicators <- xts::xts(
    cbind(DonchianUpper = rep(110, length(idx)), DonchianLower = rep(90, length(idx))),
    order.by = idx
  )
  signals <- xts::xts(
    cbind(
      LongEntry = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
      LongExit = FALSE,
      ShortEntry = FALSE,
      ShortExit = FALSE
    ),
    order.by = idx
  )

  sim <- .bt_native_simulate(
    data = data,
    prices = prices,
    indicators = indicators,
    signals = signals,
    strategy = list(type = "donchian", long = TRUE, short = FALSE, invert_signals = FALSE),
    risk = bt_risk_spec(
      mode = "risk",
      initial_equity = 1000,
      risk_pct = 10,
      reinvest = TRUE,
      integer_qty = FALSE
    ),
    execution = bt_execution_spec(execution = "close", fee = "nofee", close_on_end = FALSE),
    metadata = list(multiplier = 1, tick_size = 1, tick_value = 1, fees = 0, slippage = 0),
    symbol = "BTTEST"
  )

  expect_equal(as.numeric(sim$positions$qty[2:6]), rep(10, 5))
  expect_gt(length(unique(as.numeric(sim$positions$equity[2:6]))), 1)
})

test_that("native metadata falls back with a warning", {
  x <- bt_test_ohlc()
  attr(x, "fut_tick_size") <- NULL
  attr(x, "fut_multiplier") <- NULL

  expect_warning(
    res <- bt_eldoc(x, up = 20, down = 10, fee = "nofee", hide_details = TRUE),
    "using fallback defaults"
  )
  expect_equal(res$stats$Multiplier, 1)
  expect_equal(res$stats$TickSize, 1)
})

test_that("native wrappers require missing metadata when defaults are NULL", {
  x <- bt_test_ohlc()
  attr(x, "ps_value") <- NULL
  expect_error(
    bt_eldoc(x, up = 20, down = 10, fee = "nofee", hide_details = TRUE),
    "position sizing.*ps_value"
  )

  x <- bt_test_ohlc()
  attr(x, "fee_type") <- NULL
  expect_error(
    bt_eldoc(x, up = 20, down = 10, slip_value = 0, hide_details = TRUE),
    "fee.*fee_type"
  )

  x <- bt_test_ohlc()
  attr(x, "slip_type") <- NULL
  expect_error(
    bt_eldoc(x, up = 20, down = 10, fee_value = 0, hide_details = TRUE),
    "slippage.*slip_type"
  )
})

test_that("native return printer uses internal return math", {
  x <- bt_test_ohlc(160)
  res <- bt_eldoc(x, up = 12, down = 8, fee = "nofee", hide_details = TRUE)

  printed <- NULL
  expect_output(
    printed <- .print_returns(res$rets),
    "Returns Summary"
  )
  expect_s3_class(printed, "xts")
  expect_true(all(c("Discrete", "Log") %in% colnames(printed)))
})

test_that("normalize_risk drives performance blocks while execution stats stay raw", {
  x <- bt_test_ohlc(160)
  raw <- bt_eldoc(x, up = 12, down = 8, fee = "nofee", hide_details = TRUE)
  norm <- bt_eldoc(x, up = 12, down = 8, fee = "nofee", normalize_risk = 10, hide_details = TRUE)

  expect_true(isTRUE(norm$stats$RiskNormalized[1]))
  expect_false(isTRUE(norm$raw_stats$RiskNormalized[1]))
  expect_equal(norm$raw_stats$net_profit, raw$raw_stats$net_profit)
  expect_false(isTRUE(all.equal(norm$stats$total_return, norm$raw_stats$total_return)))
  expect_equal(norm$stats$annualized_vol * 100, 10, tolerance = 1e-8)
  expect_equal(norm$stats$RiskOriginal, norm$raw_stats$annualized_vol * 100, tolerance = 1e-8)
  expect_equal(attr(norm$info_blocks$monthly_returns, "title"), "Monthly Returns Risk-Normalized 10% (Geometric)")
  expect_equal(attr(norm$info_blocks$returns, "title"), "Returns Summary Risk-Normalized 10% (Geometric)")
  expect_true(any(norm$info_blocks$performance$stat_name == "Return Source"))
  expect_true(any(norm$info_blocks$risk_normalization$stat_name == "Execution/Costs"))

  cost_net <- function(res) {
    res$info_blocks$costs$value[match("Net P/L", res$info_blocks$costs$stat_name)]
  }
  expect_equal(cost_net(norm), cost_net(raw))

  output <- capture.output(
    bt_eldoc(x, up = 12, down = 8, fee = "nofee", normalize_risk = 10, hide_details = FALSE)
  )
  expect_true(any(grepl("Risk Normalization", output, fixed = TRUE)))
  expect_true(any(grepl("Risk-Normalized 10%", output, fixed = TRUE)))
})

test_that("normalize_risk targets daily-compounded volatility for intraday returns", {
  idx <- as.POSIXct("2024-01-02 09:00:00", tz = "UTC") +
    rep(0:119, each = 4) * 86400 +
    rep(c(0, 3600, 7200, 10800), times = 120)
  intraday_log <- xts::xts(
    cbind(Log = rep(c(0.01, -0.004, 0.006, -0.002, -0.008, 0.003), length.out = length(idx))),
    order.by = idx
  )

  scaled <- bt_normalize_risk(intraday_log, risk = 10, type = "Log")
  daily_discrete <- xts::apply.daily(
    scaled,
    function(x) exp(sum(as.numeric(x), na.rm = TRUE)) - 1
  )

  expect_equal(stats::sd(as.numeric(daily_discrete), na.rm = TRUE) * sqrt(252) * 100, 10, tolerance = 1e-8)
})

test_that("native metadata uses finharvest xts contract attrs", {
  x <- bt_test_ohlc()
  attr(x, "symbol") <- NULL
  attr(x, "ticker") <- "CCMFUT_1H_AGG"
  attr(x, "fut_tick_size") <- NULL
  attr(x, "fut_multiplier") <- NULL
  attr(x, "ticksize") <- 0.01
  attr(x, "tickvalue") <- 4.5
  attr(x, "fee_value") <- 2.5
  attr(x, "fee_type") <- "contract"
  attr(x, "slip_value") <- 7
  attr(x, "slip_type") <- "bps"

  expect_no_warning(meta <- .bt_native_metadata(x, "CCMFUT_1H_AGG"))
  expect_equal(meta$tick_size, 0.01)
  expect_equal(meta$tick_value, 4.5)
  expect_equal(meta$multiplier, 450)
  expect_equal(meta$fees, 2.5)
  expect_equal(meta$slippage_bps, 7)

  execution <- bt_execution_spec(fee = "normal")
  costs <- .bt_native_txn_cost_components(2, 100, execution, meta)
  expect_equal(costs[["fees"]], 5)
  expect_equal(costs[["slippage"]], 63)
  expect_equal(costs[["total_cost"]], 68)
  expect_equal(.bt_native_txn_cost(2, 100, execution, meta), 68)

  contract_execution <- bt_execution_spec(fee = "normal", fee_value = 4, fee_type = "contract", slip_value = 0)
  contract_costs <- .bt_native_txn_cost_components(3, 100, contract_execution, meta)
  expect_equal(contract_costs[["fees"]], 12)
  expect_equal(contract_costs[["slippage"]], 0)
  expect_equal(contract_costs[["total_cost"]], 12)

  order_execution <- bt_execution_spec(fee = "normal", fee_value = 4, fee_type = "order", slip_value = 0)
  order_costs <- .bt_native_txn_cost_components(3, 100, order_execution, meta)
  expect_equal(order_costs[["fees"]], 4)
  expect_equal(order_costs[["slippage"]], 0)
  expect_equal(order_costs[["total_cost"]], 4)

  tick_slip_execution <- bt_execution_spec(fee = "normal", fee_value = 0, slip_value = 2, slip_type = "ticks")
  tick_slip_costs <- .bt_native_txn_cost_components(3, 100, tick_slip_execution, meta)
  expect_equal(tick_slip_costs[["fees"]], 0)
  expect_equal(tick_slip_costs[["slippage"]], 27)
  expect_equal(tick_slip_costs[["total_cost"]], 27)

  attr(x, "slip_type") <- "ticks"
  expect_no_warning(tick_meta <- .bt_native_metadata(x, "CCMFUT_1H_AGG"))
  expect_equal(tick_meta$slippage_ticks, 7)
  expect_equal(.bt_native_txn_cost(2, 100, execution, tick_meta), 68)

  attr(x, "slip_type") <- NULL
  attr(x, "slip_value") <- NULL
  attr(x, "slippage_points") <- 0.02
  expect_no_warning(point_meta <- .bt_native_metadata(x, "CCMFUT_1H_AGG"))
  expect_equal(point_meta$slippage_points, 0.02)
  expect_equal(.bt_native_txn_cost(2, 100, execution, point_meta), 23)

  expect_no_warning(
    cost_res <- bt_run_native(
      x,
      strategy = bt_strategy_spec("donchian", up = 20, down = 10),
      execution = execution,
      report = FALSE
    )
  )
  expect_true(all(c("fees", "slippage", "total_cost") %in% names(cost_res$trades)))
  expect_equal(cost_res$trades$total_cost, cost_res$trades$fees + cost_res$trades$slippage)
  expect_equal(cost_res$stats$fees, sum(cost_res$trades$fees, na.rm = TRUE))
  expect_equal(cost_res$stats$slippage, sum(cost_res$trades$slippage, na.rm = TRUE))
  expect_equal(cost_res$stats$total_cost, sum(cost_res$trades$total_cost, na.rm = TRUE))

  expect_no_warning(
    order_fee_res <- bt_eldoc(
      x,
      up = 20,
      down = 10,
      fee_value = 4,
      fee_type = "order",
      slip_value = 0,
      hide_details = TRUE
    )
  )
  expect_true(NROW(order_fee_res$trades) > 0)
  expect_true(all(order_fee_res$trades$fees == 4))
  expect_true(all(order_fee_res$trades$slippage == 0))
  expect_equal(order_fee_res$stats$fees, 4 * NROW(order_fee_res$trades))
  expect_equal(order_fee_res$stats$FeeType, "order")

  nofee_res <- bt_eldoc(
    x,
    up = 20,
    down = 10,
    ps_value = 1,
    ps_type = "contract",
    execution = "same_close",
    fee = "nofee",
    long = TRUE,
    short = TRUE,
    hide_details = TRUE
  )
  ledger_res <- bt_eldoc(
    x,
    up = 20,
    down = 10,
    ps_value = 1,
    ps_type = "contract",
    execution = "same_close",
    fee_value = 2,
    fee_type = "contract",
    slip_value = 3,
    slip_type = "cash",
    long = TRUE,
    short = TRUE,
    hide_details = TRUE
  )
  expect_equal(NROW(ledger_res$trades), NROW(nofee_res$trades))
  expect_equal(ledger_res$trades$qty_delta, nofee_res$trades$qty_delta)
  expected_cost_drag <- sum(ledger_res$trades$total_cost, na.rm = TRUE)
  actual_cost_drag <- as.numeric(tail(nofee_res$equity$Equity, 1) - tail(ledger_res$equity$Equity, 1))
  expect_equal(actual_cost_drag, expected_cost_drag, tolerance = 1e-8)

  cost_table <- .bt_native_cost_summary_table(
    data.frame(num_trades = 4, net_profit = 80, fees = 10, slippage = 10, total_cost = 20),
    data.frame(x = seq_len(8))
  )
  expect_false("Orders" %in% rownames(cost_table))
  expect_false("Total Fees" %in% rownames(cost_table))
  expect_false("Total Slippage" %in% rownames(cost_table))
  expect_false("Total Cost" %in% rownames(cost_table))
  expect_false(any(grepl("^Avg ", rownames(cost_table))))
  expect_equal(cost_table["Trades", "Value"], "4")
  expect_equal(cost_table["Fees", "Value"], "10,00")
  expect_equal(cost_table["Slippage", "Value"], "10,00")
  expect_equal(cost_table["Gross P/L", "Value"], "100,00")
  expect_equal(cost_table["Fees Impact", "Value"], "10.00%")
  expect_equal(cost_table["Slippage Impact", "Value"], "10.00%")
  expect_equal(cost_table["Total Cost Impact", "Value"], "20.00%")
  expect_equal(cost_table["Impact Basis", "Value"], "profit consumed")
  expect_false("Contracts" %in% rownames(cost_table))

  futures_cost_table <- .bt_native_cost_summary_table(
    data.frame(num_trades = 4, contracts_traded = 9, IsFutures = TRUE, net_profit = 80, fees = 10, slippage = 10, total_cost = 20),
    data.frame(x = seq_len(8))
  )
  expect_equal(rownames(futures_cost_table)[1:2], c("Trades", "Contracts"))
  expect_equal(futures_cost_table["Contracts", "Value"], "9")

  loss_table <- .bt_native_cost_summary_table(
    data.frame(num_trades = 4, net_profit = -120, fees = 10, slippage = 10, total_cost = 20),
    data.frame(x = seq_len(8))
  )
  expect_equal(loss_table["Gross P/L", "Value"], "-100,00")
  expect_equal(loss_table["Total Cost Impact", "Value"], "20.00%")
  expect_equal(loss_table["Impact Basis", "Value"], "loss worsened")

  expect_no_warning(
    res <- bt_eldoc(x, up = 20, down = 10, fee = "nofee", hide_details = TRUE)
  )
  expect_equal(res$stats$Multiplier, 450)
  expect_equal(res$stats$TickSize, 0.01)
})

test_that("native report prints costs before returns summary", {
  x <- bt_test_ohlc(120)

  output <- capture.output(
    basic_res <- bt_eldoc(
      x,
      up = 12,
      down = 8,
      fee_value = 4,
      slip_value = 0,
      hide_details = FALSE
    )
  )
  expect_s3_class(basic_res, "bt_native_result")

  cost_idx <- grep("Costs & Slippage Summary", output)
  returns_idx <- grep("Returns Summary", output)
  expect_length(cost_idx, 1)
  expect_length(returns_idx, 1)
  expect_lt(cost_idx, returns_idx)
  expect_true(any(grepl("Trade Activity", output, fixed = TRUE)))
  expect_true(any(grepl("Orders", output, fixed = TRUE)))
  expect_false(any(grepl("Total Fees", output, fixed = TRUE)))
  expect_false(any(grepl("Total Slippage", output, fixed = TRUE)))
  expect_false(any(grepl("Gross P/L Before Costs", output, fixed = TRUE)))
  expect_false(any(grepl("Avg Fee / Trade", output, fixed = TRUE)))
  expect_true(any(grepl("Total Cost Impact", output, fixed = TRUE)))
  expect_true(any(grepl("Strategy", output, fixed = TRUE)))
  expect_true(any(grepl("Position Sizing", output, fixed = TRUE)))
  expect_true(any(grepl("Execution & Cost Settings", output, fixed = TRUE)))
  expect_true(any(grepl("Slippage", output, fixed = TRUE)))
  expect_true(any(grepl("Contracts", output, fixed = TRUE)))
  expect_false(any(grepl("ATR Statistics", output, fixed = TRUE)))
  expect_false(any(grepl("Mean Initial Stop ATR", output, fixed = TRUE)))
  expect_false(any(grepl("Excursion Thresholds", output, fixed = TRUE)))
  expect_false(any(grepl("ATRT", output, fixed = TRUE)))
  expect_false(any(grepl("Quarterly Returns", output, fixed = TRUE)))
  expect_false(any(grepl("Quarterly Net Profit", output, fixed = TRUE)))

  exp_output <- capture.output(
    exp_res <- bt_eldoc_exp(
      x,
      up = 12,
      down = 8,
      fee_value = 4,
      slip_value = 0,
      hide_details = FALSE
    )
  )
  expect_s3_class(exp_res, "bt_native_result")

  expect_true(any(grepl("ATR Statistics", exp_output, fixed = TRUE)))
  expect_true(any(grepl("Mean Initial Stop ATR", exp_output, fixed = TRUE)))
  expect_true(any(grepl("Excursion Thresholds", exp_output, fixed = TRUE)))
  expect_true(any(grepl("ATRT", exp_output, fixed = TRUE)))
})

test_that("native report can omit quarterly tables", {
  x <- bt_test_ohlc(120)

  output <- capture.output(
    bt_eldoc(
      x,
      up = 12,
      down = 8,
      fee = "nofee",
      hide_details = FALSE,
      show_quarterly = FALSE
    )
  )

  expect_false(any(grepl("Quarterly Returns", output, fixed = TRUE)))
  expect_false(any(grepl("Quarterly Net Profit", output, fixed = TRUE)))
  expect_true(any(grepl("Monthly Returns", output, fixed = TRUE)))
  expect_true(any(grepl("Strategy", output, fixed = TRUE)))
})

test_that("native report can print quarterly tables when requested", {
  x <- bt_test_ohlc(120)

  output <- capture.output(
    bt_eldoc(
      x,
      up = 12,
      down = 8,
      fee = "nofee",
      hide_details = FALSE,
      show_quarterly = TRUE
    )
  )

  expect_true(any(grepl("Quarterly Returns", output, fixed = TRUE)))
  expect_true(any(grepl("Quarterly Net Profit", output, fixed = TRUE)))
})

test_that("bt_stats accepts a single native result", {
  x <- bt_test_ohlc(160)
  res <- bt_eldoc(
    x,
    up = 12,
    down = 8,
    fee = "nofee",
    hide_details = TRUE
  )

  out <- capture.output(stats <- bt_stats(res, verbose = FALSE))
  expect_s3_class(stats, "data.frame")
  expect_true(length(out) > 0)
  expect_true("BTTEST" %in% names(stats))
  expect_true("Profit" %in% stats$stat_name)
  expect_false(any(grepl("Excursion Thresholds", out, fixed = TRUE)))
  expect_false("excursion_thresholds" %in% stats$block)

  research_out <- capture.output(research_stats <- bt_stats(res, blocks = "excursion_thresholds", verbose = FALSE))
  expect_true(any(grepl("Excursion Thresholds", research_out, fixed = TRUE)))
  expect_true("excursion_thresholds" %in% research_stats$block)
})

test_that("native DI uses rate OHLC for signals and PU for execution", {
  idx <- as.Date("2024-01-02") + 0:2
  rates <- c(10, 11, 12)
  pu <- c(90000, 89000, 88000)
  x <- xts::xts(
    cbind(
      Open = rates,
      High = rates + 0.1,
      Low = rates - 0.1,
      Close = rates,
      PU_Open = pu,
      PU_High = pu + 25,
      PU_Low = pu - 25,
      PU_Close = pu,
      TickSize = rep(0.01, length(rates)),
      TickValue = rep(-25, length(rates))
    ),
    order.by = idx
  )
  attr(x, "symbol") <- "DI1F28"
  attr(x, "maturity") <- as.Date("2028-01-03")
  attr(x, "ticksize") <- 0.01
  attr(x, "tickvalue") <- -25
  attr(x, "fee_value") <- 1
  attr(x, "fee_type") <- "contract"
  attr(x, "slip_value") <- 7
  attr(x, "slip_type") <- "bps"

  prices <- .bt_native_price_set(x, "DI1F28")
  expect_true(prices$uses_pu)
  expect_equal(prices$close, rates)
  expect_equal(prices$exec_close, pu)

  expect_no_warning(meta <- .bt_native_metadata(x, "DI1F28"))
  expect_equal(meta$multiplier, 1)
  expect_equal(meta$tick_value, 25)
  expect_equal(meta$slippage_bps, 7)
  di_costs <- .bt_native_txn_cost_components(2, pu[2], bt_execution_spec(fee = "normal"), meta, tick_value = 25)
  expect_equal(di_costs[["fees"]], 2)
  expect_equal(di_costs[["slippage"]], 350)
  expect_equal(di_costs[["total_cost"]], 352)
  expect_equal(.bt_native_txn_cost(2, pu[2], bt_execution_spec(fee = "normal"), meta, tick_value = 25), 352)

  order_tick_costs <- .bt_native_txn_cost_components(
    3,
    pu[2],
    bt_execution_spec(fee = "normal", fee_value = 4, fee_type = "order", slip_value = 2, slip_type = "ticks"),
    meta,
    tick_value = 25
  )
  expect_equal(order_tick_costs[["fees"]], 4)
  expect_equal(order_tick_costs[["slippage"]], 150)
  expect_equal(order_tick_costs[["total_cost"]], 154)

  point_costs <- .bt_native_txn_cost_components(
    3,
    pu[2],
    bt_execution_spec(fee = "normal", fee_value = 4, fee_type = "contract", slip_value = 0.02, slip_type = "points"),
    meta,
    tick_value = 25
  )
  expect_equal(point_costs[["fees"]], 12)
  expect_equal(point_costs[["slippage"]], 150)
  expect_equal(point_costs[["total_cost"]], 162)

  cash_costs <- .bt_native_txn_cost_components(
    3,
    pu[2],
    bt_execution_spec(fee = "normal", fee_value = 4, fee_type = "contract", slip_value = 5, slip_type = "cash"),
    meta,
    tick_value = 25
  )
  expect_equal(cash_costs[["fees"]], 12)
  expect_equal(cash_costs[["slippage"]], 15)
  expect_equal(cash_costs[["total_cost"]], 27)

  indicators <- xts::xts(
    cbind(
      DonchianUpper = c(NA, 10.5, 10.5),
      DonchianLower = c(NA, 9.5, 9.5)
    ),
    order.by = idx
  )
  signals <- xts::xts(
    cbind(
      LongEntry = c(FALSE, TRUE, FALSE),
      LongExit = FALSE,
      ShortEntry = FALSE,
      ShortExit = FALSE
    ),
    order.by = idx
  )

  sim <- .bt_native_simulate(
    data = x,
    prices = prices,
    indicators = indicators,
    signals = signals,
    strategy = list(type = "donchian", long = TRUE, short = FALSE, invert_signals = FALSE),
    risk = bt_risk_spec(mode = "fixed", initial_equity = 10000, fixed_qty = 1),
    execution = bt_execution_spec(execution = "close", fee = "nofee", close_on_end = FALSE),
    metadata = meta,
    symbol = "DI1F28"
  )

  expect_equal(as.numeric(sim$trades$price[1]), pu[2])
  expect_equal(as.numeric(sim$positions$qty), c(0, 1, 1))
  expect_gt(as.numeric(sim$equity$Equity[3]), as.numeric(sim$equity$Equity[2]))
})

test_that("DI notional fallback works without attaching bizdays", {
  out <- .calculate_futures_di_notional(
    10,
    maturity_date = as.Date("2028-01-03"),
    basis_date = as.Date("2024-01-02")
  )

  expect_true(is.finite(out$pu) && out$pu > 0)
  expect_true(is.finite(out$tick_value) && out$tick_value > 0)
})

test_that("bt_eldoc DI uses PU execution prices and converted PU risk", {
  x <- bt_test_di_ohlc()

  same_close <- bt_eldoc(
    x,
    up = 3,
    down = 3,
    ps_value = 1,
    ps_type = "contract",
    execution = "same_close",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_same_close <- same_close$trades[same_close$trades$reason == "long_entry", ][1, ]
  same_close_i <- which(as.Date(zoo::index(x)) == as.Date(first_same_close$timestamp))[1]
  expect_equal(first_same_close$price, as.numeric(x[same_close_i, "PU_Close"]))
  expect_equal(first_same_close$qty_delta, 1)

  next_avg <- bt_eldoc(
    x,
    up = 3,
    down = 3,
    ps_value = 1,
    ps_type = "contract",
    execution = "next_avg",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_next_avg <- next_avg$trades[next_avg$trades$reason == "long_entry", ][1, ]
  next_avg_i <- which(as.Date(zoo::index(x)) == as.Date(first_next_avg$timestamp))[1]
  expected_next_avg <- mean(as.numeric(x[next_avg_i, c("PU_Open", "PU_High", "PU_Low", "PU_Close")]))
  expect_equal(first_next_avg$price, expected_next_avg)
  expect_equal(first_next_avg$qty_delta, 1)

  risk_res <- bt_eldoc(
    x,
    up = 3,
    down = 3,
    ps_value = 2,
    initial_equity = 1e8,
    ps_type = "eldoc",
    execution = "same_close",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_risk_entry <- risk_res$trades[risk_res$trades$reason == "long_entry", ][1, ]
  risk_i <- which(as.Date(zoo::index(risk_res$mktdata)) == as.Date(first_risk_entry$timestamp))[1]
  stop_rate <- as.numeric(risk_res$mktdata[risk_i, "DonchianLower"])
  stop_pu <- .bt_native_di_rate_to_pu(stop_rate, x, risk_i, "DI1F28")
  tick_value <- abs(as.numeric(x[risk_i, "TickValue"]))
  expected_qty <- floor(1e8 * 0.02 / max(abs(first_risk_entry$price - stop_pu), tick_value))

  expect_true(is.finite(stop_pu) && stop_pu > 0)
  expect_equal(first_risk_entry$qty_delta, expected_qty)
  expect_equal(risk_res$spec$risk$ps_type, "eldoc")
})

test_that("DI breakout converts exact rate line to PU and pyramids in rate space", {
  x <- bt_test_di_ohlc()

  breakout <- bt_eldoc(
    x,
    up = 3,
    down = 3,
    ps_value = 1,
    ps_type = "contract",
    execution = "breakout",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_breakout <- breakout$trades[breakout$trades$reason == "long_entry", ][1, ]
  signal_i <- which(as.Date(zoo::index(breakout$mktdata)) == as.Date(first_breakout$timestamp))[1]
  upper_rate <- as.numeric(breakout$mktdata[signal_i, "DonchianUpper"])
  expected_pu <- .bt_native_di_rate_to_pu(upper_rate, x, signal_i, "DI1F28")

  expect_true(is.finite(expected_pu) && expected_pu > 0)
  expect_equal(first_breakout$price, expected_pu)

  pyramided <- bt_eldoc_exp(
    x,
    up = 3,
    down = 3,
    ps_value = 1,
    ps_type = "contract",
    execution = "breakout",
    atr_n = 3,
    pyramid = TRUE,
    pyramid_step = 0.5,
    max_units = 3,
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  expect_true(any(pyramided$trades$reason == "long_pyramid"))
  expect_equal(max(as.numeric(pyramided$positions$units), na.rm = TRUE), 3)
  expect_true(all(pyramided$trades$price[pyramided$trades$reason == "long_pyramid"] > 1000))
})

test_that("native moving-average wrappers run on synthetic data", {
  x <- bt_test_ohlc()

  ema <- bt_ema(x, fast = 10, slow = 30, fee = "nofee", hide_details = TRUE)
  sma <- bt_sma(x, fast = 10, slow = 30, fee = "nofee", hide_details = TRUE)
  tsmom <- bt_tsmom(x, lookback = 40, fee = "nofee", hide_details = TRUE)

  expect_s3_class(ema, "bt_native_result")
  expect_s3_class(sma, "bt_native_result")
  expect_s3_class(tsmom, "bt_native_result")
  expect_equal(ema$spec$strategy$type, "ema")
  expect_equal(sma$spec$strategy$type, "sma")
  expect_equal(tsmom$spec$strategy$type, "tsmom")
  expect_equal(tsmom$spec$strategy$params$lookback, 40)
  expect_equal(tsmom$spec$risk$ps_type, "notional")
  expect_equal(tsmom$spec$risk$risk_pct, 100)
  expect_true(all(is.finite(as.numeric(ema$rets))))
  expect_true(all(is.finite(as.numeric(sma$rets))))
  expect_true(all(is.finite(as.numeric(tsmom$rets))))
})

test_that("native Donchian stop mode can reverse twice in one bar", {
  idx <- as.POSIXct("2024-01-01 09:00:00", tz = "UTC") + 3600 * 0:9
  x <- xts::xts(
    cbind(
      Open = c(10, 9.5, 9.5, 9.5, 9.5, 10.5, 10.4, 10.5, 10.4, 9.8),
      High = c(10, 10, 10, 9.8, 11, 10.8, 10.7, 10.6, 12, 10),
      Low = c(9, 9, 9, 9.2, 9.5, 10.2, 10.1, 10.2, 8, 9.5),
      Close = c(9.5, 9.5, 9.5, 9.5, 10.5, 10.4, 10.5, 10.4, 9.8, 9.7)
    ),
    order.by = idx
  )
  attr(x, "symbol") <- "BT_REVERSAL"
  attr(x, "fut_tick_size") <- 1
  attr(x, "fut_multiplier") <- 1
  attr(x, "fee_value") <- 1
  attr(x, "fee_type") <- "contract"
  attr(x, "slip_value") <- 1
  attr(x, "slip_type") <- "cash"

  res <- bt_run_native(
    x,
    strategy = bt_strategy_spec("donchian", up = 3, down = 3),
    risk = bt_risk_spec(mode = "fixed", fixed_qty = 1),
    execution = bt_execution_spec(execution = "breakout", fee = "nofee", close_on_end = FALSE),
    report = FALSE
  )

  expect_equal(
    res$trades$reason,
    c("long_entry", "long_exit", "short_entry", "short_exit", "long_entry")
  )
  expect_equal(res$trades$qty_delta, c(1, -1, -1, 1, 1))
  expect_equal(res$trades$price, c(10, 10.1, 10.1, 10.8, 10.8))
  expect_equal(as.numeric(tail(res$positions$qty, 1)), 1)
})

test_that("ElDoc execution modes choose the documented fill price", {
  idx <- as.POSIXct("2024-01-01 09:00:00", tz = "UTC") + 3600 * 0:9
  x <- xts::xts(
    cbind(
      Open = c(10, 9.5, 9.5, 9.5, 9.5, 10.5, 10.4, 10.5, 10.4, 9.8),
      High = c(10, 10, 10, 9.8, 11, 10.8, 10.7, 10.6, 12, 10),
      Low = c(9, 9, 9, 9.2, 9.5, 10.2, 10.1, 10.2, 8, 9.5),
      Close = c(9.5, 9.5, 9.5, 9.5, 10.5, 10.4, 10.5, 10.4, 9.8, 9.7)
    ),
    order.by = idx
  )
  attr(x, "symbol") <- "BT_EXEC"
  attr(x, "fut_tick_size") <- 1
  attr(x, "fut_multiplier") <- 1

  first_entry_price <- function(mode) {
    res <- bt_eldoc(
      x,
      up = 3,
      down = 3,
      ps_value = 1,
      ps_type = "contract",
      execution = mode,
      fee = "nofee",
      long = TRUE,
      short = FALSE,
      hide_details = TRUE
    )
    res$trades$price[which(res$trades$reason == "long_entry")[1]]
  }

  expect_equal(bt_execution_spec("exact_price")$execution, "breakout")
  expect_equal(first_entry_price("breakout"), 10)
  expect_equal(first_entry_price("same_close"), 10.5)
  expect_equal(first_entry_price("next_open"), 10.5)
  expect_equal(first_entry_price("next_close"), 10.4)
  expect_equal(first_entry_price("next_avg"), mean(c(10.5, 10.8, 10.2, 10.4)))
})

test_that("ElDoc channel and notional sizing are hand calculated", {
  x <- bt_test_eldoc_breakout_ohlc()

  eldoc_res <- bt_eldoc(
    x,
    up = 3,
    down = 3,
    ps_value = 2,
    ps_type = "eldoc",
    execution = "breakout",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_eldoc_entry <- eldoc_res$trades[eldoc_res$trades$reason == "long_entry", ][1, ]
  expect_equal(first_eldoc_entry$price, 10)
  expect_equal(first_eldoc_entry$qty_delta, 2000)
  expect_equal(eldoc_res$spec$risk$ps_type, "eldoc")

  notional_res <- bt_eldoc(
    x,
    up = 3,
    down = 3,
    ps_value = 5,
    ps_type = "notional",
    execution = "breakout",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_notional_entry <- notional_res$trades[notional_res$trades$reason == "long_entry", ][1, ]
  expect_equal(first_notional_entry$price, 10)
  expect_equal(first_notional_entry$qty_delta, 500)
  expect_equal(notional_res$spec$risk$ps_type, "notional")
})

test_that("ElDoc supports contract and ATR position sizing", {
  x <- bt_test_ohlc(180)

  contract_res <- bt_eldoc(
    x,
    up = 12,
    down = 8,
    ps_value = 3,
    ps_type = "contract",
    execution = "same_close",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_contract_entry <- contract_res$trades[contract_res$trades$reason == "long_entry", ][1, ]
  expect_equal(first_contract_entry$qty_delta, 3)
  expect_equal(contract_res$spec$risk$ps_type, "contract")

  atr_res <- bt_eldoc(
    x,
    up = 12,
    down = 8,
    ps_value = 2,
    ps_type = "atr",
    atr_n = 10,
    atr_mult = 2,
    execution = "same_close",
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )
  first_atr_entry <- atr_res$trades[atr_res$trades$reason == "long_entry", ][1, ]
  atr_i <- which(as.Date(zoo::index(atr_res$mktdata)) == as.Date(first_atr_entry$timestamp))[1]
  atr <- as.numeric(atr_res$mktdata[atr_i, "ATR"])
  expected_qty <- floor(100000 * 0.02 / (atr * 2))
  expect_true(is.finite(atr) && atr > 0)
  expect_equal(first_atr_entry$qty_delta, expected_qty)
  expect_equal(atr_res$spec$risk$ps_type, "atr")
})

test_that("ElDoc pyramiding adds ATR-stepped units up to the cap", {
  idx <- as.Date("2024-01-01") + 0:29
  close <- c(10, 10, 10, 9.8, seq(11, 36, by = 1))
  x <- xts::xts(
    cbind(
      Open = close,
      High = c(10, 10, 10, 9.9, close[-(1:4)] + 0.6),
      Low = close - 0.6,
      Close = close
    ),
    order.by = idx
  )
  attr(x, "symbol") <- "BT_PYRAMID"
  attr(x, "fut_tick_size") <- 1
  attr(x, "fut_multiplier") <- 1

  res <- bt_eldoc_exp(
    x,
    up = 3,
    down = 3,
    ps_value = 1,
    ps_type = "contract",
    execution = "breakout",
    atr_n = 3,
    pyramid = TRUE,
    pyramid_step = 0.5,
    max_units = 3,
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )

  expect_true(any(res$trades$reason == "long_pyramid"))
  expect_equal(max(as.numeric(res$positions$units), na.rm = TRUE), 3)
  expect_lte(max(abs(as.numeric(res$positions$qty)), na.rm = TRUE), 3)
  expect_s3_class(res$pyramid_events, "data.frame")
  expect_true(NROW(res$pyramid_events) > 0)
  expect_true(all(c(
    "trade_id", "unit_number", "trigger_atr", "initial_qty", "qty_added",
    "position_after_add", "add_entry_qty_ratio", "position_entry_qty_ratio",
    "stop_distance_atr_at_add", "add_fee_per_contract", "add_slip_per_contract",
    "add_cost_per_contract", "add_cost_pct_notional", "entry_fee_per_contract",
    "entry_slip_per_contract", "entry_cost_per_contract", "entry_cost_pct_notional"
  ) %in% names(res$pyramid_events)))
  expect_true(all(res$pyramid_events$qty_added > 0))
  expect_true(all(res$pyramid_events$initial_qty > 0))
  expect_true(all(res$pyramid_events$add_entry_qty_ratio > 0))
  expect_true("Median Add / Entry Qty" %in% res$info_blocks$pyramiding$stat_name)
  expect_true("P25 Add / Entry Qty" %in% res$info_blocks$pyramiding$stat_name)
  expect_true("P75 Add / Entry Qty" %in% res$info_blocks$pyramiding$stat_name)
  expect_true("Median Pos After Add / Entry Qty" %in% res$info_blocks$pyramiding$stat_name)
  expect_true("Median Stop Distance ATR At Add" %in% res$info_blocks$pyramiding$stat_name)
  expect_true("Median Entry Cost / Contract" %in% res$info_blocks$pyramiding$stat_name)
  expect_true("Median Add Cost / Contract" %in% res$info_blocks$pyramiding$stat_name)
  expect_true("Median Add Cost % Notional" %in% res$info_blocks$pyramiding$stat_name)
  expect_equal(res$spec$strategy$params$pyramid_sizing, "risk")

  delayed <- bt_eldoc_exp(
    x,
    up = 3,
    down = 3,
    ps_value = 1,
    ps_type = "contract",
    execution = "breakout",
    atr_n = 3,
    pyramid = TRUE,
    pyramid_start = 3,
    pyramid_step = 0.5,
    max_units = 3,
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )

  expect_equal(delayed$spec$strategy$params$pyramid_start, 3)
  expect_equal(delayed$spec$strategy$params$pyramid_step, 0.5)
  expect_lte(NROW(delayed$pyramid_events), NROW(res$pyramid_events))
  if (NROW(delayed$pyramid_events) > 0) {
    expect_gte(delayed$pyramid_events$event_time[1], res$pyramid_events$event_time[1])
  }

  entry_qty <- bt_eldoc_exp(
    x,
    up = 3,
    down = 3,
    ps_value = 10,
    ps_type = "contract",
    execution = "breakout",
    atr_n = 3,
    pyramid = TRUE,
    pyramid_start = 2,
    pyramid_step = 1,
    pyramid_sizing = "entry_qty",
    pyramid_qty_pct = 0.5,
    max_units = 3,
    fee = "nofee",
    long = TRUE,
    short = FALSE,
    hide_details = TRUE
  )

  add_qty <- abs(entry_qty$trades$qty_delta[entry_qty$trades$reason == "long_pyramid"])
  expect_true(length(add_qty) > 0)
  expect_true(all(add_qty == 5))
  expect_equal(entry_qty$spec$strategy$params$pyramid_sizing, "entry_qty")
  expect_equal(entry_qty$spec$strategy$params$pyramid_qty_pct, 0.5)
  expect_equal(
    entry_qty$info_blocks$strategy$value[match("Pyramid Sizing", entry_qty$info_blocks$strategy$stat_name)],
    "entry_qty"
  )
  expect_equal(
    entry_qty$info_blocks$strategy$value[match("Pyramid Qty %", entry_qty$info_blocks$strategy$stat_name)],
    "50.00%"
  )
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
    only_returns = TRUE,
    returns_type = "Log",
    hide_details = TRUE,
    verbose = FALSE
  )

  expect_s3_class(out, "xts")
  expect_equal(NCOL(out), 2)
  expect_true(all(is.finite(as.numeric(out))))
})

test_that("bt_batch runs time-series momentum grids", {
  x <- bt_test_ohlc()
  assign("BT_NATIVE_TSMOM_BATCH", x, envir = .GlobalEnv)
  on.exit(rm("BT_NATIVE_TSMOM_BATCH", envir = .GlobalEnv), add = TRUE)

  out <- bt_batch(
    type = "tsmom",
    tickers = "BT_NATIVE_TSMOM_BATCH",
    exact_match = TRUE,
    lookback = c(40, 80),
    threshold = 0,
    fee = "nofee",
    only_returns = TRUE,
    returns_type = "Log",
    hide_details = TRUE,
    verbose = FALSE
  )

  expect_s3_class(out, "xts")
  expect_equal(NCOL(out), 2)
  expect_true(all(grepl("tsmom_lb", colnames(out), fixed = TRUE)))
  expect_true(all(is.finite(as.numeric(out))))
})

test_that("bt_batch passes starting equity through specs", {
  x <- bt_test_ohlc(160)
  assign("BT_NATIVE_EQUITY_BATCH", x, envir = .GlobalEnv)
  on.exit(rm("BT_NATIVE_EQUITY_BATCH", envir = .GlobalEnv), add = TRUE)

  out <- bt_batch(
    type = "eldoc",
    tickers = "BT_NATIVE_EQUITY_BATCH",
    exact_match = TRUE,
    mup = 12,
    mdown = 8,
    initial_equity = 250000,
    pyramid = TRUE,
    pyramid_start = 2,
    pyramid_step = 0.75,
    pyramid_sizing = "entry_qty",
    pyramid_qty_pct = 0.5,
    max_units = 3,
    fee = "nofee",
    hide_details = TRUE,
    verbose = FALSE
  )

  expect_type(out, "list")
  expect_length(out, 1)
  expect_true(any(grepl("eq_250000", names(out), fixed = TRUE)))
  expect_equal(out[[1]]$spec$risk$initial_equity, 250000)
  expect_equal(out[[1]]$spec$strategy$params$pyramid_start, 2)
  expect_equal(out[[1]]$spec$strategy$params$pyramid_step, 0.75)
  expect_equal(out[[1]]$spec$strategy$params$pyramid_sizing, "entry_qty")
  expect_equal(out[[1]]$spec$strategy$params$pyramid_qty_pct, 0.5)
  expect_equal(as.numeric(out[[1]]$equity$Equity[1]), 250000)
})

test_that("bt_batch fails strictly by default and can opt into zero-fill", {
  x <- bt_test_ohlc(140)
  attr(x, "ps_value") <- NULL
  attr(x, "ps_type") <- NULL
  assign("BT_NATIVE_BATCH_MISSING_PS", x, envir = .GlobalEnv)
  on.exit(rm("BT_NATIVE_BATCH_MISSING_PS", envir = .GlobalEnv), add = TRUE)

  expect_error(
    bt_batch(
      type = "eldoc",
      tickers = "BT_NATIVE_BATCH_MISSING_PS",
      exact_match = TRUE,
      mup = 12,
      mdown = 8,
      fee = "nofee",
      only_returns = TRUE,
      hide_details = TRUE,
      verbose = FALSE
    ),
    "Backtest failed.*position sizing"
  )

  out <- NULL
  expect_warning(
    out <- bt_batch(
      type = "eldoc",
      tickers = "BT_NATIVE_BATCH_MISSING_PS",
      exact_match = TRUE,
      mup = 12,
      mdown = 8,
      fee = "nofee",
      only_returns = TRUE,
      hide_details = TRUE,
      verbose = FALSE,
      fail_on_error = FALSE
    ),
    "Filling with zeros"
  )
  expect_s3_class(out, "xts")
  expect_true(all(as.numeric(out) == 0))
})

test_that("removed engine argument fails explicitly", {
  x <- bt_test_ohlc(120)

  expect_error(
    bt_batch(
      type = "eldoc",
      tickers = "BT_REMOVED_ENGINE",
      exact_match = TRUE,
      engine = "native",
      only_returns = TRUE
    ),
    "engine.*removed"
  )
  expect_error(
    bt_eldoc(x, up = 12, down = 8, fee = "nofee", engine = "native"),
    "engine.*removed"
  )
})
