test_that("native spec builders normalize public aliases", {
  strategy <- bt_strategy_spec(
    "time_series_momentum",
    up = 12,
    threshold = 0.5,
    long = TRUE,
    short = FALSE
  )
  expect_equal(strategy$type, "tsmom")
  expect_equal(strategy$params$lookback, 12)
  expect_equal(strategy$params$threshold, 0.5)
  expect_false(strategy$short)

  risk <- bt_risk_spec(
    mode = "risk",
    ps_type = "ATR",
    risk_pct = 1.5,
    reinvest = FALSE
  )
  expect_equal(risk$ps_type, "atr")
  expect_equal(risk$stop_source, "atr")
  expect_false(risk$reinvest)

  execution <- bt_execution_spec(
    "exact price",
    fee_value = 4,
    fee_type = "order",
    slip_value = 2,
    slip_type = "ticks",
    close_on_end = FALSE
  )
  expect_equal(execution$execution, "breakout")
  expect_equal(execution$delay, 0)
  expect_equal(execution$commission_per_order, 4)
  expect_null(execution$commission_per_contract)
  expect_equal(execution$slippage_ticks, 2)
  expect_false(execution$close_on_end)

  percent_execution <- bt_execution_spec(fee_value = 0.05, fee_type = "percent")
  expect_equal(percent_execution$fee_type, "percent")
  expect_null(percent_execution$commission_per_contract)
  expect_null(percent_execution$commission_per_order)

  bps_execution <- bt_execution_spec(fee_value = 5, fee_type = "bps")
  expect_equal(bps_execution$fee_type, "bps")
  expect_null(bps_execution$commission_per_contract)
  expect_null(bps_execution$commission_per_order)
})

test_that("native spec builders reject invalid values", {
  expect_error(
    bt_strategy_spec("ema", fast = 0),
    "positive finite"
  )
  expect_error(
    bt_strategy_spec("tsmom", threshold = -1),
    "threshold"
  )
  expect_error(
    bt_risk_spec(risk_pct = 0),
    "risk_pct"
  )
  expect_error(
    bt_execution_spec("bad-mode"),
    "execution"
  )
  expect_error(
    bt_execution_spec(fee_value = -1),
    "fee_value"
  )
  expect_error(
    bt_execution_spec(slip_type = "shares"),
    "slip_type"
  )
})
