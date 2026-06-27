.bt_first_non_null <- function(...) {
  for (arg in list(...)) {
    if (!is.null(arg) && length(arg) > 0) {
      return(arg)
    }
  }
  NULL
}

.bt_value_at <- function(vec, i, default_val) {
  if (is.null(vec) || length(vec) == 0) {
    return(default_val)
  }
  if (length(vec) == 1) {
    return(vec[[1]])
  }
  if (length(vec) < i) {
    return(default_val)
  }
  vec[[i]]
}

.bt_clean_di_column <- function(xts_object, column_name, predicate) {
  if (!inherits(xts_object, "xts")) stop("xts_object must be an xts object.")
  if (NROW(xts_object) == 0) {
    return(xts_object)
  }
  if (!(column_name %in% colnames(xts_object))) {
    return(xts_object)
  }

  col_vec <- zoo::coredata(xts_object[, column_name])
  keep <- predicate(col_vec)
  keep[is.na(keep)] <- TRUE
  xts_object[keep, ]
}

.bt_clean_di <- function(xts_object, column_name = "TickSize", value = 0.001) {
  .bt_clean_di_column(xts_object, column_name, predicate = function(x) x != value)
}

.bt_clean_di_tick <- function(xts_object, column_name = "TickValue", value_over = -5) {
  .bt_clean_di_column(xts_object, column_name, predicate = function(x) x <= value_over)
}

.bt_base_contract_symbol <- function(symbol) {
  if (is.null(symbol) || length(symbol) == 0) {
    return("")
  }
  symbol <- as.character(symbol)[1]
  if (!nzchar(symbol)) {
    return("")
  }
  strsplit(symbol, "_", fixed = TRUE)[[1]][1]
}

.bt_is_di_symbol <- function(symbol) {
  startsWith(.bt_base_contract_symbol(symbol), "DI1")
}

.bt_fill_di_maturity <- function(xts_object, symbol) {
  if (!xts::is.xts(xts_object) || !.bt_is_di_symbol(symbol)) {
    return(xts_object)
  }
  current <- .bt_xts_attr_first(
    xts_object,
    c("maturity", "maturity_date", "expiry", "expiration"),
    groups = c("contract", "metadata")
  )
  current_chr <- if (is.null(current)) character() else trimws(as.character(current))
  if (length(current_chr) > 0 && any(!is.na(current_chr) & nzchar(current_chr))) {
    return(xts_object)
  }
  if (!requireNamespace("finharvest", quietly = TRUE) ||
      !exists("finget_maturities", envir = asNamespace("finharvest"), inherits = FALSE)) {
    return(xts_object)
  }

  contract <- .bt_base_contract_symbol(symbol)
  root <- sub("^([A-Z]+[0-9]*).*", "\\1", contract)
  maturities <- tryCatch(
    finharvest::finget_maturities(tickers = unique(c(contract, root)), next_months = 120),
    error = function(e) NULL
  )
  if (is.null(maturities) || !NROW(maturities) || !"symbol" %in% names(maturities) || !"maturity_date" %in% names(maturities)) {
    return(xts_object)
  }
  idx <- which(as.character(maturities$symbol) == contract)
  if (!length(idx)) {
    return(xts_object)
  }
  maturity <- as.Date(maturities$maturity_date[idx[1]])
  if (!is.na(maturity)) {
    attr(xts_object, "maturity") <- maturity
  }
  xts_object
}

.bt_fetch_finharvest_data <- function(symbol, start_date, end_date) {
  if (!requireNamespace("finharvest", quietly = TRUE)) {
    stop("Package 'finharvest' is required to fetch market data.", call. = FALSE)
  }

  finharvest::finget(
    tickers = symbol,
    start_date = start_date,
    end_date = end_date,
    source = "auto",
    price_base = 1,
    assign = FALSE,
    single_xts = TRUE,
    consolidate = FALSE,
    attrs_source = "codigos",
    mode = "raw",
    verbose = FALSE
  )
}

.bt_backtest_execution <- function(execution = "breakout",
                                   fee = "normal",
                                   fee_value = NULL,
                                   fee_type = NULL,
                                   slip_value = NULL,
                                   slip_type = NULL) {
  bt_execution_spec(
    execution = execution,
    fee = fee,
    fee_value = fee_value,
    fee_type = fee_type,
    slip_value = slip_value,
    slip_type = slip_type
  )
}

.bt_wrapper_options <- function(extra_args, ps_value = NULL, ps_type = NULL,
                                slip_value = NULL) {
  if (length(extra_args) && "engine" %in% names(extra_args)) {
    stop("'engine' was removed; the native engine is now the only engine.", call. = FALSE)
  }
  only_returns <- FALSE
  if (length(extra_args) && "only_returns" %in% names(extra_args)) {
    only_returns <- isTRUE(extra_args$only_returns)
    extra_args$only_returns <- NULL
  }
  clean_di <- TRUE
  if (length(extra_args) && "clean_di" %in% names(extra_args)) {
    clean_di <- isTRUE(extra_args$clean_di)
    extra_args$clean_di <- NULL
  }
  funding <- NULL
  if (length(extra_args) && "funding" %in% names(extra_args)) {
    funding <- extra_args$funding
    extra_args$funding <- NULL
  }
  max_leverage <- NULL
  if (length(extra_args) && "max_leverage" %in% names(extra_args)) {
    max_leverage <- extra_args$max_leverage
    extra_args$max_leverage <- NULL
  }
  integer_qty <- NULL
  if (length(extra_args) && "integer_qty" %in% names(extra_args)) {
    integer_qty <- isTRUE(extra_args$integer_qty)
    extra_args$integer_qty <- NULL
  }
  if (length(extra_args)) {
    stop(sprintf("Unused argument(s): %s", paste(names(extra_args), collapse = ", ")), call. = FALSE)
  }
  list(
    ps_value = ps_value,
    ps_type = ps_type,
    slip_value = slip_value,
    only_returns = only_returns,
    clean_di = clean_di,
    funding = funding,
    max_leverage = max_leverage,
    integer_qty = integer_qty
  )
}

#' Run an experimental ElDoc (Donchian) backtest
#'
#' Configures a simple Donchian breakout system using the native in-memory
#' simulator.
#'
#' Data is fetched from `finharvest::finget()` if the symbol is not
#' found in the global environment; otherwise the preloaded object is used.
#'
#' The function builds the strategy, runs the backtest, prints key summaries, and
#' returns a list of results.
#'
#' @param ticker Character symbol or an `xts` object (or the name of one in the
#'   global environment) with OHLC data. When an object is supplied, its data is
#'   used directly; otherwise it is fetched via `finharvest::finget()`.
#' @param up Integer Donchian window for the upper channel (default 40).
#' @param down Integer Donchian window for the lower channel (default 40).
#' @param ps_value Position-sizing value. With `ps_type = "eldoc"` or `"atr"`,
#'   this is the percent of equity to risk per entry/add. With
#'   `ps_type = "notional"`, this is percent of equity allocated to notional
#'   exposure. With `ps_type = "contract"`, this is the fixed contract/share
#'   quantity. When `NULL`, the function reads `ps_value` metadata from the
#'   ticker.
#' @param initial_equity Starting account equity in money units.
#' @param ps_type Position-sizing type: `"eldoc"`, `"atr"`, `"notional"`, or
#'   `"contract"`. When `NULL`, the function reads `ps_type` metadata from the
#'   ticker.
#' @param execution Execution mode. `"breakout"` uses the exact touched ElDoc
#'   channel price. `"exact_price"` is an alias for `"breakout"`.
#'   `"same_close"` uses the signal candle close. `"next_open"`,
#'   `"next_close"`, and `"next_avg"` use the next candle open, close, or OHLC
#'   average.
#' @param atr_n Integer ATR lookback used by ATR sizing and pyramiding.
#' @param atr_mult ATR multiple used when `ps_type = "atr"`.
#' @param pyramid Logical; if `TRUE`, add units as the position moves in favor.
#' @param pyramid_start Favorable movement in ATR units required before the
#'   first pyramid add. `NULL` uses `pyramid_step` for backward-compatible
#'   behavior.
#' @param pyramid_step Favorable movement in ATR units required before each
#'   add after the first pyramid add.
#' @param pyramid_sizing Sizing method for pyramid adds. `"risk"` keeps the
#'   current behavior and sizes each add from the active stop/risk distance;
#'   `"entry_qty"` sizes each add as a fraction of the initial entry quantity.
#' @param pyramid_qty_pct Fraction of the initial entry quantity to add when
#'   `pyramid_sizing = "entry_qty"`. For example, `0.5` adds half the original
#'   entry size at each pyramid trigger.
#' @param max_units Maximum number of entry/add units in one position.
#' @param fee Either `"normal"` (use instrument fees/slippage if available) or
#'   `"nofee"` to disable fees.
#' @param fee_value Optional commission amount in account currency. When `NULL`,
#'   ticker metadata such as `fee_value` is required unless `fee = "nofee"`.
#' @param fee_type Fee unit. `"contract"` charges `fee_value` per traded unit;
#'   `"order"` charges `fee_value` once per executed order. When `NULL`, ticker
#'   metadata such as `fee_type` is required unless `fee = "nofee"`.
#' @param slip_value Optional slippage override. When omitted, instrument
#'   metadata such as `slip_value` is used.
#' @param slip_type Slippage unit for `slip_value`: `"bps"`, `"ticks"`,
#'   `"points"`/`"price_points"`, or `"cash"`. When `NULL`, ticker slippage
#'   metadata must include its unit.
#' @param start_date Character or Date, start date when fetching data.
#' @param end_date Date, end date when fetching data (default `Sys.Date()`).
#' @param long Logical, enable long entries.
#' @param short Logical, enable short entries.
#' @param invert_signals Logical, invert entry/exit mapping (debug/testing).
#' @param normalize_risk Optional numeric target risk used to append
#'   normalized return columns to the output.
#' @param geometric Logical, use geometric returns when reporting performance.
#' @param verbose Logical, print detailed summaries to the console.
#' @param hide_details Logical; if `TRUE`, simplifies internal object names.
#' @param show_quarterly Logical; if `TRUE`, print quarterly returns and net
#'   profit tables in the console report.
#' @param research_blocks Logical; if `TRUE`, include experimental diagnostics
#'   such as ATR statistics, excursions, threshold tables, and pyramiding details
#'   in the default report and `bt_stats()` output.
#' @param reinvest Logical; if `TRUE`, size new entries using current account
#'   equity. Open positions keep their original quantity until the next order.
#' @param plot Logical; if `TRUE`, plots the portfolio using `rTradingPlots::tplot`.
#' @param ... Advanced options. Currently supports `only_returns` (return only
#'   the returns `xts`), `clean_di` (apply DI-specific bad-row filtering), and
#'   `funding` (perpetual-futures funding events, or `TRUE` to resolve them from
#'   input data / `finharvest` only for explicit `_PERPETUAL` symbols),
#'   `max_leverage` (notional cap when `risk` is `NULL`), and `integer_qty`
#'   (quantity rounding override).
#'
#' @return If `only_returns = TRUE`, returns an `xts` with discrete and log
#'   returns. Otherwise, returns a named list with elements:
#'   - `rets`: equity/account returns `xts`
#'   - `stats`: performance summary statistics on the same scale as `rets`
#'   - `raw_stats`: unnormalised execution and cost summary statistics
#'   - `trades`: native transactions `data.frame`; `fees` is commission only,
#'     `slippage` is slippage cost, and `total_cost` is their sum
#'   - `rets_acct`: account-level returns `xts`
#'   - `raw_rets`: unnormalised account-level returns `xts`
#'   - `mktdata`: market data with indicator columns
#'   - `positions`: native position/equity path
#'   - `equity`: native account equity curve
#'   - `performance_equity`: equity curve rebuilt from `rets`, risk-normalized
#'     when `normalize_risk` is supplied
#'   - `trade_audit`: order-level audit rows for complete trades, including
#'     signal price, slippage-adjusted fill price, fees, slippage, bars held, and
#'     per-trade funding cash when funding applies
#'   - `trade_episodes`: one row per complete trade episode
#'   - `trade_excursions`: per-trade MFE/MAE diagnostics in percent, ATR, and R units
#'   - `pyramid_events`: one row per pyramid add when pyramiding is active,
#'     including add quantity, entry-quantity ratios, stop distance at add, and
#'     add/entry cost diagnostics
#'   - `info_blocks`: modular report/stat blocks consumed by `bt_stats()`
#'
#' @details
#' ElDoc can size positions from its opposite channel (`ps_type = "eldoc"`),
#' ATR distance (`ps_type = "atr"`), notional percent of equity
#' (`ps_type = "notional"`), or fixed contracts/shares
#' (`ps_type = "contract"`).
#' Pyramiding uses ATR trigger steps. By default (`pyramid_sizing = "risk"`), each
#' add is sized independently from the active stop/risk distance. With
#' `pyramid_sizing = "entry_qty"`, each add uses `pyramid_qty_pct` times the
#' initial entry quantity instead. The returned `pyramid_events` and `Pyramiding`
#' report block show how large adds were relative to the entry quantity, how wide
#' the sizing stop was at add, and whether add costs are proportional to entry
#' costs.
#'
#' If the symbol starts with `"DI1"`, the native engine keeps rate OHLC columns
#' for indicators/signals and uses PU columns for execution and mark-to-market
#' when available. It can enrich rate OHLC data through `brfutures` when
#' installed. Instrument metadata (multiplier, tick size, maturity) is
#' propagated when available. Transaction costs are charged in account currency:
#' `fee_value = 4, fee_type = "contract"` means 4 per contract/share, while
#' `fee_type = "order"` means 4 for the whole executed order.
#' @export
bt_eldoc_exp <- function(ticker, up = 40, down = 40, ps_value = NULL, initial_equity = 100000, ps_type = NULL, execution = "breakout", atr_n = 20, atr_mult = 2, pyramid = FALSE, pyramid_start = NULL, pyramid_step = 0.5, pyramid_sizing = "risk", pyramid_qty_pct = 1, max_units = if (isTRUE(pyramid)) 4L else 1L, fee = "normal", fee_value = NULL, fee_type = NULL, slip_value = NULL, slip_type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, hide_details = FALSE, show_quarterly = FALSE, reinvest = TRUE, plot = FALSE, research_blocks = TRUE, ...) {
  opts <- .bt_wrapper_options(list(...), ps_value = ps_value, ps_type = ps_type, slip_value = slip_value)
  ticker_input <- .bt_resolve_ticker_input(ticker, substitute(ticker))
  res <- bt_run_native(
    ticker = ticker_input$symbol,
    data = ticker_input$data,
    strategy = bt_strategy_spec(
      "donchian",
      up = up,
      down = down,
      atr_n = atr_n,
      atr_mult = atr_mult,
      pyramid = pyramid,
      pyramid_start = pyramid_start,
      pyramid_step = pyramid_step,
      pyramid_sizing = pyramid_sizing,
      pyramid_qty_pct = pyramid_qty_pct,
      max_units = max_units,
      long = long,
      short = short,
      invert_signals = invert_signals
    ),
    ps_value = opts$ps_value,
    ps_type = opts$ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    execution = .bt_backtest_execution(
      execution = execution,
      fee = fee,
      fee_value = fee_value,
      fee_type = fee_type,
      slip_value = opts$slip_value,
      slip_type = slip_type
    ),
    max_leverage = opts$max_leverage,
    integer_qty = opts$integer_qty,
    start_date = start_date,
    end_date = end_date,
    normalize_risk = normalize_risk,
    geometric = geometric,
    only_returns = opts$only_returns,
    verbose = verbose,
    clean_di = opts$clean_di,
    funding = opts$funding,
    report = !isTRUE(hide_details),
    show_quarterly = show_quarterly,
    research_blocks = research_blocks
  )
  if (isTRUE(plot) && !isTRUE(opts$only_returns)) .bt_tplot(res$rets)
  res
}

#' Run a basic ElDoc (Donchian) backtest
#'
#' Runs the baseline Donchian breakout system without pyramiding or experimental
#' research blocks in the default printout. Experimental diagnostics are still
#' computed internally and can be requested explicitly through `bt_stats(...,
#' blocks = ...)`; use `bt_eldoc_exp()` for the full research surface.
#'
#' @inheritParams bt_eldoc_exp
#' @param atr_n Integer ATR lookback used when ATR sizing is selected and for
#'   diagnostic columns.
#' @param ... Advanced options. Currently supports `only_returns` (return only
#'   the returns `xts`), `clean_di` (apply DI-specific bad-row filtering), and
#'   `funding` (perpetual-futures funding events, or `TRUE` to resolve them from
#'   the input data / `finharvest`), `max_leverage` (notional cap when `risk` is
#'   `NULL`), and `integer_qty` (quantity rounding override).
#'   Pyramiding/research arguments are intentionally rejected; use
#'   `bt_eldoc_exp()` for that surface.
#' @export
bt_eldoc <- function(ticker, up = 40, down = 40, ps_value = NULL, initial_equity = 100000, ps_type = NULL, execution = "breakout", atr_n = 20, atr_mult = 2, fee = "normal", fee_value = NULL, fee_type = NULL, slip_value = NULL, slip_type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, hide_details = FALSE, show_quarterly = FALSE, reinvest = TRUE, plot = FALSE, ...) {
  extra_args <- list(...)
  experimental_args <- intersect(
    names(extra_args),
    c("pyramid", "pyramid_start", "pyramid_step", "pyramid_sizing", "pyramid_qty_pct", "max_units", "research_blocks")
  )
  if (length(experimental_args)) {
    stop(
      sprintf(
        "Pyramiding/research argument(s) moved out of bt_eldoc(): %s. Use bt_eldoc_exp() for the experimental surface.",
        paste(experimental_args, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  bt_eldoc_exp(
    ticker = ticker,
    up = up,
    down = down,
    ps_value = ps_value,
    initial_equity = initial_equity,
    ps_type = ps_type,
    execution = execution,
    atr_n = atr_n,
    atr_mult = atr_mult,
    pyramid = FALSE,
    pyramid_start = NULL,
    pyramid_step = 0.5,
    pyramid_sizing = "risk",
    pyramid_qty_pct = 1,
    max_units = 1L,
    fee = fee,
    fee_value = fee_value,
    fee_type = fee_type,
    slip_value = slip_value,
    slip_type = slip_type,
    start_date = start_date,
    end_date = end_date,
    long = long,
    short = short,
    invert_signals = invert_signals,
    normalize_risk = normalize_risk,
    geometric = geometric,
    verbose = verbose,
    hide_details = hide_details,
    show_quarterly = show_quarterly,
    reinvest = reinvest,
    plot = plot,
    research_blocks = FALSE,
    ...
  )
}

#' Run an EMA crossover backtest
#'
#' Builds a two-line exponential moving average crossover system where trades
#' are opened when the fast EMA crosses the slow EMA and closed when the
#' relationship reverses. All other parameters mirror [`bt_eldoc()`], allowing
#' reuse of the same position sizing, data sourcing, and reporting pipeline.
#'
#' @inheritParams bt_eldoc
#' @param fast Integer length of the fast EMA (default 20).
#' @param slow Integer length of the slow EMA (default 50).
#' @param ... Advanced options. Currently supports `only_returns` (return only
#'   the returns `xts`), `clean_di` (apply DI-specific bad-row filtering), and
#'   `funding` (perpetual-futures funding events, or `TRUE` to resolve them from
#'   the input data / `finharvest`), `max_leverage` (notional cap when `risk` is
#'   `NULL`), and `integer_qty` (quantity rounding override).
#' @export
bt_ema <- function(ticker, fast = 20, slow = 50, ps_value = NULL, initial_equity = 100000, ps_type = NULL, execution = "same_close", fee = "normal", fee_value = NULL, fee_type = NULL, slip_value = NULL, slip_type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, hide_details = FALSE, show_quarterly = FALSE, reinvest = TRUE, plot = FALSE, ...) {
  opts <- .bt_wrapper_options(list(...), ps_value = ps_value, ps_type = ps_type, slip_value = slip_value)
  ticker_input <- .bt_resolve_ticker_input(ticker, substitute(ticker))
  res <- bt_run_native(
    ticker = ticker_input$symbol,
    data = ticker_input$data,
    strategy = bt_strategy_spec(
      "ema",
      fast = fast,
      slow = slow,
      long = long,
      short = short,
      invert_signals = invert_signals
    ),
    ps_value = opts$ps_value,
    ps_type = opts$ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    execution = .bt_backtest_execution(
      execution = execution,
      fee = fee,
      fee_value = fee_value,
      fee_type = fee_type,
      slip_value = opts$slip_value,
      slip_type = slip_type
    ),
    max_leverage = opts$max_leverage,
    integer_qty = opts$integer_qty,
    start_date = start_date,
    end_date = end_date,
    normalize_risk = normalize_risk,
    geometric = geometric,
    only_returns = opts$only_returns,
    verbose = verbose,
    clean_di = opts$clean_di,
    funding = opts$funding,
    report = !isTRUE(hide_details),
    show_quarterly = show_quarterly
  )
  if (isTRUE(plot) && !isTRUE(opts$only_returns)) .bt_tplot(res$rets)
  res
}

#' Run an SMA crossover backtest
#'
#' Identical to [`bt_ema()`] but uses simple moving averages for its crossover
#' signals. This provides a lightweight wrapper that shares the same
#' infrastructure as other backtests while swapping only the indicator module.
#'
#' @inheritParams bt_ema
#' @export
bt_sma <- function(ticker, fast = 20, slow = 50, ps_value = NULL, initial_equity = 100000, ps_type = NULL, execution = "same_close", fee = "normal", fee_value = NULL, fee_type = NULL, slip_value = NULL, slip_type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, hide_details = FALSE, show_quarterly = FALSE, reinvest = TRUE, plot = FALSE, ...) {
  opts <- .bt_wrapper_options(list(...), ps_value = ps_value, ps_type = ps_type, slip_value = slip_value)
  ticker_input <- .bt_resolve_ticker_input(ticker, substitute(ticker))
  res <- bt_run_native(
    ticker = ticker_input$symbol,
    data = ticker_input$data,
    strategy = bt_strategy_spec(
      "sma",
      fast = fast,
      slow = slow,
      long = long,
      short = short,
      invert_signals = invert_signals
    ),
    ps_value = opts$ps_value,
    ps_type = opts$ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    execution = .bt_backtest_execution(
      execution = execution,
      fee = fee,
      fee_value = fee_value,
      fee_type = fee_type,
      slip_value = opts$slip_value,
      slip_type = slip_type
    ),
    max_leverage = opts$max_leverage,
    integer_qty = opts$integer_qty,
    start_date = start_date,
    end_date = end_date,
    normalize_risk = normalize_risk,
    geometric = geometric,
    only_returns = opts$only_returns,
    verbose = verbose,
    clean_di = opts$clean_di,
    funding = opts$funding,
    report = !isTRUE(hide_details),
    show_quarterly = show_quarterly
  )
  if (isTRUE(plot) && !isTRUE(opts$only_returns)) .bt_tplot(res$rets)
  res
}

#' Run a time-series momentum backtest
#'
#' Implements a compact academic-style time-series momentum rule: go long when
#' the instrument's own trailing return over `lookback` bars is positive, go
#' short when it is negative, and rebalance on signal flips. The default uses
#' notional sizing and next-open execution to avoid same-close lookahead.
#'
#' @inheritParams bt_eldoc
#' @param lookback Integer number of bars used to compute trailing return.
#' @param threshold Non-negative return threshold. The default `0` trades on the
#'   sign of trailing return.
#' @param atr_n Integer ATR lookback used when ATR sizing is selected and for
#'   diagnostic columns.
#' @param ... Advanced options. Currently supports `only_returns` (return only
#'   the returns `xts`), `clean_di` (apply DI-specific bad-row filtering), and
#'   `funding` (perpetual-futures funding events, or `TRUE` to resolve them from
#'   the input data / `finharvest`), `max_leverage` (notional cap when `risk` is
#'   `NULL`), and `integer_qty` (quantity rounding override).
#' @export
bt_tsmom <- function(ticker, lookback = 252, threshold = 0, ps_value = 100, initial_equity = 100000, ps_type = "notional", execution = "next_open", atr_n = 20, fee = "normal", fee_value = NULL, fee_type = NULL, slip_value = NULL, slip_type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, hide_details = FALSE, show_quarterly = FALSE, reinvest = TRUE, plot = FALSE, ...) {
  if (is.null(ps_value)) ps_value <- 100
  if (is.null(ps_type)) ps_type <- "notional"
  opts <- .bt_wrapper_options(list(...), ps_value = ps_value, ps_type = ps_type, slip_value = slip_value)
  ticker_input <- .bt_resolve_ticker_input(ticker, substitute(ticker))
  res <- bt_run_native(
    ticker = ticker_input$symbol,
    data = ticker_input$data,
    strategy = bt_strategy_spec(
      "tsmom",
      lookback = lookback,
      threshold = threshold,
      atr_n = atr_n,
      long = long,
      short = short,
      invert_signals = invert_signals
    ),
    ps_value = opts$ps_value,
    ps_type = opts$ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    execution = .bt_backtest_execution(
      execution = execution,
      fee = fee,
      fee_value = fee_value,
      fee_type = fee_type,
      slip_value = opts$slip_value,
      slip_type = slip_type
    ),
    max_leverage = opts$max_leverage,
    integer_qty = opts$integer_qty,
    start_date = start_date,
    end_date = end_date,
    normalize_risk = normalize_risk,
    geometric = geometric,
    only_returns = opts$only_returns,
    verbose = verbose,
    clean_di = opts$clean_di,
    funding = opts$funding,
    report = !isTRUE(hide_details),
    show_quarterly = show_quarterly,
    research_blocks = FALSE
  )
  if (isTRUE(plot) && !isTRUE(opts$only_returns)) .bt_tplot(res$rets)
  res
}

#' Run a first-close to last-close hold backtest
#'
#' Opens one position on the first available close and closes it on the last
#' available close. The default uses 100% notional sizing so the result behaves
#' like buy-and-hold for long tests. It uses fractional quantity by default so
#' `ps_value = 100` maps to the full notional return of the price series; pass
#' `integer_qty = TRUE` through `...` to force whole-unit sizing. The wrapper
#' still passes through the same native execution, fee, slippage, funding, and
#' reporting machinery used by the other backtest wrappers.
#'
#' @inheritParams bt_eldoc
#' @param side Direction to hold, either `"long"` or `"short"`.
#' @param ... Advanced options. Currently supports `only_returns` (return only
#'   the returns `xts`), `clean_di` (apply DI-specific bad-row filtering), and
#'   `funding` (perpetual-futures funding events, or `TRUE` to resolve them from
#'   the input data / `finharvest`), `max_leverage` (notional cap when `risk` is
#'   `NULL`), and `integer_qty` (quantity rounding override).
#' @export
bt_hold <- function(ticker, side = c("long", "short"), ps_value = 100, initial_equity = 100000, ps_type = "notional", execution = "same_close", fee = "normal", fee_value = NULL, fee_type = NULL, slip_value = NULL, slip_type = NULL, start_date = "1900-01-01", end_date = Sys.Date(), normalize_risk = NULL, geometric = TRUE, verbose = FALSE, hide_details = FALSE, show_quarterly = FALSE, reinvest = TRUE, plot = FALSE, ...) {
  side <- match.arg(side)
  if (is.null(ps_value)) ps_value <- 100
  if (is.null(ps_type)) ps_type <- "notional"
  opts <- .bt_wrapper_options(list(...), ps_value = ps_value, ps_type = ps_type, slip_value = slip_value)
  integer_qty <- if (is.null(opts$integer_qty)) FALSE else opts$integer_qty
  ticker_input <- .bt_resolve_ticker_input(ticker, substitute(ticker))
  res <- bt_run_native(
    ticker = ticker_input$symbol,
    data = ticker_input$data,
    strategy = bt_strategy_spec(
      "hold",
      long = identical(side, "long"),
      short = identical(side, "short"),
      invert_signals = FALSE
    ),
    ps_value = opts$ps_value,
    ps_type = opts$ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    execution = .bt_backtest_execution(
      execution = execution,
      fee = fee,
      fee_value = fee_value,
      fee_type = fee_type,
      slip_value = opts$slip_value,
      slip_type = slip_type
    ),
    max_leverage = opts$max_leverage,
    integer_qty = integer_qty,
    start_date = start_date,
    end_date = end_date,
    normalize_risk = normalize_risk,
    geometric = geometric,
    only_returns = opts$only_returns,
    verbose = verbose,
    clean_di = opts$clean_di,
    funding = opts$funding,
    report = !isTRUE(hide_details),
    show_quarterly = show_quarterly,
    research_blocks = FALSE
  )
  if (isTRUE(plot) && !isTRUE(opts$only_returns)) .bt_tplot(res$rets)
  res
}

#' Normalize a return series to a target annualized risk level
#'
#' Searches for "Log" or "Discrete" return columns inside an `xts` object (or a
#' nested structure) and rescales them so the realized annualized volatility of
#' daily compounded returns matches the requested percentage. For intraday
#' returns, bars are compounded by calendar day before volatility is annualized
#' with `sqrt(252)`. The output is returned in the same style (`"Log"` or
#' `"Discrete"`) requested via `type`.
#'
#' @param xts An `xts` object or list containing return series.
#' @param risk Numeric target annualized volatility in percent.
#' @param type Output return type, either `"Discrete"` or `"Log"`.
#' @return A single-column `xts` series with normalized returns.
#' @export
bt_normalize_risk <- function(xts, risk = 10, type = c("Discrete", "Log")) {
  if (!requireNamespace("xts", quietly = TRUE)) {
    stop("Package 'xts' is required.")
  }

  type <- match.arg(type)

  # ---- helpers ----
  find_returns_in_xts <- function(x) {
    if (!xts::is.xts(x)) {
      return(list(log = NULL, discrete = NULL))
    }
    cn <- colnames(x)
    if (is.null(cn)) cn <- character(0)
    lc <- tolower(cn)
    log_idx <- which(lc == "log")
    disc_idx <- which(lc == "discrete")
    list(
      log = if (length(log_idx)) x[, log_idx[1], drop = FALSE] else NULL,
      discrete = if (length(disc_idx)) x[, disc_idx[1], drop = FALSE] else NULL
    )
  }

  search_returns <- function(obj, depth = 0, max_depth = 4) {
    if (depth > max_depth) {
      return(NULL)
    }
    # 1) Direct xts with columns
    if (xts::is.xts(obj)) {
      fr <- find_returns_in_xts(obj)
      if (!is.null(fr$log) || !is.null(fr$discrete)) {
        return(list(host = obj, log = fr$log, discrete = fr$discrete))
      }
      # try attribute 'rets'
      ra <- attr(obj, "rets")
      if (!is.null(ra)) {
        res <- search_returns(ra, depth + 1, max_depth)
        if (!is.null(res)) {
          return(res)
        }
      }
      return(NULL)
    }
    # 2) If it's a list, try $rets first, then elements
    if (is.list(obj)) {
      nm <- names(obj)
      # rets first (common convention)
      if (!is.null(nm) && "rets" %in% tolower(nm)) {
        rets_name <- nm[match("rets", tolower(nm))]
        res <- search_returns(obj[[rets_name]], depth + 1, max_depth)
        if (!is.null(res)) {
          return(res)
        }
      }
      # then scan all elements
      for (i in seq_along(obj)) {
        res <- search_returns(obj[[i]], depth + 1, max_depth)
        if (!is.null(res)) {
          return(res)
        }
      }
      return(NULL)
    }
    # Try attribute 'rets' if any other object
    ra <- attr(obj, "rets")
    if (!is.null(ra)) {
      res <- search_returns(ra, depth + 1, max_depth)
      if (!is.null(res)) {
        return(res)
      }
    }
    NULL
  }

  annualized_vol <- function(r) {
    .bt_annualized_daily_vol_from_log(r)
  }

  solve_scale_factor <- function(r, target_vol_ann, original_vol_ann) {
    scale_guess <- as.numeric(target_vol_ann / original_vol_ann)
    vol_at <- function(scale) .bt_annualized_daily_vol_from_log(r, scale = scale)
    f <- function(scale) vol_at(scale) - target_vol_ann
    upper <- max(1, scale_guess * 2, na.rm = TRUE)
    for (i in seq_len(50L)) {
      val <- f(upper)
      if (is.finite(val) && val >= 0) {
        break
      }
      upper <- upper * 2
    }
    root <- tryCatch(
      stats::uniroot(f, lower = 0, upper = upper, tol = 1e-12)$root,
      error = function(e) NA_real_
    )
    if (is.finite(root) && root > 0) root else scale_guess
  }

  # ---- locate returns ----
  found <- search_returns(xts)
  if (is.null(found)) {
    stop("Could not locate 'Log' or 'Discrete' returns in the provided object.")
  }

  # Choose working log returns for scaling
  if (!is.null(found$log)) {
    r_log <- found$log
  } else if (!is.null(found$discrete)) {
    disc <- as.numeric(found$discrete)
    if (any(!is.finite(disc))) {
      warning("Non-finite values found in 'Discrete' returns; they will be kept as NA.")
    }
    if (any(disc <= -1, na.rm = TRUE)) {
      stop("Discrete returns contain values <= -1, cannot convert to log returns.")
    }
    r_log <- xts::xts(log1p(disc), order.by = index(found$discrete))
    colnames(r_log) <- "Log"
  } else {
    stop("Internal error: neither 'Log' nor 'Discrete' returns found after search.")
  }

  # Compute realized annualized vol on log returns
  vol_ann <- annualized_vol(r_log)
  if (!is.finite(vol_ann) || vol_ann <= 0) {
    stop("Realized volatility is zero or undefined; cannot normalize risk.")
  }

  target_vol_ann <- risk / 100 # convert percent to decimal
  scale_factor <- solve_scale_factor(r_log, target_vol_ann, vol_ann)

  # Scale log returns
  r_log_scaled <- r_log * scale_factor

  # Convert to requested output type
  if (identical(type, "Log")) {
    out <- r_log_scaled
    colnames(out) <- "Log"
  } else {
    # Discrete
    out <- xts::xts(exp(as.numeric(r_log_scaled)) - 1, order.by = index(r_log_scaled))
    colnames(out) <- "Discrete"
  }

  attr(out, "scale_factor") <- scale_factor
  attr(out, "original_vol") <- vol_ann
  attr(out, "target_vol") <- target_vol_ann
  attr(out, "target_risk_pct") <- risk

  out
}

#' Normalise fee selection flags
#'
#' Converts user-facing strings (e.g. "no fee", "with fees") into canonical
#' modes understood by `bt_eldoc`/`bt_batch`. Defaults to "normal" when
#' the value is missing or unrecognised.
#'
#' @param val Input value supplied by the caller.
#' @param default Fallback mode when `val` cannot be interpreted.
#' @return Character scalar, either "normal" or "nofee".
#' @keywords internal
normalize_fee_mode <- function(val, default = "normal") {
  if (missing(val) || is.null(val) || length(val) == 0) {
    return(default)
  }
  raw <- as.character(val)[1]
  if (!nzchar(raw)) {
    return(default)
  }
  clean <- gsub("[^A-Za-z0-9]", "", tolower(raw))
  if (clean %in% c("nofee", "nofees", "none", "zero", "0", "nocharge", "withoutfees")) {
    return("nofee")
  }
  if (clean %in% c("normal", "fees", "withfees", "default")) {
    return("normal")
  }
  warning(sprintf("Unrecognized fee option '%s'; falling back to '%s'.", raw, default), call. = FALSE)
  default
}

.bt_batch_module_info <- function(type_name) {
  type_token <- tolower(trimws(as.character(type_name)[1]))
  switch(type_token,
    eldoc = list(
      type = "eldoc",
      runner = function(...) {
        args <- list(...)
        if (isTRUE(args$pyramid)) {
          return(do.call(bt_eldoc_exp, args))
        }
        args[c("pyramid", "pyramid_start", "pyramid_step", "pyramid_sizing", "pyramid_qty_pct", "max_units")] <- NULL
        do.call(bt_eldoc, args)
      },
      resolve_indicator_args = function(args) {
        pyramid <- isTRUE(.bt_first_non_null(args$pyramid, FALSE))
        pyramid_step <- as.numeric(.bt_first_non_null(args$pyramid_step, 0.5))
        pyramid_start <- as.numeric(.bt_first_non_null(args$pyramid_start, pyramid_step))
        pyramid_sizing <- tolower(as.character(.bt_first_non_null(args$pyramid_sizing, "risk"))[1])
        pyramid_qty_pct <- as.numeric(.bt_first_non_null(args$pyramid_qty_pct, 1))
        list(
          up = as.integer(.bt_first_non_null(args$up, args$mup, 40L)),
          down = as.integer(.bt_first_non_null(args$down, args$mdown, 40L)),
          atr_n = as.integer(.bt_first_non_null(args$atr_n, args$atr, 20L)),
          atr_mult = as.numeric(.bt_first_non_null(args$atr_mult, 2)),
          pyramid = pyramid,
          pyramid_start = pyramid_start,
          pyramid_step = pyramid_step,
          pyramid_sizing = pyramid_sizing,
          pyramid_qty_pct = pyramid_qty_pct,
          max_units = as.integer(.bt_first_non_null(args$max_units, if (isTRUE(pyramid)) 4L else 1L))
        )
      },
      batch_suffix = function(indicator_args) {
        fmt <- function(x) gsub("[^A-Za-z0-9]+", "p", format(x, trim = TRUE, scientific = FALSE))
        suffix <- paste0(indicator_args$up, "_", indicator_args$down)
        atr_changed <- !identical(as.integer(indicator_args$atr_n), 20L) ||
          !isTRUE(all.equal(as.numeric(indicator_args$atr_mult), 2))
        if (isTRUE(atr_changed) || isTRUE(indicator_args$pyramid)) {
          suffix <- paste0(suffix, "_atr", fmt(indicator_args$atr_n), "x", fmt(indicator_args$atr_mult))
        }
        if (isTRUE(indicator_args$pyramid)) {
          suffix <- paste0(
            suffix,
            "_pyr",
            fmt(indicator_args$pyramid_start),
            "s",
            fmt(indicator_args$pyramid_step),
            "u",
            fmt(indicator_args$max_units)
          )
          if (!identical(indicator_args$pyramid_sizing, "risk") ||
              !isTRUE(all.equal(as.numeric(indicator_args$pyramid_qty_pct), 1))) {
            suffix <- paste0(
              suffix,
              "_",
              fmt(indicator_args$pyramid_sizing),
              fmt(indicator_args$pyramid_qty_pct)
            )
          }
        }
        suffix
      }
    ),
    ema = list(
      type = "ema",
      runner = bt_ema,
      resolve_indicator_args = function(args) {
        list(
          fast = as.integer(.bt_first_non_null(args$fast, args$mup, args$up, 20L)),
          slow = as.integer(.bt_first_non_null(args$slow, args$mdown, args$down, 50L))
        )
      },
      batch_suffix = function(indicator_args) paste0(indicator_args$fast, "_", indicator_args$slow)
    ),
    sma = list(
      type = "sma",
      runner = bt_sma,
      resolve_indicator_args = function(args) {
        list(
          fast = as.integer(.bt_first_non_null(args$fast, args$mup, args$up, 20L)),
          slow = as.integer(.bt_first_non_null(args$slow, args$mdown, args$down, 50L))
        )
      },
      batch_suffix = function(indicator_args) paste0(indicator_args$fast, "_", indicator_args$slow)
    ),
    tsmom = list(
      type = "tsmom",
      runner = bt_tsmom,
      resolve_indicator_args = function(args) {
        list(
          lookback = as.integer(.bt_first_non_null(args$lookback, args$mup, args$up, 252L)),
          threshold = as.numeric(.bt_first_non_null(args$threshold, 0)),
          atr_n = as.integer(.bt_first_non_null(args$atr_n, args$atr, 20L))
        )
      },
      batch_suffix = function(indicator_args) {
        fmt <- function(x) gsub("[^A-Za-z0-9]+", "p", format(x, trim = TRUE, scientific = FALSE))
        suffix <- paste0("lb", fmt(indicator_args$lookback))
        if (!isTRUE(all.equal(as.numeric(indicator_args$threshold), 0))) {
          suffix <- paste0(suffix, "_thr", fmt(indicator_args$threshold))
        }
        suffix
      }
    ),
    stop(sprintf("Unsupported backtest type '%s'.", type_name), call. = FALSE)
  )
}

#' Run multiple backtests in batch
#'
#' Vectorises the single-ticker backtest wrappers (`bt_eldoc()`, `bt_ema()`,
#' `bt_sma()`, `bt_tsmom()`) across symbols, timeframes, and parameter grids. Indicator-specific
#' parameters can be supplied through `mup`/`mdown` for Donchian runs or via
#' `...` when using other modules.
#'
#' @param type Character scalar selecting the indicator module. One of
#'   `"eldoc"` (Donchian), `"ema"`, `"sma"`, or `"tsmom"`.
#' @param tickers Character vector of base symbols to backtest, or a (named)
#'   list where each element describes overrides for a specific ticker (e.g.
#'   `list(ES = list(timeframes = c("30M", "1H"), mup = c(30, 40)))`). When
#'   omitted you may pass the specification list as the first argument (e.g.
#'   `bt_batch(list(MySpec = list(ticker = "ES", type = "eldoc", timeframes = "1H"))))`),
#'   allowing each specification to provide its own `ticker`, `type`, and
#'   parameter grid.
#' @param timeframes Character vector of timeframe suffixes appended to each
#'   ticker (e.g. `"4H"`).
#' @param exact_match Logical; if `TRUE`, use each ticker exactly as supplied
#'   instead of appending `timeframes`.
#' @param mup,mdown Numeric vectors used as Donchian window lengths and also as
#'   fallbacks for moving-average speeds.
#' @param ps_value Position-sizing value. When `NULL`, ticker metadata is
#'   used.
#' @param initial_equity Starting account equity in money units.
#' @param ps_type Position-sizing type. When `NULL`, ticker metadata is
#'   used.
#' @param execution Execution mode for ElDoc runs. See [`bt_eldoc()`].
#' @param atr_n,atr_mult ATR settings for ElDoc ATR sizing and pyramiding.
#' @param pyramid,pyramid_start,pyramid_step,pyramid_sizing,pyramid_qty_pct,max_units
#'   ElDoc pyramiding controls. `pyramid_start` is the favorable ATR movement
#'   required before the first add; `pyramid_step` is used for each later add.
#'   `pyramid_sizing = "entry_qty"` uses `pyramid_qty_pct` times the initial
#'   entry quantity for each add.
#' @param fee Fee handling mode per test (`"normal"` or `"nofee"`).
#' @param fee_value Optional commission amount in account currency. `NULL`
#'   reads ticker metadata.
#' @param fee_type Fee unit, either `"contract"` or `"order"`. `NULL` reads
#'   ticker metadata.
#' @param slip_value Optional slippage override. `NULL` reads ticker
#'   metadata.
#' @param slip_type Slippage unit for `slip_value`. `NULL` reads ticker
#'   metadata.
#' @param start_date,end_date Optional boundaries for fetched data.
#' @param long,short Logical flags enabling long and/or short trades.
#' @param invert_signals Logical flag to invert generated signals.
#' @param normalize_risk Optional numeric target risk for return normalisation.
#' @param geometric Logical indicating geometric aggregation for reports.
#' @param verbose Logical or logical vector for emitting progress messages.
#' @param only_returns Logical; if `TRUE`, returns an `xts` matrix of returns
#'   instead of the full backtest objects.
#' @param hide_details Logical flag propagated to the underlying backtest.
#' @param show_quarterly Logical flag propagated to the underlying console
#'   report; if `FALSE`, quarterly tables are omitted.
#' @param fail_on_error Logical; if `TRUE` (default), any failed individual
#'   backtest stops the batch. Set to `FALSE` only when you explicitly want the
#'   behavior of warning and filling the failed series with zeros.
#' @param clean_di Logical flag propagated to the underlying backtest.
#' @param reinvest Logical; if `TRUE`, size new entries using current account
#'   equity. Open positions keep their original quantity until the next order.
#' @param returns_type Desired returns column (`"Log"`, `"Discrete"`,
#'   `"LogAdj"`, or `"DiscreteAdj"`).
#' @param plot_mult Logical flag; if `TRUE`, plots combined results via `tplot()`.
#' @param gen_portfolio Optional vector or list describing which backtest
#'   outputs to aggregate into synthetic portfolios. Each element (or the entire
#'   vector when not a list) is treated as one group of indices based on the
#'   execution order, producing an additional `Portfolio_*` series. Elements may
#'   be numeric vectors, character identifiers, or lists containing `indices`
#'   and/or `labels` entries.
#' @param gen_portfolio_weights Optional weights applied to each group defined
#'   by `gen_portfolio`. Accepts a vector (recycled) or list mirroring the group
#'   structure. When omitted or containing only zeros/`NA`, equal weights are
#'   assumed for the corresponding group.
#' @param gen_portfolio_norm_risk Optional numeric vector mirroring the groups
#'   in `gen_portfolio`, specifying the target annualised volatility for each
#'   generated portfolio. When omitted, portfolio aggregates keep the natural
#'   volatility of their components.
#' @param ... Indicator-specific parameter vectors (e.g., `fast`, `slow` for
#'   moving averages).
#'
#' @return When `only_returns = TRUE`, an `xts` object with merged returns
#'   columns; otherwise a named list of backtest results keyed by generated
#'   labels.
#' @export
bt_batch <- function(
  type = c("eldoc", "ema", "sma", "tsmom"),
  tickers,
  timeframes = "1D",
  exact_match = FALSE,
  mup = 40,
  mdown = 40,
  ps_value = NULL,
  initial_equity = 100000,
  ps_type = NULL,
  execution = "breakout",
  atr_n = 20,
  atr_mult = 2,
  pyramid = FALSE,
  pyramid_start = NULL,
  pyramid_step = 0.5,
  pyramid_sizing = "risk",
  pyramid_qty_pct = 1,
  max_units = if (isTRUE(pyramid)) 4L else 1L,
  fee = "normal",
  fee_value = NULL,
  fee_type = NULL,
  slip_value = NULL,
  slip_type = NULL,
  start_date = "1900-01-01",
  end_date = Sys.Date(),
  long = TRUE,
  short = TRUE,
  invert_signals = FALSE,
  normalize_risk = NULL,
  geometric = FALSE,
  verbose = FALSE,
  only_returns = FALSE,
  hide_details = FALSE,
  show_quarterly = FALSE,
  fail_on_error = TRUE,
  clean_di = TRUE,
  reinvest = TRUE,
  returns_type = "Log",
  plot_mult = FALSE,
  gen_portfolio = NULL,
  gen_portfolio_weights = NULL,
  gen_portfolio_norm_risk = NULL,
  ...
) {
  if (!requireNamespace("xts", quietly = TRUE)) stop("Package 'xts' is required.")

  spec_list_input <- NULL
  using_spec_list <- FALSE
  if (missing(tickers) && is.list(type) && !is.data.frame(type)) {
    spec_list_input <- type
    tickers <- NULL
    type <- NULL
    using_spec_list <- TRUE
  }

  default_type <- NULL
  if (!using_spec_list) {
    type <- match.arg(type)
    default_type <- type
  }

  tol <- 1e-8

  extra_args <- list(...)
  if ("engine" %in% names(extra_args)) {
    stop("'engine' was removed; the native engine is now the only engine.", call. = FALSE)
  }
  indicator_vectors <- c(
    list(
      mup = mup,
      mdown = mdown,
      atr_n = atr_n,
      atr_mult = atr_mult,
      pyramid = pyramid,
      pyramid_start = pyramid_start,
      pyramid_step = pyramid_step,
      pyramid_sizing = pyramid_sizing,
      pyramid_qty_pct = pyramid_qty_pct,
      max_units = max_units
    ),
    extra_args
  )

  module_cache <- new.env(parent = emptyenv())
  resolve_module <- function(type_name) {
    if (is.null(type_name) || length(type_name) == 0) {
      stop("Each specification must declare a backtest 'type'.", call. = FALSE)
    }
    type_token <- tolower(trimws(as.character(type_name)[1]))
    if (!nzchar(type_token)) {
      stop("Each specification must declare a non-empty backtest 'type'.", call. = FALSE)
    }
    if (!exists(type_token, envir = module_cache, inherits = FALSE)) {
      canonical <- switch(type_token,
        eldoc = "eldoc",
        ema = "ema",
        sma = "sma",
        tsmom = "tsmom",
        stop(sprintf("Unsupported backtest type '%s'", type_name), call. = FALSE)
      )
      assign(type_token, .bt_batch_module_info(canonical), envir = module_cache)
    }
    get(type_token, envir = module_cache, inherits = FALSE)
  }

  spec_input <- if (using_spec_list) spec_list_input else tickers

  normalize_rtype <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return("Log")
    }
    x <- as.character(x)[1]
    y <- tolower(gsub("[^A-Za-z]", "", x))
    if (y %in% c("log", "l")) {
      return("Log")
    }
    if (y %in% c("discrete", "d")) {
      return("Discrete")
    }
    if (y %in% c("logadj", "lognormalized", "lognorm")) {
      return("LogAdj")
    }
    if (y %in% c("discreteadj", "discretenormalized", "discretenorm")) {
      return("DiscreteAdj")
    }
    "Log"
  }

  format_cost_value <- function(value) {
    val <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(val)) {
      return(NULL)
    }
    out <- format(val, trim = TRUE, scientific = FALSE)
    if (grepl(".", out, fixed = TRUE)) {
      out <- sub("0+$", "", out)
      out <- sub("\\.$", "", out)
    }
    gsub("[^A-Za-z0-9]+", "p", out)
  }

  build_label <- function(tk, tf, type_name, indicator_suffix, ps_type, ps_value, initial_equity, execution_mode, L, S, fee_mode, fee_value, fee_type, slip_value, slip_type, inv, geom, nrisk, rtype, reinv) {
    suffix <- if (identical(fee_mode, "nofee")) "_nofees" else ""
    eq_value <- suppressWarnings(as.numeric(initial_equity)[1])
    eq_token_value <- format_cost_value(eq_value)
    eq_token <- if (is.finite(eq_value) && !identical(eq_value, 100000)) {
      paste0("eq_", eq_token_value)
    } else {
      NULL
    }
    fee_token_value <- format_cost_value(fee_value)
    fee_type_token <- .bt_normalize_fee_type(fee_type)
    fee_token <- if (!identical(fee_mode, "nofee") && (!is.null(fee_token_value) || !is.null(fee_type_token))) {
      paste0("fee_", fee_token_value %||% "meta", "_", fee_type_token %||% "meta")
    } else {
      NULL
    }
    slip_token_value <- format_cost_value(slip_value)
    slip_token <- if (!is.null(slip_token_value)) {
      paste0("slip_", slip_token_value, "_", .bt_normalize_slippage_type(slip_type) %||% "meta")
    } else {
      NULL
    }
    ps_type_token <- .bt_normalize_ps_type(ps_type) %||% "ps"
    ps_value_token <- format_risk_token(ps_value)
    if (!length(ps_value_token) || is.na(ps_value_token) || !nzchar(ps_value_token)) ps_value_token <- "meta"
    exec_token <- tryCatch(.bt_normalize_execution_mode(execution_mode), error = function(e) NULL)
    exec_token <- if (!is.null(exec_token) && !identical(exec_token, "breakout")) paste0("exec_", exec_token) else NULL
    parts <- c(
      sprintf("%s_%s%s", tk, tf, suffix),
      sprintf("%s_%s", type_name, indicator_suffix),
      sprintf("%s_%s", ps_type_token, ps_value_token),
      eq_token,
      exec_token,
      fee_token,
      slip_token,
      if (!isTRUE(L)) "nolong" else "long",
      if (!isTRUE(S)) "noshort" else "short",
      if (isTRUE(inv)) "inv" else NULL,
      if (!isTRUE(reinv)) "noreinv" else NULL,
      paste0("geom_", if (isTRUE(geom)) "T" else "F"),
      if (is.null(nrisk)) NULL else paste0("nr_", nrisk),
      paste0("rt_", rtype)
    )
    paste(parts[!vapply(parts, is.null, logical(1))], collapse = "_")
  }

  ensure_xts_or_remove <- function(symbol) {
    data_env <- .get_bt_data_env()
    if (exists(symbol, envir = data_env, inherits = FALSE)) {
      obj <- get(symbol, envir = data_env)
      if (!xts::is.xts(obj)) rm(list = symbol, envir = data_env)
    }
  }

  data_env <- .get_bt_data_env()

  preload_symbol <- function(symbol, base_sym) {
    if (exists(symbol, envir = data_env, inherits = FALSE)) {
      return(invisible(NULL))
    }
    if (exists(symbol, envir = .GlobalEnv, inherits = FALSE)) {
      return(invisible(NULL))
    }
    base_obj <- NULL
    if (exists(base_sym, envir = data_env, inherits = FALSE)) base_obj <- get(base_sym, envir = data_env)
    if (is.null(base_obj) && exists(base_sym, envir = .GlobalEnv, inherits = FALSE)) base_obj <- get(base_sym, envir = .GlobalEnv)
    if (!is.null(base_obj) && xts::is.xts(base_obj)) {
      assign(symbol, base_obj, envir = data_env)
    }
  }

  extract_returns <- function(rets_xts, rtype, norm_risk_numeric) {
    if (is.null(rets_xts)) {
      return(NULL)
    }
    find_col <- function(column) {
      idx <- which(tolower(colnames(rets_xts)) == tolower(column))
      if (!length(idx)) {
        return(NULL)
      }
      rets_xts[, idx[1], drop = FALSE]
    }
    base <- switch(rtype,
      "Log" = "Log",
      "Discrete" = "Discrete",
      "LogAdj" = "Log",
      "DiscreteAdj" = "Discrete"
    )
    base_col <- find_col(base)
    if (grepl("Adj$", rtype) && !is.null(norm_risk_numeric)) {
      if (is.null(base_col)) {
        other <- if (base == "Log") find_col("Discrete") else find_col("Log")
        if (!is.null(other)) {
          if (base == "Log") {
            tmp <- xts::xts(log1p(as.numeric(other)), order.by = index(other))
            colnames(tmp) <- "Log"
            base_col <- tmp
          } else {
            tmp <- xts::xts(exp(as.numeric(other)) - 1, order.by = index(other))
            colnames(tmp) <- "Discrete"
            base_col <- tmp
          }
        }
      }
      if (!is.null(base_col)) {
        norm_type <- if (base == "Log") "Log" else "Discrete"
        adj <- tryCatch(bt_normalize_risk(base_col, risk = norm_risk_numeric, type = norm_type), error = function(e) NULL)
        if (!is.null(adj)) {
          colnames(adj) <- rtype
          return(adj)
        }
      }
    }
    if (!is.null(base_col)) {
      return(base_col)
    }
    rets_xts[, 1, drop = FALSE]
  }

  normalize_ticker_specs <- function(spec_input, default_type = NULL) {
    if (is.null(spec_input) || length(spec_input) == 0) {
      stop("Argument 'tickers' cannot be empty.", call. = FALSE)
    }

    trim_token <- function(token) {
      if (is.null(token) || !length(token)) {
        return(NA_character_)
      }
      token <- as.character(token)[1]
      token <- trimws(token)
      if (!nzchar(token)) {
        return(NA_character_)
      }
      token
    }

    select_first_non_null <- function(...) {
      for (item in list(...)) {
        if (is.null(item)) next
        if (length(item) == 0) next
        return(item[[1]])
      }
      NULL
    }

    normalize_entry <- function(entry_value, entry_name) {
      alias_token <- trim_token(entry_name)
      overrides <- list()
      primary_candidate <- alias_token
      label_candidate <- alias_token
      type_candidate <- NULL

      if (is.null(entry_value)) {
        overrides <- list()
      } else if (is.list(entry_value) && !is.data.frame(entry_value)) {
        overrides <- entry_value
      } else if (is.atomic(entry_value)) {
        primary_candidate <- entry_value[1]
        overrides <- list()
      } else {
        stop("Unsupported ticker specification provided to 'bt_batch'.", call. = FALSE)
      }

      if (!is.list(overrides)) overrides <- as.list(overrides)

      if (!is.null(overrides$ticker)) {
        primary_candidate <- overrides$ticker
        overrides$ticker <- NULL
      } else if (!is.null(overrides$symbol)) {
        primary_candidate <- overrides$symbol
        overrides$symbol <- NULL
      } else if (!is.null(overrides$base)) {
        primary_candidate <- overrides$base
        overrides$base <- NULL
      }

      label_fields <- c("label", "alias", "display", "name")
      for (fld in label_fields) {
        if (!is.null(overrides[[fld]])) {
          label_candidate <- overrides[[fld]]
          overrides[[fld]] <- NULL
          break
        }
      }

      type_fields <- c("type", "module")
      for (fld in type_fields) {
        if (!is.null(overrides[[fld]])) {
          type_candidate <- overrides[[fld]]
          overrides[[fld]] <- NULL
          break
        }
      }

      ticker_token <- trim_token(select_first_non_null(primary_candidate, alias_token))
      if (is.na(ticker_token)) {
        stop("Each specification must resolve to a non-empty ticker symbol.", call. = FALSE)
      }

      label_token <- trim_token(select_first_non_null(label_candidate, alias_token, ticker_token))
      if (is.na(label_token)) label_token <- ticker_token

      type_token <- trim_token(select_first_non_null(type_candidate, default_type))
      if (is.na(type_token)) {
        stop(sprintf("No backtest type provided for specification '%s'.", label_token), call. = FALSE)
      }

      overrides <- overrides[!(vapply(overrides, function(x) is.null(x) || (is.list(x) && length(x) == 0), logical(1)))]

      list(
        alias = alias_token,
        label = label_token,
        ticker = ticker_token,
        type = type_token,
        overrides = overrides
      )
    }

    entries <- list()
    if (is.list(spec_input) && !is.data.frame(spec_input)) {
      names_vec <- names(spec_input)
      if (is.null(names_vec)) names_vec <- rep("", length(spec_input))
      for (i in seq_along(spec_input)) {
        entries[[length(entries) + 1]] <- normalize_entry(spec_input[[i]], names_vec[[i]])
      }
    } else {
      vec <- as.list(spec_input)
      names_vec <- names(spec_input)
      if (is.null(names_vec)) names_vec <- rep("", length(vec))
      for (i in seq_along(vec)) {
        entries[[length(entries) + 1]] <- normalize_entry(vec[[i]], names_vec[[i]])
      }
    }

    if (!length(entries)) {
      stop("Argument 'tickers' cannot be empty.", call. = FALSE)
    }
    entries
  }

  idx_to_posixct <- function(idx) {
    if (length(idx) == 0) {
      return(as.POSIXct(character(0), tz = "UTC"))
    }
    if (inherits(idx, "POSIXt")) {
      return(as.POSIXct(idx, tz = "UTC"))
    }
    if (inherits(idx, "Date")) {
      return(as.POSIXct(idx, tz = "UTC"))
    }
    if (inherits(idx, "yearmon") || inherits(idx, "yearqtr")) {
      if (requireNamespace("zoo", quietly = TRUE)) {
        d <- tryCatch(as.Date(idx), error = function(e) NA)
        if (!all(is.na(d))) {
          return(as.POSIXct(d, tz = "UTC"))
        }
      }
    }
    try1 <- suppressWarnings(try(as.POSIXct(idx, origin = "1970-01-01", tz = "UTC"), silent = TRUE))
    if (!inherits(try1, "try-error") && all(is.finite(as.numeric(try1)))) {
      return(try1)
    }
    try2 <- suppressWarnings(try(as.POSIXct(idx, tz = "UTC"), silent = TRUE))
    if (!inherits(try2, "try-error") && all(is.finite(as.numeric(try2)))) {
      return(try2)
    }
    as.POSIXct(character(0), tz = "UTC")
  }

  zero_returns_xts <- function(idx) {
    ob <- idx_to_posixct(idx)
    n <- length(ob)
    xts::xts(matrix(0, nrow = n, ncol = 1), order.by = ob)
  }

  zero_full_returns_xts <- function(idx, normalize_target = NA_real_) {
    ob <- idx_to_posixct(idx)
    n <- length(ob)
    discrete <- xts::xts(matrix(0, nrow = n, ncol = 1), order.by = ob)
    colnames(discrete) <- "Discrete"
    log_xts <- xts::xts(matrix(0, nrow = n, ncol = 1), order.by = ob)
    colnames(log_xts) <- "Log"
    base <- cbind(discrete, log_xts)
    attr(base, "risk_scale") <- NA_real_
    attr(base, "risk_target") <- normalize_target
    attr(base, "risk_original") <- NA_real_
    base
  }

  sanitize_dt_attrs <- function(dt) {
    if (!xts::is.xts(dt)) {
      return(NULL)
    }
    meta <- .collect_instrument_metadata(dt)
    tsz <- .sanitize_scalar_numeric(meta$tick_size, default = 1)
    mult <- .sanitize_scalar_numeric(meta$multiplier, default = NA_real_)
    tick_value <- .sanitize_scalar_numeric(meta$tick_value, default = NA_real_)
    if ((!is.finite(mult) || mult <= 0) && is.finite(tick_value) && tick_value > 0 &&
        is.finite(tsz) && tsz > 0) {
      mult <- tick_value / tsz
    }
    if (!is.finite(mult) || mult <= 0) mult <- 1
    mat <- meta$maturity
    cur <- meta$currency
    if (is.null(cur)) cur <- "USD"
    attr(dt, "fut_tick_size") <- tsz
    attr(dt, "fut_multiplier") <- mult
    attr(dt, "maturity") <- mat
    attr(dt, "currency") <- cur
    dt
  }

  fetch_xts_or_fallback <- function(symbol, base_sym) {
    if (exists(symbol, envir = data_env, inherits = FALSE)) {
      obj <- get(symbol, envir = data_env)
      if (xts::is.xts(obj)) {
        return(obj)
      }
    }
    base_obj <- NULL
    if (exists(base_sym, envir = data_env, inherits = FALSE)) base_obj <- get(base_sym, envir = data_env)
    if (is.null(base_obj) && exists(base_sym, envir = .GlobalEnv, inherits = FALSE)) base_obj <- get(base_sym, envir = .GlobalEnv)
    sanitize_dt_attrs(base_obj)
  }

  fallback_index <- function(ticker_with_tf, base_sym) {
    idx <- tryCatch(index(get(ticker_with_tf, envir = data_env)), error = function(e) NULL)
    if (!is.null(idx)) {
      return(idx)
    }
    td <- fetch_xts_or_fallback(ticker_with_tf, base_sym)
    if (!is.null(td)) {
      return(index(td))
    }
    as.POSIXct(character(0))
  }

  prepare_portfolio_specs <- function(groups) {
    if (is.null(groups) || length(groups) == 0) {
      return(list())
    }
    combos <- groups
    if (!is.list(combos)) combos <- list(combos)
    combo_names <- names(combos)
    if (is.null(combo_names)) {
      combo_names <- paste0("Portfolio", seq_along(combos))
    } else {
      combo_names[!nzchar(combo_names)] <- paste0("Portfolio", which(!nzchar(combo_names)))
    }

    unique_ordered <- function(x) {
      if (length(x) == 0) {
        return(x)
      }
      x[!duplicated(x)]
    }

    collect_tokens <- function(item, acc_nums = integer(), acc_tokens = character()) {
      if (is.null(item) || length(item) == 0) {
        return(list(nums = acc_nums, tokens = acc_tokens))
      }

      append_numeric <- function(vals) {
        vals <- vals[!is.na(vals)]
        if (!length(vals)) {
          return()
        }
        acc_nums <<- c(acc_nums, as.integer(vals))
      }

      append_token <- function(tok) {
        if (is.null(tok) || length(tok) == 0) {
          return()
        }
        tok <- trimws(tok)
        if ((startsWith(tok, "'") && endsWith(tok, "'")) || (startsWith(tok, '"') && endsWith(tok, '"'))) {
          tok <- substring(tok, 2, nchar(tok) - 1)
        }
        if (!nzchar(tok)) {
          return()
        }
        parts <- strsplit(tok, "[+,]")[[1]]
        for (part in parts) {
          part <- trimws(part)
          if ((startsWith(part, "'") && endsWith(part, "'")) || (startsWith(part, '"') && endsWith(part, '"'))) {
            part <- substring(part, 2, nchar(part) - 1)
          }
          if (!nzchar(part)) next
          num_val <- suppressWarnings(as.numeric(part))
          if (length(num_val) == 1 && is.finite(num_val)) {
            append_numeric(num_val)
          } else {
            acc_tokens <<- c(acc_tokens, part)
          }
        }
      }

      if (is.numeric(item)) {
        append_numeric(item)
      } else if (is.character(item)) {
        for (str in item) append_token(str)
      } else if (is.list(item)) {
        for (elem in item) {
          res <- collect_tokens(elem, acc_nums, acc_tokens)
          acc_nums <- res$nums
          acc_tokens <- res$tokens
        }
      } else if (is.language(item)) {
        append_token(deparse(item))
      } else {
        append_token(as.character(item))
      }

      list(nums = acc_nums, tokens = acc_tokens)
    }

    idx_fields <- c("indices", "index", "row", "rows", "line", "lines", "select", "include")
    component_fields <- c("labels", "tokens", "names", "symbols", "components", "members", "tickers")

    specs <- vector("list", length(combos))
    for (i in seq_along(combos)) {
      entry <- combos[[i]]
      manual_nums <- integer()
      manual_tokens <- character()

      append_manual_nums <- function(val) {
        if (is.null(val) || length(val) == 0) {
          return()
        }
        vec <- unlist(val, use.names = FALSE)
        if (!length(vec)) {
          return()
        }
        for (item in vec) {
          parts <- if (is.character(item)) strsplit(item, "[+,;\\s]+")[[1]] else as.character(item)
          if (length(parts) == 0) next
          for (part in parts) {
            part <- trimws(as.character(part))
            if (!nzchar(part)) next
            num_candidate <- suppressWarnings(as.numeric(part))[1]
            if (is.finite(num_candidate)) manual_nums <<- c(manual_nums, as.integer(num_candidate))
          }
        }
      }

      append_manual_components <- function(val) {
        if (is.null(val) || length(val) == 0) {
          return()
        }
        vec <- unlist(val, use.names = FALSE)
        if (!length(vec)) {
          return()
        }
        for (item in vec) {
          parts <- if (is.character(item)) strsplit(item, "[+,;]")[[1]] else as.character(item)
          if (length(parts) == 0) next
          for (part in parts) {
            part_chr <- trimws(as.character(part))
            if (!nzchar(part_chr)) next
            num_candidate <- suppressWarnings(as.numeric(part_chr))[1]
            if (is.finite(num_candidate)) {
              manual_nums <<- c(manual_nums, as.integer(num_candidate))
            } else {
              manual_tokens <<- c(manual_tokens, part_chr)
            }
          }
        }
      }

      if (is.list(entry) && !is.data.frame(entry) && length(entry) && !is.null(names(entry))) {
        for (fld in intersect(names(entry), idx_fields)) {
          append_manual_nums(entry[[fld]])
          entry[[fld]] <- NULL
        }
        for (fld in intersect(names(entry), component_fields)) {
          append_manual_components(entry[[fld]])
          entry[[fld]] <- NULL
        }
        if (length(entry) == 1 && is.null(names(entry)) && is.numeric(entry[[1]])) {
          append_manual_nums(entry[[1]])
          entry <- NULL
        } else if (length(entry) == 0) {
          entry <- NULL
        }
      }

      res <- if (is.null(entry) || length(entry) == 0) list(nums = integer(), tokens = character()) else collect_tokens(entry)
      nums <- unique_ordered(c(manual_nums, res$nums))
      tokens <- unique_ordered(c(manual_tokens, res$tokens))
      specs[[i]] <- list(name = combo_names[i], indices = nums, tokens = tokens)
    }
    specs
  }

  prepare_weight_sets <- function(weights_input, n_groups) {
    if (n_groups == 0) {
      return(list())
    }
    if (is.null(weights_input) || length(weights_input) == 0) {
      return(vector("list", n_groups))
    }
    weights_list <- weights_input
    if (!is.list(weights_list)) weights_list <- list(weights_list)
    if (length(weights_list) == 1 && n_groups > 1) {
      weights_list <- rep(weights_list, n_groups)
    }
    if (length(weights_list) < n_groups) {
      weights_list <- c(weights_list, vector("list", n_groups - length(weights_list)))
    } else if (length(weights_list) > n_groups) {
      weights_list <- weights_list[seq_len(n_groups)]
    }
    lapply(weights_list, function(w) {
      if (is.null(w) || length(w) == 0) numeric(0) else as.numeric(w)
    })
  }

  resolve_weights <- function(weight_vec, n) {
    if (n <= 0) {
      return(numeric(0))
    }
    if (is.null(weight_vec) || length(weight_vec) == 0) {
      return(rep(1, n))
    }
    w <- as.numeric(weight_vec)
    if (!any(is.finite(w)) || sum(abs(w[is.finite(w)])) == 0) {
      return(rep(1, n))
    }
    w <- rep_len(w, n)
    w[!is.finite(w)] <- 0
    w
  }

  first_finite_numeric <- function(values) {
    vals <- suppressWarnings(as.numeric(values))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) NA_real_ else vals[1]
  }

  resolve_portfolio_norms <- function(norm_input, n_groups) {
    if (n_groups == 0) {
      return(vector("list", 0))
    }
    if (is.null(norm_input) || length(norm_input) == 0) {
      return(vector("list", n_groups))
    }
    norms <- norm_input
    if (!is.list(norms)) norms <- as.list(norms)
    if (length(norms) == 1 && n_groups > 1) {
      norms <- rep(norms, n_groups)
    }
    if (length(norms) < n_groups) {
      norms <- c(norms, vector("list", n_groups - length(norms)))
    } else if (length(norms) > n_groups) {
      norms <- norms[seq_len(n_groups)]
    }
    lapply(norms, function(val) {
      num <- suppressWarnings(as.numeric(val)[1])
      if (is.finite(num)) num else NULL
    })
  }

  format_risk_token <- function(value) {
    val <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(val)) {
      out <- as.character(value)[1]
    } else {
      out <- format(val, trim = TRUE, scientific = FALSE)
    }
    out <- trimws(out)
    sub("\\.?0+$", "", out)
  }

  format_group_label <- function(name) {
    if (is.null(name) || !nzchar(name)) {
      return(NULL)
    }
    name <- as.character(name)[1]
    paste0(toupper(substr(name, 1, 1)), substring(name, 2))
  }

  defaults <- list(
    timeframes = "1D",
    ps_value = NULL,
    initial_equity = 100000,
    ps_type = NULL,
    execution = "breakout",
    fee = "normal",
    fee_value = NULL,
    fee_type = NULL,
    slip_value = NULL,
    slip_type = NULL,
    long = TRUE,
    short = TRUE,
    invert_signals = FALSE,
    normalize_risk = NULL,
    geometric = geometric,
    verbose = FALSE,
    hide_details = FALSE,
    show_quarterly = FALSE,
    fail_on_error = TRUE,
    clean_di = TRUE,
    reinvest = TRUE,
    exact_match = FALSE,
    returns_type = "Log"
  )

  default_combo_rtyp <- normalize_rtype(.bt_value_at(returns_type, 1, defaults$returns_type))

  base_param_sources <- list(
    timeframes = timeframes,
    exact_match = exact_match,
    ps_value = ps_value,
    initial_equity = initial_equity,
    ps_type = ps_type,
    execution = execution,
    fee = fee,
    fee_value = fee_value,
    fee_type = fee_type,
    slip_value = slip_value,
    slip_type = slip_type,
    long = long,
    short = short,
    invert_signals = invert_signals,
    normalize_risk = normalize_risk,
    geometric = geometric,
    verbose = verbose,
    hide_details = hide_details,
    show_quarterly = show_quarterly,
    fail_on_error = fail_on_error,
    clean_di = clean_di,
    reinvest = reinvest,
    returns_type = returns_type
  )
  recognized_override_fields <- names(base_param_sources)
  base_indicator_vectors <- indicator_vectors

  ticker_specs <- normalize_ticker_specs(spec_input, default_type = default_type)

  results_list <- list()
  returns_list <- list()
  plot_list <- list()
  label_sequence <- character()
  all_returns_map <- list()
  normalize_risk_map <- list()
  returns_type_map <- list()
  risk_scale_map <- list()
  risk_report <- list()
  risk_report_map <- list()

  for (spec in ticker_specs) {
    spec_ticker <- as.character(spec$ticker)[1]
    label_value <- spec$label
    if (length(label_value) == 0 || is.na(label_value)) label_value <- spec_ticker
    spec_label <- as.character(label_value)[1]
    overrides <- spec$overrides
    if (is.null(overrides)) overrides <- list()
    if ("engine" %in% names(overrides)) {
      stop("'engine' was removed; the native engine is now the only engine.", call. = FALSE)
    }

    module_info <- resolve_module(spec$type)
    type_name <- module_info$type

    per_params <- base_param_sources
    if (length(overrides)) {
      override_names <- intersect(names(overrides), recognized_override_fields)
      if (length(override_names)) {
        for (nm in override_names) {
          per_params[[nm]] <- overrides[[nm]]
          overrides[[nm]] <- NULL
        }
      }
    }

    indicator_vectors_local <- base_indicator_vectors
    if (length(overrides)) {
      for (nm in names(overrides)) {
        indicator_vectors_local[[nm]] <- overrides[[nm]]
      }
    }

    lens <- c(
      length(per_params$timeframes), length(per_params$ps_value), length(per_params$initial_equity), length(per_params$ps_type),
      length(per_params$execution),
      length(per_params$fee), length(per_params$fee_value), length(per_params$fee_type),
      length(per_params$slip_value), length(per_params$slip_type),
      length(per_params$long), length(per_params$short),
      length(per_params$invert_signals), length(per_params$normalize_risk),
      length(per_params$geometric), length(per_params$verbose),
      length(per_params$hide_details), length(per_params$show_quarterly),
      length(per_params$fail_on_error),
      length(per_params$clean_di),
      length(per_params$reinvest), length(per_params$exact_match),
      length(per_params$returns_type),
      if (length(indicator_vectors_local)) vapply(indicator_vectors_local, length, integer(1)) else integer(0)
    )
    n_tests <- max(c(1, lens))

    for (i in seq_len(n_tests)) {
      tf <- .bt_value_at(per_params$timeframes, i, defaults$timeframes)

      indicator_candidates <- list()
      if (length(indicator_vectors_local)) {
        for (nm in names(indicator_vectors_local)) {
          value <- .bt_value_at(indicator_vectors_local[[nm]], i, NULL)
          indicator_candidates[[nm]] <- value
          if (nm %in% c("mup", "up")) {
            indicator_candidates[["mup"]] <- value
            indicator_candidates[["up"]] <- value
          }
          if (nm %in% c("mdown", "down")) {
            indicator_candidates[["mdown"]] <- value
            indicator_candidates[["down"]] <- value
          }
        }
      }

      indicator_args_i <- module_info$resolve_indicator_args(indicator_candidates)
      indicator_suffix <- if (!is.null(module_info$batch_suffix)) module_info$batch_suffix(indicator_args_i) else paste(indicator_args_i, collapse = "_")

      rV <- .bt_value_at(per_params$ps_value, i, defaults$ps_value)
      rV_numeric <- suppressWarnings(as.numeric(rV))[1]
      initial_equityV <- .bt_value_at(per_params$initial_equity, i, defaults$initial_equity)
      psV <- .bt_value_at(per_params$ps_type, i, defaults$ps_type)
      executionV <- .bt_value_at(per_params$execution, i, defaults$execution)
      fee_raw <- .bt_value_at(per_params$fee, i, defaults$fee)
      feeV <- normalize_fee_mode(fee_raw)
      fee_valueV <- .bt_value_at(per_params$fee_value, i, defaults$fee_value)
      fee_typeV <- .bt_value_at(per_params$fee_type, i, defaults$fee_type)
      slip_valueV <- .bt_value_at(per_params$slip_value, i, defaults$slip_value)
      slip_typeV <- .bt_value_at(per_params$slip_type, i, defaults$slip_type)
      L <- isTRUE(.bt_value_at(per_params$long, i, defaults$long))
      S <- isTRUE(.bt_value_at(per_params$short, i, defaults$short))
      inv <- isTRUE(.bt_value_at(per_params$invert_signals, i, defaults$invert_signals))
      nr <- .bt_value_at(per_params$normalize_risk, i, defaults$normalize_risk)
      geom <- isTRUE(.bt_value_at(per_params$geometric, i, defaults$geometric))
      verb <- isTRUE(.bt_value_at(per_params$verbose, i, defaults$verbose))
      hide <- isTRUE(.bt_value_at(per_params$hide_details, i, defaults$hide_details))
      show_q <- isTRUE(.bt_value_at(per_params$show_quarterly, i, defaults$show_quarterly))
      fail <- isTRUE(.bt_value_at(per_params$fail_on_error, i, defaults$fail_on_error))
      clean <- isTRUE(.bt_value_at(per_params$clean_di, i, defaults$clean_di))
      reinv <- isTRUE(.bt_value_at(per_params$reinvest, i, defaults$reinvest))
      exact <- isTRUE(.bt_value_at(per_params$exact_match, i, defaults$exact_match))
      rtyp <- normalize_rtype(.bt_value_at(per_params$returns_type, i, defaults$returns_type))

      data_symbol <- if (exact) spec_ticker else paste0(spec_ticker, "_", tf)
      label_tf <- if (exact) "exact" else tf
      label <- build_label(spec_label, label_tf, type_name, indicator_suffix, psV, rV, initial_equityV, executionV, L, S, feeV, fee_valueV, fee_typeV, slip_valueV, slip_typeV, inv, geom, nr, rtyp, reinv)
      if (isTRUE(verbose) || verb) message(sprintf("Running %s ...", label))

      ensure_xts_or_remove(data_symbol)
      preload_symbol(data_symbol, base_sym = spec_ticker)

      call_args <- c(
        list(
          ticker = data_symbol,
          ps_value = rV,
          initial_equity = initial_equityV,
          ps_type = psV,
          execution = executionV,
          fee = feeV,
          fee_value = fee_valueV,
          fee_type = fee_typeV,
          slip_value = slip_valueV,
          slip_type = slip_typeV,
          start_date = start_date,
          end_date = end_date,
          long = L,
          short = S,
          invert_signals = inv,
          normalize_risk = nr,
          geometric = geom,
          verbose = isTRUE(verbose) || verb,
          only_returns = FALSE,
          hide_details = hide,
          show_quarterly = show_q,
          clean_di = clean,
          reinvest = reinv,
          plot = FALSE
        ),
        indicator_args_i
      )

      one_res <- NULL
      one_ret <- NULL
      err_msg <- NULL
      res_full <- NULL
      selected_plot <- NULL

      tryCatch(
        {
          res <- do.call(module_info$runner, call_args)
            if (!is.null(res$spec$risk)) {
              rV <- res$spec$risk$risk_pct
              rV_numeric <- suppressWarnings(as.numeric(rV))[1]
              if (is.null(psV)) {
                psV <- res$spec$risk$ps_type %||% switch(res$spec$risk$mode,
                  risk = "eldoc",
                  notional = "notional",
                  fixed = "contract",
                  res$spec$risk$mode
                )
              }
            }
          if (isTRUE(only_returns)) {
            res_full <- res$rets
            selected_plot <- tryCatch(extract_returns(res_full, rtyp, norm_risk_numeric = nr), error = function(e) NULL)
            if (is.null(selected_plot)) {
              msg <- sprintf("Could not extract returns type '%s' for %s.", rtyp, label)
              if (isTRUE(fail)) {
                stop(msg, call. = FALSE)
              }
              warning(sprintf("%s Filling with zeros.", msg), call. = FALSE)
              idx_warn <- index(res_full)
              selected_plot <- zero_returns_xts(idx_warn)
            }
            one_ret <- selected_plot
          } else {
            one_res <- res
            res_full <- res$rets
            if (!is.null(res$rets)) {
              selected_plot <- tryCatch(extract_returns(res$rets, rtyp, norm_risk_numeric = nr), error = function(e) NULL)
            }
          }
        },
        error = function(e) {
          err_msg <<- conditionMessage(e)
        }
      )

      if (!is.null(err_msg) || is.null(res_full)) {
        msg <- sprintf("Backtest failed for %s: %s.", label, if (is.null(err_msg)) "unknown error" else err_msg)
        if (isTRUE(fail)) {
          stop(msg, call. = FALSE)
        }
        warning(sprintf("%s Filling with zeros.", msg), call. = FALSE)
        idx <- fallback_index(data_symbol, base_sym = spec_ticker)
        nr_numeric <- first_finite_numeric(nr)
        res_full <- zero_full_returns_xts(idx, normalize_target = nr_numeric)
        selected_plot <- tryCatch(extract_returns(res_full, rtyp, norm_risk_numeric = nr_numeric), error = function(e) NULL)
        if (is.null(selected_plot)) {
          selected_plot <- zero_returns_xts(idx)
        }
        if (isTRUE(only_returns)) {
          one_ret <- selected_plot
        } else {
          one_res <- list(
            rets = res_full,
            stats = NULL,
            trades = NULL,
            rets_acct = NULL,
            mktdata = tryCatch(
              {
                obj <- get(data_symbol, envir = data_env)
                if (xts::is.xts(obj)) obj else NULL
              },
              error = function(e) NULL
            )
          )
          names(one_res) <- c("rets", "stats", "trades", "rets_acct", "mktdata")
        }
      }

      risk_scale_val <- if (!is.null(res_full)) attr(res_full, "risk_scale") else NA_real_
      risk_target_val <- if (!is.null(res_full)) attr(res_full, "risk_target") else NA_real_
      risk_original_val <- if (!is.null(res_full)) attr(res_full, "risk_original") else NA_real_

      base_scale_val <- suppressWarnings(as.numeric(risk_scale_val))[1]
      if (!is.finite(base_scale_val) || base_scale_val <= 0) base_scale_val <- NA_real_
      base_target_val <- suppressWarnings(as.numeric(risk_target_val))[1]
      if (!is.finite(base_target_val)) base_target_val <- NA_real_
      base_adjusted <- if (is.finite(base_scale_val) && is.finite(rV_numeric)) rV_numeric * base_scale_val else rV_numeric

      formatted_original_risk <- format_risk_token(rV)
      if (!length(formatted_original_risk) || is.na(formatted_original_risk) || !nzchar(formatted_original_risk)) {
        formatted_original_risk <- "NA"
      }
      formatted_adjusted_risk <- if (is.finite(base_scale_val) && is.finite(base_adjusted) && abs(base_adjusted - rV_numeric) > tol) format_risk_token(base_adjusted) else NA_character_

      output_label <- label
      ps_report_label <- .bt_normalize_ps_type(psV) %||% "ps"
      resolved_token <- sprintf("%s_%s", ps_report_label, formatted_original_risk)
      if (grepl("ps_meta", output_label, fixed = TRUE) && !identical(formatted_original_risk, "NA")) {
        output_label <- sub("ps_meta", resolved_token, output_label, fixed = TRUE)
      }
      old_token <- resolved_token
      if (!is.na(formatted_adjusted_risk)) {
        new_token <- sprintf("%s%s_aj%s", ps_report_label, formatted_original_risk, formatted_adjusted_risk)
        if (grepl(old_token, output_label, fixed = TRUE)) {
          output_label <- sub(old_token, new_token, output_label, fixed = TRUE)
        } else {
          output_label <- paste0(output_label, "_aj", formatted_adjusted_risk)
        }
      }

      if (!is.null(selected_plot)) {
        colnames(selected_plot) <- output_label
        plot_list[[length(plot_list) + 1]] <- selected_plot
      }

      if (!is.null(one_res)) attr(one_res, "fee_mode") <- feeV
      if (!is.null(one_ret)) attr(one_ret, "fee_mode") <- feeV

      label_sequence <- c(label_sequence, output_label)
      if (!is.null(res_full)) {
        all_returns_map[[output_label]] <- res_full
        attr(all_returns_map[[output_label]], "risk_scale") <- risk_scale_val
        attr(all_returns_map[[output_label]], "risk_target") <- risk_target_val
        attr(all_returns_map[[output_label]], "risk_original") <- risk_original_val
      }
      normalize_risk_map[[output_label]] <- risk_target_val
      returns_type_map[[output_label]] <- rtyp
      risk_scale_map[[output_label]] <- risk_scale_val

      if (isTRUE(only_returns)) {
        if (!is.null(one_ret)) {
          colnames(one_ret) <- output_label
          returns_list[[length(returns_list) + 1]] <- one_ret
        }
      } else if (!is.null(one_res)) {
        results_list[[output_label]] <- one_res
      }

      risk_report[[length(risk_report) + 1]] <- list(
        label = output_label,
        ps_label = psV,
        original = rV_numeric,
        base_adjusted = base_adjusted,
        final_adjusted = base_adjusted,
        base_scale = base_scale_val,
        portfolio_scale = NA_real_,
        base_target = base_target_val,
        portfolio_target = NA_real_,
        is_portfolio = FALSE
      )
      risk_report_map[[output_label]] <- length(risk_report)
    }
  }

  portfolio_alias_map <- list()
  external_returns_cache <- list()

  all_idx_values <- unlist(lapply(all_returns_map, function(x) if (is.null(x)) NULL else index(x)), use.names = FALSE)
  if (length(all_idx_values)) {
    default_start_date <- as.Date(min(all_idx_values))
    default_end_date <- as.Date(max(all_idx_values))
  } else {
    default_start_date <- Sys.Date() - 365
    default_end_date <- Sys.Date()
  }
  default_start_chr <- format(default_start_date, "%Y-%m-%d")
  default_end_chr <- format(default_end_date, "%Y-%m-%d")
  data_env <- .get_bt_data_env()

  resolve_alias_label <- function(token) {
    if (is.null(token) || !nzchar(token)) {
      return(NULL)
    }
    token_trim <- trimws(as.character(token)[1])
    if (!nzchar(token_trim)) {
      return(NULL)
    }
    keys <- names(portfolio_alias_map)
    if (length(keys)) {
      idx <- match(tolower(token_trim), tolower(keys))
      if (!is.na(idx)) {
        return(portfolio_alias_map[[keys[idx]]])
      }
    }
    map_keys <- names(all_returns_map)
    if (length(map_keys)) {
      idx <- match(tolower(token_trim), tolower(map_keys))
      if (!is.na(idx)) {
        return(map_keys[idx])
      }
    }
    NULL
  }

  build_return_matrix <- function(log_xts) {
    discrete <- xts::xts(exp(as.numeric(log_xts)) - 1, order.by = index(log_xts))
    colnames(discrete) <- "Discrete"
    out <- cbind(discrete, log_xts)
    attr(out, "risk_scale") <- NA_real_
    attr(out, "risk_target") <- NA_real_
    attr(out, "risk_original") <- NA_real_
    out
  }

  fetch_external_returns <- function(symbol) {
    if (is.null(symbol) || !nzchar(symbol)) {
      return(NULL)
    }
    symbol <- as.character(symbol)[1]
    if (!is.null(all_returns_map[[symbol]])) {
      return(all_returns_map[[symbol]])
    }
    if (!is.null(external_returns_cache[[symbol]])) {
      return(external_returns_cache[[symbol]])
    }

    clean_xts <- function(obj) {
      if (is.null(obj)) {
        return(NULL)
      }
      if (!xts::is.xts(obj)) {
        obj <- try(xts::as.xts(obj), silent = TRUE)
        if (inherits(obj, "try-error") || is.null(obj)) {
          return(NULL)
        }
      }
      obj
    }

    fallback_yahoo <- function(sym) {
      if (!requireNamespace("quantmod", quietly = TRUE)) {
        return(NULL)
      }
      tryCatch(
        {
          suppressWarnings(
            quantmod::getSymbols(sym,
              src = "yahoo",
              auto.assign = FALSE,
              from = as.Date(default_start_chr),
              to = as.Date(default_end_chr)
            )
          )
        },
        error = function(e) NULL
      )
    }

    data <- fallback_yahoo(symbol)
    data <- clean_xts(data)

    if (is.null(data)) {
      data <- try(
        .bt_fetch_finharvest_data(symbol,
          start_date = default_start_chr,
          end_date = default_end_chr
        ),
        silent = TRUE
      )
      if (inherits(data, "try-error")) data <- NULL
    }

    data <- clean_xts(data)

    data <- clean_xts(data)
    if (is.null(data) || NROW(data) < 2) {
      return(NULL)
    }

    if (!all(c("Log", "Discrete") %in% colnames(data))) {
      data <- .use_close_only(data)
      close_col <- NULL
      if ("Close" %in% colnames(data)) close_col <- data$Close
      if (is.null(close_col)) close_col <- try(Cl(data), silent = TRUE)
      if (inherits(close_col, "try-error") || is.null(close_col)) close_col <- data[, 1, drop = FALSE]
      close_col <- zoo::na.locf(close_col, na.rm = FALSE)
      if (NROW(close_col) < 2) {
        return(NULL)
      }
      log_ret <- diff(log(as.numeric(close_col)))
      log_xts <- xts::xts(log_ret, order.by = index(close_col)[-1])
      colnames(log_xts) <- "Log"
      combined <- build_return_matrix(log_xts)
    } else {
      discrete <- data[, "Discrete", drop = FALSE]
      log_xts <- data[, "Log", drop = FALSE]
      combined <- cbind(discrete, log_xts)
      attr(combined, "risk_scale") <- NA_real_
      attr(combined, "risk_target") <- NA_real_
      attr(combined, "risk_original") <- NA_real_
    }

    external_returns_cache[[symbol]] <<- combined
    combined
  }

  base_test_count <- length(label_sequence)
  portfolio_specs <- prepare_portfolio_specs(gen_portfolio)
  if (length(portfolio_specs) > 0) {
    weight_sets <- prepare_weight_sets(gen_portfolio_weights, length(portfolio_specs))
    portfolio_norms <- resolve_portfolio_norms(gen_portfolio_norm_risk, length(portfolio_specs))
    for (combo_idx in seq_along(portfolio_specs)) {
      spec <- portfolio_specs[[combo_idx]]

      numeric_indices <- spec$indices
      if (length(numeric_indices)) {
        numeric_indices <- numeric_indices[numeric_indices >= 1 & numeric_indices <= base_test_count]
        numeric_indices <- numeric_indices[!duplicated(numeric_indices)]
      }

      tokens <- spec$tokens
      component_labels <- character()
      component_returns <- list()

      if (length(numeric_indices)) {
        for (idx in numeric_indices) {
          if (idx > length(label_sequence)) next
          lbl <- label_sequence[idx]
          ret_obj <- all_returns_map[[lbl]]
          if (is.null(ret_obj)) next
          component_labels <- c(component_labels, lbl)
          component_returns[[length(component_returns) + 1]] <- ret_obj
        }
      }

      if (length(tokens)) {
        for (tok in tokens) {
          resolved_label <- resolve_alias_label(tok)
          ret_obj <- NULL
          if (!is.null(resolved_label)) {
            ret_obj <- all_returns_map[[resolved_label]]
            if (!is.null(ret_obj)) {
              component_labels <- c(component_labels, resolved_label)
              component_returns[[length(component_returns) + 1]] <- ret_obj
              next
            }
          }
          ret_obj <- fetch_external_returns(tok)
          if (is.null(ret_obj)) {
            warning(sprintf("Unable to resolve component '%s' for portfolio '%s'; skipping.", tok, spec$name))
            next
          }
          label_key <- as.character(tok)[1]
          all_returns_map[[label_key]] <- ret_obj
          normalize_risk_map[[label_key]] <- NA_real_
          component_labels <- c(component_labels, label_key)
          component_returns[[length(component_returns) + 1]] <- ret_obj
        }
      }

      if (!length(component_returns)) {
        warning(sprintf("Portfolio '%s' has no valid components; skipping.", spec$name))
        next
      }

      log_series <- lapply(seq_along(component_returns), function(k) {
        rets <- component_returns[[k]]
        if (!"Log" %in% colnames(rets)) {
          return(NULL)
        }
        series <- rets[, "Log", drop = FALSE]
        colnames(series) <- component_labels[k]
        series
      })
      if (any(vapply(log_series, is.null, logical(1)))) {
        warning(sprintf("Log returns not found for some components of portfolio '%s'; skipping.", spec$name))
        next
      }

      merged_logs <- tryCatch(
        {
          do.call(xts::merge.xts, c(log_series, list(all = TRUE)))
        },
        error = function(e) NULL
      )
      if (is.null(merged_logs)) next
      merged_matrix <- as.matrix(merged_logs)
      if (is.null(dim(merged_matrix))) next
      merged_matrix[is.na(merged_matrix)] <- 0

      weights_vec <- resolve_weights(weight_sets[[combo_idx]], length(component_returns))
      weights_vec <- rep_len(weights_vec, ncol(merged_matrix))
      combined_values <- as.numeric(merged_matrix %*% matrix(weights_vec, ncol = 1))
      combined_log <- xts::xts(combined_values, order.by = index(merged_logs))
      colnames(combined_log) <- "Log"
      combined <- build_return_matrix(combined_log)

      override <- portfolio_norms[[combo_idx]]
      combo_target <- if (!is.null(override)) suppressWarnings(as.numeric(override)[1]) else NA_real_
      combo_scale <- NA_real_
      combo_orig_vol <- NA_real_
      if (is.finite(combo_target)) {
        log_norm <- tryCatch(bt_normalize_risk(combined_log, risk = combo_target, type = "Log"), error = function(e) NULL)
        if (!is.null(log_norm)) {
          combo_scale <- attr(log_norm, "scale_factor")
          combo_orig_vol <- attr(log_norm, "original_vol")
          combined_log <- log_norm
          combined <- build_return_matrix(combined_log)
        }
      }

      attr(combined, "risk_scale") <- combo_scale
      attr(combined, "risk_target") <- combo_target
      attr(combined, "risk_original") <- if (is.finite(combo_orig_vol)) combo_orig_vol * 100 else NA_real_

      sanitize_label_component <- function(x) {
        x <- gsub("[^A-Za-z0-9]", "", x)
        if (!nzchar(x)) x <- "X"
        x
      }

      display_parts <- character()
      if (length(numeric_indices)) display_parts <- c(display_parts, as.character(numeric_indices))
      if (length(tokens)) display_parts <- c(display_parts, vapply(tokens, sanitize_label_component, character(1)))
      if (!length(display_parts)) display_parts <- sanitize_label_component(component_labels)

      raw_name <- spec$name
      group_label <- format_group_label(raw_name)
      combo_label <- if (is.null(group_label) || !nzchar(group_label)) {
        paste0("Portfolio_", paste(display_parts, collapse = "_"))
      } else {
        paste0(group_label, "_", paste(display_parts, collapse = "_"))
      }

      combo_rtype_candidates <- unique(unlist(returns_type_map[component_labels]))
      combo_rtype_candidates <- combo_rtype_candidates[!vapply(combo_rtype_candidates, is.null, logical(1))]
      combo_rtype_candidates <- combo_rtype_candidates[!is.na(combo_rtype_candidates)]
      combo_rtyp <- if (length(combo_rtype_candidates) == 1) combo_rtype_candidates[[1]] else default_combo_rtyp

      norm_arg <- if (is.finite(combo_target)) combo_target else NULL
      selected_portfolio <- tryCatch(extract_returns(combined, combo_rtyp, norm_risk_numeric = norm_arg), error = function(e) NULL)
      if (is.null(selected_portfolio)) {
        fallback_col <- if (identical(combo_rtyp, "Discrete")) "Discrete" else "Log"
        selected_portfolio <- combined[, fallback_col, drop = FALSE]
      }
      colnames(selected_portfolio) <- combo_label
      plot_list[[length(plot_list) + 1]] <- selected_portfolio

      label_sequence <- c(label_sequence, combo_label)
      all_returns_map[[combo_label]] <- combined
      normalize_risk_map[[combo_label]] <- combo_target
      returns_type_map[[combo_label]] <- combo_rtyp
      risk_scale_map[[combo_label]] <- combo_scale

      if (!is.null(raw_name) && nzchar(raw_name)) {
        portfolio_alias_map[[raw_name]] <- combo_label
      }
      if (!is.null(group_label) && nzchar(group_label)) {
        portfolio_alias_map[[group_label]] <- combo_label
      }

      orig_risk <- attr(combined, "risk_original")
      if (!is.finite(orig_risk)) orig_risk <- NA_real_
      final_adjusted <- if (is.finite(combo_scale) && is.finite(orig_risk)) orig_risk * combo_scale else if (is.finite(combo_target)) combo_target else orig_risk
      risk_report[[length(risk_report) + 1]] <- list(
        label = combo_label,
        ps_label = NA_character_,
        original = orig_risk,
        base_adjusted = orig_risk,
        final_adjusted = final_adjusted,
        base_scale = NA_real_,
        portfolio_scale = if (is.finite(combo_scale)) combo_scale else NA_real_,
        base_target = NA_real_,
        portfolio_target = combo_target,
        is_portfolio = TRUE
      )

      if (!is.null(override) && is.finite(combo_scale)) {
        for (member in unique(component_labels)) {
          idx <- risk_report_map[[member]]
          if (!is.null(idx)) {
            rec <- risk_report[[idx]]
            current_scale <- rec$portfolio_scale
            if (is.finite(current_scale)) {
              rec$portfolio_scale <- current_scale * combo_scale
            } else {
              rec$portfolio_scale <- combo_scale
            }
            rec$portfolio_target <- combo_target
            base_adj <- rec$base_adjusted
            if (!is.finite(base_adj)) base_adj <- rec$original
            if (is.finite(base_adj) && is.finite(rec$portfolio_scale)) {
              rec$final_adjusted <- base_adj * rec$portfolio_scale
            }
            risk_report[[idx]] <- rec
          }
        }
      }

      if (isTRUE(only_returns)) {
        if (!is.null(selected_portfolio)) returns_list[[length(returns_list) + 1]] <- selected_portfolio
      } else {
        results_list[[combo_label]] <- list(
          rets = combined,
          stats = NULL,
          trades = NULL,
          rets_acct = NULL,
          mktdata = NULL
        )
      }
    }
  }
  if (length(risk_report) > 0 && isTRUE(verbose)) {
    cat("\nRisk normalization impact (position sizing):\n")
    for (rec in risk_report) {
      descriptor <- if (isTRUE(rec$is_portfolio)) "portfolio risk" else "ps_value"
      orig_txt <- if (is.finite(rec$original)) format_risk_token(rec$original) else "NA"
      line <- sprintf(" - %s: %s %s", rec$label, descriptor, orig_txt)

      base_step <- !isTRUE(rec$is_portfolio) && is.finite(rec$base_scale) && is.finite(rec$base_adjusted) && abs(rec$base_adjusted - rec$original) > tol
      if (base_step) {
        base_txt <- format_risk_token(rec$base_adjusted)
        base_scale_txt <- sprintf("%.3fx", rec$base_scale)
        base_target_txt <- if (is.finite(rec$base_target)) paste0(format_risk_token(rec$base_target), "%") else NULL
        if (is.null(base_target_txt)) {
          line <- sprintf("%s -> %s (normalize_risk %s)", line, base_txt, base_scale_txt)
        } else {
          line <- sprintf("%s -> %s (normalize_risk %s, target %s)", line, base_txt, base_scale_txt, base_target_txt)
        }
      }

      final_step <- FALSE
      if (isTRUE(rec$is_portfolio)) {
        if (is.finite(rec$portfolio_scale) && is.finite(rec$final_adjusted) && (is.na(rec$original) || abs(rec$final_adjusted - rec$original) > tol)) {
          final_step <- TRUE
          final_txt <- format_risk_token(rec$final_adjusted)
          port_scale_txt <- sprintf("%.3fx", rec$portfolio_scale)
          port_target_txt <- if (is.finite(rec$portfolio_target)) paste0(format_risk_token(rec$portfolio_target), "%") else NULL
          if (is.null(port_target_txt)) {
            line <- sprintf("%s -> %s (portfolio %s)", line, final_txt, port_scale_txt)
          } else {
            line <- sprintf("%s -> %s (portfolio %s, target %s)", line, final_txt, port_scale_txt, port_target_txt)
          }
        }
      } else {
        if (is.finite(rec$portfolio_scale) && is.finite(rec$final_adjusted) && abs(rec$final_adjusted - rec$base_adjusted) > tol) {
          final_step <- TRUE
          final_txt <- format_risk_token(rec$final_adjusted)
          port_scale_txt <- sprintf("%.3fx", rec$portfolio_scale)
          port_target_txt <- if (is.finite(rec$portfolio_target)) paste0(format_risk_token(rec$portfolio_target), "%") else NULL
          if (is.null(port_target_txt)) {
            line <- sprintf("%s -> %s (portfolio %s)", line, final_txt, port_scale_txt)
          } else {
            line <- sprintf("%s -> %s (portfolio %s, target %s)", line, final_txt, port_scale_txt, port_target_txt)
          }
        }
      }

      if (!base_step && !final_step) {
        note <- if (isTRUE(rec$is_portfolio)) " (no normalization applied)" else " (no normalization applied)"
        line <- paste0(line, note)
      }

      cat(line, "\n")
    }
  }

  if (isTRUE(plot_mult) && length(plot_list) > 0) {
    tryCatch(
      {
            do.call(.bt_tplot, plot_list)
      },
      error = function(e) warning(sprintf("plot_mult failed: %s", conditionMessage(e)))
    )
  }

  if (isTRUE(only_returns)) {
    if (length(returns_list) == 0) {
      return(xts::xts(matrix(numeric(0), ncol = 0), order.by = as.POSIXct(character(0))))
    }
    out <- tryCatch(
      {
        do.call(xts::merge.xts, c(returns_list, list(all = TRUE)))
      },
      error = function(e) Reduce(function(a, b) merge(a, b, all = TRUE), returns_list)
    )
    return(out)
  } else {
    return(results_list)
  }
}
