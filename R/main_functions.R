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

.bt_fetch_ticker_data <- function(symbol, data_env, start_date, end_date, clean_di = TRUE) {
  if (exists(symbol, envir = data_env, inherits = FALSE)) {
    ticker_data <- get(symbol, envir = data_env)
  } else if (exists(symbol, envir = .GlobalEnv, inherits = FALSE)) {
    ticker_data <- get(symbol, envir = .GlobalEnv)
  } else {
    ticker_data <- sm_get_data(symbol,
      start_date = start_date,
      end_date = end_date,
      future_history = FALSE,
      single_xts = TRUE,
      local_data = FALSE
    )
  }

  if (isTRUE(clean_di)) {
    ticker_data <- .bt_clean_di(ticker_data, "TickSize", value = 0.001)
    ticker_data <- .bt_clean_di_tick(ticker_data, "TickValue", value_over = -5)
  }

  ticker_data
}

.bt_indicator_spec <- function(type) {
  type <- tolower(type)
  switch(type,
    eldoc = .bt_module_eldoc(),
    ema = .bt_module_ma("ema"),
    sma = .bt_module_ma("sma"),
    stop(sprintf("Unsupported backtest type '%s'", type), call. = FALSE)
  )
}

.bt_module_eldoc <- function() {
  defaults <- list(up = 40L, down = 40L)

  resolve_args <- function(args) {
    list(
      up = as.integer(.bt_first_non_null(args$up, defaults$up)),
      down = as.integer(.bt_first_non_null(args$down, defaults$down))
    )
  }

  build_label <- function(context, fee_mode_suffix = "") {
    up_str <- as.character(context$indicator_args$up)
    down_str <- as.character(context$indicator_args$down)

    if (!isTRUE(context$hide_details)) {
      paste0(
        context$base_ticker, "_eD_", up_str, "_", down_str, "_",
        context$ps, "_", context$ps_risk_value, fee_mode_suffix
      )
    } else {
      paste0(context$base_ticker, "_eD_nodets", fee_mode_suffix)
    }
  }

  configure_strategy <- function(strategy, context) {
    up <- context$indicator_args$up
    down <- context$indicator_args$down
    hi_col <- context$columns$high
    lo_col <- context$columns$low

    strategy <- add.indicator(
      strategy, "eldoc",
      arguments = list(
        ticker = quote(mktdata),
        x = up,
        y = down,
        hi.col = hi_col,
        lo.col = lo_col,
        type = "data"
      ),
      label = "el"
    )

    strategy <- add.signal(
      strategy,
      name = "sigCrossover",
      arguments = list(
        data = quote(mktdata),
        columns = c(context$columns$high, "X.el"),
        relationship = "gte"
      ),
      label = "Entry"
    )

    strategy <- add.signal(
      strategy,
      name = "sigCrossover",
      arguments = list(
        data = quote(mktdata),
        columns = c(context$columns$low, "Y.el"),
        relationship = "lte"
      ),
      label = "Exit"
    )

    list(
      strategy = strategy,
      signals = list(
        long_entry = "Entry",
        long_exit = "Exit",
        short_entry = "Exit",
        short_exit = "Entry"
      ),
      indicator_label = paste0(up, "/", down)
    )
  }

  augment_stats <- function(stats, context) {
    stats$elDoc <- paste0(context$indicator_args$up, "/", context$indicator_args$down)
    stats
  }

  list(
    type = "eldoc",
    names = list(portfolio = "elDoc", account = "elDoc", strategy = "elDoc"),
    defaults = defaults,
    resolve_indicator_args = resolve_args,
    build_bt_ticker = build_label,
    configure_strategy = configure_strategy,
    augment_stats = augment_stats,
    indicator_column_name = "Donchian",
    batch_suffix = function(indicator_args) {
      paste0(indicator_args$up, "_", indicator_args$down)
    }
  )
}

.bt_module_ma <- function(kind = c("ema", "sma")) {
  kind <- match.arg(kind)
  indicator_fun <- toupper(kind)
  defaults <- list(fast = 20L, slow = 50L)

  resolve_args <- function(args) {
    list(
      fast = as.integer(.bt_first_non_null(args$fast, args$mup, args$up, defaults$fast)),
      slow = as.integer(.bt_first_non_null(args$slow, args$mdown, args$down, defaults$slow))
    )
  }

  build_label <- function(context, fee_mode_suffix = "") {
    args <- context$indicator_args
    base <- paste0(
      context$base_ticker, "_", kind, "_", args$fast, "_", args$slow,
      "_", context$ps, "_", context$ps_risk_value
    )
    if (isTRUE(context$hide_details)) {
      base <- paste0(context$base_ticker, "_", kind, "_nodets")
    }
    paste0(base, fee_mode_suffix)
  }

  configure_strategy <- function(strategy, context) {
    args <- context$indicator_args
    price_expr <- quote(Cl(mktdata))
    fast_lab <- "fast"
    slow_lab <- "slow"

    strategy <- add.indicator(
      strategy, indicator_fun,
      arguments = list(x = price_expr, n = args$fast),
      label = fast_lab
    )

    strategy <- add.indicator(
      strategy, indicator_fun,
      arguments = list(x = price_expr, n = args$slow),
      label = slow_lab
    )

    fast_col <- paste0(indicator_fun, ".", fast_lab)
    slow_col <- paste0(indicator_fun, ".", slow_lab)

    strategy <- add.signal(
      strategy,
      name = "sigCrossover",
      arguments = list(columns = c(fast_col, slow_col), relationship = "gt"),
      label = "LongEntry"
    )

    strategy <- add.signal(
      strategy,
      name = "sigCrossover",
      arguments = list(columns = c(fast_col, slow_col), relationship = "lt"),
      label = "LongExit"
    )

    strategy <- add.signal(
      strategy,
      name = "sigCrossover",
      arguments = list(columns = c(fast_col, slow_col), relationship = "lt"),
      label = "ShortEntry"
    )

    strategy <- add.signal(
      strategy,
      name = "sigCrossover",
      arguments = list(columns = c(fast_col, slow_col), relationship = "gt"),
      label = "ShortExit"
    )

    label_prefix <- toupper(kind)

    list(
      strategy = strategy,
      signals = list(
        long_entry = "LongEntry",
        long_exit = "LongExit",
        short_entry = "ShortEntry",
        short_exit = "ShortExit"
      ),
      indicator_label = paste0(label_prefix, " ", args$fast, "/", args$slow)
    )
  }

  augment_stats <- function(stats, context) {
    label_prefix <- toupper(kind)
    stats[[label_prefix]] <- paste0(context$indicator_args$fast, "/", context$indicator_args$slow)
    stats
  }

  friendly <- toupper(kind)

  list(
    type = kind,
    names = list(
      portfolio = friendly,
      account = friendly,
      strategy = friendly
    ),
    defaults = defaults,
    resolve_indicator_args = resolve_args,
    build_bt_ticker = build_label,
    configure_strategy = configure_strategy,
    augment_stats = augment_stats,
    indicator_column_name = friendly,
    batch_suffix = function(indicator_args) {
      paste0(indicator_args$fast, "_", indicator_args$slow)
    }
  )
}

.bt_run_module <- function(
  type,
  ticker,
  indicator_args = list(),
  ps_risk_value = 2,
  ps = "pct",
  fee = "normal",
  start_date = "1900-01-01",
  end_date = Sys.Date(),
  long = TRUE,
  short = TRUE,
  invert_signals = FALSE,
  normalize_risk = NULL,
  geometric = TRUE,
  verbose = FALSE,
  only_returns = FALSE,
  hide_details = FALSE,
  stop_before_maturity = NULL,
  clean_di = TRUE,
  plot = FALSE
) {
  module <- .bt_indicator_spec(type)

  indicator_args <- module$resolve_indicator_args(indicator_args)

  fee_mode <- normalize_fee_mode(fee)

  if (exists(".blotter")) rm(list = ls(envir = .blotter), envir = .blotter)
  if (exists(".strategy")) rm(list = ls(envir = .strategy), envir = .strategy)

  #  if (exists("data_env")) rm(list = ls(envir = data_env), envir = data_env)

  if (!exists(".strategy")) .strategy <<- new.env()
  if (!exists(".blotter")) .blotter <<- new.env()
  # if (!exists("data_env")) data_env <<- new.env()

  data_env <- .get_bt_data_env()

  ticker_data <- .bt_fetch_ticker_data(ticker, data_env, start_date, end_date, clean_di = clean_di)
  ticker_data <- .use_close_only(ticker_data)
  attr(ticker_data, "bt_original_symbol") <- ticker
  attr(ticker_data, "bt_root_symbol") <- sub("_.*$", "", ticker)
  assign(ticker, ticker_data, envir = data_env)
  .register_future_from_data(ticker, ticker_data)
  original_ticker <- ticker

  if (!isTRUE(long) && !isTRUE(short)) {
    stop("You need to enable at least one side (long and/or short).", call. = FALSE)
  }

  context <- list(
    base_ticker = ticker,
    indicator_args = indicator_args,
    ps = ps,
    ps_risk_value = ps_risk_value,
    long = long,
    short = short,
    invert_signals = invert_signals,
    hide_details = hide_details,
    fee_mode = fee_mode,
    module = module
  )

  fee_suffix <- if (identical(fee_mode, "nofee")) "_nofees" else ""
  bt_ticker <- module$build_bt_ticker(context, fee_mode_suffix = fee_suffix)

  base_instrument <- tryCatch(
    {
      inst <- getInstrument(context$base_ticker, silent = TRUE)
      if (FinancialInstrument::is.instrument(inst)) inst else NULL
    },
    error = function(e) NULL
  )

  .register_future_from_data(bt_ticker, ticker_data)

  if (!is.null(base_instrument)) {
    base_ids <- base_instrument$identifiers
    if (is.environment(base_ids)) base_ids <- as.list(base_ids)
    if (!is.list(base_ids)) base_ids <- list()

    new_instrument <- tryCatch(getInstrument(bt_ticker, silent = TRUE), error = function(e) NULL)
    if (FinancialInstrument::is.instrument(new_instrument)) {
      new_ids <- new_instrument$identifiers
      if (is.environment(new_ids)) new_ids <- as.list(new_ids)
      if (!is.list(new_ids)) new_ids <- list()
      merged_ids <- utils::modifyList(base_ids, new_ids)
      try(instrument_attr(bt_ticker, "identifiers", merged_ids), silent = TRUE)

      if (is.null(new_instrument$multiplier) && !is.null(base_instrument$multiplier)) {
        try(instrument_attr(bt_ticker, "multiplier", base_instrument$multiplier), silent = TRUE)
      }
      if (is.null(new_instrument$tick_size) && !is.null(base_instrument$tick_size)) {
        try(instrument_attr(bt_ticker, "tick_size", base_instrument$tick_size), silent = TRUE)
      }
    }
  }

  ticker <- bt_ticker
  context$ticker <- ticker

  verbose_flag <- isTRUE(verbose)
  initEq <- 10000000
  path.dependence <- TRUE

  portfolio.st <- module$names$portfolio
  account.st <- module$names$account

  initPortf(portfolio.st, symbols = ticker)
  initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
  initOrders(portfolio = portfolio.st)
  my_strategy <- strategy(module$names$strategy)

  tradeSize <- 9999999
  bcontracts <- 1
  scontracts <- -1

  isDI <- startsWith(ticker, "DI1") |
    (!is.null(attr(ticker_data, "subcategoria")) &&
      grepl("juros", attr(ticker_data, "subcategoria"), ignore.case = TRUE))
  .dbg("Is it DI?", isDI)

  PositionSizing <- if (isDI) {
    function(data, timestamp, orderqty, ordertype, orderside,
             portfolio, symbol, tradeSize, maxSize, ...) {
      .psEquityPercentageDonchian_DI(data, timestamp, orderqty, ordertype, orderside,
        portfolio, symbol, tradeSize, maxSize,
        risk = ps_risk_value, ...
      )
    }
  } else {
    function(data, timestamp, orderqty, ordertype, orderside,
             portfolio, symbol, tradeSize, maxSize, ...) {
      .psEquityPercentageDonchian(data, timestamp, orderqty, ordertype, orderside,
        portfolio, symbol, tradeSize, maxSize,
        risk = ps_risk_value, ...
      )
    }
  }

  OrderType <- "market"
  LongEnabled <- isTRUE(long)
  ShortEnabled <- isTRUE(short)

  HighCol <- "High"
  LowCol <- "Low"
  if ("PU_o" %in% colnames(ticker_data)) Preference <- "PU_o" else Preference <- "Open"

  assign(bt_ticker, ticker_data, envir = data_env)
  assign(bt_ticker, ticker_data, envir = .GlobalEnv)
  ReplaceBuy <- FALSE
  ReplaceSell <- FALSE
  ReplaceShort <- FALSE
  ReplaceCover <- FALSE

  context$ticker_data <- ticker_data
  context$data_env <- data_env
  context$columns <- list(high = HighCol, low = LowCol, close = "Close")
  context$preference <- Preference

  TxnFeesVal <- if (fee_mode == "nofee") 0 else "calculate_fees"

  module_setup <- module$configure_strategy(my_strategy, context)
  my_strategy <- module_setup$strategy

  coisa <- applyIndicators(strategy = my_strategy, mktdata = get(ticker, envir = data_env))
  coisa <- applySignals(strategy = my_strategy, mktdata = coisa)

  signals <- module_setup$signals
  signal_or <- function(name, fallback) {
    val <- signals[[name]]
    if (is.null(val) || !nzchar(val)) fallback else val
  }

  long_entry_col <- signal_or("long_entry", "Entry")
  long_exit_col <- signal_or("long_exit", "Exit")
  short_entry_col <- signal_or("short_entry", long_exit_col)
  short_exit_col <- signal_or("short_exit", long_entry_col)

  n.ent <- sum(coisa[, long_entry_col] == 1, na.rm = TRUE)
  n.saida <- sum(coisa[, long_exit_col] == 1, na.rm = TRUE)
  .dbg("Signals detected - Entry:", n.ent, " Exit:", n.saida)

  my_strategy <- add.rule(
    strategy = my_strategy,
    name = "ruleSignal",
    arguments = list(
      sigcol = long_entry_col,
      sigval = TRUE,
      datax = mktdata,
      initEq = initEq,
      orderqty = tradeSize,
      portfolio = portfolio.st,
      ordertype = OrderType,
      orderside = if (!isDI) "long" else "short",
      osFUN = PositionSizing,
      tradeSize = tradeSize,
      buyorderqty = bcontracts,
      sellorderqty = scontracts,
      maxSize = 9999999,
      prefer = Preference,
      replace = ReplaceBuy,
      TxnFees = TxnFeesVal
    ),
    type = if (invert_signals) "exit" else "enter",
    label = if (invert_signals) "exitLong" else "enterLong",
    storefun = TRUE,
    path.dep = path.dependence,
    enabled = LongEnabled
  )

  my_strategy <- add.rule(
    strategy = my_strategy,
    name = "ruleSignal",
    arguments = list(
      sigcol = long_exit_col,
      sigval = TRUE,
      orderqty = "all",
      ordertype = OrderType,
      orderside = if (!isDI) "long" else "short",
      prefer = Preference,
      replace = ReplaceSell,
      TxnFees = TxnFeesVal
    ),
    type = if (invert_signals) "enter" else "exit",
    label = if (invert_signals) "enterLong" else "exitLong",
    storefun = TRUE,
    path.dep = path.dependence,
    enabled = LongEnabled
  )

  my_strategy <- add.rule(
    strategy = my_strategy,
    name = "ruleSignal",
    arguments = list(
      sigcol = short_entry_col,
      sigval = TRUE,
      datax = mktdata,
      initEq = initEq,
      orderqty = tradeSize,
      portfolio = portfolio.st,
      ordertype = OrderType,
      orderside = if (isDI) "long" else "short",
      osFUN = PositionSizing,
      tradeSize = -tradeSize,
      buyorderqty = bcontracts,
      sellorderqty = scontracts,
      maxSize = -9999999,
      prefer = Preference,
      replace = ReplaceShort,
      TxnFees = TxnFeesVal
    ),
    type = if (invert_signals) "exit" else "enter",
    label = if (invert_signals) "exitShort" else "enterShort",
    storefun = TRUE,
    path.dep = path.dependence,
    enabled = ShortEnabled
  )

  my_strategy <- add.rule(
    strategy = my_strategy,
    name = "ruleSignal",
    arguments = list(
      sigcol = short_exit_col,
      sigval = TRUE,
      orderqty = "all",
      ordertype = OrderType,
      orderside = if (isDI) "long" else "short",
      prefer = Preference,
      replace = ReplaceCover,
      TxnFees = TxnFeesVal
    ),
    type = if (invert_signals) "enter" else "exit",
    label = if (invert_signals) "enterShort" else "exitShort",
    storefun = TRUE,
    path.dep = path.dependence,
    enabled = ShortEnabled
  )

  start_t <- Sys.time()
  getInstrument(ticker)
  applyStrategy(strategy = my_strategy, portfolios = portfolio.st, verbose = FALSE, initEq = initEq, mdenv = data_env)

  tx <- getTxns(portfolio.st, ticker)
  .dbg("generated orders:", nrow(tx))
  if (nrow(tx) == 0) {
    warning("No generated order. Check column passed to 'prefer' and if the instrument has a multiplier/tick_size defined.")
    return(invisible(NULL))
  }
  updatePortf(Portfolio = portfolio.st, prefer = Preference)
  updateAcct(name = account.st)
  updateEndEq(Account = account.st)

  mktdata <- tryCatch(get(ticker, envir = data_env), error = function(e) NULL)

  port <- getPortfolio(portfolio.st)
  book <- getOrderBook(portfolio.st)
  stats <- tradeStats(portfolio.st)
  ptstats <- perTradeStats(portfolio.st)
  ptrets <- PortfReturns(portfolio.st)
  acrets <- AcctReturns(portfolio.st)
  txns <- getTxns(portfolio.st, ticker)
  Fee.n.Slip <- sum(txns$Txn.Fees)
  stats$Fee.n.Slip <- Fee.n.Slip

  cat(paste0(.dbg("Results for ", ticker, " - ", toupper(type), "\n\n")))
  if (verbose_flag) {
    print(stats)
    cat("\n")
    print(txns)
    cat("\n")
  }
  tab <- .table_monthly_returns(ptrets, return_data = TRUE, geometric = geometric)
  print(tab)
  tab_rs <- .table_monthly_profit(port)
  print(tab_rs)

  index(ptrets) <- .convert_posixct(ptrets)
  index(txns) <- .convert_posixct(txns)
  index(acrets) <- .convert_posixct(acrets)

  ptrets <- .print_returns(ptrets, normalize_risk, geometric = geometric)

  stop_t <- Sys.time()
  cat(paste("\nRuntime:", stop_t - start_t))
  cat("\n----------------------------------------\n\n")

  if (only_returns) {
    attr(ptrets, "backtest") <- TRUE
    attr(ptrets, "local") <- TRUE
    attr(ptrets, "fee_mode") <- fee_mode
    return(ptrets)
  }

  stats$PosSiz <- ps
  stats$Multiplier <- ticker_data$multiplier
  stats$TickSize <- ticker_data$tick_size
  if (fee_mode == "nofee") {
    stats$Slippage <- 0
    stats$Fees <- 0
  } else {
    stats$Slippage <- ticker_data$identifiers$slippage
    stats$Fees <- ticker_data$identifiers$fees
  }
  stats$FeeMode <- fee_mode

  if (!is.null(module$augment_stats)) {
    stats <- module$augment_stats(stats, context)
  }
  if (!is.null(module_setup$indicator_label)) {
    stats$Indicator <- module_setup$indicator_label
  }

  attr(ptrets, "backtest") <- TRUE
  attr(ptrets, "local") <- TRUE
  attr(ptrets, "fee_mode") <- fee_mode

  elements_names <- c("rets", "stats", "trades", "rets_acct", "mktdata")

  results <- setNames(
    list(ptrets, stats, txns, acrets, mktdata),
    elements_names
  )

  if (plot) tplot(portfolio.st, original_ticker)

  results
}

#' Run an ElDoc (Donchian) backtest with quantstrat
#'
#' Configures a simple Donchian breakout system using `quantstrat` where entries
#' occur on price crossing the upper/lower channel and exits occur on the
#' opposite channel signal. Position sizing can be percentage-of-equity (Donchian)
#' or DI-specific sizing, detected by the symbol prefix.
#'
#' Data is fetched from `rSenhorMercadoAPI::sm_get_data()` if the symbol is not
#' found in the global environment; otherwise the preloaded object is used.
#'
#' The function builds the strategy, runs the backtest, prints key summaries, and
#' returns a list of results.
#'
#' @param ticker Character symbol or name of an object in the global environment
#'   with OHLC data. If not found, data is fetched via `sm_get_data()`.
#' @param up Integer Donchian window for the upper channel (default 40).
#' @param down Integer Donchian window for the lower channel (default 40).
#' @param ps_risk_value Numeric risk percentage (e.g., 2 for 2%) used by the
#'   position sizing function.
#' @param ps Character label for position sizing mode (informational).
#' @param fee Either `"normal"` (use instrument fees/slippage if available) or
#'   `"nofee"` to disable fees.
#' @param start_date Character or Date, start date when fetching data.
#' @param end_date Date, end date when fetching data (default `Sys.Date()`).
#' @param long Logical, enable long entries.
#' @param short Logical, enable short entries.
#' @param invert_signals Logical, invert entry/exit mapping (debug/testing).
#' @param normalize_risk Optional numeric target risk used to append
#'   normalized return columns to the output.
#' @param geometric Logical, use geometric returns when reporting performance.
#' @param verbose Logical, print detailed summaries to the console.
#' @param only_returns Logical; if `TRUE`, returns only the portfolio returns `xts`.
#' @param hide_details Logical; if `TRUE`, simplifies internal object names.
#' @param plot Logical; if `TRUE`, plots the portfolio using `rTradingPlots::tplot`.
#'
#' @return If `only_returns = TRUE`, returns an `xts` with discrete and log
#'   returns. Otherwise, returns a named list with elements:
#'   - `rets`: portfolio returns `xts`
#'   - `stats`: `tradeStats` output with additional metadata
#'   - `trades`: transactions `xts`
#'   - `rets_acct`: account-level returns `xts`
#'   - `mktdata`: market data with indicator columns
#'
#' @details
#' If the symbol starts with `"DI1"`, DI-specific position sizing is used,
#' otherwise a Donchian risk-based sizing is applied. Instrument metadata
#' (multiplier, tick size, maturity) is propagated when available.
#' @export
bt_eldoc <- function(ticker, up = 40, down = 40, ps_risk_value = 2, ps = "pct", fee = "normal", start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, only_returns = FALSE, hide_details = FALSE, stop_before_maturity = NULL, clean_di = TRUE, plot = FALSE) {
  .bt_run_module(
    type = "eldoc",
    ticker = ticker,
    indicator_args = list(up = up, down = down),
    ps_risk_value = ps_risk_value,
    ps = ps,
    fee = fee,
    start_date = start_date,
    end_date = end_date,
    long = long,
    short = short,
    invert_signals = invert_signals,
    normalize_risk = normalize_risk,
    geometric = geometric,
    verbose = verbose,
    only_returns = only_returns,
    hide_details = hide_details,
    stop_before_maturity = stop_before_maturity,
    clean_di = clean_di,
    plot = plot
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
#' @internal
bt_ema <- function(ticker, fast = 20, slow = 50, ps_risk_value = 2, ps = "pct", fee = "normal", start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, only_returns = FALSE, hide_details = FALSE, stop_before_maturity = NULL, clean_di = TRUE, plot = FALSE) {
  .bt_run_module(
    type = "ema",
    ticker = ticker,
    indicator_args = list(fast = fast, slow = slow),
    ps_risk_value = ps_risk_value,
    ps = ps,
    fee = fee,
    start_date = start_date,
    end_date = end_date,
    long = long,
    short = short,
    invert_signals = invert_signals,
    normalize_risk = normalize_risk,
    geometric = geometric,
    verbose = verbose,
    only_returns = only_returns,
    hide_details = hide_details,
    stop_before_maturity = stop_before_maturity,
    clean_di = clean_di,
    plot = plot
  )
}

#' Run an SMA crossover backtest
#'
#' Identical to [`bt_ema()`] but uses simple moving averages for its crossover
#' signals. This provides a lightweight wrapper that shares the same
#' infrastructure as other backtests while swapping only the indicator module.
#'
#' @inheritParams bt_ema
#' @export
bt_sma <- function(ticker, fast = 20, slow = 50, ps_risk_value = 2, ps = "pct", fee = "normal", start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, only_returns = FALSE, hide_details = FALSE, stop_before_maturity = NULL, clean_di = TRUE, plot = FALSE) {
  .bt_run_module(
    type = "sma",
    ticker = ticker,
    indicator_args = list(fast = fast, slow = slow),
    ps_risk_value = ps_risk_value,
    ps = ps,
    fee = fee,
    start_date = start_date,
    end_date = end_date,
    long = long,
    short = short,
    invert_signals = invert_signals,
    normalize_risk = normalize_risk,
    geometric = geometric,
    verbose = verbose,
    only_returns = only_returns,
    hide_details = hide_details,
    stop_before_maturity = stop_before_maturity,
    clean_di = clean_di,
    plot = plot
  )
}

#' Normalize a return series to a target annualized risk level
#'
#' Searches for "Log" or "Discrete" return columns inside an `xts` object (or a
#' nested structure) and rescales them so the realized annualized volatility
#' matches the requested percentage. The output is returned in the same style
#' (`"Log"` or `"Discrete"`) requested via `type`.
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

  periods_per_year <- function(x) {
    # Robust estimator of periods per year.
    # - Daily: choose 252 if mostly weekdays, else 365.25
    # - Weekly, Monthly, Quarterly, Yearly: fixed
    # - Other/intraday/irregular: use median spacing over Gregorian year seconds
    if (!xts::is.xts(x)) stop("periods_per_year requires an xts object.")
    idx <- index(x)
    if (length(idx) < 2) {
      return(NA_real_)
    }
    p <- tryCatch(xts::periodicity(x)$scale, error = function(e) NA_character_)
    if (!is.na(p)) {
      p <- tolower(p)
      if (p == "daily") {
        # detect crypto/all-calendar vs business-day series
        w <- weekdays(as.Date(idx))
        frac_weekend <- mean(w %in% c("Saturday", "Sunday"))
        return(if (is.na(frac_weekend) || frac_weekend > 0.05) 365.25 else 252)
      } else if (p == "weekly") {
        return(52)
      } else if (p == "monthly") {
        return(12)
      } else if (p == "quarterly") {
        return(4)
      } else if (p == "yearly") {
        return(1)
      }
      # else fall through
    }
    # fallback using median spacing
    dt <- stats::median(diff(as.numeric(idx)))
    if (is.na(dt) || dt <= 0) {
      return(NA_real_)
    }
    if (inherits(idx, "Date")) {
      secs_per_period <- dt * 86400
    } else {
      secs_per_period <- dt
    }
    as.numeric((365.25 * 24 * 3600) / secs_per_period)
  }

  annualized_vol <- function(r, ppy) {
    rnum <- as.numeric(r)
    rnum <- rnum[is.finite(rnum)]
    if (length(rnum) < 2) {
      return(NA_real_)
    }
    stats::sd(rnum) * sqrt(ppy)
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

  # Determine annualization factor
  ppy <- periods_per_year(r_log)
  if (!is.finite(ppy) || ppy <= 0) {
    stop("Could not determine periods-per-year (annualization factor).")
  }

  # Compute realized annualized vol on log returns
  vol_ann <- annualized_vol(r_log, ppy)
  if (!is.finite(vol_ann) || vol_ann <= 0) {
    stop("Realized volatility is zero or undefined; cannot normalize risk.")
  }

  target_vol_ann <- risk / 100 # convert percent to decimal
  scale_factor <- as.numeric(target_vol_ann / vol_ann)

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

#' Run multiple backtests in batch
#'
#' Vectorises the single-ticker backtest wrappers (`bt_eldoc()`, `bt_ema()`,
#' `bt_sma()`) across symbols, timeframes, and parameter grids. Indicator-specific
#' parameters can be supplied through `mup`/`mdown` for Donchian runs or via
#' `...` when using other modules.
#'
#' @param type Character scalar selecting the indicator module. One of
#'   `"eldoc"` (Donchian), `"ema"`, or `"sma"`.
#' @param tickers Character vector of base symbols to backtest, or a (named)
#'   list where each element describes overrides for a specific ticker (e.g.
#'   `list(ES = list(timeframes = c("30M", "1H"), mup = c(30, 40)))`). When
#'   omitted you may pass the specification list as the first argument (e.g.
#'   `bt_batch(list(MySpec = list(ticker = "ES", type = "eldoc", timeframes = "1H"))))`),
#'   allowing each specification to provide its own `ticker`, `type`, and
#'   parameter grid.
#' @param timeframes Character vector of timeframe suffixes appended to each
#'   ticker (e.g. `"4H"`).
#' @param mup,mdown Numeric vectors used as Donchian window lengths and also as
#'   fallbacks for moving-average speeds.
#' @param mps_risk_value Numeric risk percentage passed to the position-sizing
#'   function.
#' @param mps Character string specifying the position-sizing method.
#' @param fee Fee handling mode per test (`"normal"` or `"nofee"`).
#' @param start_date,end_date Optional boundaries for fetched data.
#' @param long,short Logical flags enabling long and/or short trades.
#' @param invert_signals Logical flag to invert generated signals.
#' @param normalize_risk Optional numeric target risk for return normalisation.
#' @param geometric Logical indicating geometric aggregation for reports.
#' @param verbose Logical or logical vector for emitting progress messages.
#' @param only_returns Logical; if `TRUE`, returns an `xts` matrix of returns
#'   instead of the full backtest objects.
#' @param hide_details Logical flag propagated to the underlying backtest.
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
  type = c("eldoc", "ema", "sma"),
  tickers,
  timeframes = "1D",
  mup = 40,
  mdown = 40,
  mps_risk_value = 2,
  mps = "pct",
  fee = "normal",
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
  indicator_vectors <- c(list(mup = mup, mdown = mdown), extra_args)

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
        stop(sprintf("Unsupported backtest type '%s'", type_name), call. = FALSE)
      )
      module_obj <- .bt_indicator_spec(canonical)
      runner_sym <- switch(canonical,
        eldoc = as.name("bt_eldoc"),
        ema = as.name("bt_ema"),
        sma = as.name("bt_sma")
      )
      assign(type_token, list(module = module_obj, runner = runner_sym, type = canonical), envir = module_cache)
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

  build_label <- function(tk, tf, type_name, indicator_suffix, ps, riskv, L, S, fee_mode, inv, geom, nrisk, rtype) {
    suffix <- if (identical(fee_mode, "nofee")) "_nofees" else ""
    parts <- c(
      sprintf("%s_%s%s", tk, tf, suffix),
      sprintf("%s_%s", type_name, indicator_suffix),
      sprintf("%s_%s", ps, riskv),
      if (!isTRUE(L)) "nolong" else "long",
      if (!isTRUE(S)) "noshort" else "short",
      if (isTRUE(inv)) "inv" else NULL,
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
    tsz <- .sanitize_scalar_numeric(attr(dt, "fut_tick_size"), default = 0.01)
    mult <- .sanitize_scalar_numeric(attr(dt, "fut_multiplier"), default = 1)
    mat <- attr(dt, "maturity")
    cur <- attr(dt, "currency")
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

  with_safe_future <- function(expr_call) {
    safe_future <- function(primary_id, tick_size = NULL, multiplier = NULL, maturity = NULL, currency = "USD", ...) {
      if (!requireNamespace("FinancialInstrument", quietly = TRUE)) {
        stop("Package 'FinancialInstrument' is required for instrument definition.")
      }
      tsz <- .sanitize_scalar_numeric(tick_size, default = 0.01)
      mult <- .sanitize_scalar_numeric(multiplier, default = 1)
      cur <- if (is.null(currency)) "USD" else as.character(currency)[1]
      suppressWarnings(
        FinancialInstrument::future(
          primary_id = primary_id,
          tick_size = tsz,
          multiplier = mult,
          maturity = maturity,
          currency = cur, ...
        )
      )
    }
    exec_env <- new.env(parent = .GlobalEnv)
    assign("future", safe_future, envir = exec_env)
    eval(expr_call, envir = exec_env)
  }

  defaults <- list(
    timeframes = "1D",
    mps_risk_value = 2,
    mps = "pct",
    fee = "normal",
    long = TRUE,
    short = TRUE,
    invert_signals = FALSE,
    normalize_risk = NULL,
    geometric = geometric,
    verbose = FALSE,
    hide_details = FALSE,
    returns_type = "Log"
  )

  default_combo_rtyp <- normalize_rtype(.bt_value_at(returns_type, 1, defaults$returns_type))

  base_param_sources <- list(
    timeframes = timeframes,
    mps_risk_value = mps_risk_value,
    mps = mps,
    fee = fee,
    long = long,
    short = short,
    invert_signals = invert_signals,
    normalize_risk = normalize_risk,
    geometric = geometric,
    verbose = verbose,
    hide_details = hide_details,
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

    module_info <- resolve_module(spec$type)
    module <- module_info$module
    runner_symbol <- module_info$runner
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
      length(per_params$timeframes), length(per_params$mps_risk_value), length(per_params$mps),
      length(per_params$fee), length(per_params$long), length(per_params$short),
      length(per_params$invert_signals), length(per_params$normalize_risk),
      length(per_params$geometric), length(per_params$verbose),
      length(per_params$hide_details), length(per_params$returns_type),
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

      indicator_args_i <- module$resolve_indicator_args(indicator_candidates)
      indicator_suffix <- if (!is.null(module$batch_suffix)) module$batch_suffix(indicator_args_i) else paste(indicator_args_i, collapse = "_")

      rV <- .bt_value_at(per_params$mps_risk_value, i, defaults$mps_risk_value)
      rV_numeric <- suppressWarnings(as.numeric(rV))[1]
      psV <- .bt_value_at(per_params$mps, i, defaults$mps)
      fee_raw <- .bt_value_at(per_params$fee, i, defaults$fee)
      feeV <- normalize_fee_mode(fee_raw)
      L <- isTRUE(.bt_value_at(per_params$long, i, defaults$long))
      S <- isTRUE(.bt_value_at(per_params$short, i, defaults$short))
      inv <- isTRUE(.bt_value_at(per_params$invert_signals, i, defaults$invert_signals))
      nr <- .bt_value_at(per_params$normalize_risk, i, defaults$normalize_risk)
      geom <- isTRUE(.bt_value_at(per_params$geometric, i, defaults$geometric))
      verb <- isTRUE(.bt_value_at(per_params$verbose, i, defaults$verbose))
      hide <- isTRUE(.bt_value_at(per_params$hide_details, i, defaults$hide_details))
      rtyp <- normalize_rtype(.bt_value_at(per_params$returns_type, i, defaults$returns_type))

      data_symbol <- paste0(spec_ticker, "_", tf)
      label <- build_label(spec_label, tf, type_name, indicator_suffix, psV, rV, L, S, feeV, inv, geom, nr, rtyp)
      if (isTRUE(verbose) || verb) message(sprintf("Running %s ...", label))

      ensure_xts_or_remove(data_symbol)
      preload_symbol(data_symbol, base_sym = spec_ticker)

      call_args <- c(
        list(
          ticker = data_symbol,
          ps_risk_value = rV,
          ps = psV,
          fee = feeV,
          start_date = start_date,
          end_date = end_date,
          long = L,
          short = S,
          invert_signals = inv,
          normalize_risk = nr,
          geometric = geom,
          verbose = isTRUE(verbose) || verb,
          only_returns = only_returns,
          hide_details = hide,
          plot = FALSE
        ),
        indicator_args_i
      )

      expr_call <- as.call(c(list(runner_symbol), call_args))

      one_res <- NULL
      one_ret <- NULL
      err_msg <- NULL
      res_full <- NULL
      selected_plot <- NULL

      tryCatch(
        {
          res <- with_safe_future(expr_call)
          if (isTRUE(only_returns)) {
            res_full <- res
            selected_plot <- tryCatch(extract_returns(res_full, rtyp, norm_risk_numeric = nr), error = function(e) NULL)
            if (is.null(selected_plot)) {
              warning(sprintf("Could not extract returns type '%s' for %s; filling zeros.", rtyp, label))
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
        warning(sprintf("Backtest failed for %s: %s. Filling with zeros.", label, if (is.null(err_msg)) "unknown error" else err_msg))
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
      formatted_adjusted_risk <- if (is.finite(base_scale_val) && is.finite(base_adjusted) && abs(base_adjusted - rV_numeric) > tol) format_risk_token(base_adjusted) else NA_character_

      output_label <- label
      old_token <- sprintf("%s_%s", psV, formatted_original_risk)
      if (!is.na(formatted_adjusted_risk)) {
        new_token <- sprintf("%s%s_aj%s", psV, formatted_original_risk, formatted_adjusted_risk)
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
        sm_get_data(symbol,
          future_history = FALSE,
          single_xts = TRUE,
          local_data = FALSE
        ),
        silent = TRUE
      )
      if (inherits(data, "try-error")) data <- NULL
    }

    data <- clean_xts(data)

    if (is.null(data)) {
      data <- try(
        .bt_fetch_ticker_data(symbol,
          data_env = data_env,
          start_date = default_start_chr,
          end_date = default_end_chr,
          clean_di = TRUE
        ),
        silent = TRUE
      )
      if (inherits(data, "try-error")) data <- NULL
    }

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
  if (length(risk_report) > 0) {
    cat("\nRisk normalization impact (position sizing):\n")
    for (rec in risk_report) {
      descriptor <- if (isTRUE(rec$is_portfolio)) "portfolio risk" else "ps_risk_value"
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
        do.call(tplot, plot_list)
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
