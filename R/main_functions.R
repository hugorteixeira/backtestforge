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
bt_eldoc <- function(ticker, up = 40, down = 40, ps_risk_value = 2, ps = "pct", fee = "normal", start_date = "1900-01-01", end_date = Sys.Date(), long = TRUE, short = TRUE, invert_signals = FALSE, normalize_risk = NULL, geometric = TRUE, verbose = FALSE, only_returns = FALSE, hide_details = FALSE, plot = FALSE) {

  fee_mode <- normalize_fee_mode(fee)

  # Remove all objects from blotter/strategy environments
  if(exists('.blotter')) rm(list = ls(envir = .blotter), envir = .blotter)
  if(exists('.strategy')) rm(list = ls(envir = .strategy), envir = .strategy)

  if(!exists(".strategy")) .strategy <<- new.env()
  if(!exists(".blotter")) .blotter <<- new.env()

  if(verbose) print(list(
    ticker_val = ticker,
    class = class(ticker),
    typeof = typeof(ticker),
    length = length(ticker),
    is_character = is.character(ticker)
  ))

  data_env <- .get_bt_data_env()

  if (exists(ticker, envir = data_env, inherits = FALSE)) {
    ticker_data <- get(ticker, envir = data_env)
  } else if (exists(ticker, envir = .GlobalEnv, inherits = FALSE)) {
    ticker_data <- get(ticker, envir = .GlobalEnv)
  } else {
    ticker_data <- sm_get_data(ticker, start_date = start_date, end_date = end_date,
                               future_history = FALSE, single_xts = TRUE, local_data = FALSE)
  }
  ticker_data <- .use_close_only(ticker_data)
  assign(ticker, ticker_data, envir = data_env)
  .register_future_from_data(ticker, ticker_data)
  original_ticker <- ticker

  up_str <- as.character(up)
  down_str <- as.character(down)

  bt_ticker <- NULL

  if(!hide_details){
    if(long && short) {
      bt_ticker <- paste0(ticker, "_eD_", up_str, "_", down_str, "_",ps,"_",ps_risk_value)
    } else if(long && !short) {
      bt_ticker <- paste0(ticker, "_eD_", up_str, "_", down_str, "_",ps,"_",ps_risk_value)
    } else if(!long && short) {
      bt_ticker <- paste0(ticker, "_eD_", up_str, "_", down_str, "_",ps,"_",ps_risk_value)
    } else {
      stop("You need to be long, short or both!")
    }
  } else {
    if(long && short) {
      bt_ticker <- paste0(ticker, "_eD_nodets")
    } else if(long && !short) {
      bt_ticker <- paste0(ticker, "_eD_nodets")
    } else if(!long && short) {
      bt_ticker <- paste0(ticker, "_eD_nodets")
    } else {
      stop("You need to be long, short or both!")
    }
  }

  if (!is.null(bt_ticker) && fee_mode == "nofee") {
    bt_ticker <- paste0(bt_ticker, "_nofees")
  }

  instrument_attr(ticker, "primary_id", bt_ticker)
  ticker <- bt_ticker

  verbose = TRUE
  initEq <- 1000000
  path.dependence <- TRUE

  portfolio.st = 'elDoc'
  account.st = 'elDoc'
  initPortf(portfolio.st, symbols = ticker)
  initAcct(account.st, portfolios = portfolio.st, initEq = initEq)
  initOrders(portfolio = portfolio.st)
  my_strategy <- strategy(portfolio.st)

  tradeSize <- 9999999
  bcontracts <- 1
  scontracts <- -1

  con <- quote(.psFixedContractsQty)
  per <- quote(.psEquityPercentage)
  pct <- quote(.psEquityPercentageDonchian)
  di <- quote(.psEquityPercentageDonchian_DI)

  isDI <- startsWith(ticker,"DI1")

  if(isDI){
    PositionSizing <- function(data, timestamp, orderqty, ordertype, orderside,
                               portfolio, symbol, tradeSize, maxSize, ...) {
      return(.psEquityPercentageDonchian_DI(data, timestamp, orderqty, ordertype, orderside,
                                   portfolio, symbol, tradeSize, maxSize,
                                   risk = ps_risk_value, ...))
    }
    print("Position Sizing set to 'DI'")
  } else {
    PositionSizing <- function(data, timestamp, orderqty, ordertype, orderside,
                               portfolio, symbol, tradeSize, maxSize, ...) {
      return(.psEquityPercentageDonchian(data, timestamp, orderqty, ordertype, orderside,
                                portfolio, symbol, tradeSize, maxSize,
                                risk = ps_risk_value, ...))
    }
  }

  OrderType <- 'market'

  LongEnabled <- long
  ShortEnabled <- short

  HighCol <- "High"
  LowCol  <- "Low"
  if ("PU_o" %in% colnames(ticker_data)) Preference <- "PU_o" else Preference <- "Open"

  assign(bt_ticker, ticker_data, envir = data_env)
  assign(bt_ticker, ticker_data, envir = .GlobalEnv)  # blotter/quantstrat price lookups expect globals
  ReplaceBuy <- FALSE
  ReplaceSell <- FALSE
  ReplaceShort <- FALSE
  ReplaceCover <- FALSE

  docx = up
  docy = down

  if (fee_mode == "nofee") {
    TxnFeesVal <- 0
  } else {
    # pass name so quantstrat resolves the function lazily when applying rules
    TxnFeesVal <- ".calculate_fees"
  }

  my_strategy <- add.indicator(
    my_strategy, "eldoc",
    arguments = list(
      ticker = quote(mktdata),
      x      = docx,
      y      = docy,
      hi.col = HighCol,    # <- PU_h if DI
      lo.col = LowCol,     # <- PU_l if DI
      type   = "data"),
    label = "el")

  coisa <- applyIndicators(strategy = my_strategy, mktdata = get(ticker, envir = data_env))

  my_strategy <-   add.signal(strategy = my_strategy,
                             name = "sigCrossover",
                             arguments = list(
                               data = quote(mktdata),
                               columns = c(HighCol, "X.el"),
                               relationship = "gte"),
                             label = "Entry")

  my_strategy <-  add.signal(strategy = my_strategy,
                            name = "sigCrossover",
                            arguments = list(
                              data = quote(mktdata),
                              columns = c(LowCol, "Y.el"),
                              relationship = "lte"),
                            label = "Exit")
  coisa <- applySignals(strategy = my_strategy, mktdata = coisa)

  n.ent  <- sum(coisa$Entry == 1, na.rm = TRUE)
  n.saida<- sum(coisa$Exit   == 1, na.rm = TRUE)
  .dbg("signals   Entry:", n.ent, " Exit:", n.saida)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Entry",
                            sigval = TRUE,
                            datax = mktdata,
                            initEq = initEq,
                            orderqty = tradeSize,
                            portfolio = portfolio.st,
                            ordertype = OrderType,
                            orderside = if(!isDI) 'long' else 'short',
                            osFUN = PositionSizing,
                            tradeSize = tradeSize,
                            buyorderqty = bcontracts,
                            sellorderqty = scontracts,
                            maxSize = 9999999,
                            prefer = Preference,
                            replace = ReplaceBuy,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'exit' else 'enter',
                          label = if(invert_signals) 'exitLong' else 'enterLong',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = LongEnabled)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Exit",
                            sigval = TRUE,
                            orderqty = "all",
                            ordertype = OrderType,
                            orderside = if(!isDI) 'long' else 'short',
                            prefer = Preference,
                            replace = ReplaceSell,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'enter' else 'exit',
                          label = if(invert_signals) 'enterLong' else 'exitLong',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = LongEnabled)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Exit",
                            sigval = TRUE,
                            datax = mktdata,
                            initEq = initEq,
                            orderqty = tradeSize,
                            portfolio = portfolio.st,
                            ordertype = OrderType,
                            orderside = if(isDI) 'long' else 'short',
                            osFUN = PositionSizing,
                            tradeSize = -tradeSize,
                            buyorderqty = bcontracts,
                            sellorderqty = scontracts,
                            maxSize = -9999999,
                            prefer = Preference,
                            replace = ReplaceShort,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'exit' else 'enter',
                          label = if(invert_signals) 'exitShort' else 'enterShort',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = ShortEnabled)

  my_strategy <-  add.rule(strategy = my_strategy,
                          name = 'ruleSignal',
                          arguments = list(
                            sigcol = "Entry",
                            sigval = TRUE,
                            orderqty = "all",
                            ordertype = OrderType,
                            orderside = if(isDI) 'long' else 'short',
                            prefer = Preference,
                            replace = ReplaceCover,
                            TxnFees = TxnFeesVal
                          ),
                          type = if(invert_signals) 'enter' else 'exit',
                          label = if(invert_signals) 'enterShort' else 'exitShort',
                          storefun = TRUE,
                          path.dep = path.dependence,
                          enabled = ShortEnabled)

  start_t <- Sys.time()
  getInstrument(ticker)
  # run Backtest
  applyStrategy(strategy = my_strategy, portfolios = portfolio.st, verbose = FALSE, initEq = initEq, mdenv = data_env)

  # aftere applyStrategy
  table(mktdata$Entrada, mktdata$Saida)
  tx <- getTxns(portfolio.st, ticker)
  .dbg("generated orders:", nrow(tx))
  if (nrow(tx) == 0) {
    warning("No generated order. Check column passed to 'prefer' ",
            "and if the instrument has a multiplier/tick_size defined.")
    return(invisible(NULL))
  }
  updatePortf(Portfolio = 'elDoc', prefer = Preference)
  updateAcct(name = 'elDoc')
  updateEndEq(Account = 'elDoc')

  getTxns('elDoc', ticker)

  port = getPortfolio('elDoc')
  book    = getOrderBook('elDoc')
  stats   = tradeStats('elDoc')
  ptstats = perTradeStats('elDoc')
  ptrets  = PortfReturns('elDoc')
  acrets  = AcctReturns('elDoc')
  txns    = getTxns('elDoc', ticker)
  Fee.n.Slip <- sum(txns$Txn.Fees)
  stats$Fee.n.Slip <- Fee.n.Slip

  # show results
  cat(paste0(.dbg("Results for ", ticker, " - elDoc ", up,"/",down,"\n\n")))
  if(verbose == TRUE) {
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

  if(only_returns) {
    return(ptrets)
  }

  stats$elDoc <- paste0(up,"/",down)
  stats$PosSiz <- ps
  stats$Multiplier <- ticker_data$multiplier
  stats$TickSize <- ticker_data$tick_size
  if(fee_mode == "nofee"){
    stats$Slippage <- 0
    stats$Fees <- 0
  } else {
    stats$Slippage <- ticker_data$identifiers$slippage
    stats$Fees <- ticker_data$identifiers$fees
  }
  stats$FeeMode <- fee_mode

  attr(ptrets, "backtest") <- TRUE
  attr(ptrets, "local") <- TRUE
  attr(ptrets, "fee_mode") <- fee_mode

  elements_names <- c("rets","stats","trades","rets_acct","mktdata")

  results <- setNames(
    list(ptrets, stats, txns, acrets, mktdata),
    elements_names
  )
  #rm(list = ticker, envir = .GlobalEnv)  # clean up large data object
  #rm(mktdata, envir = .GlobalEnv)
  #  assign("results",results, envir = .GlobalEnv)
  # assign("results",results, envir = .GlobalEnv)
  if(plot) tplot(portfolio.st,original_ticker)

  #tplot(results[[1]],benchs="USD")

  return(results)
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
    if (!xts::is.xts(x)) return(list(log = NULL, discrete = NULL))
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
    if (depth > max_depth) return(NULL)
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
        if (!is.null(res)) return(res)
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
        if (!is.null(res)) return(res)
      }
      # then scan all elements
      for (i in seq_along(obj)) {
        res <- search_returns(obj[[i]], depth + 1, max_depth)
        if (!is.null(res)) return(res)
      }
      return(NULL)
    }
    # Try attribute 'rets' if any other object
    ra <- attr(obj, "rets")
    if (!is.null(ra)) {
      res <- search_returns(ra, depth + 1, max_depth)
      if (!is.null(res)) return(res)
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
    if (length(idx) < 2) return(NA_real_)
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
    if (is.na(dt) || dt <= 0) return(NA_real_)
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
    if (length(rnum) < 2) return(NA_real_)
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

  target_vol_ann <- risk / 100  # convert percent to decimal
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

  out
}

#' Normalise fee selection flags
#'
#' Converts user-facing strings (e.g. "no fee", "with fees") into canonical
#' modes understood by `bt_eldoc`/`bt_eldoc_batch`. Defaults to "normal" when
#' the value is missing or unrecognised.
#'
#' @param val Input value supplied by the caller.
#' @param default Fallback mode when `val` cannot be interpreted.
#' @return Character scalar, either "normal" or "nofee".
#' @keywords internal
normalize_fee_mode <- function(val, default = "normal") {
  if (missing(val) || is.null(val) || length(val) == 0) return(default)
  raw <- as.character(val)[1]
  if (!nzchar(raw)) return(default)
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

#' Run multiple `bt_eldoc` configurations in batch
#'
#' Vectorises `bt_eldoc()` across tickers and parameter grids, collecting either
#' full backtest objects or just the selected returns. Parameters recycle so you
#' can mix scalar defaults with per-test overrides.
#'
#' @param tickers Character vector of base symbols to backtest.
#' @param timeframes Character vector of timeframe suffixes (e.g. `"4H"`).
#' @param mup,mdown Integer/ numeric breakout lookback lengths for entry/exit.
#' @param mps_risk_value Numeric risk value passed to the position-sizing
#'   function.
#' @param mps Character string specifying the position-sizing method.
#' @param fee Fee handling mode per test (`"normal"` or `"nofee"`).
#' @param start_date,end_date Optional `Date` boundaries for fetched data.
#' @param long,short Logical flags enabling long and/or short trades.
#' @param invert_signals Logical flag to invert generated signals.
#' @param normalize_risk Optional numeric target risk for return normalisation.
#' @param geometric Logical indicating geometric aggregation for reports.
#' @param verbose Logical for emitting progress messages.
#' @param only_returns Logical; if TRUE, returns an `xts` matrix of returns
#'   instead of the full backtest objects.
#' @param hide_details Logical; if TRUE, shortens generated labels.
#' @param returns_type Desired returns column (`"Log"`, `"Discrete"`, `"LogAdj"`,
#'   or `"DiscreteAdj"`).
#' @param plot_mult Logical flag; if TRUE, plots combined results via `tplot()`.
#' @return When `only_returns = TRUE`, an `xts` object with merged returns
#'   columns; otherwise a named list of backtest results keyed by generated
#'   labels.
#' @export
bt_eldoc_batch <- function(
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
    normalize_risk = NULL,         # vectorizable per test
    geometric = FALSE,
    verbose = FALSE,
    only_returns = FALSE,
    hide_details = FALSE,
    returns_type = "Log",          # vectorizable: Log, Discrete, LogAdj, DiscreteAdj
    plot_mult = FALSE
) {
  if (!requireNamespace("xts", quietly = TRUE)) stop("Package 'xts' is required.")

  # ---------- helpers ----------
  value_at <- function(vec, i, default_val) {
    if (is.null(vec) || length(vec) < i) default_val else vec[[i]]
  }

  normalize_rtype <- function(x) {
    if (is.null(x) || length(x) == 0) return("Log")
    x <- as.character(x)[1]; y <- tolower(gsub("[^A-Za-z]", "", x))
    if (y %in% c("log","l")) return("Log")
    if (y %in% c("discrete","d")) return("Discrete")
    if (y %in% c("logadj","lognormalized","lognorm")) return("LogAdj")
    if (y %in% c("discreteadj","discretenormalized","discretenorm")) return("DiscreteAdj")
    "Log"
  }

  build_label <- function(tk, tf, up, dn, ps, riskv, L, S, fee, inv, geom, nrisk, rtype) {
    suffix <- if (identical(fee, "nofee")) "_nofees" else ""
    parts <- c(
      sprintf("%s_%s%s", tk, tf, suffix),
      sprintf("%s_%s", up, dn),          # removed slash
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

  idx_to_posixct <- function(idx) {
    if (length(idx) == 0) return(as.POSIXct(character(0), tz = "UTC"))
    if (inherits(idx, "POSIXt")) return(as.POSIXct(idx, tz = "UTC"))
    if (inherits(idx, "Date"))   return(as.POSIXct(idx, tz = "UTC"))
    if (inherits(idx, "yearmon") || inherits(idx, "yearqtr")) {
      if (requireNamespace("zoo", quietly = TRUE)) {
        d <- tryCatch(as.Date(idx), error = function(e) NA)
        if (!all(is.na(d))) return(as.POSIXct(d, tz = "UTC"))
      }
    }
    try1 <- suppressWarnings(try(as.POSIXct(idx, origin = "1970-01-01", tz = "UTC"), silent = TRUE))
    if (!inherits(try1, "try-error") && all(is.finite(as.numeric(try1)))) return(try1)
    try2 <- suppressWarnings(try(as.POSIXct(idx, tz = "UTC"), silent = TRUE))
    if (!inherits(try2, "try-error") && all(is.finite(as.numeric(try2)))) return(try2)
    as.POSIXct(character(0), tz = "UTC")
  }

  zero_returns_xts <- function(idx) {
    ob <- idx_to_posixct(idx); n <- length(ob)
    xts::xts(matrix(0, nrow = n, ncol = 1), order.by = ob)
  }

  sanitize_dt_attrs <- function(dt) {
    if (!xts::is.xts(dt)) return(NULL)
    tsz <- .sanitize_scalar_numeric(attr(dt, "fut_tick_size"), default = 0.01)
    mult <- .sanitize_scalar_numeric(attr(dt, "fut_multiplier"), default = 1)
    mat  <- attr(dt, "maturity")
    cur  <- attr(dt, "currency"); if (is.null(cur)) cur <- "USD"
    attr(dt, "fut_tick_size") <- tsz
    attr(dt, "fut_multiplier") <- mult
    attr(dt, "maturity") <- mat
    attr(dt, "currency") <- cur
    dt
  }

  # fetch helper: try suffixed symbol; if not xts or empty, try base ticker
  fetch_xts_or_fallback <- function(sym, base_sym) {
    dt <- tryCatch(
      sm_get_data(sym, start_date = start_date, end_date = end_date,
                  future_history = FALSE, single_xts = TRUE, local_data = FALSE),
      error = function(e) NULL
    )
    if (!xts::is.xts(dt) || NROW(dt) == 0L) {
      dt <- tryCatch(
        sm_get_data(base_sym, start_date = start_date, end_date = end_date,
                    future_history = FALSE, single_xts = TRUE, local_data = FALSE),
        error = function(e) NULL
      )
    }
    if (xts::is.xts(dt) && NROW(dt) > 0L) dt else NULL
  }

  data_env <- .get_bt_data_env()

  # preload suffixed symbol into the internal data environment if valid xts (else don't assign)
  preload_symbol <- function(sym, base_sym) {
    dt <- fetch_xts_or_fallback(sym, base_sym)
    if (!is.null(dt)) {
      dt <- sanitize_dt_attrs(dt)
      try(assign(sym, dt, envir = data_env), silent = TRUE)
      .register_future_from_data(sym, dt)
    }
    invisible(dt)
  }

  # remove bad preexisting symbol if not xts
  ensure_xts_or_remove <- function(sym) {
    if (exists(sym, envir = data_env, inherits = FALSE)) {
      obj <- get(sym, envir = data_env)
      if (!xts::is.xts(obj)) {
        try(rm(list = sym, envir = data_env), silent = TRUE)
      }
    }
  }

  extract_returns <- function(rets_xts, type, norm_risk_numeric = NULL) {
    if (!xts::is.xts(rets_xts)) stop("extract_returns: rets_xts must be xts")
    type <- normalize_rtype(type); cn <- colnames(rets_xts); lc <- tolower(cn)
    find_col <- function(name) { idx <- which(lc == tolower(name)); if (length(idx)) rets_xts[, idx[1], drop = FALSE] else NULL }

    res <- find_col(type); if (!is.null(res)) return(res)
    base <- if (grepl("^log", tolower(type))) "Log" else "Discrete"
    base_col <- find_col(base)

    if (grepl("adj$", tolower(type))) {
      if (is.numeric(norm_risk_numeric) && is.finite(norm_risk_numeric)) {
        work_xts <- rets_xts
        if (is.null(find_col("Log")) && is.null(find_col("Discrete"))) {
          if (!is.null(base_col)) work_xts <- base_col
          else if (!is.null(find_col(if (base == "Log") "Discrete" else "Log"))) work_xts <- find_col(if (base == "Log") "Discrete" else "Log")
          else {
            work_xts <- xts::xts(matrix(numeric(0), ncol = 1), order.by = as.POSIXct(character(0)))
            colnames(work_xts) <- "Log"
          }
        }
        norm_type <- if (base == "Log") "Log" else "Discrete"
        adj <- tryCatch(bt_normalize_risk(work_xts, risk = norm_risk_numeric, type = norm_type), error = function(e) NULL)
        if (!is.null(adj)) { colnames(adj) <- if (base == "Log") "LogAdj" else "DiscreteAdj"; return(adj) }
      }
      if (!is.null(base_col)) return(base_col)
    } else {
      if (is.null(base_col)) {
        other <- if (base == "Log") find_col("Discrete") else find_col("Log")
        if (!is.null(other)) {
          if (base == "Log") { out <- xts::xts(log1p(as.numeric(other)), order.by = index(other)); colnames(out) <- "Log"; return(out) }
          else { out <- xts::xts(exp(as.numeric(other)) - 1, order.by = index(other)); colnames(out) <- "Discrete"; return(out) }
        }
      } else return(base_col)
    }
    rets_xts[, 1, drop = FALSE]
  }

  fallback_index <- function(ticker_with_tf, base_sym) {
    idx <- tryCatch(index(get(ticker_with_tf, envir = data_env)), error = function(e) NULL)
    if (!is.null(idx)) return(idx)
    td <- fetch_xts_or_fallback(ticker_with_tf, base_sym)
    if (!is.null(td)) return(index(td))
    as.POSIXct(character(0))
  }

  # safe 'future' wrapper for the bt_eldoc call
  with_safe_future <- function(expr_call) {
    safe_future <- function(primary_id, tick_size = NULL, multiplier = NULL, maturity = NULL, currency = "USD", ...) {
      if (!requireNamespace("FinancialInstrument", quietly = TRUE)) {
        stop("Package 'FinancialInstrument' is required for instrument definition.")
      }
      tsz <- .sanitize_scalar_numeric(tick_size, default = 0.01)
      mult <- .sanitize_scalar_numeric(multiplier, default = 1)
      cur  <- if (is.null(currency)) "USD" else as.character(currency)[1]
      suppressWarnings(
        FinancialInstrument::future(primary_id = primary_id,
                                    tick_size = tsz,
                                    multiplier = mult,
                                    maturity = maturity,
                                    currency = cur, ...)
      )
    }
    exec_env <- new.env(parent = .GlobalEnv)
    assign("future", safe_future, envir = exec_env)
    eval(expr_call, envir = exec_env)
  }

  # ---------- test grid ----------
  lens <- c(
    length(timeframes), length(mup), length(mdown), length(mps_risk_value), length(mps),
    length(fee), length(long), length(short), length(invert_signals),
    length(normalize_risk), length(geometric), length(verbose),
    length(hide_details), length(returns_type)
  )
  n_tests <- max(c(1, lens))

  defaults <- list(
    timeframes = "1D",
    mup = 40L,
    mdown = 40L,
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

  results_list <- list()
  returns_list <- list()
  plot_list <- list()

  tickers <- as.character(tickers)
  for (tk in tickers) {
    for (i in seq_len(n_tests)) {
      tf   <- value_at(timeframes,      i, defaults$timeframes)
      up   <- value_at(mup,             i, defaults$mup)
      dn   <- value_at(mdown,           i, defaults$mdown)
      rV   <- value_at(mps_risk_value,  i, defaults$mps_risk_value)
      psV  <- value_at(mps,             i, defaults$mps)
      fee_raw <- value_at(fee,          i, defaults$fee)
      feeV   <- normalize_fee_mode(fee_raw)
      L    <- isTRUE(value_at(long,     i, defaults$long))
      S    <- isTRUE(value_at(short,    i, defaults$short))
      inv  <- isTRUE(value_at(invert_signals, i, defaults$invert_signals))
      nr   <- value_at(normalize_risk,  i, defaults$normalize_risk)
      geom <- isTRUE(value_at(geometric, i, defaults$geometric))
      verb <- isTRUE(value_at(verbose,  i, defaults$verbose))
      hide <- isTRUE(value_at(hide_details, i, defaults$hide_details))
      rtyp <- normalize_rtype(value_at(returns_type, i, defaults$returns_type))

      tk_tf <- paste0(tk, "_", tf)
      label <- build_label(tk, tf, up, dn, psV, rV, L, S, feeV, inv, geom, nr, rtyp)
      if (isTRUE(verbose) || isTRUE(verb)) message(sprintf("Running %s ...", label))

      # ensure no bad preexisting object blocks a fresh fetch
      ensure_xts_or_remove(tk_tf)
      # preload and define instrument if possible (uses fallback to base tk)
      preload_symbol(tk_tf, base_sym = tk)

      call_expr <- bquote(
        bt_eldoc(
          ticker = .(tk_tf),
          up = .(up),
          down = .(dn),
          ps_risk_value = .(rV),
          ps = .(psV),
          fee = .(feeV),
          start_date = .(start_date),
          end_date = .(end_date),
          long = .(L),
          short = .(S),
          invert_signals = .(inv),
          normalize_risk = .(nr),
          geometric = .(geom),
          verbose = .(isTRUE(verbose) || isTRUE(verb)),
          only_returns = .(only_returns),
          hide_details = .(hide),
          plot = FALSE
        )
      )

      one_res <- NULL
      one_ret <- NULL
      err_msg <- NULL

      tryCatch(
        {
          res <- with_safe_future(call_expr)

          if (isTRUE(only_returns)) {
            rets_xts <- res
            selected <- tryCatch(extract_returns(rets_xts, rtyp, norm_risk_numeric = nr), error = function(e) NULL)
            if (is.null(selected)) {
              warning(sprintf("Could not extract returns type '%s' for %s; filling zeros.", rtyp, label))
              idx <- index(rets_xts); selected <- zero_returns_xts(idx)
            }
            colnames(selected) <- label
            one_ret <- selected
            plot_list[[length(plot_list) + 1]] <- one_ret
          } else {
            one_res <- res
            if (!is.null(res$rets)) {
              selected <- tryCatch(extract_returns(res$rets, rtyp, norm_risk_numeric = nr), error = function(e) NULL)
              if (!is.null(selected)) {
                colnames(selected) <- label
                plot_list[[length(plot_list) + 1]] <- selected
              }
            }
          }
        },
        error = function(e) {
          err_msg <<- conditionMessage(e)
        }
      )

      if (!is.null(err_msg)) {
        warning(sprintf("Backtest failed for %s: %s. Filling with zeros.", label, err_msg))
        idx <- fallback_index(tk_tf, base_sym = tk)
        zeros <- zero_returns_xts(idx)
        colnames(zeros) <- label

        if (isTRUE(only_returns)) {
          one_ret <- zeros
          plot_list[[length(plot_list) + 1]] <- one_ret
        } else {
          one_res <- list(
            rets = zeros,
            stats = NULL,
            trades = NULL,
            rets_acct = NULL,
            mktdata = tryCatch({
              obj <- get(tk_tf, envir = data_env)
              if (xts::is.xts(obj)) obj else NULL
            }, error = function(e) NULL)
          )
          names(one_res) <- c("rets", "stats", "trades", "rets_acct", "mktdata")
          plot_list[[length(plot_list) + 1]] <- zeros
        }
      }

      if (!is.null(one_res)) attr(one_res, "fee_mode") <- feeV
      if (!is.null(one_ret)) attr(one_ret, "fee_mode") <- feeV

      if (isTRUE(only_returns)) returns_list[[length(returns_list) + 1]] <- one_ret
      else results_list[[label]] <- one_res
    }
  }

  if (isTRUE(plot_mult) && length(plot_list) > 0) {
    tryCatch({ do.call(tplot, plot_list) }, error = function(e) warning(sprintf("plot_mult failed: %s", conditionMessage(e))))
  }

  if (isTRUE(only_returns)) {
    if (length(returns_list) == 0) return(xts::xts(matrix(numeric(0), ncol = 0), order.by = as.POSIXct(character(0))))
    out <- tryCatch({ do.call(xts::merge.xts, c(returns_list, list(all = TRUE))) },
                    error = function(e) Reduce(function(a, b) merge(a, b, all = TRUE), returns_list))
    return(out)
  } else {
    return(results_list)
  }
}
