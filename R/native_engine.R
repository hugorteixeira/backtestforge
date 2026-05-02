#' Build a serializable trend-following strategy specification
#'
#' @param type Strategy type. Use `"donchian"`/`"eldoc"`, `"ema"`, or `"sma"`.
#' @param ... Indicator parameters (`up`/`down` for Donchian, `fast`/`slow` for
#'   moving averages).
#' @param long,short Logical flags enabling long and short trades.
#' @param invert_signals Logical; swaps entry and exit signals for experiments.
#' @return A list with class `bt_strategy_spec`.
#' @export
bt_strategy_spec <- function(type = c("donchian", "eldoc", "ema", "sma"),
                             ...,
                             long = TRUE,
                             short = TRUE,
                             invert_signals = FALSE) {
  raw_type <- if (length(type)) tolower(as.character(type)[1]) else "donchian"
  canonical <- switch(raw_type,
    eldoc = "donchian",
    donchian = "donchian",
    ema = "ema",
    sma = "sma",
    stop(sprintf("Unsupported native strategy type '%s'.", raw_type), call. = FALSE)
  )
  args <- list(...)
  params <- switch(canonical,
    donchian = list(
      up = as.integer(.bt_first_non_null(args$up, args$mup, 40L)),
      down = as.integer(.bt_first_non_null(args$down, args$mdown, 40L))
    ),
    ema = list(
      fast = as.integer(.bt_first_non_null(args$fast, args$mup, args$up, 20L)),
      slow = as.integer(.bt_first_non_null(args$slow, args$mdown, args$down, 50L))
    ),
    sma = list(
      fast = as.integer(.bt_first_non_null(args$fast, args$mup, args$up, 20L)),
      slow = as.integer(.bt_first_non_null(args$slow, args$mdown, args$down, 50L))
    )
  )
  if (any(!is.finite(unlist(params, use.names = FALSE))) || any(unlist(params, use.names = FALSE) <= 0)) {
    stop("Strategy window parameters must be positive finite integers.", call. = FALSE)
  }
  spec <- list(
    type = canonical,
    params = params,
    long = isTRUE(long),
    short = isTRUE(short),
    invert_signals = isTRUE(invert_signals)
  )
  class(spec) <- c("bt_strategy_spec", "list")
  spec
}

#' Build a native risk specification
#'
#' @param mode Sizing mode. `"risk"` uses stop distance when available and
#'   falls back to notional sizing. `"notional"` uses `risk_pct` as capital
#'   allocation. `"fixed"` uses `fixed_qty`.
#' @param initial_equity Starting equity.
#' @param risk_pct Percent of equity/capital to risk or allocate.
#' @param fixed_qty Quantity used by fixed sizing.
#' @param max_qty Absolute maximum contracts/shares per position.
#' @param max_leverage Optional notional cap as a multiple of equity.
#' @param integer_qty Whether quantities are floored to integers.
#' @param reinvest Whether risk sizing uses current equity instead of initial
#'   equity.
#' @param min_risk_pct Minimum stop distance as a fraction of price when no tick
#'   value is available.
#' @return A list with class `bt_risk_spec`.
#' @export
bt_risk_spec <- function(mode = c("risk", "notional", "fixed"),
                         initial_equity = 10000000,
                         risk_pct = 2,
                         fixed_qty = 1,
                         max_qty = Inf,
                         max_leverage = Inf,
                         integer_qty = TRUE,
                         reinvest = FALSE,
                         min_risk_pct = 0.0005) {
  mode <- match.arg(mode)
  spec <- list(
    mode = mode,
    initial_equity = as.numeric(initial_equity)[1],
    risk_pct = as.numeric(risk_pct)[1],
    fixed_qty = as.numeric(fixed_qty)[1],
    max_qty = as.numeric(max_qty)[1],
    max_leverage = as.numeric(max_leverage)[1],
    integer_qty = isTRUE(integer_qty),
    reinvest = isTRUE(reinvest),
    min_risk_pct = as.numeric(min_risk_pct)[1]
  )
  if (!is.finite(spec$initial_equity) || spec$initial_equity <= 0) {
    stop("'initial_equity' must be positive.", call. = FALSE)
  }
  if (!is.finite(spec$risk_pct) || spec$risk_pct <= 0) {
    stop("'risk_pct' must be positive.", call. = FALSE)
  }
  if (!is.finite(spec$fixed_qty) || spec$fixed_qty <= 0) {
    stop("'fixed_qty' must be positive.", call. = FALSE)
  }
  if (!is.finite(spec$min_risk_pct) || spec$min_risk_pct <= 0) {
    stop("'min_risk_pct' must be positive.", call. = FALSE)
  }
  class(spec) <- c("bt_risk_spec", "list")
  spec
}

#' Build a native execution specification
#'
#' @param timing Execution timing. `"next_open"` is the default and avoids
#'   lookahead by executing a signal on the following bar open. `"same_open"`
#'   mirrors the legacy quantstrat market-order convention used by the old
#'   wrappers.
#' @param fee Fee mode passed through `normalize_fee_mode()`.
#' @param commission_per_contract Optional explicit commission per unit traded.
#' @param slippage_per_contract Optional explicit slippage per unit traded.
#' @param close_on_end Whether open positions are flattened on the last bar.
#' @return A list with class `bt_execution_spec`.
#' @export
bt_execution_spec <- function(timing = c("next_open", "next_close", "same_open", "same_close"),
                              fee = "normal",
                              commission_per_contract = NULL,
                              slippage_per_contract = NULL,
                              close_on_end = TRUE) {
  timing <- match.arg(timing)
  delay <- if (grepl("^same_", timing)) 0L else 1L
  spec <- list(
    timing = timing,
    delay = delay,
    fee = normalize_fee_mode(fee),
    commission_per_contract = commission_per_contract,
    slippage_per_contract = slippage_per_contract,
    close_on_end = isTRUE(close_on_end)
  )
  class(spec) <- c("bt_execution_spec", "list")
  spec
}

#' Run a native backtest without quantstrat or blotter state
#'
#' @param ticker Character symbol or `xts` OHLC object.
#' @param data Optional preloaded `xts` object. Used when `ticker` is a label.
#' @param strategy A `bt_strategy_spec`, or a character strategy type.
#' @param risk A `bt_risk_spec`.
#' @param execution A `bt_execution_spec`.
#' @param start_date,end_date Optional date bounds used when fetching/subsetting.
#' @param normalize_risk Optional annualized risk target in percent.
#' @param geometric Kept for wrapper compatibility.
#' @param only_returns If `TRUE`, return only the `xts` returns object.
#' @param verbose If `TRUE`, print a compact stats row.
#' @param clean_di If `TRUE`, apply DI cleanup before running.
#' @return A `bt_native_result` list, or an `xts` returns object when
#'   `only_returns = TRUE`.
#' @export
bt_run_native <- function(ticker,
                          data = NULL,
                          strategy = bt_strategy_spec("donchian"),
                          risk = bt_risk_spec(),
                          execution = bt_execution_spec(),
                          start_date = "1900-01-01",
                          end_date = Sys.Date(),
                          normalize_risk = NULL,
                          geometric = TRUE,
                          only_returns = FALSE,
                          verbose = FALSE,
                          clean_di = TRUE) {
  if (is.character(strategy)) {
    strategy <- bt_strategy_spec(strategy)
  }
  if (!inherits(strategy, "bt_strategy_spec")) {
    stop("'strategy' must be created by bt_strategy_spec().", call. = FALSE)
  }
  if (!inherits(risk, "bt_risk_spec")) {
    stop("'risk' must be created by bt_risk_spec().", call. = FALSE)
  }
  if (!inherits(execution, "bt_execution_spec")) {
    stop("'execution' must be created by bt_execution_spec().", call. = FALSE)
  }
  if (!strategy$long && !strategy$short) {
    stop("At least one side must be enabled.", call. = FALSE)
  }

  prepared <- .bt_native_prepare_data(
    ticker = ticker,
    data = data,
    start_date = start_date,
    end_date = end_date,
    clean_di = clean_di
  )
  prices <- .bt_native_price_set(prepared$data, prepared$symbol)
  indicators <- .bt_native_indicators(prepared$data, prices, strategy)
  signals <- .bt_native_signals(prepared$data, prices, indicators, strategy)
  metadata <- .bt_native_metadata(prepared$data, prepared$symbol)

  sim <- .bt_native_simulate(
    data = prepared$data,
    prices = prices,
    indicators = indicators,
    signals = signals,
    strategy = strategy,
    risk = risk,
    execution = execution,
    metadata = metadata,
    symbol = prepared$symbol
  )

  rets <- .bt_native_returns(sim$equity, normalize_risk = normalize_risk)
  stats <- .bt_native_stats(
    equity = sim$equity,
    rets = rets,
    trades = sim$trades,
    risk = risk,
    execution = execution,
    metadata = metadata,
    strategy = strategy
  )
  mktdata <- .bt_native_mktdata(prepared$data, indicators, signals)

  attr(rets, "backtest") <- TRUE
  attr(rets, "local") <- TRUE
  attr(rets, "engine") <- "native"
  attr(rets, "fee_mode") <- execution$fee

  if (isTRUE(verbose)) {
    print(stats)
  }
  if (isTRUE(only_returns)) {
    return(rets)
  }

  out <- list(
    rets = rets,
    stats = stats,
    trades = sim$trades,
    rets_acct = rets,
    mktdata = mktdata,
    positions = sim$positions,
    spec = list(strategy = strategy, risk = risk, execution = execution),
    engine = "native",
    symbol = prepared$symbol
  )
  class(out) <- c("bt_native_result", "list")
  out
}

#' Search native trend-following parameter specifications
#'
#' @param ticker Character symbol or `xts` OHLC object.
#' @param space A list or data frame of strategy parameters.
#' @param budget Optional maximum number of specs to evaluate.
#' @param sample Logical; if `TRUE`, sample `budget` rows from the grid.
#' @param objective Metric used for sorting.
#' @param seed Optional random seed.
#' @param workers Number of worker processes for Unix `parallel::mclapply`.
#' @param risk,execution,start_date,end_date,clean_di Passed to `bt_run_native()`.
#' @return A data frame of evaluated specs. Full result objects are stored in
#'   `attr(result, "results")`.
#' @export
bt_search_native <- function(ticker,
                             space,
                             budget = NULL,
                             sample = FALSE,
                             objective = c("total_return", "sharpe", "max_drawdown", "num_trades"),
                             seed = NULL,
                             workers = 1L,
                             risk = bt_risk_spec(),
                             execution = bt_execution_spec(),
                             start_date = "1900-01-01",
                             end_date = Sys.Date(),
                             clean_di = TRUE) {
  objective <- match.arg(objective)
  grid <- .bt_native_strategy_grid(space)
  if (!NROW(grid)) {
    stop("'space' produced no strategy specifications.", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!is.null(budget)) {
    budget <- as.integer(budget)[1]
    if (is.finite(budget) && budget > 0 && budget < NROW(grid)) {
      idx <- if (isTRUE(sample)) sample(seq_len(NROW(grid)), budget) else seq_len(budget)
      grid <- grid[idx, , drop = FALSE]
    }
  }

  run_one <- function(i) {
    row <- grid[i, , drop = FALSE]
    args <- as.list(row)
    type <- args$type %||% "donchian"
    args$type <- NULL
    args <- args[!vapply(args, function(x) is.na(x) || is.null(x), logical(1))]
    strategy <- do.call(bt_strategy_spec, c(list(type = type), args))
    res <- bt_run_native(
      ticker = ticker,
      strategy = strategy,
      risk = risk,
      execution = execution,
      start_date = start_date,
      end_date = end_date,
      clean_di = clean_di
    )
    metrics <- res$stats
    metrics$spec_id <- i
    list(summary = cbind(row, metrics), result = res)
  }

  ids <- seq_len(NROW(grid))
  workers <- as.integer(workers)[1]
  if (is.finite(workers) && workers > 1L && .Platform$OS.type != "windows") {
    pieces <- parallel::mclapply(ids, run_one, mc.cores = workers)
  } else {
    pieces <- lapply(ids, run_one)
  }

  summary <- do.call(rbind, lapply(pieces, `[[`, "summary"))
  rownames(summary) <- NULL
  results <- lapply(pieces, `[[`, "result")
  decreasing <- objective %in% c("total_return", "sharpe", "num_trades")
  ord <- order(summary[[objective]], decreasing = decreasing, na.last = TRUE)
  summary <- summary[ord, , drop = FALSE]
  attr(summary, "results") <- results[ord]
  summary
}

.bt_native_prepare_data <- function(ticker, data = NULL, start_date = NULL, end_date = NULL, clean_di = TRUE) {
  ticker_input <- .bt_resolve_ticker_input(if (is.null(data)) ticker else data, substitute(ticker))
  symbol <- .bt_safe_scalar_chr(ticker)
  if (is.null(symbol) || xts::is.xts(ticker)) {
    symbol <- ticker_input$symbol
  }
  if (is.null(data)) {
    data <- ticker_input$data
  }
  if (is.null(data) && !is.null(symbol)) {
    data <- .bt_native_lookup_data(symbol, start_date, end_date)
  }
  if (is.null(data) || !xts::is.xts(data)) {
    data <- tryCatch(xts::as.xts(data), error = function(e) NULL)
  }
  if (is.null(data) || !xts::is.xts(data)) {
    stop("Native engine requires an xts-compatible OHLC object.", call. = FALSE)
  }

  if (!is.null(start_date) || !is.null(end_date)) {
    start_txt <- if (is.null(start_date)) "" else as.character(as.Date(start_date))
    end_txt <- if (is.null(end_date)) "" else as.character(as.Date(end_date))
    window <- paste0(start_txt, "/", end_txt)
    data <- data[window]
  }
  data <- .use_close_only(data)

  symbol <- symbol %||% attr(data, "symbol", exact = TRUE) %||% attr(data, "ticker", exact = TRUE) %||% "local_xts"
  symbol <- as.character(symbol)[1]

  data <- .bt_native_enrich_futures_data(data, symbol, clean_di = clean_di)
  if (!NROW(data)) {
    stop("No rows available after native data preparation.", call. = FALSE)
  }
  list(symbol = symbol, data = data)
}

.bt_native_lookup_data <- function(symbol, start_date, end_date) {
  data_env <- .get_bt_data_env()
  if (exists(symbol, envir = data_env, inherits = FALSE)) {
    obj <- get(symbol, envir = data_env)
    if (xts::is.xts(obj)) return(obj)
  }
  if (exists(symbol, envir = .GlobalEnv, inherits = FALSE)) {
    obj <- get(symbol, envir = .GlobalEnv)
    if (xts::is.xts(obj)) return(obj)
  }
  .bt_fetch_finharvest_data(symbol, start_date = start_date, end_date = end_date)
}

.bt_native_enrich_futures_data <- function(data, symbol, clean_di = TRUE) {
  if (.bt_is_di_symbol(symbol)) {
    data <- .bt_fill_di_maturity(data, symbol)
    if (isTRUE(clean_di)) {
      data <- .bt_clean_di(data, "TickSize", value = 0.001)
      data <- .bt_clean_di_tick(data, "TickValue", value_over = -5)
    }
    data <- .bt_native_add_di_pu(data, symbol)
  }
  data
}

.bt_native_add_di_pu <- function(data, symbol) {
  has_pu <- any(tolower(colnames(data)) %in% c("pu_o", "pu_open", "pu_c", "pu_close"))
  if (has_pu || !requireNamespace("brfutures", quietly = TRUE)) {
    return(data)
  }
  maturity <- attr(data, "maturity", exact = TRUE)
  if (is.null(maturity) || all(is.na(suppressWarnings(as.Date(maturity))))) {
    maturity <- tryCatch(brfutures::di_maturity_from_ticker(.bt_base_contract_symbol(symbol)), error = function(e) NULL)
  }
  if (is.null(maturity)) {
    return(data)
  }
  attrs <- attributes(data)
  enriched <- tryCatch(
    brfutures::di_ohlc_to_pu_augmented_xts(
      data,
      maturity_date = maturity,
      round_pu = FALSE,
      include_diagnostics = FALSE
    ),
    error = function(e) data
  )
  for (nm in setdiff(names(attrs), c("dim", "dimnames", "index", "class", "names"))) {
    if (is.null(attr(enriched, nm, exact = TRUE))) {
      attr(enriched, nm) <- attrs[[nm]]
    }
  }
  if (is.null(attr(enriched, "maturity", exact = TRUE))) {
    attr(enriched, "maturity") <- maturity
  }
  enriched
}

.bt_native_root <- function(symbol) {
  base <- .bt_base_contract_symbol(symbol)
  root <- sub("([FGHJKMNQUVXZ][0-9]{2})$", "", base, perl = TRUE)
  if (!nzchar(root)) base else root
}

.bt_native_metadata <- function(data, symbol) {
  meta <- .collect_instrument_metadata(data)
  ids <- attr(data, "identifiers", exact = TRUE)
  if (!is.list(ids)) ids <- list()
  if (is.list(meta$identifiers) && length(meta$identifiers)) {
    ids <- utils::modifyList(meta$identifiers, ids)
  }
  first_num <- function(...) {
    vals <- unlist(list(...), use.names = FALSE)
    vals <- suppressWarnings(as.numeric(vals))
    vals <- vals[is.finite(vals)]
    if (length(vals)) vals[1] else NA_real_
  }
  fallback_one <- function(value, field) {
    value <- suppressWarnings(as.numeric(value)[1])
    if (is.finite(value) && value > 0) {
      return(value)
    }
    missing_fields <<- c(missing_fields, field)
    1
  }
  missing_fields <- character()
  multiplier <- fallback_one(
    first_num(meta$multiplier, attr(data, "fut_multiplier", exact = TRUE), attr(data, "multiplier", exact = TRUE)),
    "multiplier"
  )
  tick_size <- fallback_one(
    first_num(meta$tick_size, attr(data, "fut_tick_size", exact = TRUE), attr(data, "tick_size", exact = TRUE), attr(data, "ticksize", exact = TRUE)),
    "tick_size"
  )
  tick_value <- first_num(meta$tick_value, attr(data, "fut_tick_value", exact = TRUE), attr(data, "tick_value", exact = TRUE))
  if (!is.finite(tick_value) || tick_value <= 0) {
    tick_value <- tick_size * multiplier
  }
  fees <- fallback_one(first_num(ids$fees, ids$fee, attr(data, "fees", exact = TRUE), attr(data, "fee", exact = TRUE)), "fees")
  slippage <- fallback_one(first_num(ids$slippage, attr(data, "slippage", exact = TRUE)), "slippage")
  if (length(missing_fields)) {
    warning(
      sprintf(
        "Missing native instrument metadata for '%s' (%s); using 1 as fallback. Add xts attrs such as fut_multiplier, fut_tick_size, identifiers$fees and identifiers$slippage to avoid this.",
        symbol,
        paste(unique(missing_fields), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  list(
    multiplier = multiplier,
    tick_size = tick_size,
    tick_value = tick_value,
    fees = fees,
    slippage = slippage,
    symbol = symbol,
    root = .bt_native_root(symbol)
  )
}

.bt_native_price_set <- function(data, symbol) {
  nm <- colnames(data)
  lower <- tolower(nm)
  pick <- function(candidates, required = TRUE) {
    idx <- match(tolower(candidates), lower, nomatch = 0L)
    idx <- idx[idx > 0L]
    if (!length(idx)) {
      if (required) stop("Could not resolve required OHLC columns.", call. = FALSE)
      return(rep(NA_real_, NROW(data)))
    }
    as.numeric(data[, idx[1]])
  }
  di <- .bt_is_di_symbol(symbol)
  if (di && any(lower %in% c("pu_o", "pu_open", "pu_c", "pu_close"))) {
    open <- pick(c("PU_o", "PU_open"))
    high <- pick(c("PU_h", "PU_high"))
    low <- pick(c("PU_l", "PU_low"))
    close <- pick(c("PU_c", "PU_close"))
  } else {
    open <- pick(c("Open", "open", "o"))
    high <- pick(c("High", "high", "h"))
    low <- pick(c("Low", "low", "l"))
    close <- pick(c("Close", "close", "c", "last", "settle", "settlement_price"))
  }
  list(
    open = open,
    high = high,
    low = low,
    close = close,
    index = zoo::index(data),
    uses_pu = di && any(lower %in% c("pu_o", "pu_open", "pu_c", "pu_close"))
  )
}

.bt_native_indicators <- function(data, prices, strategy) {
  n <- length(prices$close)
  idx <- prices$index
  type <- strategy$type
  if (identical(type, "donchian")) {
    up <- strategy$params$up
    down <- strategy$params$down
    upper_raw <- as.numeric(TTR::runMax(prices$high, n = up))
    lower_raw <- as.numeric(TTR::runMin(prices$low, n = down))
    raw <- xts::xts(cbind(DonchianUpper = upper_raw, DonchianLower = lower_raw), order.by = idx)
    out <- xts::lag.xts(raw)
  } else {
    fast_n <- strategy$params$fast
    slow_n <- strategy$params$slow
    fun <- if (identical(type, "ema")) TTR::EMA else TTR::SMA
    fast <- as.numeric(fun(prices$close, n = fast_n))
    slow <- as.numeric(fun(prices$close, n = slow_n))
    col_prefix <- toupper(type)
    out <- xts::xts(cbind(fast, slow), order.by = idx)
    colnames(out) <- paste0(col_prefix, c("Fast", "Slow"))
  }
  if (NROW(out) != n) stop("Internal indicator length mismatch.", call. = FALSE)
  out
}

.bt_native_signals <- function(data, prices, indicators, strategy) {
  idx <- prices$index
  cross_to_true <- function(condition) {
    condition <- as.logical(condition)
    prev <- c(NA, utils::head(condition, -1L))
    out <- condition & !prev
    out[is.na(out)] <- FALSE
    out
  }
  cross_to_false <- function(condition) {
    condition <- as.logical(condition)
    prev <- c(NA, utils::head(condition, -1L))
    out <- !condition & prev
    out[is.na(out)] <- FALSE
    out
  }
  if (identical(strategy$type, "donchian")) {
    upper <- as.numeric(indicators[, "DonchianUpper"])
    lower <- as.numeric(indicators[, "DonchianLower"])
    long_entry <- cross_to_true(prices$high >= upper)
    long_exit <- cross_to_true(prices$low <= lower)
    short_entry <- long_exit
    short_exit <- long_entry
  } else {
    fast <- as.numeric(indicators[, 1])
    slow <- as.numeric(indicators[, 2])
    rel <- fast > slow
    long_entry <- cross_to_true(rel)
    long_exit <- cross_to_false(rel)
    short_entry <- long_exit
    short_exit <- long_entry
  }
  if (isTRUE(strategy$invert_signals)) {
    tmp <- long_entry
    long_entry <- long_exit
    long_exit <- tmp
    tmp <- short_entry
    short_entry <- short_exit
    short_exit <- tmp
  }
  xts::xts(
    cbind(
      LongEntry = long_entry,
      LongExit = long_exit,
      ShortEntry = short_entry,
      ShortExit = short_exit
    ),
    order.by = idx
  )
}

.bt_native_size <- function(side, price, stop_price, equity, risk, metadata, tick_value = NA_real_) {
  mult <- metadata$multiplier
  mult <- if (is.finite(mult) && mult > 0) mult else 1
  price <- as.numeric(price)[1]
  if (!is.finite(price) || price <= 0) return(0)
  equity_basis <- if (isTRUE(risk$reinvest)) equity else risk$initial_equity
  equity_basis <- if (is.finite(equity_basis) && equity_basis > 0) equity_basis else risk$initial_equity

  if (identical(risk$mode, "fixed")) {
    qty_abs <- risk$fixed_qty
  } else if (identical(risk$mode, "risk") && is.finite(stop_price)) {
    risk_per_unit <- abs(price - stop_price) * mult
    min_risk <- if (is.finite(tick_value) && tick_value > 0) {
      abs(tick_value)
    } else {
      max(metadata$tick_size * mult, price * risk$min_risk_pct * mult, na.rm = TRUE)
    }
    risk_per_unit <- max(risk_per_unit, min_risk, na.rm = TRUE)
    allowed <- equity_basis * risk$risk_pct / 100
    qty_abs <- allowed / risk_per_unit
  } else {
    notional <- price * mult
    if (!is.finite(notional) || notional <= 0) return(0)
    qty_abs <- (equity_basis * risk$risk_pct / 100) / notional
  }

  if (is.finite(risk$max_leverage) && risk$max_leverage > 0) {
    leverage_qty <- (equity_basis * risk$max_leverage) / (price * mult)
    qty_abs <- min(qty_abs, leverage_qty, na.rm = TRUE)
  }
  if (is.finite(risk$max_qty) && risk$max_qty > 0) {
    qty_abs <- min(qty_abs, risk$max_qty, na.rm = TRUE)
  }
  if (isTRUE(risk$integer_qty)) {
    qty_abs <- floor(qty_abs)
  }
  if (!is.finite(qty_abs) || qty_abs <= 0) return(0)
  if (identical(side, "short")) -qty_abs else qty_abs
}

.bt_native_txn_cost <- function(qty_delta, price, execution, metadata) {
  if (identical(execution$fee, "nofee")) return(0)
  qty_abs <- abs(as.numeric(qty_delta)[1])
  if (!is.finite(qty_abs) || qty_abs <= 0) return(0)
  commission <- execution$commission_per_contract
  if (is.null(commission)) commission <- metadata$fees
  slippage <- execution$slippage_per_contract
  if (is.null(slippage)) slippage <- metadata$slippage
  commission <- suppressWarnings(as.numeric(commission)[1])
  slippage <- suppressWarnings(as.numeric(slippage)[1])
  if (!is.finite(commission)) commission <- 0
  if (!is.finite(slippage)) slippage <- 0
  qty_abs * (commission + slippage)
}

.bt_native_tick_value_at <- function(data, i, metadata) {
  nm <- tolower(colnames(data))
  idx <- match("tickvalue", nm, nomatch = 0L)
  if (idx > 0L) {
    val <- suppressWarnings(as.numeric(data[i, idx]))
    if (is.finite(val) && val > 0) return(abs(val))
  }
  if (!is.null(metadata$tick_value) && is.finite(metadata$tick_value) && metadata$tick_value > 0) {
    return(metadata$tick_value)
  }
  metadata$tick_size * metadata$multiplier
}

.bt_native_stop_price <- function(i, side, indicators, strategy) {
  if (!identical(strategy$type, "donchian")) {
    return(NA_real_)
  }
  if (identical(side, "long")) {
    as.numeric(indicators[i, "DonchianLower"])
  } else {
    as.numeric(indicators[i, "DonchianUpper"])
  }
}

.bt_native_simulate <- function(data, prices, indicators, signals, strategy, risk, execution, metadata, symbol) {
  n <- length(prices$close)
  idx <- prices$index
  qty <- 0
  equity <- numeric(n)
  qty_path <- numeric(n)
  equity[1] <- risk$initial_equity
  trades <- list()

  add_trade <- function(i, qty_delta, price, fees, reason, equity_after) {
    if (!is.finite(qty_delta) || qty_delta == 0) return(invisible(NULL))
    new_qty <- qty
    side <- if (new_qty > 0) "long" else if (new_qty < 0) "short" else "flat"
    trades[[length(trades) + 1L]] <<- data.frame(
      timestamp = as.POSIXct(idx[i], tz = "UTC"),
      symbol = symbol,
      side = side,
      qty = new_qty,
      qty_delta = qty_delta,
      price = price,
      fees = fees,
      reason = reason,
      equity = equity_after,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  exec_price <- switch(execution$timing,
    next_open = prices$open,
    same_open = prices$open,
    next_close = prices$close,
    same_close = prices$close
  )
  executes_on_open <- execution$timing %in% c("next_open", "same_open")

  maybe_enter <- function(i, sig_i, side, eq, px) {
    stop_px <- .bt_native_stop_price(sig_i, side, indicators, strategy)
    if (identical(strategy$type, "donchian") && !is.finite(stop_px)) {
      return(0)
    }
    size_px <- if (execution$delay > 0L) prices$close[sig_i] else px
    if (!is.finite(size_px) || size_px <= 0) {
      size_px <- px
    }
    tick_value <- .bt_native_tick_value_at(data, sig_i, metadata)
    .bt_native_size(side, size_px, stop_px, eq, risk, metadata, tick_value = tick_value)
  }

  for (i in seq_len(n)) {
    if (i == 1L) {
      qty_path[i] <- qty
      next
    }

    prev_close <- prices$close[i - 1L]
    close_i <- prices$close[i]
    px <- exec_price[i]
    if (!is.finite(px) || px <= 0) px <- close_i
    if (!is.finite(prev_close) || !is.finite(close_i)) {
      equity[i] <- equity[i - 1L]
      qty_path[i] <- qty
      next
    }

    eq <- equity[i - 1L]
    if (executes_on_open) {
      eq <- eq + qty * (px - prev_close) * metadata$multiplier
    } else {
      eq <- eq + qty * (close_i - prev_close) * metadata$multiplier
    }

    sig_i <- i - execution$delay
    if (sig_i >= 1L && sig_i <= n && is.finite(px) && px > 0) {
      le <- isTRUE(as.logical(signals[sig_i, "LongEntry"]))
      lx <- isTRUE(as.logical(signals[sig_i, "LongExit"]))
      se <- isTRUE(as.logical(signals[sig_i, "ShortEntry"]))
      sx <- isTRUE(as.logical(signals[sig_i, "ShortExit"]))

      if (qty > 0 && lx) {
        delta <- -qty
        fees <- .bt_native_txn_cost(delta, px, execution, metadata)
        qty <- 0
        eq <- eq - fees
        add_trade(i, delta, px, fees, "long_exit", eq)
      }
      if (qty < 0 && sx) {
        delta <- -qty
        fees <- .bt_native_txn_cost(delta, px, execution, metadata)
        qty <- 0
        eq <- eq - fees
        add_trade(i, delta, px, fees, "short_exit", eq)
      }
      if (qty == 0 && strategy$long && le && !(strategy$short && se)) {
        delta <- maybe_enter(i, sig_i, "long", eq, px)
        if (delta != 0) {
          fees <- .bt_native_txn_cost(delta, px, execution, metadata)
          qty <- qty + delta
          eq <- eq - fees
          add_trade(i, delta, px, fees, "long_entry", eq)
        }
      } else if (qty == 0 && strategy$short && se && !(strategy$long && le)) {
        delta <- maybe_enter(i, sig_i, "short", eq, px)
        if (delta != 0) {
          fees <- .bt_native_txn_cost(delta, px, execution, metadata)
          qty <- qty + delta
          eq <- eq - fees
          add_trade(i, delta, px, fees, "short_entry", eq)
        }
      }
    }

    if (executes_on_open) {
      eq <- eq + qty * (close_i - px) * metadata$multiplier
    }
    equity[i] <- eq
    qty_path[i] <- qty
  }

  if (isTRUE(execution$close_on_end) && qty != 0 && n > 0) {
    px <- prices$close[n]
    if (is.finite(px) && px > 0) {
      delta <- -qty
      fees <- .bt_native_txn_cost(delta, px, execution, metadata)
      qty <- 0
      equity[n] <- equity[n] - fees
      qty_path[n] <- qty
      add_trade(n, delta, px, fees, "end_exit", equity[n])
    }
  }

  equity_xts <- xts::xts(equity, order.by = idx)
  colnames(equity_xts) <- "Equity"
  positions <- xts::xts(
    cbind(
      qty = qty_path,
      close = prices$close,
      equity = equity,
      position_value = qty_path * prices$close * metadata$multiplier
    ),
    order.by = idx
  )
  trades_df <- if (length(trades)) do.call(rbind, trades) else data.frame(
    timestamp = as.POSIXct(character()),
    symbol = character(),
    side = character(),
    qty = numeric(),
    qty_delta = numeric(),
    price = numeric(),
    fees = numeric(),
    reason = character(),
    equity = numeric(),
    stringsAsFactors = FALSE
  )
  list(equity = equity_xts, positions = positions, trades = trades_df)
}

.bt_native_returns <- function(equity, normalize_risk = NULL) {
  values <- as.numeric(equity)
  disc <- c(0, values[-1L] / utils::head(values, -1L) - 1)
  disc[!is.finite(disc)] <- 0
  discrete <- xts::xts(disc, order.by = zoo::index(equity))
  colnames(discrete) <- "Discrete"
  log_xts <- xts::xts(log1p(disc), order.by = zoo::index(equity))
  colnames(log_xts) <- "Log"
  out <- cbind(discrete, log_xts)
  attr(out, "risk_scale") <- NA_real_
  attr(out, "risk_target") <- NA_real_
  attr(out, "risk_original") <- NA_real_
  if (!is.null(normalize_risk)) {
    target <- suppressWarnings(as.numeric(normalize_risk)[1])
    if (is.finite(target) && target > 0) {
      scaled <- tryCatch(bt_normalize_risk(log_xts, risk = target, type = "Log"), error = function(e) NULL)
      if (!is.null(scaled)) {
        log_xts <- scaled
        discrete <- xts::xts(exp(as.numeric(log_xts)) - 1, order.by = zoo::index(log_xts))
        colnames(discrete) <- "Discrete"
        out <- cbind(discrete, log_xts)
        attr(out, "risk_scale") <- attr(scaled, "scale_factor")
        attr(out, "risk_target") <- target
        attr(out, "risk_original") <- attr(scaled, "original_vol") * 100
      }
    }
  }
  out
}

.bt_native_stats <- function(equity, rets, trades, risk, execution, metadata, strategy) {
  eq <- as.numeric(equity)
  ret <- as.numeric(rets$Log)
  ret <- ret[is.finite(ret)]
  ppy <- .bt_native_periods_per_year(rets)
  total_return <- tail(eq, 1) / eq[1] - 1
  ann_return <- if (length(eq) > 1 && is.finite(ppy)) (tail(eq, 1) / eq[1])^(ppy / max(1, length(eq) - 1)) - 1 else NA_real_
  ann_vol <- if (length(ret) > 1 && is.finite(ppy)) stats::sd(ret) * sqrt(ppy) else NA_real_
  sharpe <- if (is.finite(ann_vol) && ann_vol > 0) ann_return / ann_vol else NA_real_
  dd <- 1 - eq / cummax(eq)
  max_dd <- max(dd, na.rm = TRUE)
  data.frame(
    num_trades = sum(trades$reason %in% c("long_entry", "short_entry")),
    total_return = total_return,
    annualized_return = ann_return,
    annualized_vol = ann_vol,
    sharpe = sharpe,
    max_drawdown = max_dd,
    net_profit = tail(eq, 1) - eq[1],
    fees = sum(trades$fees, na.rm = TRUE),
    PosSiz = risk$mode,
    RiskPct = risk$risk_pct,
    FeeMode = execution$fee,
    Multiplier = metadata$multiplier,
    TickSize = metadata$tick_size,
    Indicator = .bt_native_strategy_label(strategy),
    stringsAsFactors = FALSE
  )
}

.bt_native_periods_per_year <- function(x) {
  idx <- zoo::index(x)
  if (length(idx) < 2L) return(NA_real_)
  years <- format(as.POSIXct(idx, tz = "UTC"), "%Y")
  counts <- as.integer(table(years))
  counts <- counts[counts > 1L]
  if (length(counts)) return(stats::median(counts))
  dt <- stats::median(diff(as.numeric(idx)))
  if (!is.finite(dt) || dt <= 0) return(NA_real_)
  if (inherits(idx, "Date")) dt <- dt * 86400
  (365.25 * 86400) / dt
}

.bt_native_strategy_label <- function(strategy) {
  if (identical(strategy$type, "donchian")) {
    paste0("Donchian ", strategy$params$up, "/", strategy$params$down)
  } else {
    paste0(toupper(strategy$type), " ", strategy$params$fast, "/", strategy$params$slow)
  }
}

.bt_native_mktdata <- function(data, indicators, signals) {
  out <- cbind(data, indicators, signals)
  colnames(out) <- make.unique(colnames(out))
  out
}

.bt_native_strategy_grid <- function(space) {
  if (is.data.frame(space)) {
    if (!"type" %in% names(space)) space$type <- "donchian"
    return(space)
  }
  if (!is.list(space) || !length(space)) {
    stop("'space' must be a non-empty list or data frame.", call. = FALSE)
  }
  type <- space$type %||% "donchian"
  space$type <- NULL
  if (!length(space)) {
    space <- list(dummy = NA)
  }
  grid <- expand.grid(space, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if ("dummy" %in% names(grid)) grid$dummy <- NULL
  grid$type <- rep_len(type, NROW(grid))
  grid
}
