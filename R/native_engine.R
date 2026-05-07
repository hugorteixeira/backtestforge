#' Build a serializable trend-following strategy specification
#'
#' @param type Strategy type. Use `"eldoc"`/`"donchian"`, `"ema"`, or `"sma"`.
#' @param ... Indicator parameters (`up`/`down` for ElDoc, `fast`/`slow` for
#'   moving averages), plus ElDoc ATR/pyramiding settings.
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
      down = as.integer(.bt_first_non_null(args$down, args$mdown, 40L)),
      atr_n = as.integer(.bt_first_non_null(args$atr_n, args$atr, 20L)),
      atr_mult = as.numeric(.bt_first_non_null(args$atr_mult, 2)),
      pyramid = isTRUE(.bt_first_non_null(args$pyramid, FALSE)),
      pyramid_step = as.numeric(.bt_first_non_null(args$pyramid_step, 0.5)),
      max_units = as.integer(.bt_first_non_null(args$max_units, if (isTRUE(args$pyramid)) 4L else 1L))
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
  numeric_params <- unlist(params[!names(params) %in% "pyramid"], use.names = FALSE)
  if (any(!is.finite(numeric_params)) || any(numeric_params <= 0)) {
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
#' @param mode Sizing mode. `"risk"` uses an explicit sizing stop distance,
#'   `"notional"` uses `risk_pct` as capital allocation, and `"fixed"` uses
#'   `fixed_qty`.
#' @param initial_equity Starting equity.
#' @param risk_pct Percent of equity/capital to risk or allocate.
#' @param fixed_qty Quantity used by fixed sizing.
#' @param ps_type Public position-sizing type (`"eldoc"`, `"atr"`,
#'   `"notional"`, or `"contract"`).
#' @param stop_source Sizing stop source for risk mode (`"eldoc"` or `"atr"`).
#' @param atr_mult ATR multiple used when `stop_source = "atr"`.
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
                         initial_equity = 100000,
                         risk_pct = 2,
                         fixed_qty = 1,
                         ps_type = NULL,
                         stop_source = NULL,
                         atr_mult = 2,
                         max_qty = Inf,
                         max_leverage = Inf,
                         integer_qty = TRUE,
                         reinvest = TRUE,
                         min_risk_pct = 0.0005) {
  mode <- match.arg(mode)
  public_ps_type <- .bt_normalize_ps_type(ps_type) %||% switch(mode,
    risk = "eldoc",
    notional = "notional",
    fixed = "contract"
  )
  spec <- list(
    mode = mode,
    initial_equity = as.numeric(initial_equity)[1],
    risk_pct = as.numeric(risk_pct)[1],
    fixed_qty = as.numeric(fixed_qty)[1],
    ps_type = public_ps_type,
    stop_source = if (is.null(stop_source) && public_ps_type %in% c("eldoc", "atr")) public_ps_type else if (is.null(stop_source)) NULL else tolower(as.character(stop_source)[1]),
    atr_mult = as.numeric(atr_mult)[1],
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
  if (!is.finite(spec$atr_mult) || spec$atr_mult <= 0) {
    stop("'atr_mult' must be positive.", call. = FALSE)
  }
  if (!is.finite(spec$min_risk_pct) || spec$min_risk_pct <= 0) {
    stop("'min_risk_pct' must be positive.", call. = FALSE)
  }
  class(spec) <- c("bt_risk_spec", "list")
  spec
}

.bt_optional_non_negative_number <- function(value, name) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  num <- suppressWarnings(as.numeric(value)[1])
  if (!is.finite(num)) {
    return(NULL)
  }
  if (num < 0) {
    stop(sprintf("'%s' must be non-negative.", name), call. = FALSE)
  }
  num
}

.bt_normalize_fee_type <- function(fee_type) {
  if (is.null(fee_type) || length(fee_type) == 0) {
    return(NULL)
  }
  value <- trimws(as.character(fee_type)[1])
  if (is.na(value) || !nzchar(value)) {
    return(NULL)
  }
  clean <- tolower(gsub("[^A-Za-z0-9]", "", value))
  if (clean %in% c("contract", "contracts", "unit", "units", "percontract", "perunit")) {
    return("contract")
  }
  if (clean %in% c("order", "orders", "perorder", "trade", "pertrade")) {
    return("order")
  }
  stop("'fee_type' must be either 'contract' or 'order'.", call. = FALSE)
}

.bt_normalize_slippage_type <- function(slippage_type) {
  if (is.null(slippage_type) || length(slippage_type) == 0) {
    return(NULL)
  }
  value <- trimws(as.character(slippage_type)[1])
  if (is.na(value) || !nzchar(value)) {
    return(NULL)
  }
  clean <- tolower(gsub("[^A-Za-z0-9]", "", value))
  if (clean %in% c("bps", "bp", "basispoints", "basispoint")) {
    return("bps")
  }
  if (clean %in% c("tick", "ticks")) {
    return("ticks")
  }
  if (clean %in% c("point", "points", "pricepoint", "pricepoints", "price")) {
    return("points")
  }
  if (clean %in% c("cash", "money", "currency", "contract", "percontract")) {
    return("cash")
  }
  stop("'slip_type' must be one of 'bps', 'ticks', 'points', or 'cash'.", call. = FALSE)
}

.bt_normalize_ps_type <- function(ps_type) {
  if (is.null(ps_type) || length(ps_type) == 0) {
    return(NULL)
  }
  value <- trimws(as.character(ps_type)[1])
  if (is.na(value) || !nzchar(value)) {
    return(NULL)
  }
  clean <- tolower(gsub("[^A-Za-z0-9]", "", value))
  if (clean %in% c("eldoc", "donchian")) {
    return("eldoc")
  }
  if (clean %in% c("atr", "n", "vol", "volatility")) {
    return("atr")
  }
  if (clean %in% c("notional", "allocation", "capital", "capitalallocation")) {
    return("notional")
  }
  if (clean %in% c("contract", "contracts", "share", "shares", "fixed", "fixedqty", "fixedquantity", "qty")) {
    return("contract")
  }
  stop("'ps_type' must be one of 'eldoc', 'atr', 'notional', or 'contract'.", call. = FALSE)
}

.bt_ps_mode <- function(ps_type) {
  ps_type <- .bt_normalize_ps_type(ps_type)
  if (identical(ps_type, "contract")) {
    return("fixed")
  }
  if (identical(ps_type, "notional")) {
    return("notional")
  }
  "risk"
}

.bt_native_first_num <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[is.finite(vals)]
  if (length(vals)) vals[1] else NA_real_
}

.bt_native_first_chr <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- trimws(as.character(vals))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals)) vals[1] else NA_character_
}

.bt_native_attr_first <- function(data, names) {
  for (nm in names) {
    val <- attr(data, nm, exact = TRUE)
    if (!is.null(val) && length(val) > 0 && !all(is.na(val))) {
      return(val)
    }
  }
  NULL
}

.bt_native_list_first <- function(x, names) {
  if (is.environment(x)) x <- as.list(x)
  if (!is.list(x)) {
    return(NULL)
  }
  for (nm in names) {
    val <- x[[nm]]
    if (!is.null(val) && length(val) > 0 && !all(is.na(val))) {
      return(val)
    }
  }
  NULL
}

.bt_native_ps_metadata <- function(data) {
  list(
    value = .bt_native_first_num(.bt_native_attr_first(data, "ps_value")),
    type = .bt_native_first_chr(.bt_native_attr_first(data, "ps_type"))
  )
}

#' Build a native execution specification
#'
#' @param execution Execution mode. `"breakout"` executes ElDoc orders on the
#'   exact touched channel price. `"exact_price"` is an alias for `"breakout"`.
#'   `"same_close"` executes at the signal-bar close.
#'   `"next_open"`, `"next_close"`, and `"next_avg"` execute on the next bar.
#' @param fee Fee mode passed through `normalize_fee_mode()`.
#' @param fee_value Optional explicit commission amount in account currency.
#'   Interpreted according to `fee_type`. If `NULL`, ticker metadata is used.
#' @param fee_type Commission unit. `"contract"` charges `fee_value` per unit
#'   traded; `"order"` charges `fee_value` once per executed order. If `NULL`,
#'   ticker metadata is used.
#' @param commission_per_contract Optional explicit commission per unit traded.
#' @param commission_per_order Optional explicit commission per executed order.
#' @param slip_value Optional explicit slippage amount. Interpreted according to
#'   `slip_type`.
#' @param slip_type Slippage unit for `slip_value`: `"bps"`, `"ticks"`,
#'   `"points"`, or `"cash"`. If `NULL`, ticker metadata is used.
#' @param slippage_per_contract Optional explicit slippage per unit traded.
#' @param slippage_bps Optional explicit slippage in basis points. For DI
#'   contracts this means interest-rate basis points; for other instruments it
#'   is a basis-point fraction of execution price.
#' @param slippage_ticks Optional explicit slippage in ticks.
#' @param slippage_points Optional explicit slippage in quoted price points.
#' @param close_on_end Whether open positions are flattened on the last bar.
#' @return A list with class `bt_execution_spec`.
#' @export
bt_execution_spec <- function(execution = c("breakout", "same_close", "next_open", "next_close", "next_avg"),
                              fee = "normal",
                              fee_value = NULL,
                              fee_type = NULL,
                              commission_per_contract = NULL,
                              commission_per_order = NULL,
                              slip_value = NULL,
                              slip_type = NULL,
                              slippage_per_contract = NULL,
                              slippage_bps = NULL,
                              slippage_ticks = NULL,
                              slippage_points = NULL,
                              close_on_end = TRUE) {
  execution <- .bt_normalize_execution_mode(execution)
  delay <- if (startsWith(execution, "next_")) 1L else 0L
  fee_type <- .bt_normalize_fee_type(fee_type)
  fee_value <- .bt_optional_non_negative_number(fee_value, "fee_value")
  if (!is.null(fee_value) && fee_value == 0 && is.null(fee_type)) {
    fee_type <- "contract"
  }
  if (!is.null(fee_value) && !is.null(fee_type)) {
    if (identical(fee_type, "order")) {
      commission_per_order <- fee_value
      commission_per_contract <- NULL
    } else {
      commission_per_contract <- fee_value
      commission_per_order <- NULL
    }
  }
  slip_type <- .bt_normalize_slippage_type(slip_type)
  slip_value <- .bt_optional_non_negative_number(slip_value, "slip_value")
  if (!is.null(slip_value) && slip_value == 0 && is.null(slip_type)) {
    slip_type <- "cash"
  }
  if (!is.null(slip_value) && !is.null(slip_type)) {
    if (identical(slip_type, "bps")) {
      slippage_bps <- slip_value
    } else if (identical(slip_type, "ticks")) {
      slippage_ticks <- slip_value
    } else if (identical(slip_type, "points")) {
      slippage_points <- slip_value
    } else {
      slippage_per_contract <- slip_value
    }
  }
  spec <- list(
    execution = execution,
    delay = delay,
    fee = normalize_fee_mode(fee),
    fee_type = fee_type,
    fee_value = fee_value,
    commission_per_contract = commission_per_contract,
    commission_per_order = commission_per_order,
    slip_value = slip_value,
    slip_type = slip_type,
    slippage_per_contract = slippage_per_contract,
    slippage_bps = slippage_bps,
    slippage_ticks = slippage_ticks,
    slippage_points = slippage_points,
    close_on_end = isTRUE(close_on_end)
  )
  class(spec) <- c("bt_execution_spec", "list")
  spec
}

.bt_normalize_execution_mode <- function(execution) {
  execution <- as.character(execution)[1]
  clean <- tolower(gsub("[^A-Za-z0-9]", "", execution))
  out <- switch(clean,
    breakout = "breakout",
    exact = "breakout",
    exactprice = "breakout",
    price = "breakout",
    close = "same_close",
    sameclose = "same_close",
    nextopen = "next_open",
    open = "next_open",
    nextclose = "next_close",
    nextavg = "next_avg",
    nextaverage = "next_avg",
    average = "next_avg",
    avg = "next_avg",
    NA_character_
  )
  if (is.na(out)) {
    stop("'execution' must be one of 'breakout', 'same_close', 'next_open', 'next_close', or 'next_avg'.", call. = FALSE)
  }
  out
}

.bt_native_missing_arg_error <- function(symbol, missing, context) {
  stop(
    sprintf(
      "Missing %s metadata for '%s' (%s). Pass %s explicitly or add matching xts attrs.",
      context,
      symbol,
      paste(missing, collapse = ", "),
      paste(missing, collapse = "/")
    ),
    call. = FALSE
  )
}

.bt_native_resolve_risk <- function(risk, data, symbol, ps_value = NULL, ps_type = NULL,
                                    initial_equity = 100000, reinvest = TRUE,
                                    atr_mult = 2) {
  if (inherits(risk, "bt_risk_spec")) {
    return(risk)
  }
  if (!is.null(risk)) {
    stop("'risk' must be created by bt_risk_spec(), or left NULL for metadata/default-argument resolution.", call. = FALSE)
  }

  ps_meta <- .bt_native_ps_metadata(data)
  value <- .bt_native_first_num(ps_value, ps_meta$value)
  type <- .bt_normalize_ps_type(.bt_native_first_chr(ps_type, ps_meta$type))
  missing <- character()
  if (!is.finite(value) || value <= 0) missing <- c(missing, "ps_value")
  if (is.null(type)) missing <- c(missing, "ps_type")
  if (length(missing)) {
    .bt_native_missing_arg_error(symbol, missing, "position sizing")
  }

  mode <- .bt_ps_mode(type)
  stop_source <- if (identical(type, "atr")) {
    "atr"
  } else if (identical(type, "eldoc")) {
    "eldoc"
  } else {
    NULL
  }
  bt_risk_spec(
    mode = mode,
    initial_equity = initial_equity,
    risk_pct = value,
    fixed_qty = if (identical(mode, "fixed")) value else 1,
    ps_type = type,
    stop_source = stop_source,
    atr_mult = atr_mult,
    reinvest = reinvest
  )
}

.bt_native_resolve_execution <- function(execution, metadata, symbol) {
  if (!inherits(execution, "bt_execution_spec")) {
    stop("'execution' must be created by bt_execution_spec().", call. = FALSE)
  }
  if (identical(execution$fee, "nofee")) {
    execution$fee_type <- execution$fee_type %||% "contract"
    execution$commission_per_contract <- 0
    execution$commission_per_order <- NULL
    execution$fee_value <- execution$fee_value %||% 0
    execution$slippage_per_contract <- 0
    execution$slippage_bps <- NULL
    execution$slippage_ticks <- NULL
    execution$slippage_points <- NULL
    execution$slip_value <- execution$slip_value %||% 0
    execution$slip_type <- execution$slip_type %||% "cash"
    return(execution)
  }

  fee_value <- .bt_native_first_num(
    execution$fee_value,
    execution$commission_per_contract,
    execution$commission_per_order,
    metadata$fees
  )
  fee_type <- .bt_normalize_fee_type(.bt_native_first_chr(execution$fee_type, metadata$fee_type))
  if (is.finite(fee_value) && fee_value == 0 && is.null(fee_type)) {
    fee_type <- "contract"
  }
  missing_fee <- character()
  if (!is.finite(fee_value) || fee_value < 0) missing_fee <- c(missing_fee, "fee_value")
  if (is.null(fee_type)) missing_fee <- c(missing_fee, "fee_type")
  if (length(missing_fee)) {
    .bt_native_missing_arg_error(symbol, missing_fee, "fee")
  }
  execution$fee_value <- fee_value
  execution$fee_type <- fee_type
  if (identical(fee_type, "order")) {
    execution$commission_per_order <- fee_value
    execution$commission_per_contract <- NULL
  } else {
    execution$commission_per_contract <- fee_value
    execution$commission_per_order <- NULL
  }

  has_typed_slippage <- any(is.finite(c(
    .bt_native_first_num(execution$slippage_per_contract),
    .bt_native_first_num(execution$slippage_bps),
    .bt_native_first_num(execution$slippage_ticks),
    .bt_native_first_num(execution$slippage_points)
  )))

  if (!has_typed_slippage) {
    slip_value <- .bt_native_first_num(execution$slip_value)
    slip_type <- .bt_normalize_slippage_type(.bt_native_first_chr(execution$slip_type, metadata$slippage_unit))
    if (is.finite(slip_value) && slip_value >= 0) {
      if (slip_value == 0 && is.null(slip_type)) {
        slip_type <- "cash"
      }
      if (is.null(slip_type)) {
        .bt_native_missing_arg_error(symbol, "slip_type", "slippage")
      }
      if (identical(slip_type, "bps")) {
        execution$slippage_bps <- slip_value
      } else if (identical(slip_type, "ticks")) {
        execution$slippage_ticks <- slip_value
      } else if (identical(slip_type, "points")) {
        execution$slippage_points <- slip_value
      } else {
        execution$slippage_per_contract <- slip_value
      }
      execution$slip_type <- slip_type
    } else {
      if (is.finite(.bt_native_first_num(metadata$slippage_bps))) {
        execution$slip_value <- .bt_native_first_num(metadata$slippage_bps)
        execution$slippage_bps <- execution$slip_value
        execution$slip_type <- "bps"
      } else if (is.finite(.bt_native_first_num(metadata$slippage_ticks))) {
        execution$slip_value <- .bt_native_first_num(metadata$slippage_ticks)
        execution$slippage_ticks <- execution$slip_value
        execution$slip_type <- "ticks"
      } else if (is.finite(.bt_native_first_num(metadata$slippage_points))) {
        execution$slip_value <- .bt_native_first_num(metadata$slippage_points)
        execution$slippage_points <- execution$slip_value
        execution$slip_type <- "points"
      } else if (is.finite(.bt_native_first_num(metadata$slippage))) {
        execution$slip_value <- .bt_native_first_num(metadata$slippage)
        execution$slippage_per_contract <- execution$slip_value
        execution$slip_type <- .bt_normalize_slippage_type(metadata$slippage_unit)
        if (is.null(execution$slip_type)) {
          .bt_native_missing_arg_error(symbol, "slip_type", "slippage")
        }
      } else {
        .bt_native_missing_arg_error(symbol, c("slip_value", "slip_type"), "slippage")
      }
    }
  } else {
    execution$slip_type <- .bt_normalize_slippage_type(execution$slip_type) %||% execution$slip_type
  }

  execution
}

#' Run a native in-memory backtest
#'
#' @param ticker Character symbol or `xts` OHLC object.
#' @param data Optional preloaded `xts` object. Used when `ticker` is a label.
#' @param strategy A `bt_strategy_spec`, or a character strategy type.
#' @param risk Optional `bt_risk_spec`. If `NULL`, `ps_value`/`ps_type` are
#'   resolved from arguments or ticker metadata.
#' @param ps_value,ps_type Position-sizing value and type used when `risk` is
#'   `NULL`.
#' @param initial_equity Starting account equity when `risk` is `NULL`.
#' @param reinvest Whether risk sizing uses current equity when `risk` is `NULL`.
#' @param execution A `bt_execution_spec`.
#' @param start_date,end_date Optional date bounds used when fetching/subsetting.
#' @param normalize_risk Optional annualized risk target in percent.
#' @param geometric Kept for wrapper compatibility.
#' @param only_returns If `TRUE`, return only the `xts` returns object.
#' @param verbose If `TRUE`, print detailed stats and transactions.
#' @param clean_di If `TRUE`, apply DI cleanup before running.
#' @param report If `TRUE`, print the standard console performance tables.
#' @return A `bt_native_result` list, or an `xts` returns object when
#'   `only_returns = TRUE`. In `trades`, `fees` is commission only,
#'   `slippage` is slippage cost, and `total_cost` is their sum.
#' @export
bt_run_native <- function(ticker,
                          data = NULL,
                          strategy = bt_strategy_spec("donchian"),
                          risk = NULL,
                          ps_value = NULL,
                          ps_type = NULL,
                          initial_equity = 100000,
                          reinvest = TRUE,
                          execution = bt_execution_spec(),
                          start_date = "1900-01-01",
                          end_date = Sys.Date(),
                          normalize_risk = NULL,
                          geometric = TRUE,
                          only_returns = FALSE,
                          verbose = FALSE,
                          clean_di = TRUE,
                          report = TRUE) {
  start_t <- Sys.time()
  if (is.character(strategy)) {
    strategy <- bt_strategy_spec(strategy)
  }
  if (!inherits(strategy, "bt_strategy_spec")) {
    stop("'strategy' must be created by bt_strategy_spec().", call. = FALSE)
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
  risk <- .bt_native_resolve_risk(
    risk = risk,
    data = prepared$data,
    symbol = prepared$symbol,
    ps_value = ps_value,
    ps_type = ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    atr_mult = strategy$params$atr_mult %||% 2
  )
  execution <- .bt_native_resolve_execution(execution, metadata, prepared$symbol)

  if (isTRUE(report)) {
    .dbg("Is it DI?", .bt_is_di_symbol(prepared$symbol))
    .dbg(
      "Signals detected - Entry:",
      sum(as.logical(signals[, "LongEntry"]), na.rm = TRUE),
      " Exit:",
      sum(as.logical(signals[, "LongExit"]), na.rm = TRUE)
    )
  }

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

  if (isTRUE(report)) {
    .dbg("Generated orders:", NROW(sim$trades))
  }
  if (NROW(sim$trades) == 0) {
    warning("No generated order. Check price columns, signals, and instrument metadata.", call. = FALSE)
  }

  raw_rets <- .bt_native_returns(sim$equity, normalize_risk = NULL)
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
  attr(rets, "fee_mode") <- execution$fee

  if (isTRUE(report)) {
    .bt_native_report(
      symbol = prepared$symbol,
      strategy = strategy,
      raw_rets = raw_rets,
      rets = rets,
      equity = sim$equity,
      stats = stats,
      trades = sim$trades,
      geometric = geometric,
      verbose = verbose,
      start_time = start_t
    )
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
    equity = sim$equity,
    spec = list(strategy = strategy, risk = risk, execution = execution),
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
      clean_di = clean_di,
      report = FALSE
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
  di <- .bt_is_di_symbol(symbol)
  first_num <- function(...) {
    vals <- unlist(list(...), use.names = FALSE)
    vals <- suppressWarnings(as.numeric(vals))
    vals <- vals[is.finite(vals)]
    if (length(vals)) vals[1] else NA_real_
  }
  first_chr <- function(...) {
    vals <- unlist(list(...), use.names = FALSE)
    vals <- trimws(as.character(vals))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals)) vals[1] else NA_character_
  }
  fallback_one <- function(value, field, fallback = 1) {
    value <- suppressWarnings(as.numeric(value)[1])
    if (is.finite(value) && value > 0) {
      return(value)
    }
    missing_fields <<- c(missing_fields, field)
    fallback
  }
  missing_fields <- character()
  tick_size <- fallback_one(
    first_num(meta$tick_size, attr(data, "fut_tick_size", exact = TRUE), attr(data, "tick_size", exact = TRUE), attr(data, "ticksize", exact = TRUE)),
    "tick_size"
  )
  tick_value <- first_num(
    meta$tick_value,
    attr(data, "fut_tick_value", exact = TRUE),
    attr(data, "tick_value", exact = TRUE),
    attr(data, "tickvalue", exact = TRUE)
  )
  if (isTRUE(di) && is.finite(tick_value) && tick_value != 0) {
    tick_value <- abs(tick_value)
  }
  multiplier_raw <- first_num(meta$multiplier, attr(data, "fut_multiplier", exact = TRUE), attr(data, "multiplier", exact = TRUE))
  if (isTRUE(di)) {
    multiplier_raw <- 1
  } else if ((!is.finite(multiplier_raw) || multiplier_raw <= 0) &&
    is.finite(tick_value) && tick_value > 0 &&
    is.finite(tick_size) && tick_size > 0) {
    multiplier_raw <- tick_value / tick_size
  }
  multiplier <- fallback_one(multiplier_raw, "multiplier")
  if (!is.finite(tick_value) || tick_value <= 0) {
    tick_value <- tick_size * multiplier
  }
  fees <- first_num(attr(data, "fee_value", exact = TRUE))
  fee_type <- first_chr(
    meta$fee_type,
    attr(data, "fee_type", exact = TRUE)
  )
  slip_value <- first_num(
    meta$slip_value,
    attr(data, "slip_value", exact = TRUE)
  )
  slippage_bps <- first_num(
    meta$slippage_bps,
    attr(data, "slippage_bps", exact = TRUE)
  )
  slippage_ticks <- first_num(
    meta$slippage_ticks,
    attr(data, "slippage_ticks", exact = TRUE)
  )
  slippage_points <- first_num(
    meta$slippage_points,
    attr(data, "slippage_points", exact = TRUE)
  )
  slippage_cash <- NA_real_
  slippage_unit <- tolower(first_chr(
    meta$slippage_unit,
    attr(data, "slip_type", exact = TRUE)
  ))
  slippage_unit_missing <- is.na(slippage_unit) || !nzchar(slippage_unit)
  if (is.finite(slip_value) && slip_value >= 0 && !slippage_unit_missing) {
    if (slippage_unit %in% c("cash", "money", "currency", "per_contract")) {
      slippage_cash <- slip_value
    } else if (slippage_unit %in% c("point", "points", "price_point", "price_points", "price")) {
      slippage_points <- if (is.finite(slippage_points)) slippage_points else slip_value
    } else if (slippage_unit %in% c("tick", "ticks")) {
      slippage_ticks <- if (is.finite(slippage_ticks)) slippage_ticks else slip_value
    } else {
      slippage_bps <- if (is.finite(slippage_bps)) slippage_bps else slip_value
      slippage_unit <- "bps"
    }
  }
  if (!is.finite(slippage_bps) && !is.finite(slippage_ticks) &&
      !is.finite(slippage_points) && !is.finite(slippage_cash)) {
    slippage_cash <- NA_real_
  }
  if (length(missing_fields)) {
    warning(
      sprintf(
        "Missing native instrument metadata for '%s' (%s); using fallback defaults. Add xts attrs such as fut_multiplier and ticksize/tickvalue to avoid this.",
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
    fee_type = fee_type,
    slippage = slippage_cash,
    slip_value = slip_value,
    slippage_bps = slippage_bps,
    slippage_ticks = slippage_ticks,
    slippage_points = slippage_points,
    slippage_unit = slippage_unit,
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
  has_pu <- di && any(lower %in% c("pu_o", "pu_open", "pu_c", "pu_close"))
  pick_or <- function(candidates, fallback) {
    val <- pick(candidates, required = FALSE)
    if (all(!is.finite(val))) {
      fallback
    } else {
      ifelse(is.finite(val), val, fallback)
    }
  }
  open <- pick(c("Open", "open", "o"))
  high <- pick(c("High", "high", "h"))
  low <- pick(c("Low", "low", "l"))
  close <- pick(c("Close", "close", "c", "last", "settle", "settlement_price"))
  exec_open <- if (has_pu) pick_or(c("PU_o", "PU_open"), open) else open
  exec_high <- if (has_pu) pick_or(c("PU_h", "PU_high"), high) else high
  exec_low <- if (has_pu) pick_or(c("PU_l", "PU_low"), low) else low
  exec_close <- if (has_pu) pick_or(c("PU_c", "PU_close"), close) else close
  exec_avg <- rowMeans(cbind(exec_open, exec_high, exec_low, exec_close), na.rm = TRUE)
  exec_avg[!is.finite(exec_avg)] <- exec_close[!is.finite(exec_avg)]
  list(
    open = open,
    high = high,
    low = low,
    close = close,
    exec_open = exec_open,
    exec_high = exec_high,
    exec_low = exec_low,
    exec_close = exec_close,
    exec_avg = exec_avg,
    index = zoo::index(data),
    uses_pu = has_pu
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
    atr_n <- strategy$params$atr_n %||% 20L
    hlc <- cbind(High = prices$high, Low = prices$low, Close = prices$close)
    atr_raw <- if (is.finite(atr_n) && atr_n > 0 && atr_n <= n) {
      suppressWarnings(as.numeric(TTR::ATR(hlc, n = atr_n)[, "atr"]))
    } else {
      rep(NA_real_, n)
    }
    raw <- xts::xts(
      cbind(DonchianUpper = upper_raw, DonchianLower = lower_raw, ATR = atr_raw),
      order.by = idx
    )
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
  if (!is.finite(equity_basis) || equity_basis <= 0) {
    stop("Unable to compute valid equity for native position sizing.", call. = FALSE)
  }

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

.bt_native_slippage_per_contract <- function(price, execution, metadata, tick_value = NA_real_) {
  first_num <- function(...) {
    vals <- unlist(list(...), use.names = FALSE)
    vals <- suppressWarnings(as.numeric(vals))
    vals <- vals[is.finite(vals)]
    if (length(vals)) vals[1] else NA_real_
  }
  price <- suppressWarnings(as.numeric(price)[1])
  mult <- suppressWarnings(as.numeric(metadata$multiplier)[1])
  tick_size <- suppressWarnings(as.numeric(metadata$tick_size)[1])
  tick_value <- first_num(tick_value, metadata$tick_value, tick_size * mult)
  if (!is.finite(mult) || mult <= 0) mult <- 1
  if (!is.finite(tick_size) || tick_size <= 0) tick_size <- 1

  cash <- first_num(execution$slippage_per_contract)
  bps <- first_num(execution$slippage_bps)
  ticks <- first_num(execution$slippage_ticks)
  points <- first_num(execution$slippage_points)
  is_di <- .bt_is_di_symbol(metadata$symbol)

  if (is.finite(cash) && cash >= 0) {
    return(cash)
  }
  if (is.finite(bps) && bps >= 0) {
    if (is_di) {
      tick_bps <- tick_size * 100
      if (is.finite(tick_bps) && tick_bps > 0 && is.finite(tick_value) && tick_value > 0) {
        return((bps / tick_bps) * abs(tick_value))
      }
      return(0)
    }
    if (is.finite(price) && price > 0) {
      return(price * (bps / 10000) * mult)
    }
    return(0)
  }
  if (is.finite(ticks) && ticks >= 0) {
    if (is.finite(tick_value) && tick_value > 0) {
      return(ticks * abs(tick_value))
    }
    return(ticks * tick_size * mult)
  }
  if (is.finite(points) && points >= 0) {
    if (is_di && is.finite(tick_value) && tick_value > 0) {
      return((points / tick_size) * abs(tick_value))
    }
    return(points * mult)
  }

  bps <- first_num(metadata$slippage_bps)
  ticks <- first_num(metadata$slippage_ticks)
  points <- first_num(metadata$slippage_points)
  cash <- first_num(metadata$slippage)

  if (is.finite(bps) && bps >= 0) {
    if (is_di) {
      tick_bps <- tick_size * 100
      if (is.finite(tick_bps) && tick_bps > 0 && is.finite(tick_value) && tick_value > 0) {
        return((bps / tick_bps) * abs(tick_value))
      }
      return(0)
    }
    if (is.finite(price) && price > 0) {
      return(price * (bps / 10000) * mult)
    }
    return(0)
  }
  if (is.finite(ticks) && ticks >= 0) {
    if (is.finite(tick_value) && tick_value > 0) {
      return(ticks * abs(tick_value))
    }
    return(ticks * tick_size * mult)
  }
  if (is.finite(points) && points >= 0) {
    if (is_di && is.finite(tick_value) && tick_value > 0) {
      return((points / tick_size) * abs(tick_value))
    }
    return(points * mult)
  }
  if (is.finite(cash) && cash >= 0) {
    return(cash)
  }
  0
}

.bt_native_txn_cost_components <- function(qty_delta, price, execution, metadata, tick_value = NA_real_) {
  empty <- c(fees = 0, slippage = 0, total_cost = 0)
  if (identical(execution$fee, "nofee")) return(empty)
  qty_abs <- abs(as.numeric(qty_delta)[1])
  if (!is.finite(qty_abs) || qty_abs <= 0) return(empty)
  commission_per_order <- suppressWarnings(as.numeric(execution$commission_per_order)[1])
  commission <- execution$commission_per_contract
  if (is.null(commission) && !is.finite(commission_per_order)) commission <- metadata$fees
  slippage <- .bt_native_slippage_per_contract(price, execution, metadata, tick_value = tick_value)
  commission <- suppressWarnings(as.numeric(commission)[1])
  if (!is.finite(commission)) commission <- 0
  fees <- if (is.finite(commission_per_order)) {
    commission_per_order
  } else if (identical(execution$fee_type, "order")) {
    commission
  } else {
    qty_abs * commission
  }
  slippage <- qty_abs * slippage
  c(fees = fees, slippage = slippage, total_cost = fees + slippage)
}

.bt_native_txn_cost <- function(qty_delta, price, execution, metadata, tick_value = NA_real_) {
  unname(.bt_native_txn_cost_components(
    qty_delta = qty_delta,
    price = price,
    execution = execution,
    metadata = metadata,
    tick_value = tick_value
  )[["total_cost"]])
}

.bt_native_tick_value_at <- function(data, i, metadata) {
  nm <- tolower(colnames(data))
  idx <- match("tickvalue", nm, nomatch = 0L)
  if (idx > 0L) {
    val <- suppressWarnings(as.numeric(data[i, idx]))
    if (is.finite(val) && val != 0) return(abs(val))
  }
  if (!is.null(metadata$tick_value) && is.finite(metadata$tick_value) && metadata$tick_value > 0) {
    return(metadata$tick_value)
  }
  metadata$tick_size * metadata$multiplier
}

.bt_native_di_maturity <- function(data, symbol) {
  maturity <- attr(data, "maturity", exact = TRUE) %||%
    attr(data, "maturity_date", exact = TRUE) %||%
    attr(data, "expiry", exact = TRUE) %||%
    attr(data, "expiration", exact = TRUE)
  maturity <- suppressWarnings(as.Date(maturity)[1])
  if (!is.na(maturity)) {
    return(maturity)
  }
  if (requireNamespace("brfutures", quietly = TRUE)) {
    maturity <- tryCatch(
      brfutures::di_maturity_from_ticker(.bt_base_contract_symbol(symbol)),
      error = function(e) NA
    )
    maturity <- suppressWarnings(as.Date(maturity)[1])
    if (!is.na(maturity)) {
      return(maturity)
    }
  }
  NA
}

.bt_native_di_rate_to_pu <- function(rate, data, i, symbol, cal = NULL) {
  rate <- suppressWarnings(as.numeric(rate)[1])
  if (!is.finite(rate)) {
    return(NA_real_)
  }
  maturity <- .bt_native_di_maturity(data, symbol)
  if (is.na(maturity)) {
    return(NA_real_)
  }
  basis <- as.Date(zoo::index(data)[i])
  if (is.na(basis)) {
    return(NA_real_)
  }

  if (requireNamespace("brfutures", quietly = TRUE) &&
    exists("calculate_futures_di_notional", envir = asNamespace("brfutures"), inherits = FALSE)) {
    out <- tryCatch(
      brfutures::calculate_futures_di_notional(
        rate,
        maturity_date = maturity,
        basis_date = basis,
        round_pu = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(out)) {
      out <- tryCatch(
        brfutures::calculate_futures_di_notional(
          rate,
          maturity_date = maturity,
          basis_date = basis
        ),
        error = function(e) NULL
      )
    }
    pu <- suppressWarnings(as.numeric(out$pu)[1])
    if (is.finite(pu) && pu > 0) {
      return(pu)
    }
  }

  out <- tryCatch(
    .calculate_futures_di_notional(
      rate,
      maturity_date = maturity,
      basis_date = basis,
      cal = cal
    ),
    error = function(e) NULL
  )
  pu <- suppressWarnings(as.numeric(out$pu)[1])
  if (is.finite(pu) && pu > 0) pu else NA_real_
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

.bt_native_atr_value <- function(i, indicators) {
  if (is.null(indicators) || !"ATR" %in% colnames(indicators)) {
    return(NA_real_)
  }
  val <- suppressWarnings(as.numeric(indicators[i, "ATR"]))
  if (is.finite(val) && val > 0) val else NA_real_
}

.bt_native_sizing_stop_price <- function(i, side, price, indicators, strategy, risk, rate_price = price) {
  source <- risk$stop_source %||% if (identical(risk$ps_type, "atr")) "atr" else "eldoc"
  if (identical(source, "atr")) {
    atr <- .bt_native_atr_value(i, indicators)
    if (!is.finite(atr) || atr <= 0) {
      return(NA_real_)
    }
    basis <- suppressWarnings(as.numeric(rate_price)[1])
    if (!is.finite(basis) || basis <= 0) {
      basis <- suppressWarnings(as.numeric(price)[1])
    }
    if (!is.finite(basis) || basis <= 0) {
      return(NA_real_)
    }
    mult <- risk$atr_mult %||% strategy$params$atr_mult %||% 2
    if (identical(side, "long")) basis - atr * mult else basis + atr * mult
  } else {
    .bt_native_stop_price(i, side, indicators, strategy)
  }
}

.bt_native_clean_cost_components <- function(costs) {
  fees <- suppressWarnings(as.numeric(costs[["fees"]])[1])
  slippage <- suppressWarnings(as.numeric(costs[["slippage"]])[1])
  total_cost <- suppressWarnings(as.numeric(costs[["total_cost"]])[1])
  if (!is.finite(fees)) fees <- 0
  if (!is.finite(slippage)) slippage <- 0
  if (!is.finite(total_cost)) total_cost <- fees + slippage
  list(fees = fees, slippage = slippage, total_cost = total_cost)
}

.bt_native_trade_side <- function(qty) {
  if (qty > 0) return("long")
  if (qty < 0) return("short")
  "flat"
}

.bt_native_trade_row <- function(timestamp, symbol, qty, qty_delta, units, price, costs, reason, equity_after) {
  costs <- .bt_native_clean_cost_components(costs)
  data.frame(
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    symbol = symbol,
    side = .bt_native_trade_side(qty),
    qty = qty,
    qty_delta = qty_delta,
    units = units,
    price = price,
    fees = costs$fees,
    slippage = costs$slippage,
    total_cost = costs$total_cost,
    reason = reason,
    equity = equity_after,
    stringsAsFactors = FALSE
  )
}

.bt_native_empty_trade_frame <- function() {
  data.frame(
    timestamp = as.POSIXct(character()),
    symbol = character(),
    side = character(),
    qty = numeric(),
    qty_delta = numeric(),
    units = integer(),
    price = numeric(),
    fees = numeric(),
    slippage = numeric(),
    total_cost = numeric(),
    reason = character(),
    equity = numeric(),
    stringsAsFactors = FALSE
  )
}

.bt_native_simulate <- function(data, prices, indicators, signals, strategy, risk, execution, metadata, symbol) {
  n <- length(prices$close)
  idx <- prices$index
  exec_open <- if (!is.null(prices$exec_open)) prices$exec_open else prices$open
  exec_close <- if (!is.null(prices$exec_close)) prices$exec_close else prices$close
  exec_avg <- if (!is.null(prices$exec_avg)) prices$exec_avg else rowMeans(cbind(exec_open, exec_close), na.rm = TRUE)
  uses_pu <- isTRUE(prices$uses_pu) && .bt_is_di_symbol(symbol)
  pnl_multiplier <- metadata$multiplier * if (uses_pu) -1 else 1
  di_calendar <- if (uses_pu) tryCatch(.generate_calendar(), error = function(e) NULL) else NULL
  qty <- 0
  unit_count <- 0L
  last_add_price <- NA_real_
  last_add_signal_price <- NA_real_
  equity <- numeric(n)
  qty_path <- numeric(n)
  unit_path <- integer(n)
  equity[1] <- risk$initial_equity
  trades <- list()

  add_trade <- function(i, qty_delta, price, costs, reason, equity_after) {
    if (!is.finite(qty_delta) || qty_delta == 0) return(invisible(NULL))
    trades[[length(trades) + 1L]] <<- .bt_native_trade_row(
      timestamp = idx[i],
      symbol = symbol,
      qty = qty,
      qty_delta = qty_delta,
      units = unit_count,
      price = price,
      costs = costs,
      reason = reason,
      equity_after = equity_after
    )
    invisible(NULL)
  }

  execution_mode <- execution$execution
  exec_price <- switch(execution_mode,
    next_open = exec_open,
    next_close = exec_close,
    next_avg = exec_avg,
    same_close = exec_close,
    breakout = exec_close
  )
  executes_intrabar <- execution_mode %in% c("next_open", "next_avg")

  maybe_enter <- function(i, sig_i, side, eq, px) {
    size_px <- if (execution$delay > 0L) prices$close[sig_i] else px
    rate_px <- prices$close[sig_i]
    stop_px <- .bt_native_sizing_stop_price(
      sig_i,
      side,
      price = size_px,
      indicators = indicators,
      strategy = strategy,
      risk = risk,
      rate_price = rate_px
    )
    if (identical(strategy$type, "donchian") && identical(risk$mode, "risk") && !is.finite(stop_px)) {
      return(0)
    }
    if (uses_pu) {
      stop_pu <- .bt_native_di_rate_to_pu(stop_px, data, sig_i, symbol, cal = di_calendar)
      if (is.finite(stop_pu) && stop_pu > 0) {
        stop_px <- stop_pu
      }
      if (execution$delay > 0L) {
        size_pu <- .bt_native_di_rate_to_pu(size_px, data, sig_i, symbol, cal = di_calendar)
        if (is.finite(size_pu) && size_pu > 0) {
          size_px <- size_pu
        } else {
          size_px <- px
        }
      }
    }
    if (!is.finite(size_px) || size_px <= 0) {
      size_px <- px
    }
    tick_value <- .bt_native_tick_value_at(data, sig_i, metadata)
    .bt_native_size(side, size_px, stop_px, eq, risk, metadata, tick_value = tick_value)
  }

  signal_mode <- identical(execution_mode, "breakout") && identical(strategy$type, "donchian")

  stop_event_price <- function(sig_i, event) {
    column <- if (identical(event, "upper")) "DonchianUpper" else "DonchianLower"
    as.numeric(indicators[sig_i, column])
  }

  stop_event_exec_price <- function(sig_i, event, fallback) {
    rate_px <- stop_event_price(sig_i, event)
    if (!uses_pu) {
      return(rate_px)
    }
    pu <- .bt_native_di_rate_to_pu(rate_px, data, sig_i, symbol, cal = di_calendar)
    if (is.finite(pu) && pu > 0) pu else fallback
  }

  stop_event_sequence <- function(sig_i, qty_now) {
    upper_touched <- isTRUE(as.logical(signals[sig_i, "LongEntry"])) ||
      isTRUE(as.logical(signals[sig_i, "ShortExit"]))
    lower_touched <- isTRUE(as.logical(signals[sig_i, "LongExit"])) ||
      isTRUE(as.logical(signals[sig_i, "ShortEntry"]))
    if (!upper_touched && !lower_touched) {
      return(character())
    }
    if (upper_touched && lower_touched) {
      if (qty_now > 0) {
        return(c("lower", "upper"))
      }
      if (qty_now < 0) {
        return(c("upper", "lower"))
      }
      upper <- stop_event_price(sig_i, "upper")
      lower <- stop_event_price(sig_i, "lower")
      open_i <- prices$open[sig_i]
      if (!is.finite(upper) || !is.finite(lower) || !is.finite(open_i)) {
        return(character())
      }
      if (abs(open_i - upper) <= abs(open_i - lower)) {
        return(c("upper", "lower"))
      }
      return(c("lower", "upper"))
    }
    if (upper_touched) "upper" else "lower"
  }

  mark_to_price <- function(eq, from_px, to_px) {
    if (qty == 0 || !is.finite(from_px) || !is.finite(to_px)) {
      return(eq)
    }
    eq + qty * (to_px - from_px) * pnl_multiplier
  }

  txn_cost <- function(qty_delta, price, i) {
    tick_value <- .bt_native_tick_value_at(data, i, metadata)
    .bt_native_txn_cost_components(qty_delta, price, execution, metadata, tick_value = tick_value)
  }

  add_signal_basis <- function(sig_i, px, event = NULL) {
    if (!uses_pu) {
      return(px)
    }
    basis <- if (!is.null(event)) stop_event_price(sig_i, event) else prices$close[sig_i]
    basis <- suppressWarnings(as.numeric(basis)[1])
    if (is.finite(basis) && basis > 0) basis else NA_real_
  }

  process_stop_event <- function(i, sig_i, event, eq, px) {
    if (identical(event, "upper")) {
      if (qty < 0 && isTRUE(as.logical(signals[sig_i, "ShortExit"]))) {
        delta <- -qty
        costs <- txn_cost(delta, px, i)
        qty <<- 0
        unit_count <<- 0L
        last_add_price <<- NA_real_
        last_add_signal_price <<- NA_real_
        eq <- eq - costs[["total_cost"]]
        add_trade(i, delta, px, costs, "short_exit", eq)
      }
      if (qty == 0 && strategy$long && isTRUE(as.logical(signals[sig_i, "LongEntry"]))) {
        delta <- maybe_enter(i, sig_i, "long", eq, px)
        if (delta != 0) {
          costs <- txn_cost(delta, px, i)
          qty <<- qty + delta
          unit_count <<- 1L
          last_add_price <<- px
          last_add_signal_price <<- add_signal_basis(sig_i, px, event)
          eq <- eq - costs[["total_cost"]]
          add_trade(i, delta, px, costs, "long_entry", eq)
        }
      }
    } else {
      if (qty > 0 && isTRUE(as.logical(signals[sig_i, "LongExit"]))) {
        delta <- -qty
        costs <- txn_cost(delta, px, i)
        qty <<- 0
        unit_count <<- 0L
        last_add_price <<- NA_real_
        last_add_signal_price <<- NA_real_
        eq <- eq - costs[["total_cost"]]
        add_trade(i, delta, px, costs, "long_exit", eq)
      }
      if (qty == 0 && strategy$short && isTRUE(as.logical(signals[sig_i, "ShortEntry"]))) {
        delta <- maybe_enter(i, sig_i, "short", eq, px)
        if (delta != 0) {
          costs <- txn_cost(delta, px, i)
          qty <<- qty + delta
          unit_count <<- 1L
          last_add_price <<- px
          last_add_signal_price <<- add_signal_basis(sig_i, px, event)
          eq <- eq - costs[["total_cost"]]
          add_trade(i, delta, px, costs, "short_entry", eq)
        }
      }
    }
    eq
  }

  pyramid_enabled <- identical(strategy$type, "donchian") &&
    isTRUE(strategy$params$pyramid) &&
    is.finite(strategy$params$pyramid_step) &&
    strategy$params$pyramid_step > 0 &&
    is.finite(strategy$params$max_units) &&
    strategy$params$max_units > 1

  pyramid_trigger_price <- function(sig_i) {
    if (!pyramid_enabled || qty == 0 || unit_count >= strategy$params$max_units ||
        !is.finite(last_add_price)) {
      return(NA_real_)
    }
    basis <- if (uses_pu) last_add_signal_price else last_add_price
    if (!is.finite(basis) || basis <= 0) {
      return(NA_real_)
    }
    atr <- .bt_native_atr_value(sig_i, indicators)
    if (!is.finite(atr) || atr <= 0) {
      return(NA_real_)
    }
    if (qty > 0) {
      basis + strategy$params$pyramid_step * atr
    } else {
      basis - strategy$params$pyramid_step * atr
    }
  }

  pyramid_touched <- function(sig_i) {
    trigger <- pyramid_trigger_price(sig_i)
    if (!is.finite(trigger)) {
      return(c(signal_price = NA_real_, exec_price = NA_real_))
    }
    touched <- (qty > 0 && is.finite(prices$high[sig_i]) && prices$high[sig_i] >= trigger) ||
      (qty < 0 && is.finite(prices$low[sig_i]) && prices$low[sig_i] <= trigger)
    if (!touched) {
      return(c(signal_price = NA_real_, exec_price = NA_real_))
    }
    exec <- trigger
    if (uses_pu) {
      exec <- .bt_native_di_rate_to_pu(trigger, data, sig_i, symbol, cal = di_calendar)
    }
    if (!is.finite(exec) || exec <= 0) {
      exec <- NA_real_
    }
    c(signal_price = trigger, exec_price = exec)
  }

  process_pyramid <- function(i, sig_i, eq, px, signal_px = NA_real_) {
    if (!pyramid_enabled || qty == 0 || unit_count >= strategy$params$max_units) {
      return(eq)
    }
    side <- if (qty > 0) "long" else "short"
    delta <- maybe_enter(i, sig_i, side, eq, px)
    if (delta == 0 || !is.finite(delta)) {
      return(eq)
    }
    costs <- txn_cost(delta, px, i)
    qty <<- qty + delta
    unit_count <<- unit_count + 1L
    last_add_price <<- px
    last_add_signal_price <<- if (is.finite(signal_px)) {
      signal_px
    } else {
      add_signal_basis(sig_i, px)
    }
    eq <- eq - costs[["total_cost"]]
    add_trade(i, delta, px, costs, paste0(side, "_pyramid"), eq)
    eq
  }

  for (i in seq_len(n)) {
    if (i == 1L) {
      qty_path[i] <- qty
      unit_path[i] <- unit_count
      next
    }

    prev_close <- exec_close[i - 1L]
    close_i <- exec_close[i]
    px <- exec_price[i]
    if (!is.finite(px) || px <= 0) px <- close_i
    if (!is.finite(prev_close) || !is.finite(close_i)) {
      equity[i] <- equity[i - 1L]
      qty_path[i] <- qty
      unit_path[i] <- unit_count
      next
    }

    if (signal_mode) {
      eq <- equity[i - 1L]
      last_px <- prev_close
      events <- stop_event_sequence(i, qty)
      for (event in events) {
        px <- stop_event_exec_price(i, event, fallback = close_i)
        if (!is.finite(px) || px <= 0) {
          next
        }
        eq <- mark_to_price(eq, last_px, px)
        eq <- process_stop_event(i, i, event, eq, px)
        last_px <- px
      }
      while (pyramid_enabled && qty != 0 && unit_count < strategy$params$max_units) {
        pyr <- pyramid_touched(i)
        pyr_px <- pyr[["exec_price"]]
        if (!is.finite(pyr_px)) {
          break
        }
        if (uses_pu) {
          if (qty > 0 && pyr_px > last_px) {
            pyr_px <- last_px
          } else if (qty < 0 && pyr_px < last_px) {
            pyr_px <- last_px
          }
        } else {
          if (qty > 0 && pyr_px < last_px) {
            pyr_px <- last_px
          } else if (qty < 0 && pyr_px > last_px) {
            pyr_px <- last_px
          }
        }
        eq <- mark_to_price(eq, last_px, pyr_px)
        eq <- process_pyramid(i, i, eq, pyr_px, signal_px = pyr[["signal_price"]])
        last_px <- pyr_px
      }
      eq <- mark_to_price(eq, last_px, close_i)
      equity[i] <- eq
      qty_path[i] <- qty
      unit_path[i] <- unit_count
      next
    }

    eq <- equity[i - 1L]
    if (executes_intrabar) {
      eq <- eq + qty * (px - prev_close) * pnl_multiplier
    } else {
      eq <- eq + qty * (close_i - prev_close) * pnl_multiplier
    }

    sig_i <- i - execution$delay
    if (sig_i >= 1L && sig_i <= n && is.finite(px) && px > 0) {
      le <- isTRUE(as.logical(signals[sig_i, "LongEntry"]))
      lx <- isTRUE(as.logical(signals[sig_i, "LongExit"]))
      se <- isTRUE(as.logical(signals[sig_i, "ShortEntry"]))
      sx <- isTRUE(as.logical(signals[sig_i, "ShortExit"]))

      qty_before_signal <- qty
      exited_long <- FALSE
      exited_short <- FALSE

      if (qty > 0 && lx) {
        delta <- -qty
        costs <- txn_cost(delta, px, i)
        qty <- 0
        unit_count <- 0L
        last_add_price <- NA_real_
        last_add_signal_price <- NA_real_
        eq <- eq - costs[["total_cost"]]
        exited_long <- TRUE
        add_trade(i, delta, px, costs, "long_exit", eq)
      }
      if (qty < 0 && sx) {
        delta <- -qty
        costs <- txn_cost(delta, px, i)
        qty <- 0
        unit_count <- 0L
        last_add_price <- NA_real_
        last_add_signal_price <- NA_real_
        eq <- eq - costs[["total_cost"]]
        exited_short <- TRUE
        add_trade(i, delta, px, costs, "short_exit", eq)
      }

      if (qty == 0) {
        entry_side <- NULL
        if (qty_before_signal > 0 && exited_long) {
          if (strategy$short && se) {
            entry_side <- "short"
          } else if (strategy$long && le && !(strategy$short && se)) {
            entry_side <- "long"
          }
        } else if (qty_before_signal < 0 && exited_short) {
          if (strategy$long && le) {
            entry_side <- "long"
          } else if (strategy$short && se && !(strategy$long && le)) {
            entry_side <- "short"
          }
        } else if (strategy$long && le && !(strategy$short && se)) {
          entry_side <- "long"
        } else if (strategy$short && se && !(strategy$long && le)) {
          entry_side <- "short"
        }

        if (!is.null(entry_side)) {
          delta <- maybe_enter(i, sig_i, entry_side, eq, px)
          if (delta != 0) {
            costs <- txn_cost(delta, px, i)
            qty <- qty + delta
            unit_count <- 1L
            last_add_price <- px
            last_add_signal_price <- add_signal_basis(sig_i, px)
            eq <- eq - costs[["total_cost"]]
            add_trade(i, delta, px, costs, paste0(entry_side, "_entry"), eq)
          }
        }
      }
      if (qty != 0 && pyramid_enabled && unit_count < strategy$params$max_units) {
        pyr <- if (execution$delay > 0L) pyramid_touched(sig_i) else pyramid_touched(i)
        pyr_px <- pyr[["exec_price"]]
        if (is.finite(pyr_px)) {
          pyramid_exec_px <- if (identical(execution_mode, "breakout")) pyr_px else px
          eq <- process_pyramid(i, sig_i, eq, pyramid_exec_px, signal_px = pyr[["signal_price"]])
        }
      }
    }

    if (executes_intrabar) {
      eq <- eq + qty * (close_i - px) * pnl_multiplier
    }
    equity[i] <- eq
    qty_path[i] <- qty
    unit_path[i] <- unit_count
  }

  if (isTRUE(execution$close_on_end) && qty != 0 && n > 0) {
    px <- exec_close[n]
    if (is.finite(px) && px > 0) {
      delta <- -qty
      costs <- txn_cost(delta, px, n)
      qty <- 0
      unit_count <- 0L
      last_add_price <- NA_real_
      last_add_signal_price <- NA_real_
      equity[n] <- equity[n] - costs[["total_cost"]]
      qty_path[n] <- qty
      unit_path[n] <- unit_count
      add_trade(n, delta, px, costs, "end_exit", equity[n])
    }
  }

  equity_xts <- xts::xts(equity, order.by = idx)
  colnames(equity_xts) <- "Equity"
  positions <- xts::xts(
    cbind(
      qty = qty_path,
      units = unit_path,
      close = exec_close,
      equity = equity,
      position_value = qty_path * exec_close * pnl_multiplier,
      signal_close = prices$close
    ),
    order.by = idx
  )
  trades_df <- if (length(trades)) do.call(rbind, trades) else .bt_native_empty_trade_frame()
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

.bt_native_report <- function(symbol, strategy, raw_rets, rets, equity, stats, trades,
                              geometric = TRUE, verbose = FALSE, start_time = Sys.time()) {
  .dbg("Results for ", symbol, " - ", .bt_native_report_type(strategy), "\n")
  if (isTRUE(verbose)) {
    print(stats)
    cat("\n")
    print(trades)
    cat("\n")
  }

  tab <- .bt_native_table_monthly_returns(raw_rets$Discrete, geometric = geometric)
  cat("--- Monthly Returns (Geometric) ---\n")
  print(tab)
  .table_quarterly_returns(raw_rets$Discrete, return_data = TRUE, geometric = geometric)
  .table_quarterly_profit(.bt_native_profit_object(equity), return_data = TRUE)
  .bt_native_print_cost_summary(stats, trades)
  .bt_native_print_returns_summary(rets, geometric = geometric)

  total_secs <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))
  hrs <- total_secs %/% 3600
  mins <- (total_secs %% 3600) %/% 60
  secs <- total_secs %% 60

  cat("\n-----------------------------------\n")
  cat(sprintf("Runtime: %02dh %02dm %02ds", hrs, mins, secs))
  cat("\n-----------------------------------\n\n")

  invisible(NULL)
}

.bt_native_format_money <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) {
    return("")
  }
  format(
    round(x, digits),
    big.mark = ".",
    decimal.mark = ",",
    nsmall = digits,
    scientific = FALSE,
    trim = TRUE
  )
}

.bt_native_format_pct <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) {
    return("")
  }
  paste0(format(round(x * 100, digits), decimal.mark = ".", nsmall = digits, scientific = FALSE, trim = TRUE), "%")
}

.bt_native_cost_summary_table <- function(stats, trades) {
  pick_stat <- function(name, default = 0) {
    if (is.null(stats) || !name %in% names(stats)) {
      return(default)
    }
    val <- suppressWarnings(as.numeric(stats[[name]][1]))
    if (is.finite(val)) val else default
  }
  total_fees <- pick_stat("fees")
  total_slippage <- pick_stat("slippage")
  total_cost <- pick_stat("total_cost", total_fees + total_slippage)
  net_profit <- pick_stat("net_profit")
  gross_before_costs <- net_profit + total_cost
  trade_count <- pick_stat("num_trades", NROW(trades))
  impact_den <- abs(gross_before_costs)
  impact <- function(cost) {
    if (is.finite(impact_den) && impact_den > 0) cost / impact_den else NA_real_
  }
  impact_basis <- if (!is.finite(gross_before_costs) || gross_before_costs == 0) {
    "n/a"
  } else if (gross_before_costs > 0) {
    "profit consumed"
  } else {
    "loss worsened"
  }

  rows <- c(
    "Trades",
    "Fees",
    "Slippage",
    "Gross P/L",
    "Net P/L",
    "Impact Basis",
    "Fees Impact",
    "Slippage Impact",
    "Total Cost Impact"
  )
  vals <- c(
    format(trade_count, big.mark = ".", decimal.mark = ",", scientific = FALSE),
    .bt_native_format_money(total_fees),
    .bt_native_format_money(total_slippage),
    .bt_native_format_money(gross_before_costs),
    .bt_native_format_money(net_profit),
    impact_basis,
    .bt_native_format_pct(impact(total_fees)),
    .bt_native_format_pct(impact(total_slippage)),
    .bt_native_format_pct(impact(total_cost))
  )
  out <- matrix(vals, ncol = 1, dimnames = list(rows, "Value"))
  out
}

.bt_native_print_cost_summary <- function(stats, trades) {
  cat("\n--- Costs & Slippage Summary ---\n")
  print(.bt_native_cost_summary_table(stats, trades), quote = FALSE, right = TRUE)
  cat("\n")
  invisible(NULL)
}

.bt_native_table_monthly_returns <- function(returns_xts, geometric = TRUE) {
  idx <- zoo::index(returns_xts)
  vals <- as.numeric(returns_xts)
  years <- unique(format(as.POSIXct(idx, tz = "UTC"), "%Y"))
  mat <- matrix("", nrow = length(years), ncol = 13)
  rownames(mat) <- years
  colnames(mat) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total")

  fmt <- function(x) {
    if (!is.finite(x)) return("")
    format(round(x * 100, 1), trim = TRUE, scientific = FALSE)
  }
  agg_month <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    if (isTRUE(geometric)) prod(1 + x) - 1 else sum(x)
  }
  agg_total <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(NA_real_)
    if (isTRUE(geometric)) prod(1 + x) - 1 else sum(x)
  }

  months <- as.integer(format(as.POSIXct(idx, tz = "UTC"), "%m"))
  for (yr in years) {
    yr_idx <- format(as.POSIXct(idx, tz = "UTC"), "%Y") == yr
    month_vals <- rep(NA_real_, 12)
    for (m in seq_len(12)) {
      month_vals[m] <- agg_month(vals[yr_idx & months == m])
      mat[yr, m] <- fmt(month_vals[m])
    }
    mat[yr, 13] <- fmt(agg_total(month_vals))
  }

  as.data.frame(mat)
}

.bt_native_report_type <- function(strategy) {
  switch(strategy$type,
    donchian = "ELDOC",
    ema = "EMA",
    sma = "SMA",
    toupper(strategy$type)
  )
}

.bt_native_profit_object <- function(equity) {
  vals <- as.numeric(equity)
  pnl <- c(0, diff(vals))
  summary <- xts::xts(cbind(Net.Trading.PL = pnl), order.by = zoo::index(equity))
  list(summary = summary)
}

.bt_native_print_returns_summary <- function(rets, geometric = TRUE) {
  ppy <- .bt_native_periods_per_year(rets)
  disc <- .bt_native_return_summary_values(as.numeric(rets$Discrete), ppy, geometric = geometric)
  log_ret <- .bt_native_return_summary_values(as.numeric(rets$Log), ppy, geometric = geometric)

  res_table <- matrix(
    sprintf("%.4f%%", c(disc$annualized, log_ret$annualized, disc$cumulative, log_ret$cumulative) * 100),
    nrow = 2,
    dimnames = list(c("Discrete", "Log"), c("Annual", "Total"))
  )

  cat(paste0("\n--- Returns Summary ", if (geometric) "(Geometric)" else "(Simple)", " ---\n"))
  print(res_table, quote = FALSE, right = TRUE)
  cat("\n")

  invisible(res_table)
}

.bt_native_return_summary_values <- function(x, periods_per_year, geometric = TRUE) {
  .bt_return_summary_values(x, periods_per_year, geometric = geometric)
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
  trade_sum <- function(name) {
    if (!name %in% names(trades)) return(0)
    sum(suppressWarnings(as.numeric(trades[[name]])), na.rm = TRUE)
  }
  fees <- trade_sum("fees")
  slippage <- trade_sum("slippage")
  total_cost <- if ("total_cost" %in% names(trades)) trade_sum("total_cost") else fees + slippage
  data.frame(
    num_trades = sum(trades$reason %in% c("long_entry", "short_entry")),
    total_return = total_return,
    annualized_return = ann_return,
    annualized_vol = ann_vol,
    sharpe = sharpe,
    max_drawdown = max_dd,
    net_profit = tail(eq, 1) - eq[1],
    fees = fees,
    slippage = slippage,
    total_cost = total_cost,
    InitialEquity = risk$initial_equity,
    PosSiz = risk$ps_type %||% risk$mode,
    PsValue = if (identical(risk$mode, "fixed")) risk$fixed_qty else risk$risk_pct,
    RiskPct = risk$risk_pct,
    FeeMode = execution$fee,
    FeeType = execution$fee_type,
    Multiplier = metadata$multiplier,
    TickSize = metadata$tick_size,
    Indicator = .bt_native_strategy_label(strategy),
    stringsAsFactors = FALSE
  )
}

.bt_native_periods_per_year <- function(x) {
  .bt_periods_per_year(x)
}

.bt_native_strategy_label <- function(strategy) {
  if (identical(strategy$type, "donchian")) {
    paste0("ElDoc ", strategy$params$up, "/", strategy$params$down)
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
