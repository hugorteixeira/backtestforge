#' Build a serializable trend-following strategy specification
#'
#' @param type Strategy type. Use `"eldoc"`/`"donchian"`, `"ema"`, `"sma"`,
#'   `"tsmom"`, or `"hold"`.
#' @param ... Indicator parameters (`up`/`down` for ElDoc, `fast`/`slow` for
#'   moving averages, `lookback` for TSMOM), plus advanced ElDoc settings.
#' @param long,short Logical flags enabling long and short trades.
#' @param invert_signals Logical; swaps entry and exit signals for experiments.
#' @return A list with class `bt_strategy_spec`.
#' @export
bt_strategy_spec <- function(type = c("donchian", "eldoc", "ema", "sma", "tsmom", "hold"),
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
    tsmom = "tsmom",
    timeseriesmomentum = "tsmom",
    time_series_momentum = "tsmom",
    hold = "hold",
    buyandhold = "hold",
    buy_and_hold = "hold",
    stop(sprintf("Unsupported native strategy type '%s'.", raw_type), call. = FALSE)
  )
  args <- list(...)
  params <- switch(canonical,
    donchian = {
      pyramid <- isTRUE(.bt_first_non_null(args$pyramid, FALSE))
      pyramid_step <- as.numeric(.bt_first_non_null(args$pyramid_step, 0.5))
      pyramid_start <- as.numeric(.bt_first_non_null(args$pyramid_start, pyramid_step))
      pyramid_sizing <- tolower(as.character(.bt_first_non_null(args$pyramid_sizing, "risk"))[1])
      pyramid_sizing <- switch(pyramid_sizing,
        risk = "risk",
        entry = "entry_qty",
        entry_qty = "entry_qty",
        entryqty = "entry_qty",
        stop(sprintf("Unsupported pyramid_sizing '%s'. Use 'risk' or 'entry_qty'.", pyramid_sizing), call. = FALSE)
      )
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
    ema = list(
      fast = as.integer(.bt_first_non_null(args$fast, args$mup, args$up, 20L)),
      slow = as.integer(.bt_first_non_null(args$slow, args$mdown, args$down, 50L))
    ),
    sma = list(
      fast = as.integer(.bt_first_non_null(args$fast, args$mup, args$up, 20L)),
      slow = as.integer(.bt_first_non_null(args$slow, args$mdown, args$down, 50L))
    ),
    tsmom = list(
      lookback = as.integer(.bt_first_non_null(args$lookback, args$mup, args$up, 252L)),
      threshold = as.numeric(.bt_first_non_null(args$threshold, 0)),
      atr_n = as.integer(.bt_first_non_null(args$atr_n, args$atr, 20L))
    ),
    hold = list()
  )
  numeric_params <- unlist(params[!names(params) %in% c("pyramid", "pyramid_sizing", "threshold")], use.names = FALSE)
  if (any(!is.finite(numeric_params)) || any(numeric_params <= 0)) {
    stop("Strategy numeric parameters must be positive finite values.", call. = FALSE)
  }
  if ("threshold" %in% names(params) &&
      (!is.finite(params$threshold) || params$threshold < 0)) {
    stop("'threshold' must be a non-negative finite value.", call. = FALSE)
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
#' The returned spec tells the native simulator which sizing mode and constraints
#' to use. The final quantity calculation is delegated to `positionsizer`; this
#' package keeps strategy stop selection, execution timing, and ledger handling.
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
  if (clean %in% c("percent", "percentage", "pct", "perc")) {
    return("percent")
  }
  if (clean %in% c("bps", "bp", "basispoints", "basispoint")) {
    return("bps")
  }
  stop("'fee_type' must be one of 'contract', 'order', 'percent', or 'bps'.", call. = FALSE)
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

.bt_normalize_execution_timeframe <- function(timeframe) {
  if (is.null(timeframe) || length(timeframe) == 0) {
    return("same")
  }
  value <- trimws(as.character(timeframe)[1])
  if (is.na(value) || !nzchar(value)) {
    return("same")
  }
  clean <- tolower(gsub("[^A-Za-z0-9]", "", value))
  out <- switch(clean,
    same = "same",
    mesmo = "same",
    signal = "same",
    signaltimeframe = "same",
    current = "same",
    `5m` = "5m",
    `5min` = "5m",
    `5minute` = "5m",
    `5minutes` = "5m",
    `5minutos` = "5m",
    `1h` = "1h",
    `1hr` = "1h",
    `1hour` = "1h",
    `1hora` = "1h",
    `1horas` = "1h",
    `4h` = "4h",
    `4hr` = "4h",
    `4hour` = "4h",
    `4hours` = "4h",
    `4hora` = "4h",
    `4horas` = "4h",
    NA_character_
  )
  if (is.na(out)) {
    stop("'execution_timeframe' must be one of 'same', '5m', '1h', or '4h'.", call. = FALSE)
  }
  out
}

.bt_timeframe_seconds <- function(timeframe) {
  tf <- .bt_normalize_execution_timeframe(timeframe)
  switch(tf,
    same = NA_real_,
    `5m` = 5 * 60,
    `1h` = 60 * 60,
    `4h` = 4 * 60 * 60,
    NA_real_
  )
}

.bt_normalize_data_timeframe <- function(timeframe) {
  if (is.null(timeframe) || length(timeframe) == 0) {
    return(NA_character_)
  }
  value <- trimws(as.character(timeframe)[1])
  if (is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  clean <- tolower(gsub("[^A-Za-z0-9]", "", value))
  switch(clean,
    `5m` = "5m",
    `5min` = "5m",
    `1h` = "1h",
    `1hr` = "1h",
    `4h` = "4h",
    `4hr` = "4h",
    `1d` = "1d",
    `1day` = "1d",
    daily = "1d",
    d = "1d",
    NA_character_
  )
}

.bt_data_timeframe_seconds <- function(timeframe) {
  tf <- .bt_normalize_data_timeframe(timeframe)
  switch(tf,
    `5m` = 5 * 60,
    `1h` = 60 * 60,
    `4h` = 4 * 60 * 60,
    `1d` = 24 * 60 * 60,
    NA_real_
  )
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

.bt_native_clean_name <- function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", as.character(x)))
}

.bt_native_pick_named_col <- function(nms, candidates) {
  if (!length(nms)) {
    return(NA_character_)
  }
  clean_nms <- .bt_native_clean_name(nms)
  clean_candidates <- .bt_native_clean_name(candidates)
  hit <- match(clean_candidates, clean_nms, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (length(hit)) {
    return(nms[[hit[[1]]]])
  }
  NA_character_
}

.bt_native_empty_funding_frame <- function() {
  data.frame(
    timestamp = as.POSIXct(character(), tz = "UTC"),
    funding_rate = numeric(),
    mark_price = numeric(),
    stringsAsFactors = FALSE
  )
}

.bt_native_normalize_funding_object <- function(x) {
  if (is.null(x) || identical(x, FALSE)) {
    return(.bt_native_empty_funding_frame())
  }
  if (xts::is.xts(x)) {
    df <- as.data.frame(zoo::coredata(x), stringsAsFactors = FALSE)
    df$timestamp <- as.POSIXct(zoo::index(x), tz = "UTC")
  } else if (is.data.frame(x)) {
    df <- x
  } else {
    return(.bt_native_empty_funding_frame())
  }
  if (!nrow(df)) {
    return(.bt_native_empty_funding_frame())
  }

  nms <- names(df)
  time_col <- .bt_native_pick_named_col(nms, c("timestamp", "date", "datetime", "time"))
  rate_col <- .bt_native_pick_named_col(nms, c("funding_rate", "fundingrate", "FundingRate", "rate"))
  mark_col <- .bt_native_pick_named_col(nms, c("mark_price", "markprice", "FundingMarkPrice", "price"))
  events_col <- .bt_native_pick_named_col(nms, c("funding_events", "FundingEvents", "events"))
  if (is.na(time_col) || is.na(rate_col)) {
    return(.bt_native_empty_funding_frame())
  }

  out <- data.frame(
    timestamp = as.POSIXct(df[[time_col]], tz = "UTC"),
    funding_rate = suppressWarnings(as.numeric(df[[rate_col]])),
    mark_price = if (!is.na(mark_col)) suppressWarnings(as.numeric(df[[mark_col]])) else NA_real_,
    stringsAsFactors = FALSE
  )
  if (!is.na(events_col)) {
    event_count <- suppressWarnings(as.numeric(df[[events_col]]))
    keep_events <- !is.finite(event_count) | event_count != 0 | out$funding_rate != 0
    out <- out[keep_events, , drop = FALSE]
  }
  out <- out[!is.na(out$timestamp) & is.finite(out$funding_rate), , drop = FALSE]
  if (!nrow(out)) {
    return(.bt_native_empty_funding_frame())
  }
  out <- out[order(out$timestamp), , drop = FALSE]
  split_idx <- split(seq_len(nrow(out)), as.character(out$timestamp))
  out <- do.call(rbind, lapply(split_idx, function(idx) {
    rows <- out[idx, , drop = FALSE]
    mark <- rows$mark_price[is.finite(rows$mark_price) & rows$mark_price > 0]
    data.frame(
      timestamp = rows$timestamp[[1]],
      funding_rate = sum(rows$funding_rate, na.rm = TRUE),
      mark_price = if (length(mark)) mark[[length(mark)]] else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out[order(out$timestamp), , drop = FALSE]
}

.bt_native_funding_from_list <- function(funding, symbol) {
  if (!is.list(funding) || is.data.frame(funding) || xts::is.xts(funding)) {
    return(NULL)
  }
  keys <- unique(c(
    symbol,
    paste0(.bt_base_contract_symbol(symbol), "_PERPETUAL"),
    .bt_base_contract_symbol(symbol)
  ))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  for (key in keys) {
    if (!is.null(funding[[key]])) {
      return(funding[[key]])
    }
  }
  if (length(funding)) funding[[1]] else NULL
}

.bt_native_funding_from_data <- function(data) {
  direct <- attr(data, "funding", exact = TRUE) %||%
    attr(data, "funding_events", exact = TRUE)
  out <- .bt_native_normalize_funding_object(direct)
  if (NROW(out)) {
    return(out)
  }
  .bt_native_normalize_funding_object(data)
}

.bt_native_perpetual_funding_symbol <- function(symbol) {
  symbol <- toupper(trimws(as.character(symbol)[1]))
  if (is.na(symbol) || !nzchar(symbol) || !grepl("(^|_)PERPETUAL(_|$)", symbol)) {
    return(NA_character_)
  }
  clean <- sub("^CONTINUOUS_", "", symbol)
  base <- strsplit(clean, "_", fixed = TRUE)[[1]][1]
  if (!nzchar(base)) {
    return(NA_character_)
  }
  paste0(base, "_PERPETUAL")
}

.bt_native_fetch_finharvest_funding <- function(symbol, start_date, end_date) {
  funding_symbol <- .bt_native_perpetual_funding_symbol(symbol)
  if (is.na(funding_symbol) || !nzchar(funding_symbol)) {
    return(.bt_native_empty_funding_frame())
  }
  if (!requireNamespace("finharvest", quietly = TRUE)) {
    stop("`funding = TRUE` requires package 'finharvest' to fetch funding events.", call. = FALSE)
  }
  funding <- finharvest::finget_binance_fut_funding(
    tickers = funding_symbol,
    start_date = start_date,
    end_date = end_date,
    tz = "UTC",
    tz_mode = "preserve",
    verbose = FALSE
  )
  if (xts::is.xts(funding) || is.data.frame(funding)) {
    return(funding)
  }
  if (is.list(funding)) {
    if (!is.null(funding[[funding_symbol]])) {
      return(funding[[funding_symbol]])
    }
    if (length(funding)) {
      return(funding[[1]])
    }
  }
  NULL
}

.bt_native_resolve_funding <- function(funding, data, symbol, start_date, end_date) {
  if (is.null(funding) || identical(funding, FALSE)) {
    return(.bt_native_empty_funding_frame())
  }
  source <- if (identical(funding, TRUE)) {
    data_out <- .bt_native_funding_from_data(data)
    if (NROW(data_out)) {
      data_out
    } else {
      .bt_native_normalize_funding_object(
        .bt_native_fetch_finharvest_funding(symbol, start_date, end_date)
      )
    }
  } else if (is.character(funding) && length(funding) == 1L && nzchar(funding)) {
    .bt_native_normalize_funding_object(
      .bt_native_fetch_finharvest_funding(funding, start_date, end_date)
    )
  } else {
    explicit <- .bt_native_funding_from_list(funding, symbol)
    if (is.null(explicit)) explicit <- funding
    .bt_native_normalize_funding_object(explicit)
  }
  if (!NROW(source)) {
    warning("No funding events were resolved; funding is being treated as zero.", call. = FALSE)
    return(.bt_native_empty_funding_frame())
  }
  start_ts <- as.POSIXct(start_date, tz = "UTC")
  end_ts <- as.POSIXct(as.Date(end_date) + 1, tz = "UTC")
  if (!is.na(start_ts)) {
    source <- source[source$timestamp >= start_ts, , drop = FALSE]
  }
  if (!is.na(end_ts)) {
    source <- source[source$timestamp < end_ts, , drop = FALSE]
  }
  rownames(source) <- NULL
  source
}

.bt_native_ps_metadata <- function(data) {
  meta <- .collect_instrument_metadata(data)
  list(
    value = .bt_native_first_num(meta$ps_value),
    type = .bt_native_first_chr(meta$ps_type)
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
#'   traded; `"order"` charges `fee_value` once per executed order;
#'   `"percent"` and `"bps"` charge against traded notional. If `NULL`, ticker
#'   metadata is used.
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
#' @param execution_timeframe Timeframe used to simulate intrabar Donchian
#'   breakout execution. `"same"` uses the strategy bars; `"5m"`, `"1h"`, and
#'   `"4h"` require a resolvable lower/equal timeframe OHLC series, iterate
#'   every execution bar inside each signal bar, and fail fast when data is
#'   unavailable.
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
                              close_on_end = TRUE,
                              execution_timeframe = "same") {
  execution <- .bt_normalize_execution_mode(execution)
  execution_timeframe <- .bt_normalize_execution_timeframe(execution_timeframe)
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
    } else if (identical(fee_type, "contract")) {
      commission_per_contract <- fee_value
      commission_per_order <- NULL
    } else {
      commission_per_contract <- NULL
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
    execution_timeframe = execution_timeframe,
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

.bt_native_usdt_m_future <- function(symbol, data = NULL, metadata = NULL) {
  base <- .bt_base_contract_symbol(symbol)
  quote_match <- grepl("(USDT|USDC|BUSD|FDUSD)$", base)
  if (!isTRUE(quote_match)) {
    return(FALSE)
  }
  symbol_future <- grepl("(_PERPETUAL$|_(QTR|NQTR)$|_[0-9]{6}$)", toupper(as.character(symbol)[1]))
  classification <- .bt_native_first_chr(
    .bt_xts_attr_first(data, c("type", "class", "subtype"), groups = c("classification", "metadata"))
  )
  classified_future <- grepl("future|futures", classification, ignore.case = TRUE)
  isTRUE(symbol_future) || isTRUE(metadata$is_futures) || isTRUE(classified_future)
}

.bt_native_default_integer_qty <- function(data, symbol, metadata = NULL) {
  step <- suppressWarnings(as.numeric(metadata$quantity_step)[1])
  if (is.finite(step) && step > 0 && step < 1) {
    return(FALSE)
  }
  !.bt_native_usdt_m_future(symbol, data = data, metadata = metadata)
}

.bt_native_max_leverage_value <- function(max_leverage) {
  if (is.null(max_leverage) || length(max_leverage) == 0) {
    return(Inf)
  }
  value <- suppressWarnings(as.numeric(max_leverage)[1])
  if (is.infinite(value) && value > 0) {
    return(Inf)
  }
  if (!is.finite(value) || value <= 0) {
    stop("'max_leverage' must be positive or Inf.", call. = FALSE)
  }
  value
}

.bt_native_resolve_risk <- function(risk, data, symbol, ps_value = NULL, ps_type = NULL,
                                    initial_equity = 100000, reinvest = TRUE,
                                    atr_mult = 2, max_leverage = NULL,
                                    integer_qty = NULL, metadata = NULL) {
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

  max_leverage <- .bt_native_max_leverage_value(max_leverage)
  integer_qty <- if (is.null(integer_qty)) {
    .bt_native_default_integer_qty(data, symbol, metadata = metadata)
  } else {
    isTRUE(integer_qty)
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
    max_leverage = max_leverage,
    integer_qty = integer_qty,
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
  } else if (identical(fee_type, "contract")) {
    execution$commission_per_contract <- fee_value
    execution$commission_per_order <- NULL
  } else {
    execution$commission_per_contract <- NULL
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
#' The native engine computes indicators, signals, stops, execution prices, and
#' cost ledgers in this package. Quantity calculation is delegated to
#' `positionsizer` from the resolved risk spec and instrument metadata.
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
#' @param max_leverage Optional notional cap as a multiple of equity when `risk`
#'   is `NULL`.
#' @param integer_qty Optional quantity rounding override when `risk` is `NULL`.
#'   The default is integer quantities except for Binance-style USDT-M crypto
#'   futures or metadata with sub-unit quantity steps.
#' @param start_date,end_date Optional date bounds used when fetching/subsetting.
#' @param normalize_risk Optional annualized risk target in percent.
#' @param geometric Kept for wrapper compatibility.
#' @param only_returns If `TRUE`, return only the `xts` returns object.
#' @param verbose If `TRUE`, print detailed stats and transactions.
#' @param clean_di If `TRUE`, apply DI cleanup before running.
#' @param funding Optional funding events for perpetual futures. `TRUE` reads
#'   funding columns from `data` or fetches
#'   `finharvest::finget_binance_fut_funding()` only for explicit
#'   `_PERPETUAL` symbols; Binance `QTR`/`NQTR`/dated delivery futures keep zero
#'   funding unless an explicit `xts`/`data.frame` is supplied. Explicit
#'   funding data should include `date`/`FundingRate` or
#'   `timestamp`/`funding_rate` events.
#' @param execution_data Optional explicit lower/equal-timeframe OHLC `xts`
#'   used when `execution$execution_timeframe` is not `"same"`. Character
#'   tickers are fetched from `finharvest` automatically when omitted.
#' @param report If `TRUE`, print the standard console performance tables.
#' @param show_quarterly If `TRUE`, print quarterly return and net-profit
#'   tables in the console report.
#' @param research_blocks If `TRUE`, include experimental diagnostic blocks
#'   such as excursion thresholds in default reports and `bt_stats()`.
#' @return A `bt_native_result` list, or an `xts` returns object when
#'   `only_returns = TRUE`. In `trades`, `fees` is commission only,
#'   `slippage` is slippage cost, and `total_cost` is their sum.
#'   `trade_audit` expands completed trade episodes into order-level rows with
#'   signal price, slippage-adjusted fill price, costs, bars held, and funding
#'   summaries when funding applies.
#'   When `normalize_risk` is supplied, `rets`, `stats`, and report performance
#'   blocks use the risk-normalized return stream; `raw_rets`, `raw_stats`,
#'   trades, positions, and cost blocks keep the unnormalised execution path.
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
                          max_leverage = NULL,
                          integer_qty = NULL,
                          start_date = "1900-01-01",
                          end_date = Sys.Date(),
                          normalize_risk = NULL,
                          geometric = TRUE,
                          only_returns = FALSE,
                          verbose = FALSE,
                          clean_di = TRUE,
                          funding = NULL,
                          execution_data = NULL,
                          report = TRUE,
                          show_quarterly = FALSE,
                          research_blocks = TRUE) {
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
  funding_events <- .bt_native_resolve_funding(
    funding = funding,
    data = prepared$data,
    symbol = prepared$symbol,
    start_date = start_date,
    end_date = end_date
  )
  risk <- .bt_native_resolve_risk(
    risk = risk,
    data = prepared$data,
    symbol = prepared$symbol,
    ps_value = ps_value,
    ps_type = ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    atr_mult = strategy$params$atr_mult %||% 2,
    max_leverage = max_leverage,
    integer_qty = integer_qty,
    metadata = metadata
  )
  execution <- .bt_native_resolve_execution(execution, metadata, prepared$symbol)
  signal_timeframe <- .bt_native_data_timeframe(prepared$data, prepared$symbol)
  execution_detail <- .bt_native_prepare_execution_detail(
    signal_data = prepared$data,
    symbol = prepared$symbol,
    signal_timeframe = signal_timeframe,
    execution = execution,
    strategy = strategy,
    start_date = start_date,
    end_date = end_date,
    clean_di = clean_di,
    execution_data = execution_data
  )

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
    symbol = prepared$symbol,
    funding_events = funding_events,
    execution_detail = execution_detail
  )

  if (isTRUE(report)) {
    .dbg("Generated orders:", NROW(sim$trades))
  }
  if (NROW(sim$trades) == 0) {
    warning("No generated order. Check price columns, signals, and instrument metadata.", call. = FALSE)
  }

  raw_rets <- .bt_native_returns(sim$equity, normalize_risk = NULL)
  rets <- .bt_native_returns(sim$equity, normalize_risk = normalize_risk)
  raw_stats <- .bt_native_stats(
    equity = sim$equity,
    rets = raw_rets,
    trades = sim$trades,
    funding_events = sim$funding,
    risk = risk,
    execution = execution,
    metadata = metadata,
    strategy = strategy
  )
  performance_equity <- .bt_native_performance_equity(
    raw_equity = sim$equity,
    rets = rets,
    initial_equity = risk$initial_equity
  )
  performance_stats <- .bt_native_stats(
    equity = performance_equity,
    rets = rets,
    trades = sim$trades,
    funding_events = sim$funding,
    risk = risk,
    execution = execution,
    metadata = metadata,
    strategy = strategy
  )
  mktdata <- .bt_native_mktdata(prepared$data, indicators, signals)
  trade_diagnostics <- .bt_native_trade_diagnostics(
    trades = sim$trades,
    positions = sim$positions,
    mktdata = mktdata,
    strategy = strategy,
    risk = risk,
    metadata = metadata,
    funding_events = sim$funding
  )
  info_blocks <- .bt_native_info_blocks(
    symbol = prepared$symbol,
    strategy = strategy,
    risk = risk,
    execution = execution,
    metadata = metadata,
    raw_stats = raw_stats,
    performance_stats = performance_stats,
    trades = sim$trades,
    raw_rets = raw_rets,
    rets = rets,
    equity = sim$equity,
    performance_equity = performance_equity,
    episodes = trade_diagnostics$episodes,
    excursions = trade_diagnostics$excursions,
    pyramid_events = trade_diagnostics$pyramid_events,
    funding_events = sim$funding,
    geometric = geometric,
    research_blocks = research_blocks
  )

  attr(rets, "backtest") <- TRUE
  attr(rets, "local") <- TRUE
  attr(rets, "fee_mode") <- execution$fee
  attr(rets, "info_blocks") <- info_blocks

  if (isTRUE(report)) {
    .bt_native_report(
      symbol = prepared$symbol,
      strategy = strategy,
      info_blocks = info_blocks,
      geometric = geometric,
      verbose = verbose,
      show_quarterly = show_quarterly,
      start_time = start_t
    )
  }
  if (isTRUE(only_returns)) {
    return(rets)
  }

  out <- list(
    rets = rets,
    stats = performance_stats,
    raw_stats = raw_stats,
    performance_stats = performance_stats,
    trades = sim$trades,
    rets_acct = rets,
    raw_rets = raw_rets,
    mktdata = mktdata,
    positions = sim$positions,
    equity = sim$equity,
    funding_events = sim$funding,
    performance_equity = performance_equity,
    trade_audit = trade_diagnostics$audit,
    trade_episodes = trade_diagnostics$episodes,
    trade_excursions = trade_diagnostics$excursions,
    pyramid_events = trade_diagnostics$pyramid_events,
    spec = list(strategy = strategy, risk = risk, execution = execution),
    info_blocks = info_blocks,
    symbol = prepared$symbol
  )
  class(out) <- c("bt_native_result", "list")
  out
}

#' Run a native shared-capital multi-instrument backtest
#'
#' This runner uses one account equity curve for all instruments. Each instrument
#' has its own strategy, risk, execution, metadata, signals, and positions, while
#' later entries size from the shared account equity. Quantity calculation still
#' delegates to `positionsizer`; `backtestforge` owns the shared-capital event
#' loop and trade ledger.
#'
#' @param tickers Character vector, `xts` object, or named list of character/`xts`
#'   instruments.
#' @param data Optional `xts` object or named list of preloaded `xts` objects.
#' @param strategy A `bt_strategy_spec`, character strategy type, or named list
#'   of per-instrument strategy specs.
#' @param risk Optional `bt_risk_spec`, or named list of per-instrument specs. If
#'   `NULL`, each instrument resolves `ps_value`/`ps_type` from arguments or
#'   metadata.
#' @param ps_value,ps_type Position-sizing value and type used when `risk` is
#'   `NULL`. Scalars apply to every instrument; named vectors/lists can override
#'   by instrument label or source symbol.
#' @param initial_equity Starting shared account equity.
#' @param reinvest Whether sizing uses current shared equity. Can be scalar or
#'   named per instrument when `risk` is `NULL`.
#' @param execution A `bt_execution_spec`, or named list of per-instrument specs.
#' @param start_date,end_date Optional date bounds used when fetching/subsetting.
#' @param normalize_risk Optional annualized risk target in percent for the
#'   portfolio return stream.
#' @param geometric Kept for wrapper compatibility.
#' @param only_returns If `TRUE`, return only the portfolio `xts` returns object.
#' @param verbose If `TRUE`, print a compact execution summary.
#' @param clean_di If `TRUE`, apply DI cleanup before running.
#' @param max_positions Maximum simultaneous open instruments.
#' @param priority Entry priority. Currently only `"order"` is supported.
#' @details
#' `bt_run_portfolio()` simulates all instruments on one shared equity curve.
#' This is different from `bt_batch(gen_portfolio = ...)`, which aggregates
#' already-finished individual return streams.
#'
#' Per-instrument names are matched against the list label, source symbol,
#' ticker argument, and nested metadata such as `attr(x, "information")$ticker`.
#' Direct `xts` inputs resolve their source symbol from `attr(x, "symbol")`,
#' then `attr(x, "ticker")`, then `attr(x, "information")$ticker`, then the
#' list label.
#'
#' Current portfolio execution support is limited to `"breakout"` and
#' `"same_close"`. Portfolio pyramiding, `next_*` execution modes, margin, and
#' advanced entry-priority rules are not implemented yet.
#' @return A `bt_portfolio_result` list, or an `xts` returns object when
#'   `only_returns = TRUE`.
#' @examples
#' \dontrun{
#' res <- bt_run_portfolio(
#'   tickers = list(WDO = wdo_xts, WIN = win_xts),
#'   strategy = bt_strategy_spec("donchian", up = 40, down = 20),
#'   ps_value = c(WDO = 1, WIN = 2),
#'   ps_type = c(WDO = "contract", WIN = "contract"),
#'   execution = bt_execution_spec(
#'     execution = "breakout",
#'     fee_value = 0,
#'     fee_type = "contract",
#'     slip_value = 0,
#'     slip_type = "cash"
#'   ),
#'   initial_equity = 100000
#' )
#' }
#' @export
bt_run_portfolio <- function(tickers,
                             data = NULL,
                             strategy = bt_strategy_spec("donchian"),
                             risk = NULL,
                             ps_value = NULL,
                             ps_type = NULL,
                             initial_equity = 100000,
                             reinvest = TRUE,
                             execution = bt_execution_spec(execution = "breakout"),
                             start_date = "1900-01-01",
                             end_date = Sys.Date(),
                             normalize_risk = NULL,
                             geometric = TRUE,
                             only_returns = FALSE,
                             verbose = FALSE,
                             clean_di = TRUE,
                             max_positions = Inf,
                             priority = c("order")) {
  start_t <- Sys.time()
  priority <- match.arg(priority)

  if (inherits(risk, "bt_risk_spec")) {
    initial_equity <- risk$initial_equity
  }
  initial_equity <- suppressWarnings(as.numeric(initial_equity)[1])
  if (!is.finite(initial_equity) || initial_equity <= 0) {
    stop("'initial_equity' must be positive.", call. = FALSE)
  }
  max_positions <- suppressWarnings(as.numeric(max_positions)[1])
  if (!is.finite(max_positions)) max_positions <- Inf
  if (max_positions <= 0) {
    stop("'max_positions' must be positive.", call. = FALSE)
  }

  items <- .bt_native_portfolio_items(
    tickers = tickers,
    data = data,
    strategy = strategy,
    risk = risk,
    ps_value = ps_value,
    ps_type = ps_type,
    initial_equity = initial_equity,
    reinvest = reinvest,
    execution = execution,
    start_date = start_date,
    end_date = end_date,
    clean_di = clean_di
  )

  sim <- .bt_native_simulate_portfolio(
    items = items,
    initial_equity = initial_equity,
    max_positions = max_positions,
    priority = priority
  )
  if (NROW(sim$trades) == 0) {
    warning("No generated portfolio order. Check price columns, signals, and instrument metadata.", call. = FALSE)
  }

  raw_rets <- .bt_native_returns(sim$equity, normalize_risk = NULL)
  rets <- .bt_native_returns(sim$equity, normalize_risk = normalize_risk)
  first_item <- items[[1]]
  strategy_specs <- lapply(items, `[[`, "strategy")
  risk_specs <- lapply(items, `[[`, "risk")
  execution_specs <- lapply(items, `[[`, "execution")
  portfolio_strategy <- if (all(vapply(strategy_specs, identical, logical(1), y = first_item$strategy))) {
    first_item$strategy
  } else {
    structure(
      list(
        type = "portfolio",
        params = list(atr_n = NA_integer_),
        long = any(vapply(strategy_specs, `[[`, logical(1), "long")),
        short = any(vapply(strategy_specs, `[[`, logical(1), "short")),
        invert_signals = FALSE
      ),
      class = c("bt_strategy_spec", "list")
    )
  }
  portfolio_risk <- first_item$risk
  portfolio_risk$initial_equity <- initial_equity
  portfolio_risk$reinvest <- all(vapply(risk_specs, function(x) isTRUE(x$reinvest), logical(1)))
  portfolio_execution <- first_item$execution
  portfolio_metadata <- first_item$metadata
  portfolio_metadata$symbol <- "Portfolio"
  portfolio_stats <- .bt_native_stats(
    equity = if (.bt_native_is_risk_normalized(rets)) .bt_native_performance_equity(sim$equity, rets, initial_equity) else sim$equity,
    rets = rets,
    trades = sim$trades,
    risk = portfolio_risk,
    execution = portfolio_execution,
    metadata = portfolio_metadata,
    strategy = portfolio_strategy
  )
  instrument_stats <- lapply(items, function(item) {
    tr <- sim$trades[sim$trades$symbol == item$symbol, , drop = FALSE]
    trade_sum <- function(name) {
      if (!name %in% names(tr)) return(0)
      sum(suppressWarnings(as.numeric(tr[[name]])), na.rm = TRUE)
    }
    data.frame(
      Symbol = item$symbol,
      source_symbol = item$source_symbol,
      Strategy = .bt_native_strategy_label(item$strategy),
      num_trades = sum(tr$reason %in% c("long_entry", "short_entry")),
      num_orders = NROW(tr),
      contracts_traded = if ("qty_delta" %in% names(tr)) sum(abs(suppressWarnings(as.numeric(tr$qty_delta))), na.rm = TRUE) else 0,
      fees = trade_sum("fees"),
      slippage = trade_sum("slippage"),
      total_cost = if ("total_cost" %in% names(tr)) trade_sum("total_cost") else trade_sum("fees") + trade_sum("slippage"),
      InitialEquity = initial_equity,
      PosSiz = item$risk$ps_type %||% item$risk$mode,
      PsValue = if (identical(item$risk$mode, "fixed")) item$risk$fixed_qty else item$risk$risk_pct,
      RiskPct = item$risk$risk_pct,
      Reinvest = item$risk$reinvest,
      Execution = item$execution$execution,
      Multiplier = item$metadata$multiplier,
      TickSize = item$metadata$tick_size,
      stringsAsFactors = FALSE
    )
  })
  instrument_stats <- do.call(rbind, instrument_stats)
  rownames(instrument_stats) <- NULL

  attr(rets, "backtest") <- TRUE
  attr(rets, "local") <- TRUE
  attr(rets, "portfolio") <- TRUE
  fee_modes <- unique(vapply(execution_specs, `[[`, character(1), "fee"))
  attr(rets, "fee_mode") <- if (length(fee_modes) == 1L) fee_modes else "mixed"

  if (isTRUE(verbose)) {
    message(sprintf(
      "Portfolio backtest: %d instruments, %d orders, final equity %.2f, runtime %.2fs",
      length(items),
      NROW(sim$trades),
      as.numeric(tail(sim$equity, 1)),
      as.numeric(difftime(Sys.time(), start_t, units = "secs"))
    ))
  }
  if (isTRUE(only_returns)) {
    return(rets)
  }

  out <- list(
    rets = rets,
    stats = portfolio_stats,
    portfolio_stats = portfolio_stats,
    instrument_stats = instrument_stats,
    trades = sim$trades,
    raw_rets = raw_rets,
    positions = sim$positions,
    equity = sim$equity,
    spec = list(
      strategy = portfolio_strategy,
      strategies = strategy_specs,
      risk = portfolio_risk,
      risks = risk_specs,
      execution = portfolio_execution,
      executions = execution_specs
    ),
    instruments = names(items),
    geometric = geometric
  )
  class(out) <- c("bt_portfolio_result", "list")
  out
}

.bt_native_portfolio_arg <- function(x, i, keys, default = NULL) {
  if (is.null(x)) {
    return(default)
  }
  if (inherits(x, c("bt_strategy_spec", "bt_risk_spec", "bt_execution_spec"))) {
    return(x)
  }
  keys <- as.character(keys)
  keys <- unique(keys[!is.na(keys) & nzchar(keys)])
  default_keys <- c("default", ".default", "*", "all")

  if (is.list(x) && !xts::is.xts(x)) {
    nms <- names(x)
    if (!is.null(nms)) {
      for (key in keys) {
        hit <- match(key, nms, nomatch = 0L)
        if (hit > 0L) return(x[[hit]])
      }
      default_hit <- match(TRUE, tolower(nms) %in% default_keys, nomatch = 0L)
      if (default_hit > 0L) return(x[[default_hit]])
    }
    if (length(x) == 1L) return(x[[1L]])
    if (length(x) >= i) return(x[[i]])
    if (length(x)) return(x[[length(x)]])
    return(default)
  }

  nms <- names(x)
  if (!is.null(nms)) {
    for (key in keys) {
      hit <- match(key, nms, nomatch = 0L)
      if (hit > 0L) return(x[[hit]])
    }
    default_hit <- match(TRUE, tolower(nms) %in% default_keys, nomatch = 0L)
    if (default_hit > 0L) return(x[[default_hit]])
  }
  if (length(x) == 1L) return(x[[1L]])
  if (length(x) >= i) return(x[[i]])
  if (length(x)) return(x[[length(x)]])
  default
}

.bt_native_portfolio_keys <- function(...) {
  keys <- unlist(list(...), use.names = FALSE)
  keys <- as.character(keys)
  unique(keys[!is.na(keys) & nzchar(keys)])
}

.bt_native_portfolio_strategy <- function(strategy, i, keys) {
  raw <- if (is.list(strategy) && !inherits(strategy, "bt_strategy_spec") && !is.null(strategy$type)) {
    strategy
  } else {
    .bt_native_portfolio_arg(strategy, i, keys)
  }
  if (is.character(raw)) {
    raw <- bt_strategy_spec(raw)
  } else if (is.list(raw) && !inherits(raw, "bt_strategy_spec")) {
    type <- raw$type %||% "donchian"
    raw$type <- NULL
    raw <- do.call(bt_strategy_spec, c(list(type = type), raw))
  }
  if (!inherits(raw, "bt_strategy_spec")) {
    stop("'strategy' must be a bt_strategy_spec, character strategy, or per-instrument list.", call. = FALSE)
  }
  if (!raw$long && !raw$short) {
    stop("At least one side must be enabled for every portfolio instrument.", call. = FALSE)
  }
  if (isTRUE(raw$params$pyramid)) {
    stop("bt_run_portfolio() does not support pyramiding yet.", call. = FALSE)
  }
  raw
}

.bt_native_portfolio_risk <- function(risk, i, keys) {
  raw <- .bt_native_portfolio_arg(risk, i, keys, default = NULL)
  if (is.null(raw)) {
    return(NULL)
  }
  if (inherits(raw, "bt_risk_spec")) {
    return(raw)
  }
  if (is.character(raw)) {
    return(bt_risk_spec(mode = raw))
  }
  if (is.list(raw)) {
    return(do.call(bt_risk_spec, raw))
  }
  stop("'risk' must be a bt_risk_spec, NULL, or per-instrument list.", call. = FALSE)
}

.bt_native_portfolio_execution <- function(execution, i, keys) {
  raw <- if (is.list(execution) && !inherits(execution, "bt_execution_spec") && !is.null(execution$execution)) {
    execution
  } else {
    .bt_native_portfolio_arg(execution, i, keys)
  }
  if (is.character(raw)) {
    raw <- bt_execution_spec(execution = raw)
  } else if (is.list(raw) && !inherits(raw, "bt_execution_spec")) {
    raw <- do.call(bt_execution_spec, raw)
  }
  if (!inherits(raw, "bt_execution_spec")) {
    stop("'execution' must be a bt_execution_spec, character mode, or per-instrument list.", call. = FALSE)
  }
  if (!identical(raw$execution, "breakout") && !identical(raw$execution, "same_close")) {
    stop("bt_run_portfolio() currently supports only 'breakout' and 'same_close' execution.", call. = FALSE)
  }
  if (!identical(raw$delay, 0L)) {
    stop("bt_run_portfolio() does not support delayed next-bar execution yet.", call. = FALSE)
  }
  raw
}

.bt_native_portfolio_items <- function(tickers, data, strategy, risk, ps_value, ps_type,
                                       initial_equity, reinvest, execution,
                                       start_date, end_date, clean_di) {
  if (missing(tickers) || is.null(tickers) || length(tickers) == 0) {
    stop("'tickers' cannot be empty.", call. = FALSE)
  }
  if (xts::is.xts(tickers)) {
    tickers <- list(tickers)
  } else if (!is.list(tickers)) {
    tickers <- as.list(tickers)
  }
  ticker_names <- names(tickers)
  if (is.null(ticker_names)) ticker_names <- rep("", length(tickers))

  data_list <- NULL
  if (!is.null(data)) {
    if (xts::is.xts(data)) {
      data_list <- list(data)
    } else if (is.list(data)) {
      data_list <- data
    } else {
      stop("'data' must be an xts object or a named list of xts objects.", call. = FALSE)
    }
  }

  label_seen <- new.env(parent = emptyenv())
  unique_label <- function(label) {
    if (is.null(label) || !nzchar(label)) label <- "Asset"
    count <- label_seen[[label]]
    if (is.null(count)) {
      label_seen[[label]] <- 1L
      return(label)
    }
    count <- count + 1L
    label_seen[[label]] <- count
    paste0(label, "_", count)
  }

  items <- vector("list", length(tickers))
  for (i in seq_along(tickers)) {
    entry <- tickers[[i]]
    entry_name <- ticker_names[i]
    data_i <- NULL
    ticker_i <- NULL
    label_i <- NULL
    if (xts::is.xts(entry)) {
      data_i <- entry
      ticker_i <- attr(entry, "symbol", exact = TRUE) %||%
        attr(entry, "ticker", exact = TRUE) %||%
        .bt_xts_attr_first(entry, c("ticker", "symbol"), groups = c("information", "identity", "metadata"))
      if (is.null(ticker_i) || !nzchar(ticker_i)) {
        ticker_i <- if (!is.null(entry_name) && nzchar(entry_name)) entry_name else paste0("Asset", i)
      }
      label_i <- if (!is.null(entry_name) && nzchar(entry_name)) entry_name else ticker_i
    } else if (is.character(entry) && length(entry) == 1L && nzchar(entry)) {
      ticker_i <- entry
      label_i <- if (!is.null(entry_name) && nzchar(entry_name)) entry_name else ticker_i
      if (!is.null(data_list)) {
        if (!is.null(names(data_list)) && ticker_i %in% names(data_list)) {
          data_i <- data_list[[ticker_i]]
        } else if (length(data_list) == length(tickers)) {
          data_i <- data_list[[i]]
        }
      }
    } else {
      stop("'tickers' entries must be character symbols or xts objects.", call. = FALSE)
    }

    prepared <- .bt_native_prepare_data(
      ticker = ticker_i,
      data = data_i,
      start_date = start_date,
      end_date = end_date,
      clean_di = clean_di
    )
    label <- unique_label(label_i %||% prepared$symbol %||% ticker_i)
    keys <- .bt_native_portfolio_keys(label, prepared$symbol, ticker_i, entry_name)
    strategy_i <- .bt_native_portfolio_strategy(strategy, i, keys)
    risk_arg_i <- .bt_native_portfolio_risk(risk, i, keys)
    ps_value_i <- .bt_native_portfolio_arg(ps_value, i, keys, default = ps_value)
    ps_type_i <- .bt_native_portfolio_arg(ps_type, i, keys, default = ps_type)
    reinvest_i <- .bt_native_portfolio_arg(reinvest, i, keys, default = reinvest)
    execution_arg_i <- .bt_native_portfolio_execution(execution, i, keys)

    prices <- .bt_native_price_set(prepared$data, prepared$symbol)
    indicators <- .bt_native_indicators(prepared$data, prices, strategy_i)
    signals <- .bt_native_signals(prepared$data, prices, indicators, strategy_i)
    metadata <- .bt_native_metadata(prepared$data, prepared$symbol)
    risk_i <- .bt_native_resolve_risk(
      risk = risk_arg_i,
      data = prepared$data,
      symbol = prepared$symbol,
      ps_value = ps_value_i,
      ps_type = ps_type_i,
      initial_equity = initial_equity,
      reinvest = isTRUE(reinvest_i),
      atr_mult = strategy_i$params$atr_mult %||% 2,
      metadata = metadata
    )
    if (inherits(risk_i, "bt_risk_spec")) {
      risk_i$initial_equity <- initial_equity
    }
    execution_i <- .bt_native_resolve_execution(execution_arg_i, metadata, prepared$symbol)
    items[[i]] <- list(
      data = prepared$data,
      prices = prices,
      indicators = indicators,
      signals = signals,
      strategy = strategy_i,
      risk = risk_i,
      execution = execution_i,
      metadata = metadata,
      symbol = label,
      source_symbol = prepared$symbol
    )
  }
  names(items) <- vapply(items, `[[`, character(1), "symbol")
  items
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

  symbol <- symbol %||%
    attr(data, "symbol", exact = TRUE) %||%
    attr(data, "ticker", exact = TRUE) %||%
    .bt_xts_attr_first(data, c("ticker", "symbol"), groups = c("information", "identity", "metadata")) %||%
    "local_xts"
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

.bt_native_parse_symbol_timeframe <- function(symbol) {
  symbol <- as.character(symbol %||% "")[1]
  if (!nzchar(symbol)) {
    return(NA_character_)
  }
  parts <- strsplit(symbol, "_", fixed = TRUE)[[1]]
  if (!length(parts)) {
    return(NA_character_)
  }
  tf <- .bt_normalize_data_timeframe(utils::tail(parts, 1))
  if (!is.na(tf)) {
    return(tf)
  }
  # Binance futures without a suffix use finharvest's default daily table.
  if (grepl("_(PERPETUAL|QTR|NQTR)$|_[0-9]{6}$", toupper(symbol), perl = TRUE)) {
    return("1d")
  }
  NA_character_
}

.bt_native_data_timeframe <- function(data, symbol) {
  attr_tf <- .bt_xts_attr_first(
    data,
    c("timeframe", "bar_size", "interval"),
    groups = c("information", "contract", "metadata")
  )
  tf <- .bt_normalize_data_timeframe(attr_tf)
  if (!is.na(tf)) {
    return(tf)
  }
  .bt_native_parse_symbol_timeframe(symbol)
}

.bt_native_signal_bar_end <- function(idx, i, signal_seconds) {
  if (i < length(idx)) {
    return(as.POSIXct(idx[i + 1L], tz = "UTC"))
  }
  start <- as.POSIXct(idx[i], tz = "UTC")
  if (is.finite(signal_seconds) && signal_seconds > 0) {
    return(start + signal_seconds)
  }
  if (i > 1L) {
    prev <- as.POSIXct(idx[i - 1L], tz = "UTC")
    delta <- as.numeric(difftime(start, prev, units = "secs"))
    if (is.finite(delta) && delta > 0) {
      return(start + delta)
    }
  }
  start
}

.bt_native_execution_symbol <- function(symbol, execution_timeframe) {
  exec_tf <- .bt_normalize_execution_timeframe(execution_timeframe)
  if (identical(exec_tf, "same")) {
    return(as.character(symbol)[1])
  }
  symbol <- as.character(symbol %||% "")[1]
  if (!nzchar(symbol)) {
    stop("Cannot derive an execution-timeframe ticker from an unnamed xts object.", call. = FALSE)
  }
  parts <- strsplit(symbol, "_", fixed = TRUE)[[1]]
  if (!length(parts)) {
    stop(sprintf("Cannot derive execution-timeframe ticker for '%s'.", symbol), call. = FALSE)
  }
  last_tf <- .bt_normalize_data_timeframe(utils::tail(parts, 1))
  if (!is.na(last_tf)) {
    parts[length(parts)] <- exec_tf
    return(paste(parts, collapse = "_"))
  }
  if (grepl("_(PERPETUAL|QTR|NQTR)$|_[0-9]{6}$", toupper(symbol), perl = TRUE)) {
    return(paste0(symbol, "_", exec_tf))
  }
  stop(
    sprintf(
      "Cannot derive execution-timeframe ticker for '%s'. Add an explicit timeframe suffix or use execution_timeframe = 'same'.",
      symbol
    ),
    call. = FALSE
  )
}

.bt_native_prepare_execution_detail <- function(signal_data,
                                                symbol,
                                                signal_timeframe,
                                                execution,
                                                strategy,
                                                start_date,
                                                end_date,
                                                clean_di = TRUE,
                                                execution_data = NULL) {
  exec_tf <- execution$execution_timeframe %||% "same"
  exec_tf <- .bt_normalize_execution_timeframe(exec_tf)
  if (identical(exec_tf, "same")) {
    return(NULL)
  }
  if (!identical(strategy$type, "donchian") || !identical(execution$execution, "breakout")) {
    stop("execution_timeframe other than 'same' is currently supported only for Donchian breakout execution.", call. = FALSE)
  }
  if (isTRUE(strategy$params$pyramid)) {
    stop("execution_timeframe other than 'same' is not supported with pyramiding yet.", call. = FALSE)
  }
  signal_tf <- .bt_normalize_data_timeframe(signal_timeframe)
  signal_secs <- .bt_data_timeframe_seconds(signal_tf)
  if (!is.finite(signal_secs) || signal_secs <= 0) {
    stop(
      sprintf(
        "Cannot validate execution_timeframe='%s' because '%s' has no recognized strategy timeframe. Use a ticker suffix such as _1d, _4h, or _1h.",
        exec_tf,
        symbol
      ),
      call. = FALSE
    )
  }
  exec_secs <- .bt_timeframe_seconds(exec_tf)
  if (!is.finite(exec_secs) || exec_secs <= 0) {
    stop("'execution_timeframe' must be one of 'same', '5m', '1h', or '4h'.", call. = FALSE)
  }
  if (exec_secs > signal_secs) {
    stop(
      sprintf(
        "execution_timeframe='%s' is coarser than strategy timeframe '%s' for '%s'.",
        exec_tf,
        signal_tf,
        symbol
      ),
      call. = FALSE
    )
  }
  if (identical(exec_secs, signal_secs)) {
    return(NULL)
  }

  exec_symbol <- .bt_native_execution_symbol(symbol, exec_tf)
  prepared <- .bt_native_prepare_data(
    ticker = exec_symbol,
    data = execution_data,
    start_date = start_date,
    end_date = end_date,
    clean_di = clean_di
  )
  exec_data <- prepared$data
  exec_prices <- .bt_native_price_set(exec_data, prepared$symbol)
  exec_data_tf <- .bt_native_data_timeframe(exec_data, prepared$symbol)
  exec_data_secs <- .bt_data_timeframe_seconds(exec_data_tf)
  if (is.finite(exec_data_secs) && exec_data_secs != exec_secs) {
    stop(
      sprintf(
        "Execution ticker '%s' resolved timeframe '%s', not requested '%s'.",
        prepared$symbol,
        exec_data_tf,
        exec_tf
      ),
      call. = FALSE
    )
  }

  signal_idx <- as.POSIXct(zoo::index(signal_data), tz = "UTC")
  exec_idx <- as.POSIXct(zoo::index(exec_data), tz = "UTC")
  if (!length(exec_idx) || all(is.na(exec_idx))) {
    stop(sprintf("Execution timeframe data for '%s' has no timestamps.", prepared$symbol), call. = FALSE)
  }
  needed_start <- min(signal_idx, na.rm = TRUE)
  needed_end <- .bt_native_signal_bar_end(signal_idx, length(signal_idx), signal_secs)
  if (min(exec_idx, na.rm = TRUE) > needed_start || max(exec_idx, na.rm = TRUE) < max(signal_idx, na.rm = TRUE)) {
    stop(
      sprintf(
        "Execution timeframe '%s' for '%s' does not cover the strategy data window for '%s'.",
        exec_tf,
        prepared$symbol,
        symbol
      ),
      call. = FALSE
    )
  }

  list(
    symbol = prepared$symbol,
    data = exec_data,
    prices = exec_prices,
    timeframe = exec_tf,
    timeframe_seconds = exec_secs,
    signal_timeframe = signal_tf,
    signal_timeframe_seconds = signal_secs,
    index = exec_idx
  )
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
  maturity <- .bt_xts_attr_first(
    data,
    c("maturity", "maturity_date", "expiry", "expiration"),
    groups = c("contract", "metadata")
  )
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
  attr_names <- names(attributes(data))
  attr_names <- attr_names[!is.na(attr_names)]
  base_symbol <- .bt_base_contract_symbol(symbol)
  contract_attr <- .bt_xts_attr_list(data, "contract")
  classification_type <- .bt_native_first_chr(
    .bt_xts_attr_first(data, c("type", "subtype", "class"), groups = c("classification", "exposure", "metadata"))
  )
  has_contract_metadata <- any(vapply(
    list(meta$tick_size, meta$tick_value, meta$multiplier, meta$maturity, meta$root),
    .bt_attr_has_value,
    logical(1)
  ))
  has_futures_metadata <- any(c(
    "fut_multiplier", "fut_tick_size", "fut_tick_value",
    "ticksize", "tickvalue", "contract_symbol", "contract_year",
    "contract_month", "maturity"
  ) %in% attr_names) ||
    is.list(contract_attr) && length(contract_attr) > 0 && has_contract_metadata
  is_futures <- isTRUE(di) ||
    has_futures_metadata ||
    isTRUE(grepl("future|futures|derivative|derivatives", classification_type, ignore.case = TRUE)) ||
    grepl("(^DI1|FUT)", base_symbol, ignore.case = TRUE)
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
    first_num(meta$tick_size),
    "tick_size"
  )
  tick_value <- first_num(
    meta$tick_value
  )
  if (isTRUE(di) && is.finite(tick_value) && tick_value != 0) {
    tick_value <- abs(tick_value)
  }
  multiplier_raw <- first_num(meta$multiplier)
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
  fees <- first_num(meta$fees)
  fee_type <- first_chr(
    meta$fee_type
  )
  contract_model <- tolower(first_chr(meta$contract_model))
  if (is.na(contract_model) || !nzchar(contract_model)) {
    contract_model <- "linear"
  }
  leverage <- first_num(meta$leverage)
  quantity_step <- first_num(meta$quantity_step)
  min_qty <- first_num(meta$min_qty)
  max_qty <- first_num(meta$max_qty)
  min_notional <- first_num(meta$min_notional)
  contract_size <- first_num(meta$contract_size)
  slip_value <- first_num(
    meta$slip_value
  )
  slippage_bps <- first_num(
    meta$slippage_bps
  )
  slippage_ticks <- first_num(
    meta$slippage_ticks
  )
  slippage_points <- first_num(
    meta$slippage_points
  )
  slippage_cash <- NA_real_
  slippage_unit <- tolower(first_chr(
    meta$slippage_unit
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
    is_futures = is_futures,
    is_di = isTRUE(di),
    maturity = meta$maturity,
    symbol = symbol,
    root = first_chr(meta$root, .bt_native_root(symbol)),
    contract_model = contract_model,
    leverage = leverage,
    quantity_step = quantity_step,
    min_qty = min_qty,
    max_qty = max_qty,
    min_notional = min_notional,
    contract_size = contract_size
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
  } else if (identical(type, "tsmom")) {
    lookback <- strategy$params$lookback
    momentum <- rep(NA_real_, n)
    if (is.finite(lookback) && lookback > 0 && lookback < n) {
      lagged_close <- c(rep(NA_real_, lookback), utils::head(prices$close, -lookback))
      momentum <- prices$close / lagged_close - 1
    }
    atr_n <- strategy$params$atr_n %||% 20L
    hlc <- cbind(High = prices$high, Low = prices$low, Close = prices$close)
    atr_raw <- if (is.finite(atr_n) && atr_n > 0 && atr_n <= n) {
      suppressWarnings(as.numeric(TTR::ATR(hlc, n = atr_n)[, "atr"]))
    } else {
      rep(NA_real_, n)
    }
    out <- xts::xts(
      cbind(TSMOMReturn = momentum, ATR = atr_raw),
      order.by = idx
    )
  } else if (identical(type, "hold")) {
    out <- xts::xts(cbind(Hold = rep(1, n)), order.by = idx)
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
  } else if (identical(strategy$type, "tsmom")) {
    state_entry <- function(condition) {
      condition <- as.logical(condition)
      condition[is.na(condition)] <- FALSE
      prev <- c(FALSE, utils::head(condition, -1L))
      prev[is.na(prev)] <- FALSE
      condition & !prev
    }
    state_exit <- function(condition) {
      condition <- as.logical(condition)
      condition[is.na(condition)] <- FALSE
      prev <- c(FALSE, utils::head(condition, -1L))
      prev[is.na(prev)] <- FALSE
      !condition & prev
    }
    ret <- as.numeric(indicators[, "TSMOMReturn"])
    threshold <- strategy$params$threshold %||% 0
    long_state <- ret > threshold
    short_state <- ret < -threshold
    long_entry <- state_entry(long_state)
    long_exit <- state_exit(long_state)
    short_entry <- state_entry(short_state)
    short_exit <- state_exit(short_state)
  } else if (identical(strategy$type, "hold")) {
    n <- length(idx)
    long_entry <- rep(FALSE, n)
    long_exit <- rep(FALSE, n)
    short_entry <- rep(FALSE, n)
    short_exit <- rep(FALSE, n)
    if (n > 0) {
      long_entry[1L] <- isTRUE(strategy$long)
      short_entry[1L] <- !isTRUE(strategy$long) && isTRUE(strategy$short)
      if (n > 1L) {
        long_exit[n] <- isTRUE(strategy$long)
        short_exit[n] <- !isTRUE(strategy$long) && isTRUE(strategy$short)
      }
    }
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
  price <- as.numeric(price)[1]
  if (!is.finite(price) || price <= 0) return(0)
  equity_basis <- if (isTRUE(risk$reinvest)) equity else risk$initial_equity
  if (!is.finite(equity_basis) || equity_basis <= 0) {
    stop("Unable to compute valid equity for native position sizing.", call. = FALSE)
  }

  mult <- suppressWarnings(as.numeric(metadata$multiplier)[1])
  tick_size <- suppressWarnings(as.numeric(metadata$tick_size)[1])
  tick_value <- suppressWarnings(as.numeric(tick_value)[1])
  if (!is.finite(tick_value) || tick_value <= 0) {
    tick_value <- suppressWarnings(as.numeric(metadata$tick_value)[1])
  }
  if (!is.finite(mult) || mult <= 0) mult <- 1

  instrument_args <- list(
    ticker = metadata$symbol %||% "",
    root = metadata$root %||% NULL,
    asset_type = if (isTRUE(metadata$is_futures)) "future" else "asset",
    contract_model = metadata$contract_model %||% "linear",
    price_mode = "price",
    multiplier = mult,
    ticksize = tick_size,
    tickvalue = tick_value,
    leverage = metadata$leverage,
    quantity_step = metadata$quantity_step,
    min_qty = metadata$min_qty,
    max_qty = metadata$max_qty,
    min_notional = metadata$min_notional
  )
  ps_formals <- names(formals(positionsizer::ps_instrument_spec))
  instrument <- do.call(
    positionsizer::ps_instrument_spec,
    instrument_args[names(instrument_args) %in% ps_formals]
  )
  out <- positionsizer::ps_size_position(
    side = side,
    price = price,
    stop_price = stop_price,
    capital = equity_basis,
    risk_pct = risk$risk_pct,
    fixed_qty = risk$fixed_qty,
    mode = switch(risk$mode, fixed = "contract", notional = "notional", risk = "risk", risk$mode),
    instrument = instrument,
    max_qty = risk$max_qty,
    max_leverage = risk$max_leverage,
    integer_qty = risk$integer_qty,
    min_risk_pct = risk$min_risk_pct
  )
  qty <- suppressWarnings(as.numeric(out$quantity_signed)[1])
  if (is.finite(qty)) qty else 0
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
  px <- suppressWarnings(as.numeric(price)[1])
  mult <- suppressWarnings(as.numeric(metadata$multiplier)[1])
  if (!is.finite(mult) || mult <= 0) mult <- 1
  notional <- if (is.finite(px) && px > 0) qty_abs * px * mult else 0
  commission_per_order <- suppressWarnings(as.numeric(execution$commission_per_order)[1])
  commission <- execution$commission_per_contract
  if (is.null(commission) && !is.finite(commission_per_order)) commission <- metadata$fees
  slippage <- .bt_native_slippage_per_contract(price, execution, metadata, tick_value = tick_value)
  commission <- suppressWarnings(as.numeric(commission)[1])
  if (!is.finite(commission)) commission <- 0
  fee_value <- .bt_native_first_num(execution$fee_value, commission, metadata$fees)
  if (!is.finite(fee_value)) fee_value <- 0
  fees <- switch(
    execution$fee_type %||% "contract",
    order = if (is.finite(commission_per_order)) commission_per_order else fee_value,
    percent = notional * fee_value / 100,
    bps = notional * fee_value / 10000,
    contract = qty_abs * fee_value,
    qty_abs * fee_value
  )
  if (!is.finite(fees)) {
    fees <- 0
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

  out <- tryCatch(
    positionsizer::ps_di_rate_to_pu(
      rate,
      maturity_date = maturity,
      basis_date = basis,
      cal = cal,
      snap_to_tick = FALSE,
      round_pu = FALSE
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

.bt_native_trade_row <- function(timestamp, symbol, qty, qty_delta, units, price, costs, reason, equity_after,
                                 signal_time = NULL, execution_timeframe = "same", execution_source_ticker = NA_character_) {
  costs <- .bt_native_clean_cost_components(costs)
  timestamp <- as.POSIXct(timestamp, tz = "UTC")
  signal_time <- if (is.null(signal_time)) timestamp else as.POSIXct(signal_time, tz = "UTC")
  data.frame(
    timestamp = timestamp,
    signal_time = signal_time,
    execution_timeframe = as.character(execution_timeframe %||% "same")[1],
    execution_source_ticker = as.character(execution_source_ticker %||% NA_character_)[1],
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
    signal_time = as.POSIXct(character()),
    execution_timeframe = character(),
    execution_source_ticker = character(),
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

.bt_native_empty_funding_ledger <- function() {
  data.frame(
    timestamp = as.POSIXct(character(), tz = "UTC"),
    symbol = character(),
    qty = numeric(),
    price = numeric(),
    funding_rate = numeric(),
    funding_cash = numeric(),
    equity = numeric(),
    stringsAsFactors = FALSE
  )
}

.bt_native_simulate <- function(data, prices, indicators, signals, strategy, risk, execution, metadata, symbol, funding_events = NULL, execution_detail = NULL) {
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
  entry_qty_abs <- NA_real_
  last_add_price <- NA_real_
  last_add_signal_price <- NA_real_
  equity <- numeric(n)
  qty_path <- numeric(n)
  unit_path <- integer(n)
  equity[1] <- risk$initial_equity
  trades <- list()
  funding_ledger <- list()
  funding_events <- .bt_native_normalize_funding_object(funding_events)
  funding_times <- if (NROW(funding_events)) as.POSIXct(funding_events$timestamp, tz = "UTC") else as.POSIXct(character(), tz = "UTC")
  funding_multiplier <- suppressWarnings(as.numeric(metadata$multiplier)[1])
  if (!is.finite(funding_multiplier) || funding_multiplier <= 0) funding_multiplier <- 1
  execution_timeframe <- execution$execution_timeframe %||% "same"
  execution_source_ticker <- if (!is.null(execution_detail)) execution_detail$symbol else NA_character_
  execution_index_num <- if (!is.null(execution_detail)) as.numeric(execution_detail$index) else numeric()
  execution_open <- if (!is.null(execution_detail)) execution_detail$prices$open else numeric()
  execution_high <- if (!is.null(execution_detail)) execution_detail$prices$high else numeric()
  execution_low <- if (!is.null(execution_detail)) execution_detail$prices$low else numeric()
  lower_bound <- function(vec, value) {
    n_vec <- length(vec)
    lo <- 1L
    hi <- n_vec + 1L
    while (lo < hi) {
      mid <- floor((lo + hi) / 2)
      if (vec[[mid]] < value) {
        lo <- mid + 1L
      } else {
        hi <- mid
      }
    }
    lo
  }

  add_trade <- function(i, qty_delta, price, costs, reason, equity_after, event_time = NULL, signal_time = NULL) {
    if (!is.finite(qty_delta) || qty_delta == 0) return(invisible(NULL))
    timestamp <- if (is.null(event_time)) idx[i] else event_time
    signal_ts <- if (is.null(signal_time)) idx[i] else signal_time
    trades[[length(trades) + 1L]] <<- .bt_native_trade_row(
      timestamp = timestamp,
      symbol = symbol,
      qty = qty,
      qty_delta = qty_delta,
      units = unit_count,
      price = price,
      costs = costs,
      reason = reason,
      equity_after = equity_after,
      signal_time = signal_ts,
      execution_timeframe = execution_timeframe,
      execution_source_ticker = execution_source_ticker
    )
    invisible(NULL)
  }

  add_funding <- function(timestamp, qty_now, price, funding_rate, funding_cash, equity_after) {
    funding_ledger[[length(funding_ledger) + 1L]] <<- data.frame(
      timestamp = as.POSIXct(timestamp, tz = "UTC"),
      symbol = symbol,
      qty = qty_now,
      price = price,
      funding_rate = funding_rate,
      funding_cash = funding_cash,
      equity = equity_after,
      stringsAsFactors = FALSE
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

  pyramid_order_qty <- function(side) {
    sizing <- strategy$params$pyramid_sizing %||% "risk"
    if (!identical(sizing, "entry_qty")) {
      return(NA_real_)
    }
    qty_abs <- abs(entry_qty_abs) * (strategy$params$pyramid_qty_pct %||% 1)
    if (isTRUE(risk$integer_qty)) {
      qty_abs <- floor(qty_abs)
    }
    if (!is.finite(qty_abs) || qty_abs <= 0) {
      return(0)
    }
    if (identical(side, "long")) qty_abs else -qty_abs
  }

  signal_mode <- identical(execution_mode, "breakout") && identical(strategy$type, "donchian")
  signal_long_entry <- as.logical(signals[, "LongEntry"])
  signal_long_entry[is.na(signal_long_entry)] <- FALSE
  signal_long_exit <- as.logical(signals[, "LongExit"])
  signal_long_exit[is.na(signal_long_exit)] <- FALSE
  signal_short_entry <- as.logical(signals[, "ShortEntry"])
  signal_short_entry[is.na(signal_short_entry)] <- FALSE
  signal_short_exit <- as.logical(signals[, "ShortExit"])
  signal_short_exit[is.na(signal_short_exit)] <- FALSE
  signal_upper_enabled <- signal_long_entry | signal_short_exit
  signal_lower_enabled <- signal_long_exit | signal_short_entry
  signal_has_stop_event <- signal_upper_enabled | signal_lower_enabled

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

  execution_touch_time <- function(sig_i, event, signal_price) {
    fallback <- as.POSIXct(idx[sig_i], tz = "UTC")
    if (is.null(execution_detail)) {
      return(fallback)
    }
    signal_price <- suppressWarnings(as.numeric(signal_price)[1])
    if (!is.finite(signal_price) || signal_price <= 0) {
      return(fallback)
    }
    start <- as.POSIXct(idx[sig_i], tz = "UTC")
    end <- .bt_native_signal_bar_end(idx, sig_i, execution_detail$signal_timeframe_seconds)
    ex_idx <- execution_detail$index
    rows <- which(ex_idx >= start & ex_idx < end)
    if (!length(rows)) {
      stop(
        sprintf(
          "Execution timeframe '%s' for '%s' has no bars inside signal bar %s.",
          execution_detail$timeframe,
          execution_detail$symbol,
          format(start, "%Y-%m-%d %H:%M:%S")
        ),
        call. = FALSE
      )
    }
    px <- execution_detail$prices
    touched <- if (identical(event, "upper")) {
      suppressWarnings(as.numeric(px$high[rows])) >= signal_price
    } else {
      suppressWarnings(as.numeric(px$low[rows])) <= signal_price
    }
    touched[is.na(touched)] <- FALSE
    hit <- rows[which(touched)[1]]
    if (!length(hit) || is.na(hit)) {
      stop(
        sprintf(
          "Execution timeframe '%s' for '%s' did not confirm %s breakout %.10g inside signal bar %s.",
          execution_detail$timeframe,
          execution_detail$symbol,
          event,
          signal_price,
          format(start, "%Y-%m-%d %H:%M:%S")
        ),
        call. = FALSE
      )
    }
    ex_idx[hit]
  }

  stop_event_sequence <- function(sig_i, qty_now) {
    upper_touched <- isTRUE(as.logical(signals[sig_i, "LongEntry"])) ||
      isTRUE(as.logical(signals[sig_i, "ShortExit"]))
    lower_touched <- isTRUE(as.logical(signals[sig_i, "LongExit"])) ||
      isTRUE(as.logical(signals[sig_i, "ShortEntry"]))
    if (!upper_touched && !lower_touched) {
      return(character())
    }
    if (upper_touched && lower_touched && !is.null(execution_detail)) {
      upper_time <- execution_touch_time(sig_i, "upper", stop_event_price(sig_i, "upper"))
      lower_time <- execution_touch_time(sig_i, "lower", stop_event_price(sig_i, "lower"))
      if (!is.na(upper_time) && !is.na(lower_time) && upper_time != lower_time) {
        if (upper_time < lower_time) {
          return(c("upper", "lower"))
        }
        return(c("lower", "upper"))
      }
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

  stop_event_enabled <- function(sig_i, event) {
    if (identical(event, "upper")) {
      return(isTRUE(signal_upper_enabled[[sig_i]]))
    }
    isTRUE(signal_lower_enabled[[sig_i]])
  }

  execution_rows_for_signal <- function(sig_i) {
    if (is.null(execution_detail)) {
      return(integer())
    }
    if (!isTRUE(signal_has_stop_event[[sig_i]])) {
      return(integer())
    }
    start <- as.POSIXct(idx[sig_i], tz = "UTC")
    end <- .bt_native_signal_bar_end(idx, sig_i, execution_detail$signal_timeframe_seconds)
    start_num <- as.numeric(start)
    end_num <- as.numeric(end)
    first <- lower_bound(execution_index_num, start_num)
    last_exclusive <- lower_bound(execution_index_num, end_num)
    rows <- if (first < last_exclusive) seq.int(first, last_exclusive - 1L) else integer()
    if (!length(rows)) {
      stop(
        sprintf(
          "Execution timeframe '%s' for '%s' has no bars inside signal bar %s.",
          execution_detail$timeframe,
          execution_detail$symbol,
          format(start, "%Y-%m-%d %H:%M:%S")
        ),
        call. = FALSE
      )
    }
    upper_enabled <- isTRUE(signal_upper_enabled[[sig_i]])
    lower_enabled <- isTRUE(signal_lower_enabled[[sig_i]])
    upper_price <- if (upper_enabled) stop_event_price(sig_i, "upper") else NA_real_
    lower_price <- if (lower_enabled) stop_event_price(sig_i, "lower") else NA_real_
    touched <- rep(FALSE, length(rows))
    if (upper_enabled && is.finite(upper_price)) {
      touched <- touched | execution_high[rows] >= upper_price
    }
    if (lower_enabled && is.finite(lower_price)) {
      touched <- touched | execution_low[rows] <= lower_price
    }
    touched[is.na(touched)] <- FALSE
    rows[touched]
  }

  execution_bar_event_sequence <- function(sig_i, exec_row, qty_now) {
    if (is.null(execution_detail)) {
      return(character())
    }
    upper_price <- stop_event_price(sig_i, "upper")
    lower_price <- stop_event_price(sig_i, "lower")
    upper_touched <- stop_event_enabled(sig_i, "upper") &&
      is.finite(upper_price) &&
      execution_high[exec_row] >= upper_price
    lower_touched <- stop_event_enabled(sig_i, "lower") &&
      is.finite(lower_price) &&
      execution_low[exec_row] <= lower_price
    if (!upper_touched && !lower_touched) {
      return(character())
    }
    if (upper_touched && lower_touched) {
      open_px <- execution_open[exec_row]
      if (is.finite(open_px)) {
        if (open_px >= upper_price) {
          return(c("upper", "lower"))
        }
        if (open_px <= lower_price) {
          return(c("lower", "upper"))
        }
      }
      if (qty_now > 0) {
        return(c("lower", "upper"))
      }
      if (qty_now < 0) {
        return(c("upper", "lower"))
      }
      if (!is.finite(open_px)) {
        return(character())
      }
      if (abs(open_px - upper_price) <= abs(open_px - lower_price)) {
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

  apply_funding <- function(i, eq, fallback_price) {
    if (!NROW(funding_events) || qty == 0 || i <= 1L) {
      return(eq)
    }
    prev_time <- as.POSIXct(idx[i - 1L], tz = "UTC")
    current_time <- as.POSIXct(idx[i], tz = "UTC")
    rows <- which(funding_times > prev_time & funding_times <= current_time)
    if (!length(rows)) {
      return(eq)
    }
    for (row_i in rows) {
      rate <- suppressWarnings(as.numeric(funding_events$funding_rate[row_i]))
      if (!is.finite(rate)) {
        next
      }
      mark <- suppressWarnings(as.numeric(funding_events$mark_price[row_i]))
      if (!is.finite(mark) || mark <= 0) {
        mark <- suppressWarnings(as.numeric(fallback_price)[1])
      }
      if (!is.finite(mark) || mark <= 0) {
        next
      }
      cash <- -qty * mark * funding_multiplier * rate
      if (!is.finite(cash) || cash == 0) {
        next
      }
      eq <- eq + cash
      add_funding(funding_times[row_i], qty, mark, rate, cash, eq)
    }
    eq
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

  open_hold_position <- function() {
    if (!identical(strategy$type, "hold") || n < 1L || qty != 0) {
      return(invisible(NULL))
    }
    px <- exec_close[1L]
    if (!is.finite(px) || px <= 0) {
      return(invisible(NULL))
    }
    side <- if (isTRUE(strategy$long)) {
      "long"
    } else if (isTRUE(strategy$short)) {
      "short"
    } else {
      NULL
    }
    if (is.null(side)) {
      return(invisible(NULL))
    }
    delta <- .bt_native_size(side, px, NA_real_, equity[1L], risk, metadata,
      tick_value = .bt_native_tick_value_at(data, 1L, metadata)
    )
    if (!is.finite(delta) || delta == 0) {
      return(invisible(NULL))
    }
    costs <- txn_cost(delta, px, 1L)
    qty <<- qty + delta
    unit_count <<- 1L
    entry_qty_abs <<- abs(delta)
    last_add_price <<- px
    last_add_signal_price <<- add_signal_basis(1L, px)
    equity[1L] <<- equity[1L] - costs[["total_cost"]]
    qty_path[1L] <<- qty
    unit_path[1L] <<- unit_count
    add_trade(1L, delta, px, costs, paste0(side, "_entry"), equity[1L])
    invisible(NULL)
  }

  process_stop_event <- function(i, sig_i, event, eq, px, event_time = NULL) {
    event_signal_price <- stop_event_price(sig_i, event)
    event_timestamp <- if (is.null(event_time)) {
      execution_touch_time(sig_i, event, event_signal_price)
    } else {
      as.POSIXct(event_time, tz = "UTC")
    }
    signal_time <- as.POSIXct(idx[sig_i], tz = "UTC")
    if (identical(event, "upper")) {
      if (qty < 0 && isTRUE(as.logical(signals[sig_i, "ShortExit"]))) {
        delta <- -qty
        costs <- txn_cost(delta, px, i)
        qty <<- 0
        unit_count <<- 0L
        entry_qty_abs <<- NA_real_
        last_add_price <<- NA_real_
        last_add_signal_price <<- NA_real_
        eq <- eq - costs[["total_cost"]]
        add_trade(i, delta, px, costs, "short_exit", eq, event_time = event_timestamp, signal_time = signal_time)
      }
      if (qty == 0 && strategy$long && isTRUE(as.logical(signals[sig_i, "LongEntry"]))) {
        delta <- maybe_enter(i, sig_i, "long", eq, px)
        if (delta != 0) {
          costs <- txn_cost(delta, px, i)
          qty <<- qty + delta
          unit_count <<- 1L
          entry_qty_abs <<- abs(delta)
          last_add_price <<- px
          last_add_signal_price <<- add_signal_basis(sig_i, px, event)
          eq <- eq - costs[["total_cost"]]
          add_trade(i, delta, px, costs, "long_entry", eq, event_time = event_timestamp, signal_time = signal_time)
        }
      }
    } else {
      if (qty > 0 && isTRUE(as.logical(signals[sig_i, "LongExit"]))) {
        delta <- -qty
        costs <- txn_cost(delta, px, i)
        qty <<- 0
        unit_count <<- 0L
        entry_qty_abs <<- NA_real_
        last_add_price <<- NA_real_
        last_add_signal_price <<- NA_real_
        eq <- eq - costs[["total_cost"]]
        add_trade(i, delta, px, costs, "long_exit", eq, event_time = event_timestamp, signal_time = signal_time)
      }
      if (qty == 0 && strategy$short && isTRUE(as.logical(signals[sig_i, "ShortEntry"]))) {
        delta <- maybe_enter(i, sig_i, "short", eq, px)
        if (delta != 0) {
          costs <- txn_cost(delta, px, i)
          qty <<- qty + delta
          unit_count <<- 1L
          entry_qty_abs <<- abs(delta)
          last_add_price <<- px
          last_add_signal_price <<- add_signal_basis(sig_i, px, event)
          eq <- eq - costs[["total_cost"]]
          add_trade(i, delta, px, costs, "short_entry", eq, event_time = event_timestamp, signal_time = signal_time)
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
    step <- if (unit_count <= 1L) {
      strategy$params$pyramid_start %||% strategy$params$pyramid_step
    } else {
      strategy$params$pyramid_step
    }
    if (!is.finite(step) || step <= 0) {
      return(NA_real_)
    }
    if (qty > 0) {
      basis + step * atr
    } else {
      basis - step * atr
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
    delta <- pyramid_order_qty(side)
    if (!is.finite(delta)) {
      delta <- maybe_enter(i, sig_i, side, eq, px)
    }
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

  open_hold_position()

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
      eq <- apply_funding(i, eq, close_i)

      if (!is.null(execution_detail)) {
        exec_rows <- execution_rows_for_signal(i)
        for (exec_row in exec_rows) {
          events <- execution_bar_event_sequence(i, exec_row, qty)
          if (!length(events)) {
            next
          }
          event_time <- execution_detail$index[exec_row]
          for (event in events) {
            px <- stop_event_exec_price(i, event, fallback = close_i)
            if (!is.finite(px) || px <= 0) {
              next
            }
            eq <- mark_to_price(eq, last_px, px)
            eq <- process_stop_event(i, i, event, eq, px, event_time = event_time)
            last_px <- px
          }
        }
      } else {
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
          unit_before <- unit_count
          eq <- process_pyramid(i, i, eq, pyr_px, signal_px = pyr[["signal_price"]])
          if (unit_count <= unit_before) {
            break
          }
          last_px <- pyr_px
        }
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
    eq <- apply_funding(i, eq, close_i)

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
        entry_qty_abs <- NA_real_
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
        entry_qty_abs <- NA_real_
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
            entry_qty_abs <- abs(delta)
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
      entry_qty_abs <- NA_real_
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
  funding_df <- if (length(funding_ledger)) do.call(rbind, funding_ledger) else .bt_native_empty_funding_ledger()
  list(equity = equity_xts, positions = positions, trades = trades_df, funding = funding_df)
}

.bt_native_simulate_portfolio <- function(items, initial_equity, max_positions = Inf, priority = c("order")) {
  priority <- match.arg(priority)
  if (!length(items)) {
    stop("'items' cannot be empty.", call. = FALSE)
  }
  if (!identical(priority, "order")) {
    stop("Only order priority is supported.", call. = FALSE)
  }

  to_posix <- function(x) {
    if (inherits(x, "POSIXt")) {
      return(as.POSIXct(x, tz = "UTC"))
    }
    as.POSIXct(as.Date(x), tz = "UTC")
  }
  time_num <- lapply(items, function(item) as.numeric(to_posix(item$prices$index)))
  timeline_num <- sort(unique(unlist(time_num, use.names = FALSE)))
  timeline_num <- timeline_num[is.finite(timeline_num)]
  if (!length(timeline_num)) {
    stop("Portfolio instruments have no valid timestamps.", call. = FALSE)
  }
  timeline_index <- as.POSIXct(timeline_num, origin = "1970-01-01", tz = "UTC")
  time_keys <- as.character(timeline_num)
  row_lookup <- lapply(time_num, function(x) {
    stats::setNames(seq_along(x), as.character(x))
  })

  n_times <- length(timeline_index)
  n_items <- length(items)
  symbols <- names(items)
  if (is.null(symbols) || any(!nzchar(symbols))) {
    symbols <- vapply(items, `[[`, character(1), "symbol")
  }

  item_uses_pu <- vapply(seq_along(items), function(j) {
    item <- items[[j]]
    isTRUE(item$prices$uses_pu) && .bt_is_di_symbol(item$source_symbol %||% item$symbol)
  }, logical(1))
  item_pnl_multiplier <- vapply(seq_along(items), function(j) {
    mult <- suppressWarnings(as.numeric(items[[j]]$metadata$multiplier)[1])
    if (!is.finite(mult) || mult <= 0) mult <- 1
    mult * if (isTRUE(item_uses_pu[j])) -1 else 1
  }, numeric(1))
  item_calendars <- lapply(item_uses_pu, function(uses_pu) {
    if (isTRUE(uses_pu)) tryCatch(.generate_calendar(), error = function(e) NULL) else NULL
  })
  states <- lapply(seq_along(items), function(j) {
    list(qty = 0, units = 0L, last_price = NA_real_, last_i = NA_integer_)
  })

  qty_path <- matrix(0, nrow = n_times, ncol = n_items)
  value_path <- matrix(0, nrow = n_times, ncol = n_items)
  close_path <- matrix(NA_real_, nrow = n_times, ncol = n_items)
  colnames(qty_path) <- paste0(symbols, ".qty")
  colnames(value_path) <- paste0(symbols, ".position_value")
  colnames(close_path) <- paste0(symbols, ".close")
  equity <- numeric(n_times)
  equity_now <- suppressWarnings(as.numeric(initial_equity)[1])
  if (!is.finite(equity_now) || equity_now <= 0) {
    stop("'initial_equity' must be positive.", call. = FALSE)
  }
  trades <- list()

  current_row <- function(j, key) {
    hit <- match(key, names(row_lookup[[j]]), nomatch = 0L)
    if (hit <= 0L) {
      return(NA_integer_)
    }
    as.integer(row_lookup[[j]][hit])
  }
  exec_close_at <- function(item, i) {
    px <- if (!is.null(item$prices$exec_close)) item$prices$exec_close[i] else item$prices$close[i]
    suppressWarnings(as.numeric(px)[1])
  }
  event_price <- function(item, j, i, event, fallback) {
    if (!identical(item$execution$execution, "breakout") || !identical(item$strategy$type, "donchian")) {
      return(fallback)
    }
    column <- if (identical(event, "upper")) "DonchianUpper" else "DonchianLower"
    rate_px <- suppressWarnings(as.numeric(item$indicators[i, column])[1])
    if (!is.finite(rate_px) || rate_px <= 0) {
      return(fallback)
    }
    if (!isTRUE(item_uses_pu[j])) {
      return(rate_px)
    }
    pu <- .bt_native_di_rate_to_pu(
      rate_px,
      item$data,
      i,
      item$source_symbol %||% item$symbol,
      cal = item_calendars[[j]]
    )
    if (is.finite(pu) && pu > 0) pu else fallback
  }
  sizing_qty <- function(item, j, i, side, eq, price) {
    rate_px <- suppressWarnings(as.numeric(item$prices$close[i])[1])
    stop_px <- .bt_native_sizing_stop_price(
      i,
      side,
      price = price,
      indicators = item$indicators,
      strategy = item$strategy,
      risk = item$risk,
      rate_price = rate_px
    )
    if (identical(item$strategy$type, "donchian") &&
        identical(item$risk$mode, "risk") &&
        !is.finite(stop_px)) {
      return(0)
    }
    if (isTRUE(item_uses_pu[j])) {
      stop_pu <- .bt_native_di_rate_to_pu(
        stop_px,
        item$data,
        i,
        item$source_symbol %||% item$symbol,
        cal = item_calendars[[j]]
      )
      if (is.finite(stop_pu) && stop_pu > 0) {
        stop_px <- stop_pu
      }
    }
    tick_value <- .bt_native_tick_value_at(item$data, i, item$metadata)
    .bt_native_size(side, price, stop_px, eq, item$risk, item$metadata, tick_value = tick_value)
  }
  txn_cost <- function(item, i, qty_delta, price) {
    tick_value <- .bt_native_tick_value_at(item$data, i, item$metadata)
    .bt_native_txn_cost_components(qty_delta, price, item$execution, item$metadata, tick_value = tick_value)
  }
  add_trade <- function(t, j, qty_delta, price, costs, reason) {
    item <- items[[j]]
    trades[[length(trades) + 1L]] <<- .bt_native_trade_row(
      timestamp = timeline_index[t],
      symbol = item$symbol,
      qty = states[[j]]$qty,
      qty_delta = qty_delta,
      units = states[[j]]$units,
      price = price,
      costs = costs,
      reason = reason,
      equity_after = equity_now
    )
    invisible(NULL)
  }
  signal_flag <- function(item, i, column) {
    isTRUE(as.logical(item$signals[i, column]))
  }
  open_positions <- function() {
    sum(vapply(states, function(st) st$qty != 0, logical(1)))
  }

  for (t in seq_len(n_times)) {
    key <- time_keys[t]
    rows <- vapply(seq_along(items), current_row, integer(1), key = key)
    close_i <- rep(NA_real_, n_items)
    row_last_px <- rep(NA_real_, n_items)

    for (j in seq_along(items)) {
      i <- rows[j]
      if (is.na(i)) next
      item <- items[[j]]
      px <- exec_close_at(item, i)
      if (!is.finite(px) || px <= 0) next
      close_i[j] <- px
      row_last_px[j] <- px
      if (states[[j]]$qty != 0 && is.finite(states[[j]]$last_price)) {
        equity_now <- equity_now + states[[j]]$qty * (px - states[[j]]$last_price) * item_pnl_multiplier[j]
      }
      states[[j]]$last_price <- px
      states[[j]]$last_i <- i
    }

    exited_from <- rep(NA_character_, n_items)
    for (j in seq_along(items)) {
      i <- rows[j]
      if (is.na(i) || states[[j]]$qty == 0 || !is.finite(close_i[j])) next
      item <- items[[j]]
      qty_now <- states[[j]]$qty
      exit_event <- NULL
      reason <- NULL
      if (qty_now > 0 && signal_flag(item, i, "LongExit")) {
        exit_event <- "lower"
        reason <- "long_exit"
        exited_from[j] <- "long"
      } else if (qty_now < 0 && signal_flag(item, i, "ShortExit")) {
        exit_event <- "upper"
        reason <- "short_exit"
        exited_from[j] <- "short"
      }
      if (is.null(exit_event)) next
      px <- event_price(item, j, i, exit_event, fallback = close_i[j])
      if (!is.finite(px) || px <= 0) next
      if (is.finite(row_last_px[j])) {
        equity_now <- equity_now + qty_now * (px - row_last_px[j]) * item_pnl_multiplier[j]
      }
      row_last_px[j] <- px
      delta <- -qty_now
      costs <- txn_cost(item, i, delta, px)
      states[[j]]$qty <- 0
      states[[j]]$units <- 0L
      equity_now <- equity_now - costs[["total_cost"]]
      add_trade(t, j, delta, px, costs, reason)
    }

    candidates <- list()
    for (j in seq_along(items)) {
      i <- rows[j]
      if (is.na(i) || states[[j]]$qty != 0 || !is.finite(close_i[j])) next
      item <- items[[j]]
      le <- signal_flag(item, i, "LongEntry")
      se <- signal_flag(item, i, "ShortEntry")
      side <- NULL
      event <- NULL
      if (identical(exited_from[j], "long") && item$strategy$short && se) {
        side <- "short"
        event <- "lower"
      } else if (identical(exited_from[j], "short") && item$strategy$long && le) {
        side <- "long"
        event <- "upper"
      } else if (item$strategy$long && le && !(item$strategy$short && se)) {
        side <- "long"
        event <- "upper"
      } else if (item$strategy$short && se && !(item$strategy$long && le)) {
        side <- "short"
        event <- "lower"
      }
      if (is.null(side)) next
      candidates[[length(candidates) + 1L]] <- list(j = j, i = i, side = side, event = event)
    }

    slots <- max_positions - open_positions()
    if (is.finite(slots) && slots <= 0) {
      candidates <- list()
    } else if (is.finite(slots) && length(candidates) > slots) {
      candidates <- candidates[seq_len(slots)]
    }
    for (candidate in candidates) {
      j <- candidate$j
      i <- candidate$i
      item <- items[[j]]
      px <- event_price(item, j, i, candidate$event, fallback = close_i[j])
      if (!is.finite(px) || px <= 0) next
      delta <- sizing_qty(item, j, i, candidate$side, equity_now, px)
      if (!is.finite(delta) || delta == 0) next
      costs <- txn_cost(item, i, delta, px)
      states[[j]]$qty <- states[[j]]$qty + delta
      states[[j]]$units <- 1L
      row_last_px[j] <- px
      equity_now <- equity_now - costs[["total_cost"]]
      add_trade(t, j, delta, px, costs, paste0(candidate$side, "_entry"))
    }

    for (j in seq_along(items)) {
      if (is.na(rows[j]) || states[[j]]$qty == 0 || !is.finite(close_i[j])) next
      if (is.finite(row_last_px[j]) && row_last_px[j] != close_i[j]) {
        equity_now <- equity_now + states[[j]]$qty * (close_i[j] - row_last_px[j]) * item_pnl_multiplier[j]
      }
      states[[j]]$last_price <- close_i[j]
    }

    equity[t] <- equity_now
    for (j in seq_along(items)) {
      qty_path[t, j] <- states[[j]]$qty
      px <- states[[j]]$last_price
      if (is.finite(close_i[j])) {
        close_path[t, j] <- close_i[j]
      } else if (is.finite(px)) {
        close_path[t, j] <- px
      }
      if (states[[j]]$qty != 0 && is.finite(px)) {
        value_path[t, j] <- states[[j]]$qty * px * item_pnl_multiplier[j]
      }
    }
  }

  if (n_times > 0) {
    t <- n_times
    for (j in seq_along(items)) {
      item <- items[[j]]
      if (!isTRUE(item$execution$close_on_end) || states[[j]]$qty == 0) next
      i <- states[[j]]$last_i
      px <- states[[j]]$last_price
      if (!is.finite(i) || !is.finite(px) || px <= 0) next
      delta <- -states[[j]]$qty
      costs <- txn_cost(item, i, delta, px)
      states[[j]]$qty <- 0
      states[[j]]$units <- 0L
      equity_now <- equity_now - costs[["total_cost"]]
      add_trade(t, j, delta, px, costs, "end_exit")
      qty_path[t, j] <- 0
      value_path[t, j] <- 0
    }
    equity[t] <- equity_now
  }

  equity_xts <- xts::xts(equity, order.by = timeline_index)
  colnames(equity_xts) <- "Equity"
  positions <- xts::xts(
    cbind(qty_path, close_path, value_path, equity = equity),
    order.by = timeline_index
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

.bt_native_is_risk_normalized <- function(rets) {
  target <- suppressWarnings(as.numeric(attr(rets, "risk_target", exact = TRUE))[1])
  scale <- suppressWarnings(as.numeric(attr(rets, "risk_scale", exact = TRUE))[1])
  is.finite(target) && target > 0 && is.finite(scale) && scale > 0
}

.bt_native_risk_target <- function(rets) {
  target <- suppressWarnings(as.numeric(attr(rets, "risk_target", exact = TRUE))[1])
  if (is.finite(target) && target > 0) target else NA_real_
}

.bt_native_risk_label <- function(rets) {
  if (!.bt_native_is_risk_normalized(rets)) {
    return("Raw simulation")
  }
  paste0("Risk-normalized ", .bt_native_format_plain_num(.bt_native_risk_target(rets)), "% vol")
}

.bt_native_risk_title_suffix <- function(rets) {
  if (!.bt_native_is_risk_normalized(rets)) {
    return("")
  }
  paste0(" Risk-Normalized ", .bt_native_format_plain_num(.bt_native_risk_target(rets)), "%")
}

.bt_native_performance_equity <- function(raw_equity, rets, initial_equity) {
  if (!.bt_native_is_risk_normalized(rets) || is.null(rets) || !"Discrete" %in% colnames(rets)) {
    return(raw_equity)
  }
  start <- suppressWarnings(as.numeric(initial_equity)[1])
  if (!is.finite(start) || start <= 0) {
    start <- suppressWarnings(as.numeric(raw_equity[1]))
  }
  if (!is.finite(start) || start <= 0) {
    start <- 1
  }
  discrete <- suppressWarnings(as.numeric(rets$Discrete))
  discrete[!is.finite(discrete)] <- 0
  values <- start * cumprod(1 + discrete)
  out <- xts::xts(values, order.by = zoo::index(rets))
  colnames(out) <- colnames(raw_equity)[1] %||% "Equity"
  out
}

.bt_native_risk_normalization_block <- function(rets) {
  if (!.bt_native_is_risk_normalized(rets)) {
    return(.bt_info_block("risk_normalization", "Risk Normalization", list(), order = 55, contexts = c("report", "stats")))
  }
  target <- .bt_native_risk_target(rets)
  scale <- suppressWarnings(as.numeric(attr(rets, "risk_scale", exact = TRUE))[1])
  original <- suppressWarnings(as.numeric(attr(rets, "risk_original", exact = TRUE))[1])
  rows <- list(
    "Performance Source" = .bt_native_risk_label(rets),
    "Target Annual Vol" = .bt_native_format_pct(target / 100),
    "Original Annual Vol" = if (is.finite(original)) .bt_native_format_pct(original / 100) else NULL,
    "Return Scale" = .bt_native_format_plain_num(scale),
    "Execution/Costs" = "Raw simulation"
  )
  .bt_info_block("risk_normalization", "Risk Normalization", rows, order = 55, contexts = c("report", "stats"))
}

.bt_native_report <- function(symbol, strategy, info_blocks,
                              geometric = TRUE, verbose = FALSE,
                              show_quarterly = FALSE,
                              start_time = Sys.time()) {
  .dbg("Results for ", symbol, " - ", .bt_native_report_type(strategy), "\n")
  if (isTRUE(verbose)) {
    full_blocks <- .bt_filter_info_blocks(info_blocks, context = NULL)
    print(full_blocks)
  }

  .bt_print_info_blocks(
    info_blocks,
    context = "report",
    include = if (isTRUE(show_quarterly)) c("quarterly_returns", "quarterly_profit") else NULL
  )

  total_secs <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")))
  hrs <- total_secs %/% 3600
  mins <- (total_secs %% 3600) %/% 60
  secs <- total_secs %% 60

  cat("\n-----------------------------------\n")
  cat(sprintf("Runtime: %02dh %02dm %02ds", hrs, mins, secs))
  cat("\n-----------------------------------\n\n")

  invisible(NULL)
}

.bt_info_block <- function(id, title, rows, order = 100, contexts = c("report", "stats")) {
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    out <- data.frame(stat_name = character(), value = character(), stringsAsFactors = FALSE)
  } else {
    values <- vapply(rows, function(x) {
      if (length(x) == 0 || is.na(x[1])) {
        ""
      } else {
        as.character(x[1])
      }
    }, character(1))
    keep <- nzchar(values)
    out <- data.frame(
      stat_name = names(values)[keep],
      value = unname(values[keep]),
      stringsAsFactors = FALSE
    )
  }
  attr(out, "id") <- id
  attr(out, "title") <- title
  attr(out, "order") <- order
  attr(out, "contexts") <- contexts
  attr(out, "type") <- "key_value"
  class(out) <- c("bt_info_block", "data.frame")
  out
}

.bt_info_block_from_matrix <- function(id, title, mat, order = 100, contexts = c("report", "stats")) {
  if (is.null(mat) || !length(mat)) {
    rows <- list()
  } else if (NCOL(mat) == 1) {
    vals <- as.character(mat[, 1])
    rows <- as.list(vals)
    names(rows) <- rownames(mat)
  } else {
    rows <- list()
  }
  .bt_info_block(id, title, rows, order = order, contexts = contexts)
}

.bt_info_table_block <- function(id, title, table, order = 100, contexts = "report") {
  out <- as.data.frame(table, stringsAsFactors = FALSE)
  attr(out, "id") <- id
  attr(out, "title") <- title
  attr(out, "order") <- order
  attr(out, "contexts") <- contexts
  attr(out, "type") <- "table"
  class(out) <- c("bt_info_table_block", "data.frame")
  out
}

.bt_info_block_id <- function(block) attr(block, "id", exact = TRUE) %||% ""
.bt_info_block_title <- function(block) attr(block, "title", exact = TRUE) %||% .bt_info_block_id(block)
.bt_info_block_contexts <- function(block) attr(block, "contexts", exact = TRUE) %||% character()
.bt_info_block_order <- function(block) attr(block, "order", exact = TRUE) %||% 100
.bt_info_block_type <- function(block) attr(block, "type", exact = TRUE) %||% "key_value"

.bt_filter_info_blocks <- function(blocks, context = "report", include = NULL, type = NULL) {
  if (is.null(blocks) || !length(blocks)) {
    return(list())
  }
  keep <- vapply(blocks, function(block) {
    id <- .bt_info_block_id(block)
    context_ok <- is.null(context) || context %in% .bt_info_block_contexts(block)
    include_ok <- !is.null(include) && id %in% include
    type_ok <- is.null(type) || identical(.bt_info_block_type(block), type)
    has_rows <- NROW(block) > 0
    (context_ok || include_ok) && type_ok && has_rows
  }, logical(1))
  out <- blocks[keep]
  if (!length(out)) {
    return(out)
  }
  out[order(vapply(out, .bt_info_block_order, numeric(1)))]
}

.bt_print_info_blocks <- function(blocks, context = "report", include = NULL) {
  selected <- .bt_filter_info_blocks(blocks, context = context, include = include)
  for (block in selected) {
    title <- .bt_info_block_title(block)
    cat(sprintf("\n--- %s ---\n", title))
    if (identical(.bt_info_block_type(block), "key_value")) {
      mat <- matrix(block$value, ncol = 1, dimnames = list(block$stat_name, "Value"))
      print(mat, quote = FALSE, right = TRUE)
    } else {
      print(as.data.frame(block), quote = FALSE, right = TRUE)
    }
    cat("\n")
  }
  invisible(selected)
}

.bt_native_pick_stat_chr <- function(stats, name, default = "") {
  if (is.null(stats) || !name %in% names(stats)) {
    return(default)
  }
  value <- as.character(stats[[name]][1])
  if (is.na(value) || !nzchar(value)) default else value
}

.bt_native_pick_stat_num <- function(stats, name, default = NA_real_) {
  if (is.null(stats) || !name %in% names(stats)) {
    return(default)
  }
  value <- suppressWarnings(as.numeric(stats[[name]][1]))
  if (is.finite(value)) value else default
}

.bt_native_format_plain_num <- function(x, digits = 4) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(x)) {
    return("")
  }
  out <- format(round(x, digits), trim = TRUE, scientific = FALSE)
  out <- sub("(\\.[0-9]*?)0+$", "\\1", out)
  sub("\\.$", "", out)
}

.bt_native_position_sizing_label <- function(stats) {
  ps_type <- .bt_native_pick_stat_chr(stats, "PosSiz", "unknown")
  ps_value <- .bt_native_pick_stat_num(stats, "PsValue")
  value <- .bt_native_format_plain_num(ps_value)
  if (!nzchar(value)) {
    return(ps_type)
  }
  if (identical(ps_type, "contract")) {
    return(paste(value, "contracts/shares"))
  }
  if (identical(ps_type, "notional")) {
    return(paste0(value, "% equity notional"))
  }
  if (identical(ps_type, "atr")) {
    return(paste0(value, "% equity risk to ATR stop"))
  }
  if (identical(ps_type, "eldoc")) {
    return(paste0(value, "% equity risk to ElDoc stop"))
  }
  paste(ps_type, value)
}

.bt_native_cost_setting_label <- function(value, type) {
  value_chr <- .bt_native_format_plain_num(value)
  type_chr <- if (is.null(type) || is.na(type) || !nzchar(type)) "" else as.character(type)
  if (!nzchar(value_chr) && !nzchar(type_chr)) {
    return("")
  }
  if (!nzchar(type_chr)) {
    return(value_chr)
  }
  paste(value_chr, type_chr)
}

.bt_native_config_summary_table <- function(stats) {
  fee <- .bt_native_cost_setting_label(
    .bt_native_pick_stat_num(stats, "FeeValue"),
    .bt_native_pick_stat_chr(stats, "FeeType")
  )
  slippage <- .bt_native_cost_setting_label(
    .bt_native_pick_stat_num(stats, "SlipValue"),
    .bt_native_pick_stat_chr(stats, "SlipType")
  )
  rows <- c(
    "Strategy",
    "Execution",
    "Position Sizing",
    "Initial Equity",
    "Reinvest",
    "Fee Mode",
    "Fee",
    "Slippage",
    "Multiplier",
    "Tick Size"
  )
  vals <- c(
    .bt_native_pick_stat_chr(stats, "Indicator"),
    .bt_native_pick_stat_chr(stats, "Execution"),
    .bt_native_position_sizing_label(stats),
    .bt_native_format_money(.bt_native_pick_stat_num(stats, "InitialEquity")),
    .bt_native_pick_stat_chr(stats, "Reinvest"),
    .bt_native_pick_stat_chr(stats, "FeeMode"),
    fee,
    slippage,
    .bt_native_format_plain_num(.bt_native_pick_stat_num(stats, "Multiplier")),
    .bt_native_format_plain_num(.bt_native_pick_stat_num(stats, "TickSize"))
  )
  matrix(vals, ncol = 1, dimnames = list(rows, "Value"))
}

.bt_native_print_config_summary <- function(stats) {
  cat("\n--- Backtest Configuration ---\n")
  print(.bt_native_config_summary_table(stats), quote = FALSE, right = TRUE)
  cat("\n")
  invisible(NULL)
}

.bt_native_empty_trade_episodes <- function() {
  data.frame(
    trade_id = integer(),
    symbol = character(),
    side = character(),
    entry_time = as.POSIXct(character()),
    exit_time = as.POSIXct(character()),
    entry_price = numeric(),
    exit_price = numeric(),
    entry_qty = numeric(),
    max_qty = numeric(),
    units_max = integer(),
    bars_held = integer(),
    gross_pnl = numeric(),
    net_pnl = numeric(),
    fees = numeric(),
    slippage = numeric(),
    funding = numeric(),
    total_cost = numeric(),
    entry_atr = numeric(),
    entry_atr_value_per_unit = numeric(),
    initial_risk_price = numeric(),
    initial_risk_pct = numeric(),
    initial_risk_cash = numeric(),
    initial_stop_atr = numeric(),
    initial_risk_per_unit = numeric(),
    final_R = numeric(),
    exit_reason = character(),
    stringsAsFactors = FALSE
  )
}

.bt_native_empty_trade_audit <- function() {
  data.frame(
    trade_id = integer(),
    symbol = character(),
    side = character(),
    event_type = character(),
    event_time = as.POSIXct(character(), tz = "UTC"),
    signal_time = as.POSIXct(character(), tz = "UTC"),
    execution_timeframe = character(),
    execution_source_ticker = character(),
    reason = character(),
    entry_time = as.POSIXct(character(), tz = "UTC"),
    exit_time = as.POSIXct(character(), tz = "UTC"),
    bars_held = integer(),
    signal_price = numeric(),
    fill_price = numeric(),
    qty_delta = numeric(),
    position_qty = numeric(),
    units = integer(),
    fees = numeric(),
    slippage = numeric(),
    total_cost = numeric(),
    funding_cash = numeric(),
    funding_received = numeric(),
    funding_paid = numeric(),
    funding_events = integer(),
    gross_pnl = numeric(),
    net_pnl = numeric(),
    equity = numeric(),
    stringsAsFactors = FALSE
  )
}

.bt_native_audit_event_type <- function(reason) {
  reason <- as.character(reason)[1]
  if (reason %in% c("long_entry", "short_entry")) {
    return("entry")
  }
  if (reason %in% c("long_pyramid", "short_pyramid")) {
    return("pyramid")
  }
  if (reason %in% c("long_exit", "short_exit", "end_exit")) {
    return("exit")
  }
  "order"
}

.bt_native_slippage_adjusted_price <- function(row, multiplier) {
  price <- suppressWarnings(as.numeric(row$price[1]))
  qty_delta <- suppressWarnings(as.numeric(row$qty_delta[1]))
  slippage <- suppressWarnings(as.numeric(row$slippage[1]))
  multiplier <- suppressWarnings(abs(as.numeric(multiplier)[1]))
  if (!is.finite(price) || !is.finite(qty_delta) || qty_delta == 0 ||
      !is.finite(slippage) || slippage == 0 ||
      !is.finite(multiplier) || multiplier <= 0) {
    return(price)
  }
  slip_price <- slippage / (abs(qty_delta) * multiplier)
  if (!is.finite(slip_price)) {
    return(price)
  }
  if (qty_delta > 0) price + slip_price else price - slip_price
}

.bt_native_excursion_thresholds <- function() {
  c(5, 10, 15, 20, 25, 30, 35, 40)
}

.bt_native_threshold_col <- function(prefix, threshold, suffix = "") {
  paste0(prefix, "_", gsub("\\.", "_", sprintf("%.1f", threshold)), suffix)
}

.bt_native_empty_trade_excursions <- function() {
  out <- data.frame(
    trade_id = integer(),
    symbol = character(),
    side = character(),
    entry_time = as.POSIXct(character()),
    exit_time = as.POSIXct(character()),
    entry_price = numeric(),
    exit_price = numeric(),
    entry_atr = numeric(),
    mfe_price = numeric(),
    mae_price = numeric(),
    mfe_pct = numeric(),
    mae_pct = numeric(),
    mfe_atr = numeric(),
    mae_atr = numeric(),
    mfe_R = numeric(),
    mae_R = numeric(),
    final_R = numeric(),
    bars_to_mfe = integer(),
    bars_to_mae = integer(),
    stringsAsFactors = FALSE
  )
  for (threshold in .bt_native_excursion_thresholds()) {
    out[[.bt_native_threshold_col("hit", threshold, "_atr")]] <- logical()
    out[[.bt_native_threshold_col("post", threshold, "_atr_R")]] <- numeric()
  }
  out
}

.bt_native_empty_pyramid_events <- function() {
  data.frame(
    trade_id = integer(),
    event_time = as.POSIXct(character()),
    side = character(),
    unit_number = integer(),
    trigger_atr = numeric(),
    fill_price = numeric(),
    initial_qty = numeric(),
    qty_added = numeric(),
    position_after_add = numeric(),
    add_entry_qty_ratio = numeric(),
    position_entry_qty_ratio = numeric(),
    atr_at_add = numeric(),
    risk_price_at_add = numeric(),
    stop_distance_atr_at_add = numeric(),
    fees = numeric(),
    slippage = numeric(),
    total_cost = numeric(),
    add_fee_per_contract = numeric(),
    add_slip_per_contract = numeric(),
    add_cost_per_contract = numeric(),
    add_cost_pct_notional = numeric(),
    entry_fee_per_contract = numeric(),
    entry_slip_per_contract = numeric(),
    entry_cost_per_contract = numeric(),
    entry_cost_pct_notional = numeric(),
    stringsAsFactors = FALSE
  )
}

.bt_native_price_columns_for_excursion <- function(mktdata) {
  cn <- colnames(mktdata)
  pick <- function(cands) {
    hit <- intersect(cands, cn)
    if (length(hit)) hit[1] else NA_character_
  }
  high_col <- pick(c("PU_High", "PU_H", "exec_high", "High", "high", "Hi"))
  low_col <- pick(c("PU_Low", "PU_L", "exec_low", "Low", "low", "Lo"))
  close_col <- pick(c("PU_Close", "PU_C", "exec_close", "Close", "close", "Cl"))
  list(
    high = high_col,
    low = low_col,
    close = close_col,
    ok = !any(is.na(c(high_col, low_col, close_col)))
  )
}

.bt_native_which_index <- function(idx, time) {
  if (!length(idx) || is.null(time) || is.na(time)) {
    return(NA_integer_)
  }
  time <- as.POSIXct(time, tz = "UTC")
  idx_posix <- as.POSIXct(idx, tz = "UTC")
  hit <- which(idx_posix == time)
  if (length(hit)) {
    return(hit[1])
  }
  before <- which(idx_posix <= time)
  if (length(before)) {
    return(utils::tail(before, 1))
  }
  NA_integer_
}

.bt_native_trade_risk_price <- function(i, side, entry_price, mktdata, strategy, risk) {
  if (!is.finite(entry_price) || entry_price <= 0 || !is.finite(i) || i < 1) {
    return(NA_real_)
  }
  source <- risk$stop_source %||% if (identical(risk$ps_type, "atr")) "atr" else "eldoc"
  if (identical(source, "atr") && "ATR" %in% colnames(mktdata)) {
    atr <- suppressWarnings(as.numeric(mktdata[i, "ATR"]))
    mult <- risk$atr_mult %||% strategy$params$atr_mult %||% 2
    if (is.finite(atr) && atr > 0 && is.finite(mult) && mult > 0) {
      return(atr * mult)
    }
  }
  if (identical(strategy$type, "donchian")) {
    stop_col <- if (identical(side, "long")) "DonchianLower" else "DonchianUpper"
    if (stop_col %in% colnames(mktdata)) {
      stop_price <- suppressWarnings(as.numeric(mktdata[i, stop_col]))
      if (is.finite(stop_price) && stop_price > 0) {
        return(abs(entry_price - stop_price))
      }
    }
  }
  NA_real_
}

.bt_native_trade_diagnostics <- function(trades, positions, mktdata, strategy, risk, metadata, funding_events = NULL) {
  empty <- list(
    episodes = .bt_native_empty_trade_episodes(),
    audit = .bt_native_empty_trade_audit(),
    excursions = .bt_native_empty_trade_excursions(),
    pyramid_events = .bt_native_empty_pyramid_events()
  )
  if (is.null(trades) || !NROW(trades) || is.null(mktdata) || !NROW(mktdata)) {
    return(empty)
  }

  entry_reasons <- c("long_entry", "short_entry")
  exit_reasons <- c("long_exit", "short_exit", "end_exit")
  pyramid_reasons <- c("long_pyramid", "short_pyramid")
  idx <- zoo::index(mktdata)
  price_cols <- .bt_native_price_columns_for_excursion(mktdata)
  if (!isTRUE(price_cols$ok)) {
    return(empty)
  }
  high <- suppressWarnings(as.numeric(mktdata[, price_cols$high]))
  low <- suppressWarnings(as.numeric(mktdata[, price_cols$low]))
  atr <- if ("ATR" %in% colnames(mktdata)) suppressWarnings(as.numeric(mktdata[, "ATR"])) else rep(NA_real_, NROW(mktdata))
  multiplier <- abs(suppressWarnings(as.numeric(metadata$multiplier)[1]))
  if (!is.finite(multiplier) || multiplier <= 0) multiplier <- 1
  pnl_multiplier <- multiplier * if (isTRUE(metadata$is_di) && grepl("^PU_", price_cols$close)) -1 else 1
  if (!is.data.frame(funding_events) || !"funding_cash" %in% names(funding_events)) {
    funding_events <- .bt_native_empty_funding_ledger()
  }
  if (NROW(funding_events) && !"timestamp" %in% names(funding_events)) {
    funding_events <- .bt_native_empty_funding_ledger()
  }
  funding_times <- if (NROW(funding_events)) as.POSIXct(funding_events$timestamp, tz = "UTC") else as.POSIXct(character(), tz = "UTC")
  trade_funding_rows <- function(entry_time, exit_time) {
    if (!NROW(funding_events)) {
      return(integer())
    }
    entry_time <- as.POSIXct(entry_time, tz = "UTC")
    exit_time <- as.POSIXct(exit_time, tz = "UTC")
    rows <- funding_times > entry_time & funding_times <= exit_time
    if (!any(rows, na.rm = TRUE)) {
      return(integer())
    }
    which(rows)
  }
  episodes <- list()
  audit <- list()
  excursions <- list()
  pyramid_events <- list()
  current <- NULL
  trade_id <- 0L

  add_audit_rows <- function(exit_row, bars_held, gross_pnl, net_pnl, funding_cash, funding_count) {
    if (is.null(current) || !NROW(current$rows)) {
      return(invisible(NULL))
    }
    exit_time <- as.POSIXct(exit_row$timestamp[1], tz = "UTC")
    order_rows <- current$rows
    for (row_i in seq_len(NROW(order_rows))) {
      row <- order_rows[row_i, , drop = FALSE]
      reason <- as.character(row$reason[1])
      event_type <- .bt_native_audit_event_type(reason)
      is_exit <- identical(event_type, "exit")
      row_signal_time <- if ("signal_time" %in% names(row)) row$signal_time[1] else row$timestamp[1]
      row_execution_timeframe <- if ("execution_timeframe" %in% names(row)) row$execution_timeframe[1] else "same"
      row_execution_source <- if ("execution_source_ticker" %in% names(row)) row$execution_source_ticker[1] else NA_character_
      audit[[length(audit) + 1L]] <<- data.frame(
        trade_id = current$trade_id,
        symbol = current$symbol,
        side = current$side,
        event_type = event_type,
        event_time = as.POSIXct(row$timestamp[1], tz = "UTC"),
        signal_time = as.POSIXct(row_signal_time, tz = "UTC"),
        execution_timeframe = as.character(row_execution_timeframe %||% "same"),
        execution_source_ticker = as.character(row_execution_source %||% NA_character_),
        reason = reason,
        entry_time = current$entry_time,
        exit_time = exit_time,
        bars_held = if (is_exit) bars_held else NA_integer_,
        signal_price = suppressWarnings(as.numeric(row$price[1])),
        fill_price = .bt_native_slippage_adjusted_price(row, multiplier),
        qty_delta = suppressWarnings(as.numeric(row$qty_delta[1])),
        position_qty = suppressWarnings(as.numeric(row$qty[1])),
        units = suppressWarnings(as.integer(row$units[1])),
        fees = suppressWarnings(as.numeric(row$fees[1])),
        slippage = suppressWarnings(as.numeric(row$slippage[1])),
        total_cost = suppressWarnings(as.numeric(row$total_cost[1])),
        funding_cash = 0,
        funding_received = 0,
        funding_paid = 0,
        funding_events = 0L,
        gross_pnl = if (is_exit) gross_pnl else NA_real_,
        net_pnl = if (is_exit) net_pnl else NA_real_,
        equity = suppressWarnings(as.numeric(row$equity[1])),
        stringsAsFactors = FALSE
      )
    }
    if (funding_count > 0L || !identical(funding_cash, 0)) {
      audit[[length(audit) + 1L]] <<- data.frame(
        trade_id = current$trade_id,
        symbol = current$symbol,
        side = current$side,
        event_type = "funding",
        event_time = exit_time,
        signal_time = exit_time,
        execution_timeframe = "same",
        execution_source_ticker = NA_character_,
        reason = "funding_settlement",
        entry_time = current$entry_time,
        exit_time = exit_time,
        bars_held = bars_held,
        signal_price = NA_real_,
        fill_price = NA_real_,
        qty_delta = 0,
        position_qty = 0,
        units = 0L,
        fees = 0,
        slippage = 0,
        total_cost = 0,
        funding_cash = funding_cash,
        funding_received = max(funding_cash, 0),
        funding_paid = max(-funding_cash, 0),
        funding_events = as.integer(funding_count),
        gross_pnl = gross_pnl,
        net_pnl = net_pnl,
        equity = suppressWarnings(as.numeric(exit_row$equity[1])),
        stringsAsFactors = FALSE
      )
    }
    invisible(NULL)
  }

  close_episode <- function(exit_row) {
    if (is.null(current)) {
      return(NULL)
    }
    entry_i <- .bt_native_which_index(idx, current$entry_time)
    exit_i <- .bt_native_which_index(idx, exit_row$timestamp)
    if (!is.finite(entry_i) || !is.finite(exit_i) || exit_i < entry_i) {
      return(NULL)
    }
    window <- entry_i:exit_i
    side_sign <- if (identical(current$side, "long")) 1 else -1
    entry_price <- current$entry_price
    exit_price <- suppressWarnings(as.numeric(exit_row$price)[1])
    window_high <- high[window]
    window_low <- low[window]

    if (identical(current$side, "long")) {
      mfe_i_rel <- which.max(window_high)
      mae_i_rel <- which.min(window_low)
      mfe_price <- max(window_high, na.rm = TRUE) - entry_price
      mae_price <- entry_price - min(window_low, na.rm = TRUE)
    } else {
      mfe_i_rel <- which.min(window_low)
      mae_i_rel <- which.max(window_high)
      mfe_price <- entry_price - min(window_low, na.rm = TRUE)
      mae_price <- max(window_high, na.rm = TRUE) - entry_price
    }
    mfe_price <- max(0, mfe_price)
    mae_price <- max(0, mae_price)
    entry_atr <- atr[entry_i]
    if (!is.finite(entry_atr) || entry_atr <= 0) entry_atr <- NA_real_
    risk_price <- .bt_native_trade_risk_price(entry_i, current$side, entry_price, mktdata, strategy, risk)
    if (!is.finite(risk_price) || risk_price <= 0) risk_price <- NA_real_
    gross_pnl <- -sum(current$rows$qty_delta * current$rows$price, na.rm = TRUE) * pnl_multiplier
    fees <- sum(current$rows$fees, na.rm = TRUE)
    slippage <- sum(current$rows$slippage, na.rm = TRUE)
    total_cost <- sum(current$rows$total_cost, na.rm = TRUE)
    funding_rows <- trade_funding_rows(current$entry_time, exit_row$timestamp)
    funding_cash <- if (length(funding_rows)) {
      sum(suppressWarnings(as.numeric(funding_events$funding_cash[funding_rows])), na.rm = TRUE)
    } else {
      0
    }
    net_pnl <- gross_pnl - total_cost + funding_cash
    bars_held <- length(window) - 1L
    initial_risk_cash <- risk_price * abs(current$entry_qty) * abs(pnl_multiplier)
    if (!is.finite(initial_risk_cash) || initial_risk_cash <= 0) initial_risk_cash <- NA_real_
    entry_atr_value_per_unit <- if (is.finite(entry_atr)) entry_atr * abs(pnl_multiplier) else NA_real_
    initial_stop_atr <- if (is.finite(entry_atr) && is.finite(risk_price) && entry_atr > 0) risk_price / entry_atr else NA_real_
    initial_risk_per_unit <- if (is.finite(initial_risk_cash) && abs(current$entry_qty) > 0) {
      initial_risk_cash / abs(current$entry_qty)
    } else if (is.finite(risk_price)) {
      risk_price * abs(pnl_multiplier)
    } else {
      NA_real_
    }
    final_R <- if (is.finite(initial_risk_cash)) net_pnl / initial_risk_cash else NA_real_
    max_qty <- max(abs(current$rows$qty), na.rm = TRUE)
    units_max <- max(current$rows$units, na.rm = TRUE)

    episodes[[length(episodes) + 1L]] <<- data.frame(
      trade_id = current$trade_id,
      symbol = current$symbol,
      side = current$side,
      entry_time = current$entry_time,
      exit_time = as.POSIXct(exit_row$timestamp, tz = "UTC"),
      entry_price = entry_price,
      exit_price = exit_price,
      entry_qty = abs(current$entry_qty),
      max_qty = max_qty,
      units_max = units_max,
      bars_held = bars_held,
      gross_pnl = gross_pnl,
      net_pnl = net_pnl,
      fees = fees,
      slippage = slippage,
      funding = funding_cash,
      total_cost = total_cost,
      entry_atr = entry_atr,
      entry_atr_value_per_unit = entry_atr_value_per_unit,
      initial_risk_price = risk_price,
      initial_risk_pct = if (is.finite(risk_price)) risk_price / entry_price else NA_real_,
      initial_risk_cash = initial_risk_cash,
      initial_stop_atr = initial_stop_atr,
      initial_risk_per_unit = initial_risk_per_unit,
      final_R = final_R,
      exit_reason = as.character(exit_row$reason),
      stringsAsFactors = FALSE
    )

    add_audit_rows(
      exit_row = exit_row,
      bars_held = bars_held,
      gross_pnl = gross_pnl,
      net_pnl = net_pnl,
      funding_cash = funding_cash,
      funding_count = length(funding_rows)
    )

    thresholds <- .bt_native_excursion_thresholds()
    hit_cols <- .bt_native_threshold_col("hit", thresholds, "_atr")
    post_cols <- .bt_native_threshold_col("post", thresholds, "_atr_R")
    hit_vals <- if (is.finite(entry_atr)) mfe_price / entry_atr >= thresholds else rep(NA, length(thresholds))
    names(hit_vals) <- hit_cols
    post_threshold_R <- function(threshold) {
      if (!isTRUE(hit_vals[[.bt_native_threshold_col("hit", threshold, "_atr")]]) ||
          !is.finite(entry_atr) || !is.finite(risk_price) || risk_price <= 0 ||
          !is.finite(exit_price)) {
        return(NA_real_)
      }
      threshold_price <- entry_price + side_sign * threshold * entry_atr
      side_sign * (exit_price - threshold_price) / risk_price
    }
    excursion_row <- data.frame(
      trade_id = current$trade_id,
      symbol = current$symbol,
      side = current$side,
      entry_time = current$entry_time,
      exit_time = as.POSIXct(exit_row$timestamp, tz = "UTC"),
      entry_price = entry_price,
      exit_price = exit_price,
      entry_atr = entry_atr,
      mfe_price = mfe_price,
      mae_price = mae_price,
      mfe_pct = mfe_price / entry_price,
      mae_pct = mae_price / entry_price,
      mfe_atr = if (is.finite(entry_atr)) mfe_price / entry_atr else NA_real_,
      mae_atr = if (is.finite(entry_atr)) mae_price / entry_atr else NA_real_,
      mfe_R = if (is.finite(risk_price)) mfe_price / risk_price else NA_real_,
      mae_R = if (is.finite(risk_price)) mae_price / risk_price else NA_real_,
      final_R = final_R,
      bars_to_mfe = if (length(mfe_i_rel)) mfe_i_rel[1] - 1L else NA_integer_,
      bars_to_mae = if (length(mae_i_rel)) mae_i_rel[1] - 1L else NA_integer_,
      stringsAsFactors = FALSE
    )
    for (j in seq_along(thresholds)) {
      excursion_row[[hit_cols[[j]]]] <- hit_vals[[j]]
      excursion_row[[post_cols[[j]]]] <- post_threshold_R(thresholds[[j]])
    }
    excursions[[length(excursions) + 1L]] <<- excursion_row
    invisible(NULL)
  }

  for (row_i in seq_len(NROW(trades))) {
    row <- trades[row_i, , drop = FALSE]
    reason <- as.character(row$reason[1])
    if (reason %in% entry_reasons) {
      trade_id <- trade_id + 1L
      current <- list(
        trade_id = trade_id,
        symbol = as.character(row$symbol[1]),
        side = if (grepl("^long", reason)) "long" else "short",
        entry_time = as.POSIXct(row$timestamp[1], tz = "UTC"),
        entry_price = suppressWarnings(as.numeric(row$price[1])),
        entry_qty = suppressWarnings(as.numeric(row$qty_delta[1])),
        rows = row
      )
    } else if (reason %in% pyramid_reasons && !is.null(current)) {
      current$rows <- rbind(current$rows, row)
      event_i <- .bt_native_which_index(idx, row$timestamp[1])
      event_atr <- if (is.finite(event_i)) atr[event_i] else NA_real_
      trigger_atr <- if (is.finite(event_atr) && event_atr > 0 && is.finite(current$entry_price)) {
        abs(suppressWarnings(as.numeric(row$price[1])) - current$entry_price) / event_atr
      } else {
        NA_real_
      }
      risk_at_add <- if (is.finite(event_i)) {
        .bt_native_trade_risk_price(event_i, current$side, suppressWarnings(as.numeric(row$price[1])), mktdata, strategy, risk)
      } else {
        NA_real_
      }
      initial_qty <- abs(suppressWarnings(as.numeric(current$entry_qty[1])))
      qty_added <- abs(suppressWarnings(as.numeric(row$qty_delta[1])))
      position_after_add <- abs(suppressWarnings(as.numeric(row$qty[1])))
      add_entry_qty_ratio <- if (is.finite(initial_qty) && initial_qty > 0) qty_added / initial_qty else NA_real_
      position_entry_qty_ratio <- if (is.finite(initial_qty) && initial_qty > 0) position_after_add / initial_qty else NA_real_
      stop_distance_atr_at_add <- if (is.finite(risk_at_add) && risk_at_add > 0 &&
                                      is.finite(event_atr) && event_atr > 0) {
        risk_at_add / event_atr
      } else {
        NA_real_
      }
      entry_row <- current$rows[1, , drop = FALSE]
      add_fees <- suppressWarnings(as.numeric(row$fees[1]))
      add_slippage <- suppressWarnings(as.numeric(row$slippage[1]))
      add_total_cost <- suppressWarnings(as.numeric(row$total_cost[1]))
      entry_fees <- suppressWarnings(as.numeric(entry_row$fees[1]))
      entry_slippage <- suppressWarnings(as.numeric(entry_row$slippage[1]))
      entry_total_cost <- suppressWarnings(as.numeric(entry_row$total_cost[1]))
      entry_price <- suppressWarnings(as.numeric(entry_row$price[1]))
      add_price <- suppressWarnings(as.numeric(row$price[1]))
      add_notional <- if (is.finite(add_price) && add_price > 0 && qty_added > 0) {
        add_price * multiplier * qty_added
      } else {
        NA_real_
      }
      entry_notional <- if (is.finite(entry_price) && entry_price > 0 && initial_qty > 0) {
        entry_price * multiplier * initial_qty
      } else {
        NA_real_
      }
      per_unit <- function(value, qty_abs) {
        if (is.finite(value) && is.finite(qty_abs) && qty_abs > 0) value / qty_abs else NA_real_
      }
      pct_notional <- function(value, notional) {
        if (is.finite(value) && is.finite(notional) && notional > 0) value / notional else NA_real_
      }
      pyramid_events[[length(pyramid_events) + 1L]] <- data.frame(
        trade_id = current$trade_id,
        event_time = as.POSIXct(row$timestamp[1], tz = "UTC"),
        side = current$side,
        unit_number = suppressWarnings(as.integer(row$units[1])),
        trigger_atr = trigger_atr,
        fill_price = suppressWarnings(as.numeric(row$price[1])),
        initial_qty = initial_qty,
        qty_added = qty_added,
        position_after_add = position_after_add,
        add_entry_qty_ratio = add_entry_qty_ratio,
        position_entry_qty_ratio = position_entry_qty_ratio,
        atr_at_add = event_atr,
        risk_price_at_add = risk_at_add,
        stop_distance_atr_at_add = stop_distance_atr_at_add,
        fees = add_fees,
        slippage = add_slippage,
        total_cost = add_total_cost,
        add_fee_per_contract = per_unit(add_fees, qty_added),
        add_slip_per_contract = per_unit(add_slippage, qty_added),
        add_cost_per_contract = per_unit(add_total_cost, qty_added),
        add_cost_pct_notional = pct_notional(add_total_cost, add_notional),
        entry_fee_per_contract = per_unit(entry_fees, initial_qty),
        entry_slip_per_contract = per_unit(entry_slippage, initial_qty),
        entry_cost_per_contract = per_unit(entry_total_cost, initial_qty),
        entry_cost_pct_notional = pct_notional(entry_total_cost, entry_notional),
        stringsAsFactors = FALSE
      )
    } else if (reason %in% exit_reasons && !is.null(current)) {
      current$rows <- rbind(current$rows, row)
      close_episode(row)
      current <- NULL
    }
  }

  list(
    episodes = if (length(episodes)) do.call(rbind, episodes) else empty$episodes,
    audit = if (length(audit)) do.call(rbind, audit) else empty$audit,
    excursions = if (length(excursions)) do.call(rbind, excursions) else empty$excursions,
    pyramid_events = if (length(pyramid_events)) do.call(rbind, pyramid_events) else empty$pyramid_events
  )
}

.bt_native_bool_label <- function(x) {
  if (isTRUE(x)) "TRUE" else "FALSE"
}

.bt_native_strategy_block <- function(symbol, strategy) {
  rows <- list(
    Symbol = symbol,
    Strategy = .bt_native_strategy_label(strategy),
    Type = toupper(strategy$type),
    Long = .bt_native_bool_label(strategy$long),
    Short = .bt_native_bool_label(strategy$short),
    "Invert Signals" = .bt_native_bool_label(strategy$invert_signals)
  )
  if (identical(strategy$type, "donchian")) {
    rows <- c(rows, list(
      "Upper Channel" = strategy$params$up,
      "Lower Channel" = strategy$params$down
    ))
    if (isTRUE(strategy$params$pyramid)) {
      rows <- c(rows, list(
        Pyramid = "TRUE",
        "ATR Lookback" = strategy$params$atr_n,
        "Pyramid Start ATR" = .bt_native_format_plain_num(strategy$params$pyramid_start),
        "Pyramid Step ATR" = .bt_native_format_plain_num(strategy$params$pyramid_step),
        "Pyramid Sizing" = strategy$params$pyramid_sizing %||% "risk",
        "Pyramid Qty %" = if (identical(strategy$params$pyramid_sizing %||% "risk", "entry_qty")) {
          .bt_native_format_pct(strategy$params$pyramid_qty_pct %||% 1)
        } else {
          NULL
        },
        "Max Units" = strategy$params$max_units
      ))
    }
  } else if (identical(strategy$type, "tsmom")) {
    rows <- c(rows, list(
      "Lookback Bars" = strategy$params$lookback,
      Threshold = .bt_native_format_pct(strategy$params$threshold),
      "ATR Lookback" = strategy$params$atr_n
    ))
  } else if (identical(strategy$type, "hold")) {
    rows <- c(rows, list(
      "Entry" = "first close",
      "Exit" = "last close"
    ))
  } else {
    rows <- c(rows, list(
      "Fast Window" = strategy$params$fast,
      "Slow Window" = strategy$params$slow
    ))
  }
  .bt_info_block("strategy", "Strategy", rows, order = 10, contexts = c("report", "stats"))
}

.bt_native_sizing_block <- function(risk, stats) {
  finite_or_null <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (is.finite(x)) .bt_native_format_plain_num(x) else NULL
  }
  rows <- list(
    "Position Sizing" = .bt_native_position_sizing_label(stats),
    "Sizing Type" = risk$ps_type %||% risk$mode,
    "Sizing Value" = .bt_native_format_plain_num(if (identical(risk$mode, "fixed")) risk$fixed_qty else risk$risk_pct),
    "Sizing Mode" = risk$mode,
    "Stop Source" = risk$stop_source,
    "ATR Lookback" = if (!is.null(risk$stop_source) && identical(risk$stop_source, "atr")) stats$AtrLookback[1] else NULL,
    "ATR Multiple" = if (!is.null(risk$stop_source) && identical(risk$stop_source, "atr")) .bt_native_format_plain_num(risk$atr_mult) else NULL,
    "Initial Equity" = .bt_native_format_money(risk$initial_equity),
    Reinvest = .bt_native_bool_label(risk$reinvest),
    "Integer Qty" = .bt_native_bool_label(risk$integer_qty),
    "Max Qty" = finite_or_null(risk$max_qty),
    "Max Leverage" = finite_or_null(risk$max_leverage),
    "Min Risk Pct" = finite_or_null(risk$min_risk_pct)
  )
  .bt_info_block("sizing", "Position Sizing", rows, order = 20, contexts = c("report", "stats"))
}

.bt_native_execution_block <- function(execution) {
  rows <- list(
    Execution = execution$execution,
    "Execution Timeframe" = execution$execution_timeframe %||% "same",
    "Close On End" = .bt_native_bool_label(execution$close_on_end),
    "Fee Mode" = execution$fee,
    "Fee Value" = .bt_native_format_plain_num(execution$fee_value),
    "Fee Type" = execution$fee_type,
    "Slippage Value" = .bt_native_format_plain_num(execution$slip_value),
    "Slippage Type" = execution$slip_type
  )
  .bt_info_block("execution", "Execution & Cost Settings", rows, order = 30, contexts = c("report", "stats"))
}

.bt_native_instrument_block <- function(metadata) {
  rows <- list(
    "Is Futures" = .bt_native_bool_label(metadata$is_futures),
    "Is DI" = .bt_native_bool_label(metadata$is_di),
    Root = metadata$root,
    Multiplier = .bt_native_format_plain_num(metadata$multiplier),
    "Tick Size" = .bt_native_format_plain_num(metadata$tick_size),
    "Tick Value" = .bt_native_format_plain_num(metadata$tick_value),
    Maturity = if (!is.null(metadata$maturity) && !is.na(metadata$maturity)) as.character(metadata$maturity) else NULL
  )
  .bt_info_block("instrument", "Instrument", rows, order = 40, contexts = c("report", "stats"))
}

.bt_native_trade_activity_block <- function(trades, stats, strategy) {
  count_reason <- function(reason) {
    if (is.null(trades) || !"reason" %in% names(trades)) {
      return(0L)
    }
    sum(trades$reason == reason, na.rm = TRUE)
  }
  contracts <- .bt_native_pick_stat_num(stats, "contracts_traded")
  rows <- list(
    Trades = .bt_native_format_plain_num(.bt_native_pick_stat_num(stats, "num_trades", 0), digits = 0),
    Orders = if (!is.null(trades)) .bt_native_format_plain_num(NROW(trades), digits = 0) else "0",
    Contracts = if (isTRUE(.bt_native_pick_stat_chr(stats, "IsFutures") %in% "TRUE") && is.finite(contracts)) .bt_native_format_plain_num(contracts, digits = 2) else NULL,
    "Long Entries" = count_reason("long_entry"),
    "Short Entries" = count_reason("short_entry"),
    "Long Exits" = count_reason("long_exit"),
    "Short Exits" = count_reason("short_exit"),
    Pyramids = if (identical(strategy$type, "donchian") && isTRUE(strategy$params$pyramid)) count_reason("long_pyramid") + count_reason("short_pyramid") else NULL
  )
  .bt_info_block("trade_activity", "Trade Activity", rows, order = 50, contexts = c("report", "stats"))
}

.bt_native_performance_block <- function(stats, rets = NULL) {
  risk_normalized <- isTRUE(.bt_native_pick_stat_chr(stats, "ReturnSource") == "risk_normalized") ||
    .bt_native_is_risk_normalized(rets)
  target <- .bt_native_pick_stat_num(stats, "RiskTarget")
  scale <- .bt_native_pick_stat_num(stats, "RiskScale")
  source_label <- if (isTRUE(risk_normalized)) {
    paste0("Risk-normalized ", .bt_native_format_plain_num(target), "% vol")
  } else {
    NULL
  }
  rows <- list(
    "Return Source" = source_label,
    "Risk Scale" = if (isTRUE(risk_normalized)) .bt_native_format_plain_num(scale) else NULL,
    "Original Vol" = if (isTRUE(risk_normalized)) .bt_native_format_pct(.bt_native_pick_stat_num(stats, "RiskOriginal") / 100) else NULL,
    "Target Vol" = if (isTRUE(risk_normalized)) .bt_native_format_pct(target / 100) else NULL,
    Profit = .bt_native_format_money(.bt_native_pick_stat_num(stats, "net_profit")),
    "Total Return" = .bt_native_format_pct(.bt_native_pick_stat_num(stats, "total_return")),
    "Annualized Return" = .bt_native_format_pct(.bt_native_pick_stat_num(stats, "annualized_return")),
    "Annualized Vol" = .bt_native_format_pct(.bt_native_pick_stat_num(stats, "annualized_vol")),
    Sharpe = .bt_native_format_plain_num(.bt_native_pick_stat_num(stats, "sharpe")),
    "Max Drawdown" = .bt_native_format_pct(.bt_native_pick_stat_num(stats, "max_drawdown"))
  )
  .bt_info_block("performance", "Performance", rows, order = 80, contexts = c("report", "stats"))
}

.bt_native_summary_num <- function(x, fun = stats::median) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  fun(x)
}

.bt_native_summary_pct <- function(x) {
  .bt_native_format_pct(.bt_native_summary_num(x))
}

.bt_native_quantile_num <- function(x, prob) {
  .bt_native_summary_num(x, function(vals) stats::quantile(vals, prob, names = FALSE, na.rm = TRUE))
}

.bt_native_threshold_label <- function(threshold) {
  .bt_native_format_plain_num(threshold, digits = 1)
}

.bt_native_hit_rate <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return("")
  }
  .bt_native_format_pct(mean(as.logical(x)))
}

.bt_native_hit_rate_count <- function(x) {
  if (!length(x)) {
    return("")
  }
  hits <- sum(as.logical(x), na.rm = TRUE)
  paste0(.bt_native_format_pct(hits / length(x)), " (", hits, "/", length(x), ")")
}

.bt_native_post_threshold_win_rate <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) {
    return("")
  }
  .bt_native_format_pct(mean(x > 0))
}

.bt_native_unit_label <- function(metadata) {
  if (isTRUE(metadata$is_futures)) "Contract" else "Unit"
}

.bt_native_atr_stats_block <- function(episodes, excursions, metadata) {
  if (is.null(episodes) || !NROW(episodes)) {
    return(.bt_info_block("atr_stats", "ATR Statistics", list(), order = 84, contexts = c("report", "stats")))
  }
  diag <- episodes
  if (!"entry_atr" %in% names(diag) &&
      !is.null(excursions) &&
      all(c("trade_id", "entry_atr") %in% names(excursions))) {
    diag <- merge(diag, excursions[, c("trade_id", "entry_atr"), drop = FALSE], by = "trade_id", all.x = TRUE)
  }

  entry_atr <- if ("entry_atr" %in% names(diag)) suppressWarnings(as.numeric(diag$entry_atr)) else rep(NA_real_, NROW(diag))
  initial_risk_price <- if ("initial_risk_price" %in% names(diag)) suppressWarnings(as.numeric(diag$initial_risk_price)) else rep(NA_real_, NROW(diag))
  initial_stop_atr <- if ("initial_stop_atr" %in% names(diag)) {
    suppressWarnings(as.numeric(diag$initial_stop_atr))
  } else {
    initial_risk_price / entry_atr
  }

  multiplier <- abs(suppressWarnings(as.numeric(metadata$multiplier)[1]))
  if (!is.finite(multiplier) || multiplier <= 0) {
    multiplier <- 1
  }
  entry_atr_value <- if ("entry_atr_value_per_unit" %in% names(diag)) {
    suppressWarnings(as.numeric(diag$entry_atr_value_per_unit))
  } else {
    entry_atr * multiplier
  }
  initial_risk_value <- if ("initial_risk_per_unit" %in% names(diag)) {
    suppressWarnings(as.numeric(diag$initial_risk_per_unit))
  } else {
    initial_risk_price * multiplier
  }

  unit <- .bt_native_unit_label(metadata)
  rows <- list(
    Episodes = NROW(episodes),
    "Mean Entry ATR Price" = .bt_native_format_plain_num(.bt_native_summary_num(entry_atr, mean)),
    "Median Entry ATR Price" = .bt_native_format_plain_num(.bt_native_summary_num(entry_atr)),
    "Mean Initial Stop ATR" = .bt_native_format_plain_num(.bt_native_summary_num(initial_stop_atr, mean)),
    "Median Initial Stop ATR" = .bt_native_format_plain_num(.bt_native_summary_num(initial_stop_atr)),
    "P75 Initial Stop ATR" = .bt_native_format_plain_num(.bt_native_quantile_num(initial_stop_atr, 0.75)),
    "P90 Initial Stop ATR" = .bt_native_format_plain_num(.bt_native_quantile_num(initial_stop_atr, 0.90))
  )
  rows[[paste0("Mean 1 ATR Value / ", unit)]] <- .bt_native_format_money(.bt_native_summary_num(entry_atr_value, mean))
  rows[[paste0("Median 1 ATR Value / ", unit)]] <- .bt_native_format_money(.bt_native_summary_num(entry_atr_value))
  rows[[paste0("Mean 10 ATR Value / ", unit)]] <- .bt_native_format_money(.bt_native_summary_num(entry_atr_value * 10, mean))
  rows[[paste0("Mean 20 ATR Value / ", unit)]] <- .bt_native_format_money(.bt_native_summary_num(entry_atr_value * 20, mean))
  rows[[paste0("Mean Initial Stop Value / ", unit)]] <- .bt_native_format_money(.bt_native_summary_num(initial_risk_value, mean))
  rows[[paste0("Median Initial Stop Value / ", unit)]] <- .bt_native_format_money(.bt_native_summary_num(initial_risk_value))

  .bt_info_block("atr_stats", "ATR Statistics", rows, order = 84, contexts = c("report", "stats"))
}

.bt_native_excursion_block <- function(excursions) {
  if (is.null(excursions) || !NROW(excursions)) {
    return(.bt_info_block("excursions", "Trade Excursions", list(), order = 85, contexts = c("report", "stats")))
  }
  wins <- excursions[is.finite(excursions$final_R) & excursions$final_R > 0, , drop = FALSE]
  losses <- excursions[is.finite(excursions$final_R) & excursions$final_R <= 0, , drop = FALSE]
  rows <- list(
    Episodes = NROW(excursions),
    "Avg MFE %" = .bt_native_format_pct(mean(excursions$mfe_pct, na.rm = TRUE)),
    "Median MFE %" = .bt_native_summary_pct(excursions$mfe_pct),
    "Median MAE %" = .bt_native_summary_pct(excursions$mae_pct),
    "Median MFE ATR" = .bt_native_format_plain_num(.bt_native_summary_num(excursions$mfe_atr)),
    "Median MAE ATR" = .bt_native_format_plain_num(.bt_native_summary_num(excursions$mae_atr)),
    "P75 MFE ATR" = .bt_native_format_plain_num(.bt_native_quantile_num(excursions$mfe_atr, 0.75)),
    "P85 MFE ATR" = .bt_native_format_plain_num(.bt_native_quantile_num(excursions$mfe_atr, 0.85)),
    "P90 MFE ATR" = .bt_native_format_plain_num(.bt_native_quantile_num(excursions$mfe_atr, 0.90)),
    "P95 MFE ATR" = .bt_native_format_plain_num(.bt_native_quantile_num(excursions$mfe_atr, 0.95)),
    "P75 MAE ATR" = .bt_native_format_plain_num(.bt_native_quantile_num(excursions$mae_atr, 0.75)),
    "Win Median MFE ATR" = if (NROW(wins)) .bt_native_format_plain_num(.bt_native_summary_num(wins$mfe_atr)) else NULL,
    "Loss Median MFE ATR" = if (NROW(losses)) .bt_native_format_plain_num(.bt_native_summary_num(losses$mfe_atr)) else NULL,
    "Median Final R" = .bt_native_format_plain_num(.bt_native_summary_num(excursions$final_R))
  )
  .bt_info_block("excursions", "Trade Excursions", rows, order = 85, contexts = c("report", "stats"))
}

.bt_native_excursion_threshold_table <- function(excursions) {
  if (is.null(excursions) || !NROW(excursions)) {
    return(data.frame())
  }
  thresholds <- .bt_native_excursion_thresholds()
  threshold_rows <- lapply(thresholds, function(threshold) {
    hit_col <- .bt_native_threshold_col("hit", threshold, "_atr")
    post_col <- .bt_native_threshold_col("post", threshold, "_atr_R")
    hits <- if (hit_col %in% names(excursions)) {
      excursions[[hit_col]]
    } else {
      suppressWarnings(as.numeric(excursions$mfe_atr)) >= threshold
    }
    hits <- as.logical(hits)
    total_n <- length(hits)
    hit_n <- sum(hits, na.rm = TRUE)
    hit_subset <- if (total_n && hit_n) excursions[!is.na(hits) & hits, , drop = FALSE] else excursions[FALSE, , drop = FALSE]
    post_vals <- if (post_col %in% names(excursions)) suppressWarnings(as.numeric(excursions[[post_col]])) else numeric()
    data.frame(
      ATRT = .bt_native_threshold_label(threshold),
      Hits = if (total_n) paste0(hit_n, "/", total_n) else "",
      `Hit %` = if (total_n) .bt_native_format_pct(hit_n / total_n) else "",
      `Med FR` = if (NROW(hit_subset)) .bt_native_format_plain_num(.bt_native_summary_num(hit_subset$final_R)) else "",
      `Mean FR` = if (NROW(hit_subset)) .bt_native_format_plain_num(.bt_native_summary_num(hit_subset$final_R, mean)) else "",
      `Med PR` = .bt_native_format_plain_num(.bt_native_summary_num(post_vals)),
      `Mean PR` = .bt_native_format_plain_num(.bt_native_summary_num(post_vals, mean)),
      `PR Win %` = .bt_native_post_threshold_win_rate(post_vals),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, threshold_rows)
}

.bt_native_excursion_threshold_block <- function(excursions) {
  .bt_info_table_block(
    "excursion_thresholds",
    "Excursion Thresholds",
    .bt_native_excursion_threshold_table(excursions),
    order = 85.5,
    contexts = c("report", "stats")
  )
}

.bt_native_pyramid_block <- function(pyramid_events) {
  if (is.null(pyramid_events) || !NROW(pyramid_events)) {
    return(.bt_info_block("pyramiding", "Pyramiding", list(), order = 86, contexts = character()))
  }
  add_entry_ratio <- pyramid_events$add_entry_qty_ratio
  if (is.null(add_entry_ratio) && all(c("qty_added", "initial_qty") %in% names(pyramid_events))) {
    add_entry_ratio <- pyramid_events$qty_added / pyramid_events$initial_qty
  }
  position_entry_ratio <- pyramid_events$position_entry_qty_ratio
  if (is.null(position_entry_ratio) && all(c("position_after_add", "initial_qty") %in% names(pyramid_events))) {
    position_entry_ratio <- pyramid_events$position_after_add / pyramid_events$initial_qty
  }
  stop_distance_atr <- pyramid_events$stop_distance_atr_at_add
  if (is.null(stop_distance_atr) && all(c("risk_price_at_add", "atr_at_add") %in% names(pyramid_events))) {
    stop_distance_atr <- pyramid_events$risk_price_at_add / pyramid_events$atr_at_add
  }
  add_fee_per_contract <- pyramid_events$add_fee_per_contract
  if (is.null(add_fee_per_contract) && all(c("fees", "qty_added") %in% names(pyramid_events))) {
    add_fee_per_contract <- pyramid_events$fees / pyramid_events$qty_added
  }
  add_slip_per_contract <- pyramid_events$add_slip_per_contract
  if (is.null(add_slip_per_contract) && all(c("slippage", "qty_added") %in% names(pyramid_events))) {
    add_slip_per_contract <- pyramid_events$slippage / pyramid_events$qty_added
  }
  add_cost_per_contract <- pyramid_events$add_cost_per_contract
  if (is.null(add_cost_per_contract) && all(c("total_cost", "qty_added") %in% names(pyramid_events))) {
    add_cost_per_contract <- pyramid_events$total_cost / pyramid_events$qty_added
  }
  entry_fee_per_contract <- pyramid_events$entry_fee_per_contract
  entry_slip_per_contract <- pyramid_events$entry_slip_per_contract
  entry_cost_per_contract <- pyramid_events$entry_cost_per_contract
  add_cost_pct_notional <- pyramid_events$add_cost_pct_notional
  entry_cost_pct_notional <- pyramid_events$entry_cost_pct_notional
  rows <- list(
    "Pyramid Adds" = NROW(pyramid_events),
    "Trades With Adds" = length(unique(pyramid_events$trade_id)),
    "Median Add Trigger ATR" = .bt_native_format_plain_num(.bt_native_summary_num(pyramid_events$trigger_atr)),
    "Median Initial Qty" = .bt_native_format_plain_num(.bt_native_summary_num(pyramid_events$initial_qty)),
    "Median Qty Added" = .bt_native_format_plain_num(.bt_native_summary_num(pyramid_events$qty_added)),
    "Median Add / Entry Qty" = .bt_native_format_plain_num(.bt_native_summary_num(add_entry_ratio)),
    "P25 Add / Entry Qty" = .bt_native_format_plain_num(.bt_native_quantile_num(add_entry_ratio, 0.25)),
    "P75 Add / Entry Qty" = .bt_native_format_plain_num(.bt_native_quantile_num(add_entry_ratio, 0.75)),
    "Median Pos After Add / Entry Qty" = .bt_native_format_plain_num(.bt_native_summary_num(position_entry_ratio)),
    "Median Stop Distance ATR At Add" = .bt_native_format_plain_num(.bt_native_summary_num(stop_distance_atr)),
    "Median Entry Fee / Contract" = .bt_native_format_money(.bt_native_summary_num(entry_fee_per_contract)),
    "Median Entry Slip / Contract" = .bt_native_format_money(.bt_native_summary_num(entry_slip_per_contract)),
    "Median Entry Cost / Contract" = .bt_native_format_money(.bt_native_summary_num(entry_cost_per_contract)),
    "Median Add Fee / Contract" = .bt_native_format_money(.bt_native_summary_num(add_fee_per_contract)),
    "Median Add Slip / Contract" = .bt_native_format_money(.bt_native_summary_num(add_slip_per_contract)),
    "Median Add Cost / Contract" = .bt_native_format_money(.bt_native_summary_num(add_cost_per_contract)),
    "Median Entry Cost % Notional" = .bt_native_format_pct(.bt_native_summary_num(entry_cost_pct_notional)),
    "Median Add Cost % Notional" = .bt_native_format_pct(.bt_native_summary_num(add_cost_pct_notional)),
    "Total Added Qty" = .bt_native_format_plain_num(sum(abs(pyramid_events$qty_added), na.rm = TRUE), digits = 2),
    "Add Fees" = .bt_native_format_money(sum(pyramid_events$fees, na.rm = TRUE)),
    "Add Slippage" = .bt_native_format_money(sum(pyramid_events$slippage, na.rm = TRUE))
  )
  .bt_info_block("pyramiding", "Pyramiding", rows, order = 86, contexts = c("report", "stats"))
}

.bt_native_info_blocks <- function(symbol, strategy, risk, execution, metadata, raw_stats,
                                   performance_stats, trades, raw_rets, rets, equity,
                                   performance_equity, episodes, excursions, pyramid_events,
                                   funding_events = NULL,
                                   geometric = TRUE, research_blocks = TRUE) {
  monthly_title <- paste0(
    "Monthly Returns",
    .bt_native_risk_title_suffix(rets),
    " (",
    if (isTRUE(geometric)) "Geometric" else "Simple",
    ")"
  )
  quarterly_title <- paste0(
    "Quarterly Returns",
    .bt_native_risk_title_suffix(rets),
    " (",
    if (isTRUE(geometric)) "Geometric" else "Simple",
    ")"
  )
  quarterly_profit_title <- paste0("Quarterly Net Profit", .bt_native_risk_title_suffix(rets))
  blocks <- list(
    strategy = .bt_native_strategy_block(symbol, strategy),
    sizing = .bt_native_sizing_block(risk, raw_stats),
    execution = .bt_native_execution_block(execution),
    instrument = .bt_native_instrument_block(metadata),
    trade_activity = .bt_native_trade_activity_block(trades, raw_stats, strategy),
    risk_normalization = .bt_native_risk_normalization_block(rets),
    monthly_returns = .bt_info_table_block(
      "monthly_returns",
      monthly_title,
      .bt_native_table_monthly_returns(rets$Discrete, geometric = geometric),
      order = 60,
      contexts = "report"
    ),
    quarterly_returns = .bt_info_table_block(
      "quarterly_returns",
      quarterly_title,
      .table_quarterly_returns(rets$Discrete, return_data = TRUE, geometric = geometric, print_table = FALSE),
      order = 61,
      contexts = "quarterly"
    ),
    quarterly_profit = .bt_info_table_block(
      "quarterly_profit",
      quarterly_profit_title,
      .table_quarterly_profit(.bt_native_profit_object(performance_equity), return_data = TRUE, print_table = FALSE),
      order = 62,
      contexts = "quarterly"
    ),
    costs = .bt_info_block_from_matrix(
      "costs",
      "Costs & Slippage Summary",
      .bt_native_cost_summary_table(raw_stats, trades),
      order = 70,
      contexts = c("report", "stats")
    ),
    performance = .bt_native_performance_block(performance_stats, rets = rets),
    atr_stats = .bt_native_atr_stats_block(episodes, excursions, metadata),
    excursions = .bt_native_excursion_block(excursions),
    excursion_thresholds = .bt_native_excursion_threshold_block(excursions),
    pyramiding = .bt_native_pyramid_block(pyramid_events),
    returns = .bt_info_table_block(
      "returns",
      paste0("Returns Summary", .bt_native_risk_title_suffix(rets), " ", if (isTRUE(geometric)) "(Geometric)" else "(Simple)"),
      .bt_native_returns_summary_table(rets, geometric = geometric),
      order = 90,
      contexts = "report"
    )
  )
  if (!isTRUE(research_blocks)) {
    for (id in c("atr_stats", "excursions", "excursion_thresholds", "pyramiding")) {
      if (!is.null(blocks[[id]])) {
        attr(blocks[[id]], "contexts") <- "research"
      }
    }
  }
  attr(blocks, "label") <- symbol
  class(blocks) <- c("bt_info_blocks", "list")
  blocks
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
  total_funding <- pick_stat("funding")
  total_cost <- pick_stat("total_cost", total_fees + total_slippage)
  net_profit <- pick_stat("net_profit")
  gross_before_costs <- net_profit + total_cost - total_funding
  trade_count <- pick_stat("num_trades", NROW(trades))
  is_futures <- FALSE
  if (!is.null(stats) && "IsFutures" %in% names(stats)) {
    is_futures <- isTRUE(stats$IsFutures[1])
  }
  contracts_traded <- pick_stat("contracts_traded", NA_real_)
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

  rows <- "Trades"
  vals <- format(trade_count, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  if (isTRUE(is_futures) && is.finite(contracts_traded)) {
    rows <- c(rows, "Contracts")
    vals <- c(vals, .bt_native_format_plain_num(contracts_traded, digits = 2))
  }
  rows <- c(
    rows,
    "Fees",
    "Slippage",
    "Funding",
    "Gross P/L",
    "Net P/L",
    "Impact Basis",
    "Fees Impact",
    "Slippage Impact",
    "Funding Impact",
    "Total Cost Impact"
  )
  vals <- c(
    vals,
    .bt_native_format_money(total_fees),
    .bt_native_format_money(total_slippage),
    .bt_native_format_money(total_funding),
    .bt_native_format_money(gross_before_costs),
    .bt_native_format_money(net_profit),
    impact_basis,
    .bt_native_format_pct(impact(total_fees)),
    .bt_native_format_pct(impact(total_slippage)),
    .bt_native_format_pct(impact(total_funding)),
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
    tsmom = "TSMOM",
    hold = "HOLD",
    toupper(strategy$type)
  )
}

.bt_native_profit_object <- function(equity) {
  vals <- as.numeric(equity)
  pnl <- c(0, diff(vals))
  summary <- xts::xts(cbind(Net.Trading.PL = pnl), order.by = zoo::index(equity))
  list(summary = summary)
}

.bt_native_returns_summary_table <- function(rets, geometric = TRUE) {
  ppy <- .bt_native_periods_per_year(rets)
  disc <- .bt_native_return_summary_values(as.numeric(rets$Discrete), ppy, geometric = geometric)
  log_ret <- .bt_native_return_summary_values(as.numeric(rets$Log), ppy, geometric = geometric)

  matrix(
    sprintf("%.4f%%", c(disc$annualized, log_ret$annualized, disc$cumulative, log_ret$cumulative) * 100),
    nrow = 2,
    dimnames = list(c("Discrete", "Log"), c("Annual", "Total"))
  )
}

.bt_native_print_returns_summary <- function(rets, geometric = TRUE) {
  res_table <- .bt_native_returns_summary_table(rets, geometric = geometric)

  cat(paste0("\n--- Returns Summary ", if (geometric) "(Geometric)" else "(Simple)", " ---\n"))
  print(res_table, quote = FALSE, right = TRUE)
  cat("\n")

  invisible(res_table)
}

.bt_native_return_summary_values <- function(x, periods_per_year, geometric = TRUE) {
  .bt_return_summary_values(x, periods_per_year, geometric = geometric)
}

.bt_native_stats <- function(equity, rets, trades, risk, execution, metadata, strategy, funding_events = NULL) {
  eq <- as.numeric(equity)
  ret <- as.numeric(rets$Log)
  ret <- ret[is.finite(ret)]
  ppy <- .bt_native_periods_per_year(rets)
  total_return <- tail(eq, 1) / eq[1] - 1
  ann_return <- if (length(eq) > 1 && is.finite(ppy)) (tail(eq, 1) / eq[1])^(ppy / max(1, length(eq) - 1)) - 1 else NA_real_
  ann_vol <- if (length(ret) > 1) .bt_annualized_daily_vol(rets) else NA_real_
  sharpe <- if (is.finite(ann_vol) && ann_vol > 0) ann_return / ann_vol else NA_real_
  dd <- 1 - eq / cummax(eq)
  max_dd <- max(dd, na.rm = TRUE)
  trade_sum <- function(name) {
    if (!name %in% names(trades)) return(0)
    sum(suppressWarnings(as.numeric(trades[[name]])), na.rm = TRUE)
  }
  contracts_traded <- trade_sum("qty_delta")
  contracts_traded <- abs(contracts_traded)
  if ("qty_delta" %in% names(trades)) {
    contracts_traded <- sum(abs(suppressWarnings(as.numeric(trades$qty_delta))), na.rm = TRUE)
  }
  fees <- trade_sum("fees")
  slippage <- trade_sum("slippage")
  total_cost <- if ("total_cost" %in% names(trades)) trade_sum("total_cost") else fees + slippage
  funding <- 0
  if (!is.null(funding_events) && is.data.frame(funding_events) && "funding_cash" %in% names(funding_events)) {
    funding <- sum(suppressWarnings(as.numeric(funding_events$funding_cash)), na.rm = TRUE)
  }
  resolved_slip_value <- .bt_native_first_num(
    execution$slip_value,
    execution$slippage_bps,
    execution$slippage_ticks,
    execution$slippage_points,
    execution$slippage_per_contract
  )
  resolved_slip_type <- execution$slip_type %||% if (is.finite(.bt_native_first_num(execution$slippage_bps))) {
    "bps"
  } else if (is.finite(.bt_native_first_num(execution$slippage_ticks))) {
    "ticks"
  } else if (is.finite(.bt_native_first_num(execution$slippage_points))) {
    "points"
  } else if (is.finite(.bt_native_first_num(execution$slippage_per_contract))) {
    "cash"
  } else {
    NA_character_
  }
  risk_normalized <- .bt_native_is_risk_normalized(rets)
  risk_target <- if (isTRUE(risk_normalized)) .bt_native_risk_target(rets) else NA_real_
  risk_scale <- if (isTRUE(risk_normalized)) suppressWarnings(as.numeric(attr(rets, "risk_scale", exact = TRUE))[1]) else NA_real_
  risk_original <- if (isTRUE(risk_normalized)) suppressWarnings(as.numeric(attr(rets, "risk_original", exact = TRUE))[1]) else NA_real_
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
    funding = funding,
    total_cost = total_cost,
    contracts_traded = contracts_traded,
    IsFutures = isTRUE(metadata$is_futures),
    InitialEquity = risk$initial_equity,
    PosSiz = risk$ps_type %||% risk$mode,
    PsValue = if (identical(risk$mode, "fixed")) risk$fixed_qty else risk$risk_pct,
    RiskPct = risk$risk_pct,
    Reinvest = risk$reinvest,
    Execution = execution$execution,
    ExecutionTimeframe = execution$execution_timeframe %||% "same",
    AtrLookback = strategy$params$atr_n %||% NA_integer_,
    FeeMode = execution$fee,
    FeeType = execution$fee_type %||% NA_character_,
    FeeValue = execution$fee_value %||% NA_real_,
    SlipType = resolved_slip_type,
    SlipValue = if (is.finite(resolved_slip_value)) resolved_slip_value else NA_real_,
    Multiplier = metadata$multiplier,
    TickSize = metadata$tick_size,
    Indicator = .bt_native_strategy_label(strategy),
    RiskNormalized = risk_normalized,
    RiskTarget = risk_target,
    RiskScale = risk_scale,
    RiskOriginal = risk_original,
    ReturnSource = if (isTRUE(risk_normalized)) "risk_normalized" else "raw",
    stringsAsFactors = FALSE
  )
}

.bt_native_periods_per_year <- function(x) {
  .bt_periods_per_year(x)
}

.bt_native_strategy_label <- function(strategy) {
  if (identical(strategy$type, "donchian")) {
    paste0("ElDoc ", strategy$params$up, "/", strategy$params$down)
  } else if (identical(strategy$type, "tsmom")) {
    paste0("TSMOM ", strategy$params$lookback)
  } else if (identical(strategy$type, "hold")) {
    "Hold"
  } else if (identical(strategy$type, "portfolio")) {
    "Portfolio mixed"
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
