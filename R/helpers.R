#' Print a timestamped debug message
#'
#' Prints the provided values prefixed by the current time (HH:MM:SS),
#' useful for lightweight debugging during backtests.
#'
#' @param ... Values to print. Passed to `cat()`.
#' @return Invisibly returns `NULL`. Called for its side effects (console output).
#' @keywords internal
.dbg <- function(...) cat(format(Sys.time(), "%T"), "-", ..., "\n")

# Internal environment that mimics `.GlobalEnv` for market data storage
.bt_data_env <- new.env(parent = emptyenv())

# Accessor for the internal market data environment
.get_bt_data_env <- function() .bt_data_env

.bt_tplot <- function(...) {
  if (!requireNamespace("tradeplotr", quietly = TRUE)) {
    stop("Package 'tradeplotr' is required for plotting.", call. = FALSE)
  }
  tradeplotr::tplot(...)
}

# Null-coalescing helper
#
# Returns `y` when `x` is `NULL`, zero-length, or entirely `NA`; otherwise
# returns `x` unchanged. Useful for filling metadata defaults.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.atomic(x) && all(is.na(x))) {
    return(y)
  }
  x
}

# Turn an expression (from substitute) into a readable label
.bt_expr_to_label <- function(expr) {
  if (is.null(expr)) {
    return(NULL)
  }
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  if (is.character(expr) && length(expr)) {
    return(expr[[1]])
  }
  NULL
}

# Coerce inputs to a non-empty scalar character when possible
.bt_safe_scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (is.list(x) || is.environment(x)) {
    return(NULL)
  }
  val <- suppressWarnings(as.character(x)[1])
  if (length(val) == 0 || is.na(val) || !nzchar(val)) {
    return(NULL)
  }
  val
}

# Locate an xts object nested inside list-like inputs
.bt_locate_xts_in_object <- function(obj, max_depth = 2) {
  if (xts::is.xts(obj)) {
    return(obj)
  }
  if (max_depth <= 0 || !is.list(obj) || length(obj) == 0) {
    return(NULL)
  }

  preferred <- c("data", "xts", "mktdata", "prices")
  for (nm in preferred) {
    if (!is.null(obj[[nm]]) && xts::is.xts(obj[[nm]])) {
      return(obj[[nm]])
    }
  }

  for (item in obj) {
    candidate <- .bt_locate_xts_in_object(item, max_depth - 1)
    if (!is.null(candidate)) {
      return(candidate)
    }
  }
  NULL
}

# Resolve ticker input to a symbol label plus optional inline data
.bt_resolve_ticker_input <- function(value, expr = NULL) {
  expr_label <- .bt_safe_scalar_chr(.bt_expr_to_label(expr))
  direct_label <- NULL
  if (is.character(value) && length(value) > 0) {
    direct_label <- .bt_safe_scalar_chr(value[1])
  } else if (is.atomic(value) && length(value) == 1 && !is.list(value)) {
    direct_label <- .bt_safe_scalar_chr(value)
  }

  pick_label <- function(...) {
    for (candidate in list(...)) {
      val <- .bt_safe_scalar_chr(candidate)
      if (!is.null(val)) {
        return(val)
      }
    }
    NULL
  }

  xts_obj <- .bt_locate_xts_in_object(value, max_depth = 2)
  if (!is.null(xts_obj)) {
    attr_candidates <- list(
      attr(xts_obj, "bt_requested_symbol", exact = TRUE),
      attr(xts_obj, "bt_original_symbol", exact = TRUE),
      attr(xts_obj, "symbol", exact = TRUE),
      attr(xts_obj, "ticker", exact = TRUE),
      attr(xts_obj, "bt_fetched_symbol", exact = TRUE),
      direct_label,
      expr_label
    )
    symbol <- NULL
    for (candidate in attr_candidates) {
      val <- .bt_safe_scalar_chr(candidate)
      if (!is.null(val)) {
        symbol <- val
        break
      }
    }
    if (is.null(symbol)) symbol <- "local_xts"
    return(list(symbol = symbol, data = xts_obj))
  }

  symbol <- pick_label(direct_label, expr_label, "local_input")
  if (is.null(symbol)) symbol <- "local_input"

  list(symbol = symbol, data = NULL)
}

#' Extract a single finite numeric from mixed inputs
#'
#' Coerces the first finite numeric value from `x`, which may be a list or
#' atomic vector. Returns `default` when no finite value is found.
#'
#' @param x Value or list of values to inspect.
#' @param default Numeric scalar returned when no finite number is located.
#' @keywords internal
.sanitize_scalar_numeric <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) default else x[1]
}

#' Gather futures metadata embedded in an xts object
#'
#' Inspects the direct attributes of an xts time series to collect
#' futures-specific metadata such as tick size, multiplier, maturity,
#' currency, fees, and slippage. Missing pieces fall back to `NULL`.
#'
#' @param object xts object containing market data plus metadata attributes.
#' @return A list with `tick_size`, `tick_value`, `multiplier`, `maturity`,
#'   `currency`, `fee_type`, `slip_value`, `slippage_bps`,
#'   `slippage_ticks`, `slippage_points`, and `slippage_unit` entries.
#' @keywords internal
.collect_instrument_metadata <- function(object) {
  meta <- list(
    tick_size = NULL,
    tick_value = NULL,
    multiplier = NULL,
    maturity = NULL,
    currency = NULL,
    fee_type = NULL,
    slip_value = NULL,
    slippage_bps = NULL,
    slippage_ticks = NULL,
    slippage_unit = NULL,
    slippage_points = NULL
  )
  if (is.null(object)) {
    return(meta)
  }

  fetch_attr <- function(name) attr(object, name, exact = TRUE)
  fetch_attr_first <- function(names) {
    for (name in names) {
      val <- fetch_attr(name)
      if (!is.null(val)) {
        return(val)
      }
    }
    NULL
  }
  fetch_list_first <- function(x, names) {
    if (is.environment(x)) x <- as.list(x)
    if (!is.list(x)) {
      return(NULL)
    }
    for (name in names) {
      val <- x[[name]]
      if (!is.null(val)) {
        return(val)
      }
    }
    NULL
  }

  tick_size_names <- c("fut_tick_size", "tick_size", "ticksize", "TickSize")
  tick_value_names <- c("fut_tick_value", "tick_value", "tickvalue", "TickValue")
  multiplier_names <- c("fut_multiplier", "multiplier", "Multiplier")
  maturity_names <- c("maturity", "expiry", "expiration")
  currency_names <- c("currency", "Currency", "currency_id")
  slip_value_names <- "slip_value"
  fee_type_names <- "fee_type"
  slippage_bps_names <- "slippage_bps"
  slippage_ticks_names <- "slippage_ticks"
  slippage_points_names <- c("slippage_points", "slippage_points_per_contract")
  slippage_unit_names <- "slip_type"

  meta$tick_size <- fetch_attr_first(tick_size_names)
  meta$tick_value <- fetch_attr_first(tick_value_names)
  meta$multiplier <- fetch_attr_first(multiplier_names)
  meta$maturity <- fetch_attr_first(maturity_names)
  meta$currency <- fetch_attr_first(currency_names)

  meta$fee_type <- fetch_attr_first(fee_type_names)
  meta$slip_value <- fetch_attr_first(slip_value_names)
  meta$slippage_bps <- fetch_attr_first(slippage_bps_names)
  meta$slippage_ticks <- fetch_attr_first(slippage_ticks_names)
  meta$slippage_points <- fetch_attr_first(slippage_points_names)
  meta$slippage_unit <- fetch_attr_first(slippage_unit_names)

  meta_attr <- fetch_attr("metadata")
  if (is.environment(meta_attr)) meta_attr <- as.list(meta_attr)
  if (is.list(meta_attr)) {
    meta$tick_size <- meta$tick_size %||% fetch_list_first(meta_attr, tick_size_names)
    meta$tick_value <- meta$tick_value %||% fetch_list_first(meta_attr, tick_value_names)
    meta$multiplier <- meta$multiplier %||% fetch_list_first(meta_attr, multiplier_names)
    meta$maturity <- meta$maturity %||% fetch_list_first(meta_attr, maturity_names)
    meta$currency <- meta$currency %||% fetch_list_first(meta_attr, currency_names)
  }

  meta
}
#' Ensure OHLC columns exist, falling back to Close
#'
#' Ensures an object has Open, High, Low, Close columns. If only a Close
#' column exists, replicates it into missing OHLC columns and reorders the
#' first columns to `Open, High, Low, Close`.
#'
#' @param object An `xts` or data.frame-like object with price columns.
#' @return The same object class with guaranteed OHLC columns present.
#' @keywords internal
.use_close_only <- function(object) {
  data_columns <- tolower(colnames(object))

  has_open <- "open" %in% data_columns
  has_high <- "high" %in% data_columns
  has_low <- "low" %in% data_columns
  has_close <- "close" %in% data_columns

  if (has_open && has_high && has_low && has_close) {
    return(object)
  }

  if (has_close) {
    col_close <- which(data_columns == "close")
    if (!has_open) {
      object$Open <- object[, col_close]
    }
    if (!has_high) {
      object$High <- object[, col_close]
    }
    if (!has_low) {
      object$Low <- object[, col_close]
    }

    desired_order <- c("Open", "High", "Low", "Close")
    updated_cols <- colnames(object)
    lower_map <- tolower(updated_cols)
    desired_indices <- match(tolower(desired_order), lower_map)
    desired_indices <- desired_indices[!is.na(desired_indices)]
    other_cols <- setdiff(seq_along(updated_cols), desired_indices)
    new_order <- c(desired_indices, other_cols)
    object <- object[, new_order]
  }

  return(object)
}
#' Convert an xts index to POSIXct at midnight
#'
#' Converts the index of an `xts` object to `POSIXct` with time set to
#' midnight (00:00:00) in the provided timezone.
#'
#' @param xts_object An object of class `xts`.
#' @param tz A timezone string. Defaults to `"America/Sao_Paulo"`.
#' @return A `POSIXct` vector aligned to midnight in the chosen timezone.
#' @keywords internal
.convert_posixct <- function(xts_object, tz = NULL) {
  if (is.null(tz)) {
    tz <- "America/Sao_Paulo"
  }
  if (inherits(xts_object, "xts")) {
    dados <- as.POSIXct(format(index(xts_object), "%Y-%m-%d 00:00:00"),
      tz = tz
    )
    return(dados)
  } else {
    stop("This is not an xts object.")
  }
}
#' Annualize volatility from returns
#'
#' Computes annualized volatility from a vector of (daily) returns by
#' multiplying the sample standard deviation by `sqrt(252)`.
#'
#' @param r Numeric vector of returns.
#' @return A single numeric value: annualized volatility.
#' @keywords internal
.annualize_vol <- function(r) {
  sd(r, na.rm = TRUE) * sqrt(252)
}

.bt_periods_per_year <- function(x) {
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

.bt_daily_log_sums <- function(x) {
  if (!xts::is.xts(x)) stop("Input must be an xts object.", call. = FALSE)
  out <- xts::apply.daily(x, function(z) {
    vals <- as.numeric(z)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) NA_real_ else sum(vals)
  })
  colnames(out) <- "Log"
  out
}

.bt_daily_discrete_returns <- function(x) {
  if (!xts::is.xts(x)) stop("Input must be an xts object.", call. = FALSE)
  out <- xts::apply.daily(x, function(z) {
    vals <- as.numeric(z)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) NA_real_ else prod(1 + vals) - 1
  })
  colnames(out) <- "Discrete"
  out
}

.bt_annualized_daily_vol_from_log <- function(x, scale = 1, periods_per_year = 252) {
  daily_log <- .bt_daily_log_sums(x)
  vals <- as.numeric(daily_log)
  vals <- vals[is.finite(vals)]
  scale <- suppressWarnings(as.numeric(scale)[1])
  if (length(vals) < 2L || !is.finite(scale) ||
      !is.finite(periods_per_year) || periods_per_year <= 0) {
    return(NA_real_)
  }
  stats::sd(exp(vals * scale) - 1, na.rm = TRUE) * sqrt(periods_per_year)
}

.bt_annualized_daily_vol <- function(x, periods_per_year = 252) {
  if (!xts::is.xts(x)) stop("Input must be an xts object.", call. = FALSE)
  cn <- tolower(colnames(x) %||% character())
  log_idx <- which(cn == "log")
  if (length(log_idx)) {
    return(.bt_annualized_daily_vol_from_log(x[, log_idx[1], drop = FALSE], periods_per_year = periods_per_year))
  }
  disc_idx <- which(cn == "discrete")
  disc <- if (length(disc_idx)) x[, disc_idx[1], drop = FALSE] else x[, 1, drop = FALSE]
  daily <- .bt_daily_discrete_returns(disc)
  vals <- as.numeric(daily)
  vals <- vals[is.finite(vals)]
  if (length(vals) < 2L || !is.finite(periods_per_year) || periods_per_year <= 0) {
    return(NA_real_)
  }
  stats::sd(vals, na.rm = TRUE) * sqrt(periods_per_year)
}

.bt_return_summary_values <- function(x, periods_per_year, geometric = TRUE) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(list(annualized = NA_real_, cumulative = NA_real_))
  }

  cumulative <- if (isTRUE(geometric)) prod(1 + x) - 1 else sum(x)
  if (!is.finite(periods_per_year) || periods_per_year <= 0) {
    annualized <- cumulative
  } else if (isTRUE(geometric)) {
    n_periods <- max(1L, length(x) - 1L)
    annualized <- if (1 + cumulative > 0) {
      (1 + cumulative)^(periods_per_year / n_periods) - 1
    } else {
      NA_real_
    }
  } else {
    annualized <- mean(x) * periods_per_year
  }

  list(annualized = annualized, cumulative = cumulative)
}

#' Create a business day calendar (bizdays)
#'
#' Convenience wrapper to create a `bizdays` calendar using the provided
#' calendar name (defaults to the ANBIMA/B3 calendar) and Saturday/Sunday as
#' non-business days.
#'
#' @param name Character scalar, calendar name to load from `bizdays::holidays`.
#' @return Invisibly returns the created bizdays calendar object.
#' @keywords internal
.generate_calendar <- function(name = "Brazil/ANBIMA") {
  tryCatch(bizdays::load_builtin_calendars(), error = function(e) NULL)
  holidays <- tryCatch(
    bizdays::holidays(name),
    error = function(e) as.Date(character())
  )
  cal_b3 <- bizdays::create.calendar(
    name      = name,
    holidays  = holidays,
    weekdays  = c("saturday", "sunday")
  )
  invisible(cal_b3)
}

.print_returns <- function(returns_xts, normalize_risk = NULL, geometric = TRUE) {
  if (!xts::is.xts(returns_xts)) {
    stop("returns_xts must be an xts object.")
  }

  cols <- colnames(returns_xts)
  discrete_col <- which(tolower(cols %||% character()) == "discrete")
  discrete <- if (length(discrete_col)) {
    returns_xts[, discrete_col[1], drop = FALSE]
  } else {
    returns_xts[, 1, drop = FALSE]
  }
  colnames(discrete) <- "Discrete"
  log_xts <- xts::xts(log1p(as.numeric(discrete$Discrete)), order.by = index(discrete))
  colnames(log_xts) <- "Log"

  risk_scale <- NA_real_
  risk_target <- NA_real_
  risk_original <- NA_real_

  target_input <- normalize_risk
  if (!is.null(target_input)) {
    target_val <- suppressWarnings(as.numeric(target_input)[1])
    if (is.finite(target_val)) {
      scaled_log <- bt_normalize_risk(log_xts, risk = target_val, type = "Log")
      scale_factor <- attr(scaled_log, "scale_factor")
      orig_vol <- attr(scaled_log, "original_vol")
      if (is.finite(scale_factor) && scale_factor > 0) {
        log_xts <- scaled_log
        discrete <- xts::xts(exp(as.numeric(log_xts)) - 1, order.by = index(log_xts))
        colnames(discrete) <- "Discrete"
        risk_scale <- scale_factor
        risk_original <- if (is.finite(orig_vol)) orig_vol * 100 else NA_real_
        risk_target <- target_val
      }
    }
  }

  out <- cbind(discrete, log_xts)
  attr(out, "risk_scale") <- risk_scale
  attr(out, "risk_target") <- risk_target
  attr(out, "risk_original") <- risk_original

  ppy <- .bt_periods_per_year(out)
  disc <- .bt_return_summary_values(as.numeric(out$Discrete), ppy, geometric = geometric)
  log_ret <- .bt_return_summary_values(as.numeric(out$Log), ppy, geometric = geometric)

  res_table <- matrix(
    sprintf("%.4f%%", c(disc$annualized, log_ret$annualized, disc$cumulative, log_ret$cumulative) * 100),
    nrow = 2,
    dimnames = list(c("Discrete", "Log"), c("Annual", "Total"))
  )

  cat(paste0("\n--- Returns Summary ", if (geometric) "(Geometric)" else "(Simple)", " ---\n"))
  print(res_table, quote = FALSE, right = TRUE)
  cat("\n")

  out
}
