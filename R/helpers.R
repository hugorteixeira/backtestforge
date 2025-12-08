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
#' Inspects the attributes of an xts time series to collect futures-specific
#' metadata such as tick size, multiplier, maturity, currency, and identifier
#' fields (fees/slippage). Missing pieces fall back to `NULL`.
#'
#' @param object xts object containing market data plus metadata attributes.
#' @return A list with `tick_size`, `multiplier`, `maturity`, `currency`, and
#'   `identifiers` entries.
#' @keywords internal
.collect_instrument_metadata <- function(object) {
  meta <- list(
    tick_size = NULL,
    multiplier = NULL,
    maturity = NULL,
    currency = NULL,
    identifiers = list()
  )
  if (is.null(object)) {
    return(meta)
  }

  fetch_attr <- function(name) attr(object, name, exact = TRUE)

  meta$tick_size <- fetch_attr("fut_tick_size") %||% fetch_attr("tick_size") %||% fetch_attr("TickSize")
  meta$multiplier <- fetch_attr("fut_multiplier") %||% fetch_attr("multiplier") %||% fetch_attr("Multiplier")
  meta$maturity <- fetch_attr("maturity") %||% fetch_attr("expiry") %||% fetch_attr("expiration")
  meta$currency <- fetch_attr("currency") %||% fetch_attr("Currency") %||% fetch_attr("currency_id")

  ids <- fetch_attr("identifiers")
  if (is.environment(ids)) ids <- as.list(ids)
  if (!is.list(ids)) ids <- list()

  attr_candidates <- list(
    slippage = c("slippage", "Slippage", "spread", "Spread"),
    fees = c("fees", "fee", "Fees", "commission", "commission_per_contract")
  )

  for (nm in names(attr_candidates)) {
    if (is.null(ids[[nm]])) {
      for (cand in attr_candidates[[nm]]) {
        val <- fetch_attr(cand)
        if (!is.null(val)) {
          ids[[nm]] <- val
          break
        }
      }
    }
  }

  meta_attr <- fetch_attr("metadata")
  if (is.environment(meta_attr)) meta_attr <- as.list(meta_attr)
  if (is.list(meta_attr)) {
    for (nm in names(attr_candidates)) {
      if (is.null(ids[[nm]]) && !is.null(meta_attr[[nm]])) {
        ids[[nm]] <- meta_attr[[nm]]
      }
    }
  }

  meta$identifiers <- ids
  meta
}

#' Register/update a `FinancialInstrument` future from raw data
#'
#' Uses metadata found on an xts data object to define or update a futures
#' instrument inside `FinancialInstrument`. Automatically ensures the currency
#' exists and merges identifiers when the instrument is already present.
#'
#' @param symbol Character identifier to register.
#' @param data_xts xts object with market data and metadata attributes.
#' @param overwrite Passed to `FinancialInstrument::future()`; defaults to TRUE.
#' @return Invisibly returns TRUE on successful registration, NULL otherwise.
#' @keywords internal
.register_future_from_data <- function(symbol, data_xts, overwrite = TRUE) {
  if (!requireNamespace("FinancialInstrument", quietly = TRUE)) {
    return(invisible(NULL))
  }
  if (missing(symbol) || is.null(symbol) || !nzchar(symbol[1])) {
    return(invisible(NULL))
  }

  meta <- .collect_instrument_metadata(data_xts)

  existing <- try(FinancialInstrument::getInstrument(symbol, silent = TRUE), silent = TRUE)
  if (!inherits(existing, "try-error") && FinancialInstrument::is.instrument(existing)) {
    meta$tick_size <- meta$tick_size %||% existing$tick_size
    meta$multiplier <- meta$multiplier %||% existing$multiplier
    meta$maturity <- meta$maturity %||% existing$maturity
    meta$currency <- meta$currency %||% existing$currency
    existing_ids <- existing$identifiers
    if (is.null(existing_ids)) existing_ids <- list()
    if (!is.list(meta$identifiers)) meta$identifiers <- list()
    meta$identifiers <- utils::modifyList(existing_ids, meta$identifiers)
  }

  tick_size <- .sanitize_scalar_numeric(meta$tick_size, default = 0.01)
  multiplier <- .sanitize_scalar_numeric(meta$multiplier, default = 1)

  currency <- meta$currency
  if (is.null(currency) || length(currency) == 0 || !nzchar(as.character(currency)[1])) {
    currency <- "USD"
  } else {
    currency <- as.character(currency)[1]
  }

  identifiers <- meta$identifiers
  if (!is.list(identifiers)) identifiers <- list()

  if (!FinancialInstrument::is.currency(currency)) {
    try(FinancialInstrument::currency(currency), silent = TRUE)
  }

  suppressWarnings(try(
    FinancialInstrument::future(
      primary_id = symbol,
      tick_size = tick_size,
      multiplier = multiplier,
      maturity = meta$maturity,
      currency = currency,
      identifiers = identifiers,
      overwrite = overwrite
    ),
    silent = TRUE
  ))

  invisible(TRUE)
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
  cal_b3 <- create.calendar(
    name      = name,
    holidays  = holidays(name),
    weekdays  = c("saturday", "sunday")
  )
  return(invisible(cal_b3))
}

.print_returns <- function(returns_xts, normalize_risk = NULL, geometric = TRUE) {
  if (!xts::is.xts(returns_xts)) {
    stop("returns_xts must be an xts object.")
  }

  discrete <- returns_xts
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

  # cat(paste0("\nAnnualized Returns Discrete",
  #            if (geometric) " (Geometric): " else ": ",
  #            sprintf("%.4f%%", Return.annualized(out$Discrete, geometric = geometric) * 100), "\n"))
  # cat(paste0("Annualized Returns Log",
  #            if (geometric) " (Geometric): " else ": ",
  #            sprintf("%.4f%%", Return.annualized(out$Log, geometric = geometric) * 100), "\n"))
  # cat(paste0("Cumulative Returns Discrete",
  #            if (geometric) " (Geometric): " else ": ",
  #            sprintf("%.4f%%", Return.cumulative(out$Discrete, geometric = geometric) * 100), "\n"))
  # cat(paste0("Cumulative Returns Log",
  #            if (geometric) " (Geometric): " else ": ",
  #            sprintf("%.4f%%", Return.cumulative(out$Log, geometric = geometric) * 100), "\n\n"))
  ann_disc <- as.numeric(Return.annualized(out$Discrete, geometric = geometric))
  ann_log <- as.numeric(Return.annualized(out$Log, geometric = geometric))
  cum_disc <- as.numeric(Return.cumulative(out$Discrete, geometric = geometric))
  cum_log <- as.numeric(Return.cumulative(out$Log, geometric = geometric))

  res_table <- matrix(
    sprintf("%.4f%%", c(ann_disc, ann_log, cum_disc, cum_log) * 100),
    nrow = 2,
    dimnames = list(c("Discrete", "Log"), c("Annual", "Total"))
  )

  cat(paste0("\n--- Returns Summary ", if (geometric) "(Geometric)" else "(Simple)", " ---\n"))
  print(res_table, quote = FALSE, right = TRUE)
  cat("\n")

  out
}
