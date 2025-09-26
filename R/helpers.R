#' Print a timestamped debug message
#'
#' Prints the provided values prefixed by the current time (HH:MM:SS),
#' useful for lightweight debugging during backtests.
#'
#' @param ... Values to print. Passed to `cat()`.
#' @return Invisibly returns `NULL`. Called for its side effects (console output).
#' @keywords internal
.dbg <- function(...) cat(format(Sys.time(), "%T"), "-", ..., "\n")

# Null-coalescing helper
#
# Returns `y` when `x` is `NULL`, zero-length, or entirely `NA`; otherwise
# returns `x` unchanged. Useful for filling metadata defaults.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (is.atomic(x) && all(is.na(x))) return(y)
  x
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
  if (is.null(x) || length(x) == 0) return(default)
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
  if (is.null(object)) return(meta)

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
  if (!requireNamespace("FinancialInstrument", quietly = TRUE)) return(invisible(NULL))
  if (missing(symbol) || is.null(symbol) || !nzchar(symbol[1])) return(invisible(NULL))

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

  has_open  <- "open"  %in% data_columns
  has_high  <- "high"  %in% data_columns
  has_low   <- "low"   %in% data_columns
  has_close <- "close" %in% data_columns

  if (has_open && has_high && has_low && has_close) {
    return(object)
  }

  if (has_close) {
    col_close <- which(data_columns == "close")
    if(!has_open) {
      object$Open <- object[, col_close]
    }
    if(!has_high) {
      object$High <- object[, col_close]
    }
    if(!has_low) {
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
  if(is.null(tz)){
    tz = "America/Sao_Paulo"
  }
  if(inherits(xts_object, "xts")) {
    dados <- as.POSIXct(format(index(xts_object), "%Y-%m-%d 00:00:00"),
                        tz = tz)
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
.generate_calendar <- function(name = "Brazil/ANBIMA"){
  cal_b3 <- create.calendar(
    name      = name,
    holidays  = holidays(name),
    weekdays  = c("saturday","sunday")
  )
  return(invisible(cal_b3))
}

.print_returns <- function(returns_xts, normalize_risk = NULL, geometric = TRUE) {
  colnames(returns_xts) <- "Discrete"
  returns_xts$Log <- log(1 + returns_xts$Discrete)
  colnames(returns_xts) <- c("Discrete","Log")
  if(!is.null(normalize_risk)) {
    column_name <- paste0("Log_AdjRisk_", normalize_risk)
    temp_log <- log(1 + returns_xts$Discrete)
    new_column <- bt_normalize_risk(temp_log,risk = normalize_risk,type = "Log")
    names(new_column) <- column_name
    ptrets <- cbind(returns_xts, new_column)
  }
  if(!is.null(normalize_risk)) {
    column_name <- paste0("Discrete_AdjRisk_", normalize_risk)
    temp_dis <- returns_xts$Discrete
    new_column <- bt_normalize_risk(temp_dis,risk = normalize_risk,type = "Discrete")
    names(new_column) <- column_name
    returns_xts <- cbind(returns_xts, new_column)
  }

  cat(paste0("\nAnnualized Returns Discrete",
             if(geometric) " (Geometric): " else ": ",
             sprintf("%.4f%%", Return.annualized(returns_xts$Discrete, geometric = geometric) * 100), "\n"))
  cat(paste0("Annualized Returns Log",
             if(geometric) " (Geometric): " else ": ",
             sprintf("%.4f%%", Return.annualized(returns_xts$Log, geometric = geometric) * 100), "\n"))
  cat(paste0("Cumulative Returns Discrete",
             if(geometric) " (Geometric): " else ": ",
             sprintf("%.4f%%", Return.cumulative(returns_xts$Discrete, geometric = geometric) * 100), "\n"))
  cat(paste0("Cumulative Returns Log",
             if(geometric) " (Geometric): " else ": ",
             sprintf("%.4f%%", Return.cumulative(returns_xts$Log, geometric = geometric) * 100), "\n\n"))
  return(returns_xts)
}
