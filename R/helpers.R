#' Print a timestamped debug message
#'
#' Prints the provided values prefixed by the current time (HH:MM:SS),
#' useful for lightweight debugging during backtests.
#'
#' @param ... Values to print. Passed to `cat()`.
#' @return Invisibly returns `NULL`. Called for its side effects (console output).
#' @keywords internal
.dbg <- function(...) cat(format(Sys.time(), "%T"), "-", ..., "\n")
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
