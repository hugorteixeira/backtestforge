.dbg <- function(...) cat(format(Sys.time(), "%T"), "-", ..., "\n")
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
.annualize_vol <- function(r) {

  sd(r, na.rm = TRUE) * sqrt(252)

}
.generate_calendar <- function(name = "Brazil/ANBIMA"){
  cal_b3 <- create.calendar(
    name      = name,
    holidays  = holidays(name),
    weekdays  = c("saturday","sunday")
  )
  return(invisible(cal_b3))
}

