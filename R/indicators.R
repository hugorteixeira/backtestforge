eldoc <- function(ticker, name_to_use = NULL, x = 25, y = 25,  hi.col = "High", lo.col = "Low", type = "chart") {
  sm <- chart_theme()
  sm$col$line.col <- "blue"
  sm$col$dn.col <- "firebrick2"
  sm$col$up.col <- "forestgreen"
  sm$col$dn.border <- "black"
  sm$col$up.border <- "black"
  sm$lylab = FALSE

  smpars <- chart_pars()
  smpars$cex <- 1

  hi <- ticker[, hi.col]
  lo <- ticker[, lo.col]

  high <- runMax(hi, x)
  low <- runMin(lo, y)
  result <- cbind(high, low)
  colnames(result) <- c("X", "Y")

  if (is.null(name_to_use)) {
    name_to_use <- toupper(deparse(substitute(ticker)))
  }

  if (type == "chart") {
    p <- format(median(diff(index(ticker))), format = "%H:%M:%S")
    lines <- "add_Series(high, on = 1, type = 'line'); add_Series(low, on = 1, type='line'); add_Vo()"
    chart_Series(ticker, pars = smpars, theme = sm, TA = lines,
                 name = paste0(name_to_use, ", ", p, ", ", "ElDoc ", x, "/", y,
                               ", X = ", tail(high, 1), ", Y = ", tail(low, 1)))
  } else if (type == "data") {
    res <- lag.xts(result)
    return(res)
  } else if (type == "fulldata") {
    ticker_subset <- ticker[, 1:5, drop = FALSE]
    final <- cbind(ticker_subset, result)
    final <- as.xts(final)
    return(final)
  }
}
