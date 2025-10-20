#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr content POST add_headers http_status upload_file
#' @importFrom stats lag median sd setNames start
#' @importFrom utils str tail
#' @importFrom zoo index
#' @importFrom zoo index<-
#' @importFrom xts lag.xts as.xts apply.monthly
#' @importFrom quantmod chart_Series chart_theme chart_pars Cl Hi Lo
#' @importFrom PerformanceAnalytics table.CalendarReturns Return.annualized Return.cumulative
#' @importFrom FinancialInstrument future stock getInstrument instrument_attr
#' @importFrom blotter .getPortfolio initAcct initPortf getPortfolio updatePortf perTradeStats getEndEq updateEndEq AcctReturns PortfReturns updateAcct getTxns tradeStats perTradeStats getPosQty
#' @importFrom quantstrat applyIndicators applySignals add.rule add.signal add.indicator addPosLimit strategy applyRules getOrderBook applyStrategy initOrders initStrategy
#' @importFrom bizdays create.calendar holidays bizdays
#' @importFrom lubridate interval
#' @importFrom TTR runMax runMin
#' @importFrom magrittr %>%
#' @importFrom senhormercado sm_get_data
#' @importFrom tradeplotr tplot
#' @importFrom bizdays create.calendar
## usethis namespace: end
NULL

# Suppress R CMD check notes for non-standard evaluation
utils::globalVariables(c(
  "mktdata", "cal_b3", "Symbol", "Portfolio", "stat_name", "value", "order_key"
))
