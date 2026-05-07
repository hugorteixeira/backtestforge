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
#' @importFrom bizdays create.calendar holidays bizdays
#' @importFrom TTR runMax runMin
#' @importFrom magrittr %>%
## usethis namespace: end
NULL

# Suppress R CMD check notes for non-standard evaluation
utils::globalVariables(c(
  "mktdata", "cal_b3", "Symbol", "Portfolio", "stat_name", "value", "order_key"
))
