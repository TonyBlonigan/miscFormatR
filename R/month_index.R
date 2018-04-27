# month_index --------------------------------------------------------------
#' Create a month index for analysis and plotting
#'
#' @param df Data Frame: Data set you want an index for
#' @param SLS_YR Bare Column Name: Name of column containing the year as integer
#' @param SLS_MNTH_INT Bare Column Name: Name of column containing the month as integer
#'
#' @return Data Frame containing the columns provided as SLS_YR, SLS_MNTH_INT and a index
#' @export
#' @importFrom magrittr '%>%'
#'
#' @examples make_month_index(data.frame(SLS_YR = c(2016, 2017),
#' SLS_MNTH_INT = c(10,4)),
#' SLS_YR = SLS_YR, SLS_MNTH_INT = SLS_MNTH_INT)
make_month_index = function(df, SLS_YR, SLS_MNTH_INT) {

  # enquo bare col names
  enquo_SLS_YR = rlang::enquo(SLS_YR)
  enquo_SLS_MNTH_INT = rlang::enquo(SLS_MNTH_INT)

  # enquo bare col names as col names
  quo_name_SLS_YR = rlang::quo_name(enquo_SLS_YR)
  quo_name_SLS_MNTH_INT = rlang::quo_name(enquo_SLS_MNTH_INT)

  # shrink the data set
  month_index = dplyr::select(df, !!enquo_SLS_YR, !!enquo_SLS_MNTH_INT) %>%
    dplyr::distinct()

  # setup date boundaries
  minYr = dplyr::select(month_index, !!enquo_SLS_YR) %>%
    min()

  minMnth = dplyr::filter(month_index, !!enquo_SLS_YR == minYr) %>%
    min()

  maxYr = dplyr::select(month_index, !!enquo_SLS_YR) %>%
    max()

  maxMnth = dplyr::filter(month_index, !!enquo_SLS_YR == maxYr) %>%
    dplyr::select(!!enquo_SLS_MNTH_INT) %>%
    max()

  # make sure all months have at least one observation
  fillObs = data.frame(yr = rep(maxYr, 12), mnth = seq(12)) %>%
    dplyr::rename(!!quo_name_SLS_YR := yr,
                  !!quo_name_SLS_MNTH_INT := mnth)

  month_index = dplyr::bind_rows(month_index, fillObs) %>%
    dplyr::distinct()

  # create the index
  month_index = tidyr::complete(month_index, !!enquo_SLS_YR, !!enquo_SLS_MNTH_INT) %>%
    dplyr::filter(!!enquo_SLS_YR == minYr & !!enquo_SLS_MNTH_INT >= minMnth
                  | !!enquo_SLS_YR > minYr & !!enquo_SLS_YR < maxYr
                  | !!enquo_SLS_YR == maxYr & !!enquo_SLS_MNTH_INT <= maxMnth) %>%
    dplyr::arrange(!!enquo_SLS_YR, !!enquo_SLS_MNTH_INT) %>%
    dplyr::mutate(month_index = 1:n()) %>%
    data.frame()

  # remove extra months when there is just one year of data
  if(minYr == maxYr) {
    month_index = dplyr::filter(month_index, !!enquo_SLS_MNTH_INT >= minMnth,
                               !!enquo_SLS_MNTH_INT <= maxMnth)
  }

  return(month_index)
}
