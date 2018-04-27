#' dollarChar
#'
#' @param x Numeric. Dollar amount to be formatted.
#' @param ndecimal Integer. Number of decimal places to round to
#' @param unit What units to convert x to
#' \itemize{
#'   \item d = dollars (ones)
#'   \item k = thousands
#'   \item m = millions
#'   \item b = billions
#'   \item t = trillions (this is a lot of money, are you sure you
#'   don't have a data issue?)
#' }
#' @param currency String. Symbol to use as prefix of number.
#' @param direction Boolean. Should + proceed the number for
#' positive numbers?
#'
#' @return String.
#' @export
#'
#' @examples dollarChar(x = 2118999, unit = 'm')
dollarChar = function(x = 'dollars', ndecimal = 2, unit = 'd',
                      currency = '$', direction = FALSE) {

  #convert to requested units
  if (unit == 'd') {
    y = round(x, ndecimal)
  } else if (unit == 'k') {
    y = round(x/1000, ndecimal)
  } else if (unit == 'm') {
    y = round(x/1000000, ndecimal)
  } else if (unit == 'b') {
    y = round(x/1000000000, ndecimal)
  } else if (unit == 't') {
    y = round(x/1000000000000, ndecimal)
  }

  if (direction) {
    drctn = ifelse(y >= 0, '+', '-')
  }


  #format to string
  if (unit == 'd') {
    unit = ''
  }

  y[!is.na(y)] = prettyNum(round(y[!is.na(y)], ndecimal), big.mark = ',')

  y[!is.na(y)] = paste0(currency, y[!is.na(y)], toupper(unit))

  #convert NAs to blanks so they don't print "NA"
  y[is.na(y)] = ''

  if (exists('drctn')) {
    y = gsub('-','',y)
    y = paste0(drctn, y)
  } else {
    y[grepl('-', y)] = paste0('-', gsub('-','',y[grepl('-', y)]))
  }

  # TODO: add commas

  return(y)
}


# copywriteDate
#' copywriteDate
#'
#' @return string representation of date like "Sep 25, 2017"
#' @export
#'
#' @examples copywriteDate()
copywriteDate = function(){
  d = lubridate::day(Sys.Date())
  m = lubridate::month(Sys.Date(), label = TRUE)
  y = lubridate::year(Sys.Date())

  return(paste0(m, ' ', d, ', ', y))
}




#' Convert percents to string representations
#'
#' @param percent a percent, like 0.63
#' @param ndecimal number of decimal places to round  to
#'
#' @return string
#' @export
#'
#' @examples percentChar(.05364, ncecimal = 2)
percentChar = function(percent, ndecimal = 1) {
  percent = round(percent * 100, ndecimal)
  percent = paste0(percent, '%')
  return(percent)
}




#' Format customer names and addresses for fuzzy matching
#'
#' Performs common transformations like making co, corp and corporation all
#' co; and converting all letters to lower case
#'
#' @param x string
#'
#' @return String
#' @importFrom magrittr '%>%'
#' @export
#'
#' @examples cleanCust('3M Corp., Suite #18, Innovation Boulevard')
cleanCust = function(x) {
  #make sure character vector
  if (!is.character(x)) {return(x)}

  x = gsub('\\s+',' ',x) %>% #remove extra spaces
    gsub('[[:punct:]]','',.) %>% #remove all punctuation
    tolower(.) %>% #convert to all lower-case letters
    gsub('\\bcorp\\b|\\bcorporation\\b', 'co',.) %>% #consolidate versions of common words
    gsub('\\bincorporated\\b','inc',.) %>%
    gsub('\\bmanufacturing\\b', 'mfg',.) %>%
    gsub('\\bcooperative\\b','coop',.) %>%
    gsub('\\bavenue\\b',' ave ',.) %>%
    gsub('\\bstreet\\b',' st ',.) %>%
    gsub('\\bdrive\\b','dr',.) %>%
    gsub('\\bboulevard\\b','blvd',.) %>%
    gsub('\\bhighway\\b','hwy',.) %>%
    gsub('\\bplaza\\b','plz',.) %>%
    gsub('\\bplace\\b','pl',.) %>%
    gsub('\\broad\\b','rd',.) %>%
    gsub('\\bsuite\\b','ste',.) %>%
    gsub('\\bnorth\\b','n',.) %>%
    gsub('\\bsouth\\b','s',.) %>%
    gsub('\\beast\\b','e',.) %>%
    gsub('\\bwest\\b','w',.) %>%
    gsub('\\bnortheast\\b','ne',.) %>%
    gsub('\\bnorthwest\\b','nw',.) %>%
    gsub('\\bsoutheast\\b','se',.) %>%
    gsub('\\bsouthwest\\b','sw',.) %>%
    gsub('\\band\\b','&',.) %>%
    gsub('\\bnum\\b|\\bnumber\\b','#',.) %>%
    gsub('\\s+',' ',.) %>% #remove extra spaces
    gsub('^\\s+|\\s+$','',.) #trim leading and trailing spaces

  return(x)
}


