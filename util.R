
util <- list(
  today = function(){format(Sys.Date(), "%Y%m%d")})

`%!in%` <- function(a,b) ! a %in% b

softmax <- function(x) exp(x - min(x)) / sum(exp(x - min(x)))

sigmoid <- function(x) 1 / (1 + exp(-x))

minkowski_distance <- function(x, x1, y, y1, p_value) {
  root_value <- 1 / as.numeric(p_value)
  result <- sum(c((abs(x - x1) ^ root_value), 
                  (abs(y - y1) ^ root_value)))
  return(result)}

radius <- function(a){sqrt((a/pi))}

util$read_csv <- function(a){
  readr::read_delim(
    a, 
    delim = ";", 
    locale = readr::locale(decimal_mark = ","),
    escape_double = FALSE, 
    show_col_types = FALSE,
    trim_ws = TRUE)}

util$name_shorter_classif <- function(string){
  stringr::str_split_i(string, "\\_", i =1)}

