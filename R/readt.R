
#' Reading table with automatic separator guessing and tibble output
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @export
#'
#'

readt <- function(data, na.strings = c(".", "NA"), sep = NULL, header =T, nrows = -1, stringsAsFactors = F){

  if(is.null(sep)){
  # use first line to guess separator
  firstline <-   readLines(data, n = 1)

  tibble(test = c(";", ",", ":", "  "))  %>%
   mutate(ncoltest = map_dbl(test, ~ length(str_split(firstline, pattern = .x)[[1]]))) %>%
    arrange(desc(ncoltest)) %>%
    pull(test) -> sep
  }

  read.table(data, sep = sep, header = header, nrows = nrows, na.strings = na.strings ,stringsAsFactors = stringsAsFactors) %>%
    as_tibble

}
