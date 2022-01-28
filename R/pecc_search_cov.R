#' Extract every covariates of a dataset
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description \code{pecc_search_cov} simplify a dataset to extract only covariates
#' @param dataset dataset to analyze
#' @param ... grouping columns (ID, occasion,...)
#' @examples
#' pecc_search_cov(Theoph, Subject)
#' @export
# pecc_search_cov(Theoph, Subject)
# dataset <- as.tibble(Theoph)
# key <- exprs(Subject)
pecc_search_cov <- function(dataset, ..., returnExp = F){

namedataset <- enexpr(dataset)
dataset <- as.tibble(dataset)

key <- enexprs(...)

# how many groups
  dataset %>%
    select(!!!key) %>%
    distinct() %>%
    nrow() -> nrowref

  # For each columnes excepted the ones inputed in "..."
  for(a in names(dataset)[ ! names(dataset) %in% map_chr(key, ~deparse(.x) )]){


    # Compute numbers of different rows including inputed columns in "..." and the one currently tested inside the loop
    dataset %>%
      filter(!is.na(!!a)) %>%
      select(!!!key, !!a) %>%
      distinct() %>%
      nrow -> nrowtest


     # Remove the column if there is more rows than the number of "IDs", or remove it if there is NA everywhere
    if(nrowtest > nrowref | (  length(unique(dataset[[a]])) == 1 & is.na(dataset[[a]][[1]]))){

      dataset <- dataset %>%
        select(- !!a)
    }

  }

  if(returnExp == F){

  return(dataset %>%
           distinct())

  }else{


    return(expr(!!namedataset %>% distinct(!!!parse_exprs(names(dataset)))))

  }

}

# pecc_search_cov <- function(explo, key = c("NMID2", "DOSE_A8_bool")){
#
#
#    explo %>%
#      select(!!!parse_exprs(key)) %>%
#      distinct() %>%
#      nrow() -> nrowref
#
#   for(a in names(explo)[ ! names(explo) %in% key]){
#
#     explo %>%
#       filter(!is.na(!!a)) %>%
#       select(!!!parse_exprs(key), !!a) %>%
#       distinct() %>%
#       nrow -> nrowtest
#
#
#
#
#     if(nrowtest > nrowref | (  length(unique(explo[[a]])) == 1 & is.na(explo[[a]][[1]]))){
#
#       explo <- explo %>%
#         select(- !!a)
#     }
#
#   }
#
#    return(explo %>%
#      distinct())
#
#
# }
