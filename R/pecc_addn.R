#' Add number of occurences of a covariate
#' @description This function is used to modify a column containing a categorical covariate to add the number of occurences. It is mostly used with boxplot  (inside \code{\link{plot_boxplot}} function for instance).
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @param dataset dataset to modify
#' @param col a column to modify
#' @param ... all columns to used for grouping (ID most of the time)
#' @examples
#' mtcars %>%
#'  addn(carb) %>%
#'  ggplot()+
#'  geom_boxplot(aes(carb, mpg))
#'
#'
#'  Theoph %>%
#'    mutate(cov = sample(LETTERS[1:3], size = nrow(Theoph), replace = T)) %>%
#'    addn(cov, Subject)
#' @export


#  dataset %>%
# addn(col = cov, Subject)
# dataset <- mtcars
# col = expr(carb)


#note ridiculously long, in fact possible to do group_by(cov) %>% mutate(cov = paste0(cov, length(cov))) ...

addn <- function(dataset, col, ...){

  col <- enexpr(col)
  y <- enexprs(...)

  datasettemp <- dataset

  if(length(y) > 0 ){

 datasettemp <- datasettemp %>%
   distinct(!!!y, !!col)

  }


  datasettemp  %>%
    group_by(!!col) %>%
    tally -> temp

  for(a in unique(temp[[1]])){


    if(is.factor(dataset[[deparse(col)]]) == T){
      whichlevel <- which(levels(dataset[[deparse(col)]]) == a)

      levels(dataset[[deparse(col)]])[whichlevel] <- paste0(a,"\n(n=", temp$n[temp[[1]] == a], ")")
    } else {


    dataset[[deparse(col)]][  dataset[[deparse(col)]] == a]  <- paste0(a,"\n(n=", temp$n[temp[[1]] == a], ")")
    }
  }
  return(dataset)
}
