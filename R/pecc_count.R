# df <- explo %>%
#   distinct(NMID2, CENTRE, SEX2, goodresArticle)
# metric = "goodresArticle"
# col_cov =  "SEX2"
# # row_cov = NULL
# row_cov = "CENTRE"
# # row_cov = NULL
# str_input = T
# couunt <- pecc_count(df,metric =  "goodresArticle", row_cov = "SEX2",col_cov =   "CENTRE", str_input = T)
#' Statistique of a boolean covariate, analyze by categorical covariates.
#'
#' @description We often have a boolean column (responder = 1, non responder = 0 for instance). Pecc_count create a table with the number of
#'  occurence (value = 1) divided by the number of total patients with or without specific categorical covariate (two can be simultaneously analyzed)
#' @param df Name of the dataframe to analyse
#' @param metric Name of the column metric (NSA)
#' @param col_cov Name of the first column
#' @param row_cov Name of the second column
#' @param str_input If true, metric, col_cov and row_cov argument must be strings. If False (default), provide expressions (unquoted arguments)
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @return
#' @export
#'
#' @examples pecc_count(mtcars,metric = vs, col_cov = cyl, row_cov = carb)
pecc_count <- function(df, metric, col_cov = NULL, row_cov = NULL, str_input = F){

  if(str_input == F){
    metric <- enexpr(metric)
    col_cov <- enexpr(col_cov)
    row_cov <- enexpr(row_cov)
  }

  if(str_input == T){

    metric <- parse_expr(metric)
    if(!is.null(col_cov)) col_cov <- parse_expr(col_cov)
    if(!is.null(row_cov)) row_cov <- parse_expr(row_cov)

  }

  if(deparse(row_cov) == "NULL"){


    df <- df %>%
      mutate(peccrow_covnul = "")
    row_cov <- expr(peccrow_covnul)
  }

  if(deparse(col_cov) == "NULL"){


    df <- df %>%
      mutate(pecccol_covnul = "")
    col_cov <- expr(pecccol_covnul)
  }

  sum(is.na(df[[metric]])) -> nna

  df <- df %>%
    filter(!is.na(!!metric))

  # how many do we have with 1
  n <- df %>%
    select(!!metric, !!col_cov, !!row_cov) %>%
    group_by(!!col_cov, !!row_cov) %>%
    summarise(n = sum(!!metric)) %>%
    spread(!!row_cov, n)

  cn <- c(as.character(n[[1]]), "Total")
  rn <- c(names(n)[-1], "Total")

  n <- n %>%
    column_to_rownames(var = deparse(col_cov)) %>%
    map_dfr(~ c(.x,sum(.x, na.rm = T))) %>%
    apply(1, function(x) c(x, sum(x, na.rm = T)))

  # how many do we have in total
  on <- df %>%
    select(!!metric, !!col_cov, !!row_cov) %>%
    group_by(!!col_cov, !!row_cov) %>%
    summarise(n = length(!!metric)) %>%
    spread(!!row_cov, n) %>%
    column_to_rownames(var = deparse(col_cov)) %>%
    map_dfr(~ c(.x,sum(.x, na.rm = T))) %>%
    apply(1, function(x) c(x, sum(x, na.rm = T)))




  map_chr(1:length(n), function(x){

    paste0(n[[x]], "/", on[[x]])

  }
  ) -> values

  matrix(values, nrow = nrow(n), ncol = ncol(n)) %>%
    as_tibble() %>%
    map_dfr(function(x){

      x[x == "NA/NA"] <- "-"
      x

    }) %>%
    mutate(test = rn) %>%
    select(test, everything())-> temp

  colnames(temp) <- c(paste0(deparse(row_cov),"/", deparse(col_cov), " (", deparse(metric), ")"), cn)

  if(deparse(col_cov)  == "pecccol_covnul"){

    temp <- temp[-2]
    colnames(temp)[1] <- gsub("/pecccol_covnul", "",  colnames(temp)[1])

  }

  if(deparse(row_cov)  == "peccrow_covnul"){

    temp <-temp[-1, ]
    colnames(temp)[1] <- gsub("peccrow_covnul/?", "",  colnames(temp)[1])

  }


rownna <- temp %>% slice(1) %>% as.data.frame()
rownna[1, ] <- c(paste0("(", nna, " NA)"), rep("",length(rownna) -1))
temp <- rbind(temp, rownna)

  return(temp)
}



