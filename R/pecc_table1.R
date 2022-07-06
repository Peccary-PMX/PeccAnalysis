#' Table1 creation
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description  Re compute table1 from sratch
#' @param dataset dataset to analysis
#' @param rowl vector of dataset column names (quoted) with addition of _cont ("X_cont") or _cat ("X_cat") to force to consider a numeric column into a categorical one
#' @param ... columns (generally ID, unquoted) used to reduce the dataset. "..." integrated into pecc_search_cov(...) function.
#' @param coll optional name of a categorical covariate (quoted)
#' @param NArp if NA are produced, which string to display (default = " ")
#' @param geomMean if True, compute geometric mean
#' @param na.rm if True (default), NA values are removed and ignored. If false, NA are kept with NA as result of most statistics parameter.
#' @param round number of digit displayed
#' @examples
#' pecc_table1(mtcars, rowl = c("mpg_cont", "gear_cont", "gear_cat"), coll = "cyl")
#'
#' # if you have a dataset with multiple rows per ID, you will have too many patients (one per row):
#' pecc_table1(Theoph, rowl = c("Wt_cont", "Dose_cont"))
#'
#' # after rowl parameter, you can add all ID columns you want in "..." to extract one line per patient:
# pecc_table1(Theoph, rowl = c("Wt_cont", "Dose_cont"), Subject )
#' @export


# dataset <- dataset %>%
#   distinct(NMID2, AGE, STDY2)
# rowl = "AGE_cont"
# coll = "STDY2"
# pecc_table1(mtcars, rowl = c("mpg_cont", "gear_cont", "gear_cat"), coll = "cyl")

# dataset <- NCAres %>%
#   filter(YTYPE == 60)
# rowl = c("Cmax_cont",  "AUCTlast_cont", "AUC28_cont")
# coll = "dose_explained"
# NArp = " "
# geomMean = F
# na.rm = T
# round = 1
#
# max(dataset$AUCTlast)
#   pecc_table1(coll = "dose_explained" , , na.rm = F)

# pecc_table1(mtcars, rowl = c("mpg_cont", "gear_cont", "gear_cat"), coll = "cyl")
# as.tibble(Theoph) %>%
#   pecc_search_cov(Subject)

pecc_table1 <- function(dataset, rowl, ..., coll = "",  NArp = " ", geomMean = F,  na.rm = T, round = 1){
#

y <- enexprs(...)

if(length(y) > 0){

  forreduction <- gsub("(_cont)|(_cat)", "",rowl)
  if(coll != "") forreduction <- c(forreduction, coll)
  forreduction <- unique(forreduction)

  dataset <- pecc_search_cov(dataset, !!!parse_exprs(forreduction), !!!y)
}
#print("coll")
#print(coll)
  ## n par groupe, in the end
  if(coll == "" | is.na(coll)){
    dataset$All <- "All"
    coll <- "All"
  }

crossing(x = rowl, y = coll ) %>%
  mutate(map2(x, y, function(x, y){

    temp <- dataset %>%
      rowid_to_column("IDPECC")


    # continue or categorial?
    if(length(grep("_cont$", x)) ==1){
      type <- 1
      x <- gsub("_cont$", "", x)

    }else if(length(grep("_cat$", x)) ==1){
      type <- 0
      x <- gsub("_cat$", "", x)

    }else{

      # try to guess, to improve
        length(unique(temp$x)) -> len
        type <- 1
        if(len < 10 | is.character(temp$x)) type <- 0
    }


    names(temp)[names(temp) == x] <- "x"
    names(temp)[names(temp) == y] <- "y"



# categocial
if(type == 0){

    temp$x[is.na(temp$x)] <- "Missing"

  temp %>%
    group_by(x, y) %>%
    summarise(n = length(IDPECC)) %>%
    spread(key = y, value = n) %>%
    map_dfr(function(x){

      x[is.na(x)] <- 0
      x

    }) -> temp


  temp$Overall <- map_dbl(1:nrow(temp), function(z){

    sum(temp[z, -1])

  })
  # #print("pouet")

  for(a in 2:length(temp)){

    sum(temp[[a]]) -> sumcol
    temp[[a]] <- paste0(temp[[a]], " (", round(temp[[a]]  * 100 / sumcol, 1), "%)")
  }

  names(temp)[1] <- "Cov"

  temp$Cov <- as.character(temp$Cov)


  temp <- bind_rows(tibble(Cov = x), temp, tibble(Cov = NA))

  map_dfr(temp, function(x){

    x[x == "0(0%)"] <- "/"
    x
  })

  namesss <- as.character(unique(dataset[[coll]]))
  namesss <- namesss[order(namesss)]
  temp <- temp[c("Cov",namesss , "Overall")]

  return(temp)

}else{
 # continue

  if(geomMean == F){


    temp %>%
      mutate(y = as.character(y)) %>%
      bind_rows(temp %>% mutate(y = "Overall")) %>%
      group_by(y) %>%
      summarise(median = median(x, na.rm =  na.rm), mean = mean(x, na.rm =  na.rm), sd = sd(x, na.rm =  na.rm), min = min(x, na.rm =  na.rm), max = max(x, na.rm =  na.rm)) %>%
      mutate(`Mean (SD)` = paste0(round(mean,round), " (", round(sd,2),")")) %>%
      mutate(`Median [Min;Max]` = paste0(round(median,round), " [", round(min,round),";",round(max,round),"]")) %>%
      select(-median, - mean, -sd, -min, -max) %>%
      gather(-y, key = "Cov", value = "value") %>%
      spread(key = y, value = value) -> temp

  }else{

    temp %>%
      mutate(y = as.character(y)) %>%
      bind_rows(temp %>% mutate(y = "Overall")) %>%
      group_by(y) %>%
      summarise(median = median(x, na.rm =  na.rm), mean = exp(mean(log(x), na.rm =  na.rm)), sd = exp(sd(log(x), na.rm =  na.rm)), min = min(x, na.rm =  na.rm), max = max(x, na.rm =  na.rm)) %>%
      mutate(`Geom Mean (SD)` = paste0(round(mean,round), " (", round(sd,2),")")) %>%
      mutate(`Median [Min;Max]` = paste0(round(median,round), " [", round(min,round),";",round(max,round),"]")) %>%
      select(-median, - mean, -sd, -min, -max) %>%
      gather(-y, key = "Cov", value = "value") %>%
      spread(key = y, value = value) -> temp


  }



  # To have the right order !
  namesss <- as.character(unique(dataset[[coll]]))
  namesss <- namesss[order(namesss)]
  temp <- temp[c("Cov",namesss , "Overall")]

  temp <- bind_rows(tibble(Cov = x), temp, tibble(Cov = NA))



}

  })) %>%
  unnest %>%
  select(-x, -y) -> temp


names(dataset)[names(dataset) == coll] <- "y"

dataset %>%
  group_by(y) %>%
  tally %>%
  pull(n) -> nn


names(temp)[1] <- paste0(coll, "")
names(temp)[-1] <- paste0(  names(temp)[-1], " (n=",c(nn, sum(nn)), ")" )

  if(coll == "All"){

    names(temp)[1] <- "Table1"
    temp <- temp[-3]
  }

temp <- map_dfr(temp, function(x){

  x[is.na(x)] <- NArp
  x

})

  return(temp)
}

## version en préparation metaprogramming

# Version metaprogrammé en préparation ------------------------------------

# pecc_table1(mtcars, ..., rowl = c("mpg_cont", "gear_cont", "gear_cat"), col1 = NA, col2 = NA)

#' Table1 packagecreation
#'
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description  Re compute table1 from sratch
#' @param df dataset to analyze
#' @param ... name of the columns to analyze, optionally with "_cont" or "_cat" to force a continuous or categorical behavior. If ... is empty, analyze every columns (using reduce by to extract ID).
#' @param col1 optional name of a categorical covariate to use as first column
#' @param col2 optional name of a categorical covariate to use as second column
#' @param reduceBy optional name of the columns to reduce the dataset by, separeated by "+"  (eg ID + occ)
#' @param outputExpr If F return directly the table1 output, if "Both" return both the code and its evaluation, else return only the code
#' @export

pecc_table1_original <- function(df, ..., col1 = NA, col2 = NA,  reduceBy = NA,  outputExpr = F){

  if(outputExpr == F){
    output <- expr(df)
  }else{

    output <- substitute(df)
  }

  reduceBy <- enexpr(reduceBy)
  col1 <- enexpr(col1)
  col2 <- enexpr(col2)

if(!is.na(reduceBy)){

  reduceBy <-  str_split(deparse(reduceBy), pattern = " *\\+ *" )[[1]] %>%
    parse_exprs

  output <- expr(pecc_search_cov(dataset = df,!!!reduceBy, returnExp = T)) %>%
    eval %>%
    deparse() %>%
    gsub(pattern = "^df", replacement = deparse(output))

  output <- parse_expr(output)

}

rowl <- enexprs(...)


if(length(rowl) == 0){

  if(!is.na(reduceBy)){
    rowl <-  expr(pecc_search_cov(dataset = df,!!!reduceBy, returnExp = F)) %>%
      eval %>%
      names


  }else{

    rowl <- df %>% names

  }

  torem <- c(deparse(col1),deparse(col2), map_chr(reduceBy, ~deparse(.x)))
  torem <- torem[-which(torem == "NA")]
if(length(torem) > 0)  rowl <- rowl[- which(rowl %in% torem)]

}else{

  rowl <- map_chr(rowl, ~ deparse(.x))

}


listmodif <- list()

# see if modification needed
  for(a in rowl){

   iscont <- grepl("_cont$", a)
   iscat <- grepl("_cat$", a)

   if(iscont |iscat){
     namevar <- gsub("(_cont$)|(_cat$)", "", a)


     if(iscont & !is.numeric(df[[namevar]])) listmodif[[namevar]] <- expr(as.numeric(!!parse_expr(namevar)))
     if(iscat & is.numeric(df[[namevar]])) listmodif[[namevar]] <- expr(as.factor(!!parse_expr(namevar)))
   }


  }



if(length(listmodif) > 0) output <- expr(!!output %>% mutate(!!!listmodif))

inrows <- gsub("(_cont$)|(_cat$)", "", rowl)

linetemp <- paste0(inrows, collapse = " + ")


if(!is.na(col1)) linetemp <- paste0(linetemp, " | ", deparse(col1))
if(!is.na(col2)) linetemp <- paste0(linetemp, " * ", deparse(col2))

final_expr <- expr(table1::table1(~ !!parse_expr(linetemp), data = !!output))

if(outputExpr == F){
 return(eval(final_expr))
}else if(outputExpr == "Both"){

  print(final_expr)
  return(eval(final_expr))
}else{

  return(final_expr)
}


}

#
# a %>%
#   pecc_table1_original( outputExpr = F)
#
#
# a <- read.table("D:/Peccary_Annexe/Exemple_demo/DATA/Theoph.txt", header = T, sep = ";")
#
# pecc_table1_original(a, c("cov", "doseCAT", "Dose"), coll = c("doseCAT"), outputExpr = F)
#
#
# pecc_table1_original(df = a, col1 = doseCAT,reduceBy = ID, outputExpr = F)
#
#
# pecc_table1_original(a)
#
# a %>%
# pecc_table1_original(reduceBy = ID, outputExpr = F)
#
#
# pecc_table1_original(df = a,   col1 = doseCAT,reduceBy = ID + Dose)
#
#
# table1::table1(~cov + Dose | doseCAT, data = a)
#
#
