#' Table1 creation
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description \code{peccary_NCA} compute basic NCA analysis (no extrapolation)
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
#' pecc_table1(Theoph, rowl = c("Wt_cont", "Dose_cont"), Subject )
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


# dataset = mtcars
# rowl = c("mpg", "cyl")
# coll = "gear"
# NArp = " "
# geomMean = F
# na.rm = T
# round = 1
# nmaxcat = 5
# pecc_table1 <- function(dataset, rowl = c("SEX", "AGE"), coll = "", NArp = " ", geomMean = F,  na.rm = T, round = 1, nmaxcat = 5, expr = F){
#   #
#
#
#   if(expr == F){
#     # dataframe_origin <- expr(df)
#     dataset_expr <- expr(dataset <- !!expr(dataset))
#   }else{
#
#     dataset_expr <- expr(dataset <- !!substitute(dataset))
#   }
#
#
#
#   if(coll == "" | is.na(coll)){
#
#     dataset_expr <- expr(!!dataset_expr  %>%
#                            mutate(All = "All"))
#
#
#     dataset$All <- "All"
#     coll <- "All"
#
#
#   }
#
# # If user does not mention wehter it's continuous our categorical, guess for him
# for(a in rowl[!grepl("(_cont)|(_cat)", rowl)]){
#
#   test <- length(unique(dataset[[a]]))
#
#   if(test <= nmaxcat){
#     rowl[rowl == a] <- paste0(a, "_cat")
#   }else{
#
#     rowl[rowl == a] <- paste0(a, "_cont")
#   }
# }
#
#
#
# # Categorical function ----------------------------------------------------
#
#
# cat_fun <-   expr(function(row,col){
#
#     temp <- dataset
#
#
#     names(temp)[names(temp) == gsub("_cat", "", row )] <- "rowforpecc"
#     names(temp)[names(temp) == col] <- "colforpecc"
#     temp$rowforpecc[is.na(temp$rowforpecc)] <- "Missing"
#
#    # Compute for each subgroup
#     temp %>%
#       group_by(rowforpecc, colforpecc) %>%
#       tally %>%
#       spread(key = colforpecc, value = n) %>%
#       map_dfr(function(x){
#
#         x[is.na(x)] <- 0
#         x
#
#       }) -> temp
#
#     # Add Overall sum
#     temp$Overall <- map_dbl(1:nrow(temp), ~   sum(temp[.x, -1]))
#
#     # Statistiques
#     for(a in 2:length(temp)){
#
#       sum(temp[[a]]) -> sumcol
#       temp[[a]] <- paste0(temp[[a]], " (", round(temp[[a]]  * 100 / sumcol, 1), "%)")
#     }
#
#
#     temp$rowforpecc <- as.character(temp$rowforpecc)
#
#
#     bind_rows(tibble(rowforpecc = gsub("_cat", "", row )), temp, tibble(rowforpecc = NA))
#
#
#
#     # namesss <- as.character(unique(dataset[[coll]]))
#     # namesss <- namesss[order(namesss)]
#     # temp <- temp[c("Cov",namesss , "Overall")]
#
#
# })
#
#
# # eval(cat_fun)(row = "cyl",col = "gear")
#
#
#
# # Continuous function -----------------------------------------------------
#
#
# # Arithmetic of geometric Mean?
# if(geomMean == F) {
#   meanfunc <- expr(mean(rowforpecc, na.rm =  !!na.rm))
#   sdfunc <- expr(sd(rowforpecc, na.rm =  !!na.rm))
# }else{
#   meanfunc <- expr(exp(mean(log(rowforpecc), na.rm =  !!na.rm)))
#   sdfunc <- expr(exp(sd(log(rowforpecc), na.rm =  !!na.rm)))
#
#   }
#
# cont_fun <-expr(function(row,col){
#
#
#   temp <-  dataset
#
#
#   names(temp)[names(temp) == gsub("_cont", "", row )] <- "rowforpecc"
#   names(temp)[names(temp) == col] <- "colforpecc"
#
#
#
#
#   temp %>%
#          mutate(colforpecc = as.character(colforpecc)) %>%
#          bind_rows(temp %>% mutate(colforpecc = "Overall")) %>%
#          group_by(colforpecc) %>%
#          summarise(median = median(rowforpecc, na.rm =  !!na.rm),
#                    mean = !!meanfunc,
#                    sd = !!sdfunc,
#                    min = min(rowforpecc, na.rm =  !!na.rm),
#                    max = max(rowforpecc, na.rm =  !!na.rm)) %>%
#          mutate(`Mean (SD)` = paste0(round(mean,!!round), " (", round(sd,2),")")) %>%
#          mutate(`Median [Min;Max]` = paste0(round(median,!!round), " [", round(min,!!round),";",round(max,!!round),"]")) %>%
#          select(-median, - mean, -sd, -min, -max) %>%
#          gather(-colforpecc, key= "rowforpecc", value = "value") %>%
#          spread(key = colforpecc, value = value) -> temp
#
#          bind_rows(tibble(rowforpecc = gsub("_cont", "", row )), temp, tibble(rowforpecc = NA))
#
#
# })
#
#
# # eval(cont_fun)(row = "mpg",col = "gear")
#
#
# # Final function ----------------------------------------------------------
#
# # if only cat data
# # rowl <- c( "cyl_cat" , "am_cat")
# if(sum(grepl("_cont", rowl)) == 0 ){
#
#   expr({crossing(row = !!rowl, col = !!coll ) %>%
#          mutate(percov = map2(row, col,!!cat_fun)) %>%
#          pull(percov) %>%
#          bind_rows()%>%
#          map_dfr( function(x){
#
#            x[is.na(x)] <- !!NArp
#            x
#
#          }) %>%
#          rename(!!col := rowforpecc) -> table1
#
#
#     dataset %>%
#       group_by(!!parse_expr(col)) %>%
#       tally %>%
#       pull(n) -> nn
#
#     names(table1)[-1] <- paste0(  names(table1)[-1], " (n=",c(nn, sum(nn)), ")" )
#
#     table1
#
#
#
#       } ) -> res
# # eval(test)
#
# }else if(sum(grepl("_cat", rowl)) == 0 ){
#
# # if only cont
#
#   # rowl <- c("mpg_cont", "disp_cont" )
#   expr({crossing(row = !!rowl, col = !!coll ) %>%
#          mutate(percov = map2(row, col,!!cont_fun)) %>%
#          pull(percov) %>%
#          bind_rows()%>%
#          map_dfr( function(x){
#
#            x[is.na(x)] <- !!NArp
#            x
#
#          })  %>%
#          rename(!!parse_expr(coll) := rowforpecc)-> table1
#
#
#        dataset %>%
#          group_by(!!parse_expr(col)) %>%
#          tally %>%
#          pull(n) -> nn
#
#        names(table1)[-1] <- paste0(  names(table1)[-1], " (n=",c(nn, sum(nn)), ")" )
#
#        table1
#
#
#   }) -> res
#   # eval(test)
#
# }else{
#
# # ICI ---------------------------------------------------------------------
#
#
#  # if both
#   # rowl <- c( "mpg_cont" , "am_cat")
#
#
#
#   expr({cont_fun <- !!cont_fun
#   cat_fun <- !!cat_fun
#
#
#
#   crossing(row = !!rowl, col = !!coll ) %>%
#     mutate(percov = map2(row, col,function(row, col){
#
#       if(grepl("_cat", row)) return(cat_fun(row,col))
#
#       return( cont_fun(row,col) )
#
#     })) %>%
#     pull(percov) %>%
#     bind_rows() %>%
#     map_dfr( function(x){
#
#       x[is.na(x)] <- !!NArp
#       x
#
#     }) %>%
#     rename(!!parse_expr(coll) := rowforpecc) -> table1
#
#   dataset %>%
#     group_by(!!parse_expr(coll)) %>%
#     tally %>%
#     pull(n) -> nn
#
#   names(table1)[-1] <- paste0(  names(table1)[-1], " (n=",c(nn, sum(nn)), ")" )
#
#
#
#
#   table1
#
#     }) -> res
#
#
#
# }
#
# if(coll == "All"){
#
#
#
#
#   if(expr == F){
#     # dataframe_origin <- expr(df)
#
#     return(eval(expr({!!dataset_expr
#       table1 <- !!res
#
#       table1 %>% {.[-2]}} %>%
#         rename(` ` = All))))
#   }else{
#
#
#     return(expr({!!dataset_expr
#       table1 <- !!res
#
#       table1 %>% {.[-2]}} %>%
#         rename(` ` = All)))
#   }
#
#
#
#
#
# }
#
#
# if(expr == F){
#   # dataframe_origin <- expr(df)
#
#   return(eval(expr({!!dataset_expr
#     !!res})))
# }else{
#
#
#   return(expr({!!dataset_expr
#     !!res}))
# }
#
#
#
# }

# if(coll == "All"){
#
#   names(temp)[1] <- "Table1"
#   temp <- temp[-3]
# }



