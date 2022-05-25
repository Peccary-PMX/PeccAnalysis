
#' Boxplot simplified creation
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description Easy creation of a boxplot
##' @param df dataframe to analyze
#' @param x categorica covariate
#' @param ... continue value for y abcis. If several values, will create one plot per ... column vs x
#' @param ylog If True, set log y scale
#' @param addPoints If True, add all points in addition to boxplots
#' @param statTest If True, use statistiscal test through ggpubr package
#' @param addn If True, add numer of patietn per group
#' @param outputExpr if True return the Peccary independant R code instead of the output
#' @param colwrap  name of a column to wrap the output. Used only if length of ... is equal to 1
#' @param wrapscale scale of wrap
#' @param methodCompar method to use for statistical test (same as ggpubr)
#' @export

# readt("file:///D:/these/Pecc_test/1_Data/3_final/main_dataset_extended3_20_12_02.data") %>%
#   filter(YTYPE == 1, NMID2 > 200) %>%
#   peccary_NCA(timecol =  TIME_NEG_A,obscol =  DV2, NMID2, SEX)  -> df
#
# x <- expr(SEX)
# y <- exprs(Cmax, Cmin)
# output <- expr(df)
# addPoints = T
# outputExpr = F
# wrapscale = "free"
# colwrap <- expr(NA)
# methodCompar = "wilcox.test"
# statTest <- T
# ylog = T
#
#
# df <- NCA_test
# x = expr(doseCAT)
#
# y <- exprs(Cmax, AUCTlast, Tmax, Tlast)

# NCA_test %>%
#   mutate(no_grp = "All IDs") %>%
#   plot_boxplot(x = doseCAT, Cmax, AUCTlast, Tmax, Tlast)


plot_boxplot <- function(df, x, ..., ylog = T, addPoints = T, statTest = T, addn = T,   outputExpr = F,colwrap = NA, wrapscale = "free",   methodCompar = "wilcox.test"){

x <- enexpr(x)
y <- enexprs(...)

colwrap <- enexpr(colwrap)

if(outputExpr == F){
  output <- expr(df)
}else{

  output <- substitute(df)
}


if(addn == T){

  # output <- expr(!!output %>% addn(col = !!x))

  output <- expr(!!output %>%
                   group_by(!!x) %>%
                   mutate(!!x := paste0(!!x, "\n(n=", length(!!x), ")"))
                   )


}

# handle multiple y
multiy <- F

if(length(y) > 1){

multiy <-T
output <-   expr(!!output %>%
         gather(!!!y, key = "key", value = "value"))
  y <- expr(value)
}else{

  y <- y[[1]]

}


# addn



# Create the base

output <- expr(

  !!output %>%
       ggplot(aes(factor(!!x), !!y ))+
      geom_boxplot()

     )

# Add points if required
if(addPoints == T){

  output <- expr(

    !!output +
      geom_point()
  )
  }

# Facet wrap
# print(colwrap)
if(multiy == T){

 output <-  expr(!!output+
         facet_wrap(~key, scales = !!wrapscale))

}

if(!is.na(colwrap) & multiy == F){


  output <-  expr(!!output+
                    facet_wrap(~ !!colwrap, scales = !!wrapscale))

}

if(!is.na(colwrap) & multiy == T) warning("You can not use colwrap if ... has multiple input")

# Statistical test


if(statTest == T){



    dftemp <- df %>%
      ungroup()


    if(addn == T){

      dftemp <- dftemp %>%
        addn(col = !!x)





    }

dftemp[[deparse(x)]] <- as.character(dftemp[[deparse(x)]])
    dftemp %>%
      distinct(!!x) %>%
      pull(!!x) -> allvalues


crossing(col1 = allvalues, col2 = allvalues) %>%
  mutate(col1order = map2_chr(col1, col2, ~ c(.x,.y)[order(c(.x,.y))][[1]])) %>%
  mutate(col2order = map2_chr(col1, col2, ~ c(.x,.y)[order(c(.x,.y))][[2]])) %>%
  distinct(col1order, col2order) %>%
  filter(col1order != col2order) %>%
  mutate(col2order = map2(col1order, col2order, ~ c(.x,.y))) %>%
  pull(col2order) -> groupforcomparison

output <- expr(!!output +
       ggpubr::stat_compare_means(comparisons = list(!!!groupforcomparison), method = !!methodCompar))
}

# Y log

if(ylog == T){

  output <- expr(!!output +
                   scale_y_log10())
}

output <- expr(!!output +  labs(x = !!deparse(x)) + theme_bw())

if(outputExpr == F){
  return(eval(output))
}else if(outputExpr == T){

  return(output)
}else{

  print(output)
  return(eval(output))

}


}

