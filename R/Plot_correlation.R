
#' Correlation simplified creation
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @export
#'


# readt("file:///D:/these/Pecc_test/1_Data/3_final/main_dataset_extended3_20_12_02.data") %>%
#   filter(YTYPE == 1, NMID2 > 200) %>%
#   peccary_NCA(timecol =  TIME_NEG_A,obscol =  DV2, NMID2, AGE)  -> df
#
# x <- expr(AGE)
# y <- exprs(Cmax, Cmin)
# output <- expr(df)
# outputExpr = F
# wrapscale = "free"
# colwrap <- expr(NA)
#
# ylog = T
# add = "reg.line"
# conf.int = T
# cor.coef = T
# cor.method = "spearman"
# xlog = F
# caption = F
plot_correlation <- function(df, x, ..., ylog = T, xlog = F,  add = "reg.line",cor.coef = T, cor.method = "spearman",  conf.int = F,    outputExpr = F,colwrap = NA, wrapscale = "free", caption = F){

x <- enexpr(x)
y <- enexprs(...)

colwrap <- enexpr(colwrap)

if(outputExpr == F){
  output <- expr(df)
}else{

  output <- substitute(df)
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



# Create the base

 output <-
  expr(

  !!output %>%
       ggscatter(x = !!deparse(x), y = !!deparse(y), add = add, cor.coef = cor.coef, cor.method =  cor.method,  conf.int = conf.int)+
    theme_bw()

     ) %>%
    eval


# Facet wrap
# print(colwrap)
if(multiy == T){

 output <-  expr(!!output+
         facet_wrap(~key, scales = wrapscale))

}

if(!is.na(colwrap) & multiy == F){


  output <-  expr(!!output+
                    facet_wrap(~ !!colwrap, scales = wrapscale))

}

if(!is.na(colwrap) & multiy == T) warning("You can not use colwrap if ... has multiple input")


# Y log

if(ylog == T){

  output <- expr(!!output +
                   scale_y_log10())
}

# X log

if(ylog == T){

    output <- expr(!!output +
                     scale_x_log10())
}


output <- expr(!!output +  labs(x = !!deparse(x)))

if(caption == T){


nrows <- nrow(df %>% filter(!is.na(!!x)))

output <- expr(!!output +  labs(x = !!deparse(x), caption  = !!paste0(nrows, " lines ",if_else(cor.coef == F ,"" ,  paste0("; ", cor.method, " test")))))

}


if(outputExpr == F){
  return(eval(output))
}else{

  return(output)
}


}

