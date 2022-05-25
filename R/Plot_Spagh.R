#' Plot Spagh
#' @author Thibaud Derippe
#' @description Create spaghetti plot with various customisation
#'
#' @param df dataframe to analyze
#' @param x x-axis
#' @param y y-axis
#' @param group grouping variable
#' @param col coloring variable
#' @param shape shaping variable
#' @param lty lty variable
#' @param facetwrap variable for facetwrap
#' @param facetgrid seecond variable for facetgrid
#' @param facetscale classical "free", "free_x", "free_y"
#' @param median if wanting median to be plotted
#' @param point_alpha point alpha (transparancy) value, between 0 and 1
#' @param line_alpha line alpha  (transparancy) value, between 0 and 1
#' @param standardisation column of number value for standardisation (y = y / standardisation)
#' @param ylog True or False
#' @param xlog True or False
#' @param loq LOQ value
#' @param filter main filter
#' @param title title
#' @param subtitle subtitle
#' @param caption caption
#' @param xlabs xlabs
#' @param ylabs ylabs
#' @param collabs col labs
#' @param colmanual col selection
#' @param shapelabs ylabs
#' @param shapemanual shape selection
#' @param ltylabs ylabs
#' @param ltymanual ylabs
#' @param output output
#' @param sizetext Sizetext (default 15)
#' @param removeNA If True, automatically remove all NA values
#' @param bkgrd If True, activate the "background" system
#' @param bkgrdalpha Alpha for background, between 0 and 1
#' @param addlines Addline table (for ShinyApp)
#' @param group_split To create a list of plot
#' @param filter2 Filter2 (after fixing background)
#' @param plotly True to activate plotly (dynamic plot)
#' @param workwithexpr True to use expression (console), false to use character string (ShinyApp)
#'
#' @export

plot_spagh <- function(df, x = "TIME", y = "DV", group = "", col ="", shape = "",lty = "", facetwrap = "", facetgrid ="", group_split = "", facetscale = "free", median = F, point_alpha = 1, line_alpha = 1,  standardisation = "",  ylog = T, xlog = F,   loq = "", filter = "", title= "", subtitle ="", caption = "", xlabs = "", ylabs = "", collabs = "", shapelabs = "", ltylabs = "", colmanual = "", shapemanual = "", ltymanual = "", output = "plot", sizetext = 15, removeNA = T,bkgrd = T, filter2 = "", bkgrdalpha = 0.3, addlines = tibble(type = "none"), plotly = F, workwithexpr = T){


  ### The function has been first coded with charachers input (Shiny app), here is transformation
  # for using the function in consol mode with working for expression (= consol use)
  if(workwithexpr == T){
    x <- deparse(substitute(x))
    y <- deparse(substitute(y))
    col <- deparse(substitute(col))  ; if(col == "\"\"") col <- ""
    facetwrap <- deparse(substitute(facetwrap))  ; if(facetwrap == "\"\"") facetwrap <- ""
    group <- deparse(substitute(group)) ; if(group == "\"\"") group <- ""
    facetgrid <- deparse(substitute(facetgrid))  ; if(facetgrid == "\"\"") facetgrid <- ""
    group_split <- deparse(substitute(group_split))  ; if(group_split == "\"\"") group_split <- ""
    standardisation <- deparse(substitute(standardisation))  ; if(standardisation == "\"\"") standardisation <- ""
    filter2 <- deparse(substitute(filter2)) ; if(filter2 == "\"\"") filter2 <- ""
    filter <- deparse(substitute(filter)) ; if(filter == "\"\"") filter <- ""
    shape <- deparse(substitute(shape)) ; if(shape == "\"\"") shape <- ""
    lty <- deparse(substitute(lty)) ; if(lty == "\"\"") lty <- ""

    }


  ## According the output, handling differently dataframe expression

  if(output == "plot"){
    dataframe_origin <- expr(df)
  }else{

    dataframe_origin <- substitute(df)
  }


  dataframe <- expr(!!dataframe_origin)

  ## Apply first filter

  if(filter != ""){

    dataframe <- expr(

      !!dataframe %>%
        filter(!!parse_expr(filter)))

  }

  ## Remove all NA data

  if(removeNA == T){

    dataframe <- expr(!!dataframe %>% filter(!is.na(!!parse_expr(y))))

  }


  ## In case of standardisation (ex: by dose)

  if(standardisation != ""){


    dataframe <- expr(!!dataframe %>%
                        mutate(!!parse_expr(y) := !!parse_expr(y) / !!parse_expr(standardisation)))

  }



  ## handle loq

  if(loq != "" & loq >= 0){

    dataframe <- expr(!!dataframe %>%
                        mutate(!!parse_expr(y) := if_else(!!parse_expr(y) <= !!loq, !!loq, !!parse_expr(y) )) %>%
                        mutate(BLQ := if_else(!!parse_expr(y) <= !!loq, "Yes", "No")))

  }


  if((loq != "" & loq >= 0) | median ==T | bkgrd == T){

    dataframe <- expr(!!dataframe %>%
    {explo_post_treatment <<- .}
    )


  }

  if(filter2 != ""){


    dataframe <- expr(!!dataframe %>%
                        filter(!!parse_expr(filter2)))

  }

  #C O V A R I A T E       I N T E G R A T I O N  ##

  ### dot handling
  aespoint_list <- list()

  if(loq != "" & loq >=0) aespoint_list$shape <- expr(BLQ)
  if(col != "") aespoint_list$col <- expr(factor(!!parse_expr(col)))
  if(shape != "") aespoint_list$shape <- expr(factor(!!parse_expr(shape)))

  aespoint <- expr(aes(x = !!parse_expr(x), y = !!parse_expr(y),!!!aespoint_list))


  ### line handling
  aespoint_list <- list()
  if(group != "") aespoint_list$group <- expr(!!parse_expr(group))
  if(col != "") aespoint_list$col <- expr(factor(!!parse_expr(col)))
  if(lty != "") aespoint_list$lty <- expr(factor(!!parse_expr(lty)))

  aesline <- expr(aes(x = !!parse_expr(x), y = !!parse_expr(y),!!!aespoint_list))



  plot <-  expr(ggplot())




  ### bckground if needed

  if(bkgrd == T){

    test_group <- list()
    if(group != "")  test_group$group <- expr(!!parse_expr(group))
    if(col != "")  test_group$col <-  expr(!!parse_expr(col))
    if(lty != "")  test_group$lty <-  expr(!!parse_expr(lty))
    if(shape != "")  test_group$shape <-  expr(!!parse_expr(shape))

    if(length(test_group) > 0){

    databck = expr(explo_post_treatment %>% mutate(peccaryTemp = paste0(!!!test_group)))

    }else{

      databck = expr(explo_post_treatment )

    }

    if(facetwrap != "") databck <- expr(!!databck %>% select(-!!parse_expr(facetwrap)))

    # expr( mutate(peccaryTemp = paste0(!!!test_group)))

    if(length(test_group) > 0){


      plot <-  expr(!!plot +
                      geom_line(data = !!databck, aes(x = !!parse_expr(x), y = !!parse_expr(y), group = peccaryTemp), col = "darkgrey", alpha = !!parse_expr(as.character(bkgrdalpha))))

    }else{

        plot <-  expr(!!plot +
                        geom_line(data = !!databck, aes(x = !!parse_expr(x), y = !!parse_expr(y), !!!test_group), col = "darkgrey", alpha = !!parse_expr(as.character(bkgrdalpha))))


    }

  }


  ####### Median if needed


  if(median == T){



    groupbylist <- list(expr(!!parse_expr(x)))


    if(col != "" & col != group) groupbylist[[2]] <- expr(!!parse_expr(col))

    if(facetwrap != "") groupbylist[[3]] <- expr(!!parse_expr(facetwrap))

    if(facetgrid != "") groupbylist[[4]] <- expr(!!parse_expr(facetgrid))

    if(col == group & group != "") groupbylist[[5]] <- expr(!!parse_expr(group))




    covs <- expr(explo_post_treatment %>%
                   group_by(!!!groupbylist) %>%
                   summarise(!!!list(median = expr(median(!!parse_expr(y), na.rm = T)))))


    if(col == ""){

      plot <- expr(!!plot +
                     geom_line(data = !!covs, mapping = aes(x = !!parse_expr(x), y = median)))

    }else{


      plot <- expr(!!plot +
                     geom_line(data = !!covs %>% filter(!is.na(median)), mapping = aes(x = !!parse_expr(x), y = median,  col = factor(!!parse_expr(col)))))



    }


  }


  labels_plot <- list()

  if(loq !=""  & loq >=0) {


    labels_plot$lty <- expr("")
    labels_plot$shape <-  expr(paste0("BLQ (",   nrow(explo_post_treatment %>% filter(BLQ == "Yes")),")"))


    plot <- expr(!!plot +
                   scale_shape_manual(values = c(19,8)) +
                   geom_hline(aes(yintercept = !!loq, lty = paste0("LOQ\n(", !!loq, ")" )))+
                   scale_linetype_manual(values = 3)
    )

  }



  if( nrow(addlines %>% filter(type == "hline")) == T){

    for(x in 1:nrow(addlines %>% filter(type == "hline"))){

      addlines %>%
        filter(type == "hline") %>%
        slice(x) -> addlinestemp

      plot <- expr(!!plot +  geom_hline(yintercept =  !!parse_expr(addlinestemp$intercept), lty = !!as.character(addlinestemp$lty)))

    }

  }

  if( nrow(addlines %>% filter(type == "vline")) == T){

    for(x in 1:nrow(addlines %>% filter(type == "vline"))){

      addlines %>%
        filter(type == "vline") %>%
        slice(x) -> addlinestemp

      plot <- expr(!!plot +  geom_vline(xintercept =  !!parse_expr(addlinestemp$intercept), lty = !!as.character(addlinestemp$lty)))

    }

  }

  ### Add points

  plot <- expr(!!plot +
                 geom_point(!!aespoint, alpha = !! point_alpha))

  ## Add lines

  if(group != ""){
  plot <- expr(!!plot +
                 geom_line(!!aesline, alpha = !! line_alpha))
  }
  ### Faceting

  if(facetwrap != "" & facetgrid ==""){

    plot <- expr(!!plot+
                   facet_wrap(~!!parse_expr(facetwrap),  labeller = label_both, scales = !!facetscale))


  }else if(facetwrap != "" & facetgrid !=""){

    plot <- expr(!!plot+
                   facet_grid(!!parse_expr(facetgrid)~!!parse_expr(facetwrap),  labeller = label_both, scales = !!facetscale))

  }

  ### Log axis

  if(ylog == T & plotly == F) plot <- expr(!!plot +  scale_y_log10(labels = labels_log, breaks = breaks_log))
  if(ylog == T & plotly == T) plot <- expr(!!plot +  scale_y_log10())

  if(xlog == T & plotly == F) plot <- expr(!!plot +  scale_x_log10(labels = labels_log, breaks = breaks_log))
  if(xlog == T & plotly == T) plot <- expr(!!plot +  scale_x_log10())





  if(col !="") labels_plot$col <- expr(!!col) #plot <- expr(!!plot + labs(col = !!col))
  if(caption != "") labels_plot$caption <- expr(!!caption) # plot <- expr(!!plot + labs(caption = !!parse_expr(caption)))
  if(title != "") labels_plot$title <-    expr(!!title) #   plot <- expr(!!plot + labs(title = !!parse_expr(title)))
  if(subtitle != "") labels_plot$subtitle <-    expr(!!subtitle)
  if(shape != "") labels_plot$shape <-    expr(!!shape)
  if(lty != "") labels_plot$lty <-    expr(!!lty)
  if(xlabs != "") labels_plot$x <-    expr(!!xlabs)
  if(ylabs != "") labels_plot$y <-    expr(!!ylabs)
  if(collabs != "") labels_plot$col <-    expr(!!collabs)
  if(shapelabs != "") labels_plot$shape <-    expr(!!shapelabs)
  if(ltylabs != "") labels_plot$lty <-    expr(!!ltylabs)





  plot <- expr(!!plot +
                 labs(!!!labels_plot))

  # if(as.double(loq) < 0) plot <- expr(!!plot + guides(shape = F, lty = F))

  plot <- expr(!!plot+
                 theme_bw(base_size = !!sizetext)+
                 theme(plot.title = element_text(hjust = 0.5),
                       plot.subtitle = element_text(hjust = 0.5),
                       plot.caption = element_text(hjust = 0, face = "italic", colour = "grey18")))


  if( colmanual != "")plot <- expr(!!plot +
                                     scale_color_manual(values = !!colmanual))

  if( shapemanual != "")plot <- expr(!!plot +
                                     scale_shape_manual(values = !!shapemanual))



  if( ltymanual != "")plot <- expr(!!plot +
                                       scale_linetype_manual(values = !!ltymanual))


  if(group_split == ""){
    deparsea <- paste0(deparse(plot), collapse = "")  %>% reduce(paste0, collapse = "")
    dataframa <-  paste0(deparse(dataframe), collapse = "")  %>% reduce(paste0, collapse = "")
    # plot <- expr(!!dataframe %>%  !!plot)
    plot <-  paste0(dataframa, "%>% ", deparsea) %>%
      parse_expr()

  }else{


    deparsea <- deparse(plot, width.cutoff = 500)  %>% reduce(paste0, collapse = "")
    dataframa <-  paste0(deparse(dataframe), collapse = "")  %>% reduce(paste0, collapse = "")
    plot <-  paste0(dataframa, "%>% group_split(", group_split,") %>% map(~ .x %>% ", deparsea,  " + ggtitle(unique(.x$", group_split, ")))") %>%
      parse_expr()

  }
  # print("here")
  if(output == "plot"){


    if(plotly == F) return(eval(plot))
    if(plotly == T) return(ggplotly(eval(plot)))

  }else if(output == "expr"){

    return(plot)

  }else if(output == "Both"){

    print(plot)
    return(eval(plot))

  }else{


    return(
      deparse(plot) %>%
        paste(collapse = " ") %>%
        gsub(pattern = "%>% *", replacement = "%>%\n") %>%
        gsub(pattern = "\\+ *", replacement = "+\n") %>%
        gsub(pattern = "  *", replacement = " ")

    )
  }


}



# Testing the function ----------------------------------------------------

# mtcars %>%
#   plot_spagh(mpg, disp, cyl, shape = gear, lty = cyl)
# #
# data <- read.table("D:/these/Pecc_test/1_Data/3_final/main_dataset_extended3_20_12_02.data", header = T, sep = ";", na.strings = ".") %>%
#   as_tibble
# #
# data %>%
# plot_spagh(TIME_NEG_A, DV2,group = NMID2,  filter = YTYPE == 1 & TIME_NEG_A < 200, bkgrd = T, shape = SEX2, lty = SEX2, shapemanual = c(8,16))
# #
#
# breaks_log <- 1:10
# labels_log <- as.character(1:10)
# plot_spagh(df = temp5, x = "TIME_NEG_A", y = DV2,col = central_explained, group_split = "YTYPE", output = "sdf", filter = NMID2 != 201  & !is.na(YTYPE),  id = central_explained,  facetwrap = NMID2)
# shinyApp(pecc_ui, pecc_server)
# df <- read.table("file:///D:/these/Pecc_test/1_Data/3_final/main_dataset_extended.data", header = T, sep= ";", na.strings = ".") %>%
# as_tibble
# plot_spagh2( title = "are", df = df,facetwrap = DOSE, x = "TIME", y = DV, id = "NMID2", col = "NMID2", filter = YTYPE == 1, median = F, output = "plot", loq = 0.1)
#
# plot_spagh2( title = "are", df = df,facetwrap = DOSE, x = "TIME", y = DV, id = "NMID2", col = "NMID2", filter = YTYPE == 1, median = F, output = "plot", loq = 0.1)
#
# col =""
# facetwrap = ""
# facetgrid =""
# group_split = ""
# facetscale = "free"
# median = F
# point = 1
# line_alpha = 0.3
# standardisation = ""
# ylog = T
# xlog = F
# loq = ""
# filter = ""
# title= ""
# subtitle =""
# caption = ""
# xlabs = ""
# ylabs = ""
# output = "plot"
# sizetext = 15
# removeNA = T
# bkgrd = F
# bkgrdalpha = 0.3
# addlines = tibble(type = "none")
# idfocus = ""


