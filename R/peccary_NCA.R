#' Basic Non Compartimental Analysis
#' @author Thibaud Derippe (\email{Thibaud.Derippe@@gmail.com})
#' @description \code{peccary_NCA} compute basic NCA analysis (no extrapolation)
#' @param dataset Dataset to analyze
#' @param timecol The name (unquoted) of the time column. Mandatory.
#' @param obscol  The name  (unquoted) of the observation column. Mandatory.
#' @param ...     All the names (unquoted) of the grouping column (ID, YTYPE, nAdmin, any covariate....).
#' @param evidcol  The name  (unquoted) of the evid column. Optional. Only line with evidcol = 0 will be kept
#' @param BLQcol  The name  (unquoted) of the evid column. Optional. BLQ values are replaced by 0.
#' @param auc_0_X Numeric input. Compute AUC from time 0 to time auc_0_X
#' @param filter  Optional filter to be applied on the dataset prior to NCA computation.
#' @param keepcov Boolean. If true (default), use \code{\link{pecc_search_cov}} to keep automatically all covariates
#' @param plotQC  Boolean. If T, create plots to control the output  (default = F)
#' @param IVbackextr  Boolean. If T, compute C0 based on the first nIVback points.
#' @param nIVback Numeric input. Number of point used for the backextrapolation. Default 2 as in PKAnalix
#' @param outputExpr Boolean. If F (default) returns directly NCA results. If T, return R code instead.
#' @param add_Obs0_Time0 Boolean. If T, add observation 0 at times 0 for all patients. Usefull in case of non intravscular administration(default = F)
#' @examples
# peccary_NCA(Theoph,Time,conc, Subject, auc_0_X = 12, filter = Subject %in% 1:5, plotQC = T) %>%
  # ungroup() %>% slice(1) %>% pull(plot_QC)
#' # To get the Peccary-independant code, add outputExpr = T
# peccary_NCA( outputExpr = T, Theoph,Time,conc, Subject, auc_0_X = 12, filter = Subject %in% 1:5, plotQC = T)
#' @export


# dataset2 = expr(Indometh)
# IVbackextr = T
# timecol = expr(time)
# obscol = expr(conc)
# group = exprs(Subject)
#
# BLQcol = expr(NULL)
# auc_0_X = 24
# filter = expr("")
# keepcov = F
# plotQC = F
# evidcol = expr(NULL)
# outputExpr = T
# add_Obs0_Time0 = F
# IVbackextr = T
# nIVback = 2
#' @examples
peccary_NCA <- function(dataset, timecol , obscol , ... , evidcol = NULL, BLQcol = NULL, auc_0_X = 0, filter = "",  keepcov = T, plotQC = F , outputExpr = F,
                        IVbackextr = F, add_Obs0_Time0 = F,  nIVback = 2){

  ## handling expressions
  timecol <- enexpr(timecol)
  obscol <- enexpr(obscol)
  group <- enexprs(...)
  filter <- enexpr(filter)
  BLQcol <- enexpr(BLQcol)
  evidcol <- enexpr(evidcol)

  if(outputExpr == F){
    dataset2 <- expr(dataset)
  }else{

    dataset2 <- substitute(dataset)
  }


  if(!is.null(evidcol)){

    dataset2 <- expr(!!dataset2 %>% filter(!!evidcol == 0))

  }

  dataset2 <- expr(!!dataset2 %>% mutate(pecc_extra = FALSE))

  if(add_Obs0_Time0 == T){

    dataset2 <-
    expr(!!dataset2 %>%
           bind_rows(!!dataset2 %>% group_by(!!!group) %>% slice(1) %>% mutate(!!timecol := 0, !!obscol := 0, pecc_extra = TRUE)) %>%
           arrange(!!!group, !!timecol))

  }


  if(!is.null(BLQcol)) dataset2 <- expr(!!dataset2 %>%
                                          mutate(!!obscol := if_else(!!BLQcol == 1, 0, !!obscol)))



  ## filter handling

  if(deparse(filter) != "\"\""){

    dataset2 <- expr(!!dataset2 %>%
                       filter(!!filter)
    )

  }



## back extrapolation
if(IVbackextr == T){




  ## Extrapolation of the value at time = 0 if needed
  dataset2 <- expr(!!dataset2 %>%
    group_split(!!!group) %>%
    map(function(x){

lm(log(x[[!!deparse(obscol)]][1:!!nIVback]) ~ x[[!!deparse(timecol)]][1:!!nIVback] )$coefficients[[1]] -> extrapo

      x %>%
        slice(1) %>%
        mutate(!!obscol := exp(extrapo),
               !!timecol := 0, pecc_extra := TRUE) %>%
        bind_rows(x)

}) %>%
  bind_rows()

)
}



# Here we select all the measure we want to do
tocompute <- list(Cmax = expr(max(data[[!!deparse(obscol)]][data$pecc_extra == F], na.rm = T)),
                  Cmin =  expr(min(data[[!!deparse(obscol)]][data$pecc_extra == F], na.rm = T)),
                  AUCTlast   = expr(sum(AUCblocs$AUCbloc, na.rm = T)),
                  AUCTlastlog = expr(sum(AUCblocs$AUCblocloglinear, na.rm = T))
                  ); tocompute

if(IVbackextr == T) tocompute$C0 = expr(data[[!!deparse(obscol)]][[which(data[[!!deparse(timecol)]] == 0)]])


  tocompute$Tmax <- expr(min(data[[!!deparse(timecol)]][data[[!!deparse(obscol)]]==  max(data[[!!deparse(obscol)]][data$pecc_extra == F], na.rm = T ) & data$pecc_extra == F]))
  tocompute$Tfirst = expr(min(data[[!!deparse(timecol)]][data$pecc_extra == F], na.rm = T)[[1]]) # la premiere valeur NON BLQ
  tocompute$Tlast = expr(max(data[[!!deparse(timecol)]][data$pecc_extra == F], na.rm = T)) #





if(auc_0_X != 0){

  tocompute[[paste0("AUC",auc_0_X )]] <- expr(sum(AUCblocs$AUCbloc[AUCblocs[[!!deparse(timecol)]] <= !!auc_0_X], na.rm = T))
  tocompute[[paste0("AUC",auc_0_X, "log" )]] <- expr(sum(AUCblocs$AUCblocloglinear[AUCblocs[[!!deparse(timecol)]] <= !!auc_0_X], na.rm = T))

}



NCAexpr <- expr(
  !!dataset2 %>%
       filter(!is.na(!!obscol)) %>%
       group_by(!!!group) %>%
       nest() %>%
        mutate(AUCblocs = map(data,  function(data){




          # data correspond donc à un patient et à un YTYPE bien déterminé
          data %>%
            mutate(TimePrev = lag(!!timecol)) %>% # au temps précédant
            mutate(ObsPrev = lag(!!obscol)) %>%  # valeure précédante
            slice(-1) %>%  # du coup premiere ligne inutile
            mutate(AUCbloc =   (!!timecol - TimePrev) * (!!obscol +  ObsPrev)/2)  %>%  # pour chaque bloc on calcule le trapèz
            # n calcule le trapèze
          mutate(AUCblocloglinear = pmap_dbl(list(TimePrev, !!timecol , ObsPrev, !!obscol), function(TimePrev,time, ObsPrev, obs){

            if(obs >= ObsPrev | obs ==0){ # If second concentration greater of equal to previous one

              ## Linear trapezoidal method
              (time - TimePrev) * (obs +  ObsPrev)/2

            } else { # If second concentration lower than previous one

              ## Log-linear trapezoidal method
              (time - TimePrev) * (ObsPrev - obs) / (log(ObsPrev/ obs))

            }
          }))

    })) %>%
      # select(-data) %>%
      mutate(outp = map2(AUCblocs, data, function(AUCblocs, data){

      tibble(!!!tocompute)


      })) %>%
      unnest(outp)
)


if(plotQC == T){

  # dataset_NCA <<- dataset_NCA
  NCAexpr <- expr(!!NCAexpr %>%
          mutate(idcomplete = map_chr(!!!group, function(...){paste0(paste0(!!map_chr(group, ~ deparse(.x)), collapse = " - "), " : ", ..., collapse = ", ")})) %>%
          mutate(label = paste0(idcomplete, "        AUCTlast = ", AUCTlast, "      AUCTlastlog = ", AUCTlastlog)) %>%
         mutate(plot_QC = pmap(list(AUCblocs,Cmax, Tmax, label), function(AUCblocs, Cmax, Tmax, label){



           breakscon <-AUCblocs %>%
                  distinct(!!timecol) %>%
                  pull(!!timecol)

           AUCblocs %>%
           ggplot()+
             geom_point(aes(!!timecol, !!obscol))+
             geom_line(aes(!!timecol , !!obscol))+
             scale_y_log10()+
             scale_x_continuous(breaks = breakscon)+
             geom_segment(aes(x = !!timecol , xend = !!timecol, y = 0, yend = !!obscol), alpha = 0.5, lty = 3)+
             geom_text( aes(x = (TimePrev+ !!timecol) / 2 , y = (ObsPrev + !!obscol)/100, label = paste0(signif(AUCbloc), "\n(lin)")))+
             geom_text(aes(x = (TimePrev+ !!timecol) / 2 , y = (ObsPrev + !!obscol)/1000, label = paste0(signif(AUCblocloglinear), "\n(log)")))+
             geom_point( aes(Tmax, Cmax), col = "red")+
              geom_segment( aes(x = Tmax , xend = Tmax, y = 0, yend = Cmax), col ="red")+
             geom_text( aes(Tmax, Cmax * 2, label  = paste0("Cmax = ",signif(Cmax))), col = "red")+
             ggtitle(label)+
             theme_bw()


           })) %>%
           select(-label, -idcomplete)
       )

}

if(keepcov == T){

expr(pecc_search_cov(dataset %>% as_tibble,  !!!group) ) %>% eval %>%
    names -> namescov

  NCAexpr <- expr(!!NCAexpr %>%
         left_join(!!dataset2 %>% distinct(!!!parse_exprs(namescov))))

}



if(outputExpr == T)return( NCAexpr)

  return(eval(NCAexpr))
}



#'
#' @export

peccary_pknca <- function(dataset,Time, conc, Subject, dose = NA, EVID = NA, AUC0_x = 0, computeMedian = T,  route = NA, rate = 0, duration = 0, option = NA, outputExpr = F){


  if(outputExpr == F){
    dataset2 <- expr(dataset)
  }else{

    dataset2 <- substitute(dataset)
  }

  Time <- enexpr(Time)
  conc <- enexpr(conc)
  Subject <- enexpr(Subject)
  Subject_ind <- parse_exprs(str_split(deparse(Subject), pattern = " *\\+ *")[[1]])

  dose <- enexpr(dose)
  EVID <- enexpr(EVID)
  option <- enexpr(option)
  rate <- enexpr(rate)
  duration <- enexpr(duration)
  # Time = Time, conc = DV, Subject = Subject, dose = dose, ADM = EVID

  # handle concentration dataset
  conclist <- list()



# hand evid

if(is.na(EVID)){

    conclist$data <-  expr(!!dataset2)

  }else{

    conclist$data <- expr(!!dataset2 %>%
                            filter(!!EVID == 0))
  }

# handle median

if(computeMedian == T){
  conclist$data <- expr(!!conclist$data  %>%
    group_by(!!!Subject_ind, !!Time) %>%
    summarise(!!conc := median(!!conc))
  )

}




  subject_formula <- Subject_ind[[1]] %>% as.character()
  if(length(Subject) > 1) subject_formula <- paste0(subject_formula, "/", paste0(Subject_ind[-1], collapse = "+"))

  conclist$formula <- expr(!!conc~!!Time|!!parse_expr(subject_formula))

  # handle dose dataset
  # if is na dose, just say dose 0 at time 0


  doselist <- list()

  if(is.na(EVID)){

    doselist$data <- expr(!!dataset2 %>%
                            group_by(!!!Subject_ind) %>%
                            slice(1) %>%
                            mutate(!!Time:=0, !!conc:=0 )
    )

  }else{


    doselist$data <- expr(!!dataset2 %>%
                            filter(!!EVID == 1))



  }


  if(is.na(dose)){

    doselist$data <- expr(!!doselist$data %>% mutate(dosenull = 0))
    dose <- expr(dosenull)
  }


  doselist$formula <- expr( !!dose~!!Time|!!parse_expr(subject_formula))



if(!is.na(route)){

 doselist$route <- expr(!!route)

 if(route == "intravascular" & rate != 0 ) doselist$rate <- expr(!!rate)
 if(route == "intravascular" & duration != 0 ) doselist$duration <- expr(!!duration)
}



  if(is.na(deparse(dose))|deparse(dose) == "") dose <- "." # to create "one-sided (missing left side)"

  # print("beginoption")
  # optionslist <- list()
  #
  # optionslist$auc.method <- expr(!!isolate(input$pknca_auc.method))
  # if(isolate(input$pknca_adj.r.squared.factor) != 0.0001) optionslist$adj.r.squared.factor <- expr(!!isolate(input$pknca_adj.r.squared.factor))
  # if(isolate(input$pknca_max.missing)!= "drop") optionslist$max.missing <- expr(!!isolate(input$pknca_max.missing))
  # if(isolate(input$pknca_conc.na)!= "drop") optionslist$conc.na <- expr(!!isolate(input$pknca_conc.na))
  # if(isolate(input$pknca_first.tmax)!= T) optionslist$first.tmax <- expr(!!isolate(input$pknca_first.tmax))
  # if(isolate(input$pknca_allow.tmax.in.half.life)!= F) optionslist$allow.tmax.in.half.life <- expr(!!isolate(input$pknca_allow.tmax.in.half.life))
  # if(isolate(input$pknca_min.hl.points)!= 3) optionslist$min.hl.points <- expr(!!isolate(input$pknca_min.hl.points))
  # if(isolate(input$pknca_min.span.ratio)!= 2) optionslist$min.span.ratio <- expr(!!isolate(input$pknca_min.span.ratio))
  # if(isolate(input$pknca_max.aucinf.pext)!= 20) optionslist$max.aucinf.pext <- expr(!!isolate(input$pknca_max.aucinf.pext))
  # if(isolate(input$pknca_min.hl.r.squared)!= 0.9) optionslist$min.hl.r.squared <- expr(!!isolate(input$pknca_min.hl.r.squared))
  #

  # gestion what to compute
  if(is.na(AUC0_x)) AUC0_x <- 0
  if( AUC0_x < 0 | !is.numeric(AUC0_x))  AUC0_x <- 0
  if(AUC0_x == 0){

    itvs <- expr(intervals_manual <- data.frame(start = 0, end = Inf, cmax = T, tmax = T, auclast = T,
                                                aucinf.obs = T, cmin = T, tlast = T, vss.last = F, cl.obs = F))

  }else{

    itvs <- expr(intervals_manual <- data.frame(start=0, end=c(!!AUC0_x, Inf),
                                               cmax=c(F, T),
                                               tmax=c(F, T),
                                               auclast=c(T, T),
                                               aucinf.obs=c(F, T),
                                               cmin = c(F, T), tlast = c(F, T),
                                               vss.last = c(F, F), cl.obs  = c(F, F) )
)

  }


  finallist<- list( expr(conc_obj), expr(dose_obj), intervals = expr(intervals_manual))

  if(!is.na(option[[1]])) finallist$options <- expr(!!option)


  NCA_expr <- expr(NCA_eval <- {
    conc_obj <- PKNCAconc(!!!conclist)

    dose_obj <- PKNCAdose(!!!doselist)

    !!itvs

    data_obj_automatic <-PKNCAdata(!!!finallist)

    pk.nca(data_obj_automatic)

  })


  testtry <- try({
    eval(NCA_expr)
  })


  # search cov
 covexpr <-  expr(pecc_search_cov(!!dataset2, !!!Subject_ind, returnExp = T))

  if(AUC0_x == 0){
  Indiv_table_expr <- expr( NCA_eval$result %>%
                              select(!!!Subject_ind,PPTESTCD,  PPORRES) %>%
                              distinct() %>% ## carefull with this distinct!
                              spread(key = PPTESTCD, value = PPORRES) %>%
                              left_join(!!eval(covexpr)))
  }else{


    Indiv_table_expr <- expr(

      left_join(


        NCA_eval$result %>% filter(end != !!AUC0_x) %>% select(!!!Subject_ind, PPTESTCD, PPORRES, end) %>% distinct() %>%
          spread(key = PPTESTCD, value = PPORRES),

        NCA_eval$result %>%
          filter(end == !!AUC0_x) %>%
          select(!!!Subject_ind, PPORRES) %>%
          rename(!!parse_expr(paste0("AUC", AUC0_x)) := PPORRES)

      ) %>%
        left_join(!!eval(covexpr))

    )


  }



if(outputExpr == T){


  return(list(NCA_expr, Indiv_table_expr))

}else if(outputExpr == F){

return(eval(Indiv_table_expr))

}else{

print(NCA_expr)
print(Indiv_table_expr)
return(eval(Indiv_table_expr))

  }




}

#
# dataset <- read.table(file = "D:/Peccary_Annexe/Exemple_demo/DATA//Theoph.txt", header = T, na.strings = ".", sep = ";", dec = ".")
# # outputExpr = F
# # EVID = expr(EVID)
# # conc = expr(DV )
# # Subject = expr(ID)
# # Time = expr(TIME)
# # dose = expr(Dose)
# # route = "IV perf (rate)"
# # ratenumber = 0
# #
# #
# # # with auc0_24
# #
# peccary_pknca(dataset, Time = TIME, conc = DV, Subject = ID, AUC0_x = 24, outputExpr = "both")



#
#
# peccary_pknca(dataset, Time = TIME, conc = DV, Subject = ID, AUC0_x = 0,route = "intravascular",rate = Wt, duration = 0,  outputExpr = "both")
# #
# #
# #
# # # without DOSE
# #
# peccary_pknca(dataset, Time = TIME, conc = DV, Subject = ID)
# #
# # # without EVID
# #
# # peccary_pknca(dataset, Time = TIME, conc = DV, Subject = ID, dose = Dose)
# #
# # # With both
# #
# # peccary_pknca(dataset, Time = TIME, conc = DV, Subject = ID, dose = Dose, EVID = EVID, computeMedian = F)
# #
# # # With rate
# #
# # peccary_pknca(dataset, Time = TIME, conc = DV, Subject = ID, dose = Dose, EVID = EVID,route = "intravascular",rate = 2,
# #              computeMedian = F)
# #
# # # With
# #
# #
# # a <- peccary_pknca(dataset, Time = TIME, conc = DV, Subject = ID, dose = Dose, EVID = EVID )
# #
# # a
# #
# #
#
# dose_obj <- PKNCAdose(data = dataset %>% group_by(ID + cov) %>%
#                         slice(1) %>% mutate(`:=`(TIME, 0), `:=`(DV,
#                                                                 0)) %>% mutate(dosenull = 0), formula = dosenull ~ TIME |
#                         ID/cov)

# peccary_pknca(demoDF, TIME, DV, ID + cov,EVID = EVID, dose = Dose, computeMedian = F,  outputExpr = "both")
