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

    dataset2 <- expr(!!dataset2 %>% filte(!!evidcol := 0))

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




# ## Flow chart TAG4: add admin at 0 if not present (and no EVID column )
#   if( eval(dataset) %>% group_split(!!!group) %>% map_dbl( ~ .x[[deparse(timecol)]][[1]]) %>% max != 0){
#
#     dataset2 <- expr(!!dataset2 %>%
#                        group_split(!!!group) %>%
#                        map(function(x){
#
#                          if(x[[!!deparse(timecol)]][[1]] > 0){
#
#                           return( x %>%
#                              slice(1) %>%
#                              mutate(!!obscol := 0, !!timecol := 0, !!evidcol :=1) %>%
#                              bind_rows(x)
#                           )}
#
#                          x
#                        }) %>%
#                        bind_rows()
#
#     )
#
#
#   } ## end automatic adding time = 0
