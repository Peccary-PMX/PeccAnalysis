# new_df <- read.table("D:/these/Pecc_test/1_Data/3_final/main_dataset_extended3_20_11_25.data", header = T, stringsAsFactors = F, na.strings = ".", sep = ";") %>%
#   as_tibble()
#
#
# read.table("D:/these/Pecc_test/1_Data/3_final/main_dataset_extended3_final.data", header = T, stringsAsFactors = F, na.strings = ".", sep = ";") %>%
#   as_tibble  -> previous_df
#
#
# YTYPE <- expr(ytype_explained)
# DV <- expr(DV2)
# ID <- expr(NMID2)
# TIME <- expr(TIME_NEG_A)
# listtojoin <- c( "CENTRAL", "YTYPE")

#' @export
pecc_compare_ds <- function(new_df , previous_df, YTYPE, ID, DV, TIME ){


  new_df %>%
    select(!!ID, !!YTYPE, !!DV, !!TIME, !!!parse_exprs(listtojoin)) %>%
    rename( DV_new = !!DV) %>%
    full_join(

      previous_df %>%
        select(!!ID, !!YTYPE, !!DV, !!TIME, !!!parse_exprs(listtojoin)) %>%
        rename( DV_prev = !!DV)

    ) %>%
    filter(DV_new != DV_prev | is.na(DV_new) | is.na(DV_prev)) %>%
    # filter(NMID2 == 224 & YTYPE == 122)
    group_by(!!ID, !!YTYPE,  !!TIME, !!!parse_exprs(listtojoin)) %>%
    nest() %>%
    # slice(1) %>%
    # {.[[1, "data"]]} -> x
    mutate(data2 = map(data, function(x){

      # if only one raw -> no issu
        if(nrow(x) == 1) return(x)

      # otherwise

      new <- x$DV_new[order(x$DV_new)]
      prev <- x$DV_prev[order(x$DV_prev)]
      test <- sum(new != prev)

      if(is.na(test)) return(x)

      if(test== 0){
        return(x %>% slice(0))
      }else{
        return(x)
      }
    })) %>%
    select(-data) %>%
    unnest() %>%
    mutate(modif = case_when(is.na(DV_prev) & !is.na(DV_new) ~ "added",
                             !is.na(DV_prev) & is.na(DV_new) ~ "removed",
                             T ~ "modified"
                             ))



  # remove multiple value with correct values
  temp %>%
    left_join(
  temp %>%
    group_by(!!ID, !!YTYPE,  !!TIME, !!!parse_exprs(listtojoin)) %>%
    tally %>%
    filter(n == 2)
    )

#
#   map(unique(df$ytype_explained), function(a){
#
#
#     ggplot() + ggtitle(a)-> temp
#
#     previoustemp <- previousdf %>% filter(ytype_explained == a)
#
#
#     if(nrow(previoustemp) > 0) temp <- temp +
#       geom_point(data = previoustemp, aes(TIME_NEG_A, DV2, col = "previous"), shape = 3, size = 3)
#
#     newtemp <-df %>% filter(ytype_explained == a)
#
#     if(nrow(newtemp) > 0) temp <- temp +
#       geom_point(data = newtemp, aes(TIME_NEG_A, DV2,  col = "new"),  shape = 16)
#
#
#     if(nrow(previoustemp) > 0 | nrow(newtemp) > 0) temp <- temp +
#       facet_wrap(~NMID2, scales = "free")+
#       # scale_shape_manual(values = c(3,16))+
#       scale_color_manual(values = c("black","red"))
#
#     return(temp)
#   })




}

