
test_that("peccary_NCA - Pkanalix absorption BLQ", {


  # To generate the code of data below
  # data <- read.table("C:/Users/titi7/lixoft/pkanalix/pkanalix2019R2/demos/1.basic_examples/data/data_BLQ.csv", header = T, sep =",", na.strings = ".") %>%
    # as_tibble
  #
  # pecc_df_to_code(data %>% filter(ID %in% 1:2))


  # code generated with pecc_df_to_code
  data <- tibble(ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                 TIME = c(0, 0.5, 2, 4, 6, 8,  12, 18, 24, 30, 0, 0.5, 2, 4, 6, 8, 12, 18, 24, 30),
                 AMT = c(50L,NA, NA, NA, NA, NA, NA, NA, NA, NA, 50L, NA, NA, NA, NA, NA,  NA, NA, NA, NA),
                 CONC = c(NA, 3.05, 5.92, 4.7, 4.14, 4.65, 2.64,  2, 1.8, 1.8, NA, 6.24, 7.08, 5.82, 4.48, 4.88, 2.85, 2.74, 2.21, 1.8),
                 BLQ = c(NA, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, NA, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
                 STUDY = c(101L, 101L, 101L,101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L, 101L)) %>%
                 filter(is.na(AMT))




  NCA <- peccary_NCA(dataset = data, timecol =  TIME, obscol = CONC, ID, IVbackextr = F, BLQcol = BLQ, outputExpr = F,  add_Obs0_Time0 = T); NCA


  # compared to: PKanalix (V2019R2)
  # to_compare <- read.table("D:/PeccAnalysis/tests/dataset_fo_test/PKanalix_BLQ.txt", sep = ",", header = T, skip = 1) %>% as_tibble
  to_compare <- read.table("../dataset_fo_test/PKanalix_BLQ.txt", sep = ",", header = T, skip = 1) %>% as_tibble

  NCA %>%
    select(ID, Cmax, Cmin, AUCTlast, AUCTlastlog) %>%
    left_join(

      to_compare %>%
        select(id, AUCLST, CMAX) %>%
        rename(ID = id)

    ) %>%
    mutate(testAUCTlast = abs(AUCTlast - AUCLST),
           testCmax = abs(Cmax - CMAX)) %>%
    ungroup() %>%
    # select(test1, test2) %>%
    gather("test", "value", testAUCTlast, testCmax) %>%
    summarise(maxabs = max(abs(value))) -> temp



  finaltest <-  temp$maxabs < 0.1
  expect_equal( finaltest , TRUE)


})







