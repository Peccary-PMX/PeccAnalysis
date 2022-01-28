
test_that("peccary_NCA - Pkanalix ivinfusion", {


  # To generate the code of data below
  # data <- read.table("C:/Users/titi7/lixoft/pkanalix/pkanalix2019R2/demos/1.basic_examples/data/data_ivinfusion.csv", header = T, sep =",", na.strings = ".") %>%
  #   as_tibble
  #
  # pecc_df_to_code(data %>% filter(ID %in% 1:2))


  # code generated with pecc_df_to_code
  data <- tibble(ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,2L, 2L, 2L, 2L, 2L, 2L),
                 TIME = c(0L, 0L, 3L, 4L, 6L, 8L, 12L, 18L, 24L, 0L, 0L, 3L, 4L, 6L, 8L, 12L, 18L, 24L),
                 CONC = c(0,  NA, 3.54211, 3.15839, 2.27295, 2.02452, 1.73403, 0.90528, 0.65119,  0, NA, 4.08297, 3.95885, 3.21795, 2.49062, 1.68614, 0.78953, 0.37451),
                 DOSE = c(NA, 50L, NA, NA, NA, NA, NA, NA, NA, NA, 50L, NA, NA, NA, NA, NA, NA, NA),
                 TINF = c(NA, 3L, NA, NA, NA, NA,  NA, NA, NA, NA, 3L, NA, NA, NA, NA, NA, NA, NA))




  NCA <- peccary_NCA(dataset = data, timecol =  TIME, obscol = CONC, ID, IVbackextr = F, add_Obs0_Time0 = 0)


  # compared to: PKanalix (V2019R2)
  # to_compare <- read.table("D:/PeccAnalysis/tests/dataset_fo_test/PKanalix_ivinfusion.txt", sep = ",", header = T, skip = 1) %>% as_tibble
  to_compare <- read.table("../dataset_fo_test/PKanalix_ivinfusion.txt", sep = ",", header = T, skip = 1) %>% as_tibble

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







