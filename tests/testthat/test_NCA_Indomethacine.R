test_that("peccary_NCA - Indo - IVbolus with backextrapolation", {


Indo <- as.data.frame(Indometh)
NCA <- peccary_NCA(dataset = Indo, timecol = time, obscol = conc, Subject, outputExpr = F, IVbackextr = T, nIVback = 2); NCA
# NCA$data[[1]]


# compared to: PKanalix (V2019R2)
# to_compare <- read.table("file:///D:/PeccAnalysis/tests/dataset_fo_test/Indomethacin_NCA_Monolix_Dose25.txt", sep = ",", header = T, skip = 1) %>% as_tibble

to_compare <- read.table("../dataset_fo_test/Indomethacin_NCA_Monolix_Dose25.txt", sep = ",", header = T, skip = 1) %>% as_tibble

NCA %>%
  mutate(id = as.double(as.character(Subject))) %>%
  select(id, Cmax, Cmin, AUCTlast, AUCTlastlog, C0) %>%
  left_join(

    to_compare %>%
      select(id, AUCLST, CMAX,C0) %>%
      rename(C0ref = C0)

    ) %>%
  mutate(testAUCTlast = abs(AUCTlast - AUCLST),
         testCmax = abs(Cmax - CMAX),
         testC0 = abs(C0 - C0ref )) %>%
  ungroup() %>%
  # select(test1, test2) %>%
  gather("test", "value", testAUCTlast, testCmax, testC0) %>%
  summarise(maxabs = max(abs(value))) -> temp



finaltest <-  temp$maxabs < 0.1
expect_equal( finaltest , TRUE)

})



