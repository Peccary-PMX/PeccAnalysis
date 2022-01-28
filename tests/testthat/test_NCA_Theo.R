test_that("peccary_NCA - Theophyline  - IVbolus without backextrapolation", {



  Theoph <- as.data.frame(Theoph) %>% as_tibble()

  NCA <- peccary_NCA(dataset = Theoph, timecol = Time, obscol = conc, Subject)

  # NCA

  # source: https://asancpt.github.io/pkr/reference/NCA.html#examples; also confirmed in PKanalix
  to_compare <- tribble(~ Subject, ~CmaxRef, ~ AUCTlastRef,
                        1, 10.5, 148.92305,
                        2, 8.33, 91.52680,
                        3, 8.20, 99.28650,
                        4, 8.60, 106.79630,
                        5, 11.40, 121.29440,
                        6, 6.44, 73.77555)




  NCA  %>%
    mutate(Subject = as.double(as.character(Subject))) %>%
    filter(Subject < 7) %>%
    left_join(to_compare) %>%
    mutate(test1 = AUCTlast - AUCTlastRef) %>%
    mutate(test2 = Cmax - CmaxRef) %>%
    ungroup() %>%
    select(test1, test2) %>%
    gather("test", "value", test1, test2) %>%
    summarise(maxabs = max(abs(value))) -> temp

  finaltest <-  temp$maxabs < 0.1
  expect_equal( finaltest , TRUE)

})



