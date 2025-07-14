test_that("getC works", {
  
  ## get data
  input <- mixedfishery_MixME_input
  
  ## check equivalence
  opt1 <- lapply(input$om$flts, "[[", "cod")
  opt2 <- FLFishery::landings.n(input$om$flts) 
  opt3 <- MixME:::getC(input$om$flts, "cod","landings.n")
  
  ## Matches OK
  expect_equal(Reduce("+", lapply(opt1, landings.n)), opt3)
  expect_equal(opt2$cod, opt3)
})

test_that("getC works", {
  
  ## get data
  input <- mixedfishery_MixME_input
  
  ## check equivalence
  opt1 <- FLFishery::landings(input$om$flts, by = "catch")$cod 
  opt2 <- MixME:::getCW(input$om$flts, "cod","landings")
  
  ## Matches OK
  expect_equal(opt1, opt2)
})