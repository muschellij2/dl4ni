context("get_dataset")


test_that("get_dataset works as expected", {

  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  
  expect(file.exists(problem_path), "Error in loading dataset.")
  
  # info <- problem_path %>% get_problem_info()
  
  problem <- "foo"
  expect_error(problem %>% get_dataset())

})
