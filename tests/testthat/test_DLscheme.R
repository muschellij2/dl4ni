context("DLscheme")


test_that("DLscheme initializes  as expected", {

  scheme <- DLscheme$new()
  
  expect_is(scheme, "DLscheme")

})

test_that("DLscheme adds attributes", {
  
  scheme <- DLscheme$new()
  scheme$add(A = 1, B = "foo", C = list())
  
  scheme %>% expect_named()
  expect_true(all(c("A", "B", "C") %in% names(scheme)))
  
})

test_that("DLscheme generates a model", {
  
  skip_if_not(length(installed_datasets()) > 0)

  available_datasets <- installed_datasets()
  
  scheme <- DLscheme$new()
  scheme$add(add_last_layer = FALSE)
  
  expect_error(model <- scheme$instantiate())
  
  problem_path <- get_dataset(available_datasets[1])
  info <- get_problem_info(problem_path, interactive = FALSE)
  model <- scheme$instantiate(problem_info = info)
  expect_is(model, "DLmodel")
  
})

test_that("DLscheme returns lists", {
  
  scheme <- DLscheme$new()
  scheme$add(A = 1, B = "foo", C = list())
  
  scheme_list <- scheme$to_list()
  
  expect_is(scheme_list, "list")
  scheme_list %>% expect_named()
  expect_true(all(c("A", "B", "C") %in% names(scheme_list)))
  
})

test_that("DLscheme is created from lists", {
  
  scheme <- DLscheme$new()
  scheme_list <- list("A" = 1, num_inputs = 100)
  
  scheme$from_list(scheme_list)
  
  scheme %>% expect_named()
  expect_true("A" %in% names(scheme))
  expect_false("num_inputs" %in% names(scheme))
  
})

test_that("DLscheme is created from a model", {
  
  skip_if_not(length(installed_datasets()) > 0)
  
  available_datasets <- installed_datasets()
  
  scheme <- DLscheme$new()
  scheme$add(add_last_layer = FALSE,
             vol_layers_pattern = list(10))

  problem_path <- get_dataset(available_datasets[1])
  info <- get_problem_info(problem_path, interactive = FALSE)
  model <- scheme$instantiate(problem_info = info)
  
  scheme2 <- DLscheme$new()
  scheme2$from_model(model)
  
  scheme2 %>% expect_named()
  expect_true("vol_layers_pattern" %in% names(scheme2))

})
