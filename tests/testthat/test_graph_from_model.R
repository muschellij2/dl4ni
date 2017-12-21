context("graph_from_model")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("graph_from_model works as expected", {

  load_keras()
  
  # Get the dataset
  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  info <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)
  
  info %>% split_train_test_sets()
  
  # Model scheme
  scheme <- DLscheme$new()
  
  scheme$add(width = 7,
             only_convolutionals = FALSE,
             output_width = 3,
             num_features = 3,
             vol_layers_pattern = list( 
               dense(25)),
             vol_dropout = 0.15,
             feature_layers = list(dense(10)),
             feature_dropout = 0.15,
             common_layers = list(
               dense(20)),
             common_dropout = 0.25,
             last_hidden_layers = list(dense(10)),
             optimizer = "adadelta",
             scale = "z",
             scale_y = "none")
  
  scheme$add(memory_limit = "2G")
  
  # Network instatiation
  model <- scheme$instantiate(problem_info = info)
  
  expect_works(g1 <- model$graph())
  expect_works(g2 <- model$graph(mode = "igraph"))
  
  expect_works(g1 %>% plot_graph())
  expect_works(g2 %>% plot_graph())
  expect_warning(g2 %>% plot_graph(interactive = TRUE))
  
})
