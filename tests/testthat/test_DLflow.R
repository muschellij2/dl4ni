context("DLflow")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("DLflow initializes as expected", {

  # Create flow
  flow <- DLflow$new(name = "foo", inputs = c("A", "B"))

  # Expectations
  # Class
  expect_is(flow, "DLflow")

  # Correct name
  expect_true(flow$name() == "foo")

  # Retrieving inputs
  expect_identical(sort(flow$get_inputs()), c("A", "B"))

  # No outputs at the beginning
  expect_identical(setdiff(flow$get_outputs(), flow$get_inputs()), character(0))

})

test_that("DLflow adds inputs", {

  # Create flow
  flow <- DLflow$new(name = "foo", inputs = c("A", "B"))
  original_inputs <- flow$get_inputs()

  # Add a new input
  flow$add(inputs = "C")

  # Expect we have the new input added
  expect_identical(setdiff(flow$get_inputs(), original_inputs), "C")

})

test_that("DLflow adds schemes as output", {

  # Create the flow
  flow <- DLflow$new(name = "foo", inputs = c("A", "B"))

  # Create simplest scheme
  scheme <- DLscheme$new()
  scheme$add(add_last_layer = FALSE)

  # To add an scheme, we need at least inputs and outputs
  expect_error(flow$add(what = scheme))
  expect_error(flow$add(what = scheme, inputs = "A"))

  # This does not work because "C" is not an input of the flow.
  expect_error(flow$add(what = scheme, inputs = c("A", "C"), output = "new_output"))

  # This should work
  flow$add(what = scheme, inputs = c("A", "B"), output = "new_output")

  # Expectations
  # We have added an output that does not correspond to an input
  expect_identical(setdiff(flow$get_outputs(), flow$get_inputs()), "new_output")

  # We can retrieve the scheme
  scheme2 <- flow$get_model("new_output")
  expect_is(scheme2, "DLscheme")

})

test_that("DLflow adds models as output", {

  skip_if_not(length(installed_datasets()) > 0)

  # Create flow
  flow <- DLflow$new(name = "foo", inputs = c("A", "B"))

  # Create simplest scheme to build a model
  scheme <- DLscheme$new()
  scheme$add(add_last_layer = FALSE)

  available_datasets <- installed_datasets()

  problem_path <- get_dataset(available_datasets[1])
  info <- get_problem_info(problem_path, interactive = FALSE)
  model <- scheme$instantiate(problem_info = info)

  # To add a model, we need at least inputs and outputs
  expect_error(flow$add(what = model))
  expect_error(flow$add(what = model, inputs = "A"))

  # "C" is not an input
  expect_error(flow$add(what = model, inputs = c("A", "C"), output = "new_output"))

  # This should work
  flow$add(what = model, inputs = c("A", "B"), output = "new_output")

  # Expectations
  # New output is added to the flow
  expect_identical(setdiff(flow$get_outputs(), flow$get_inputs()), "new_output")

  # We can retrieve the model
  model2 <- flow$get_model("new_output")
  expect_is(model2, "DLmodel")

})

test_that("A DLflow can do basic operations", {

  # Model scheme
  scheme <- DLscheme$new()

  scheme$add(width = 7,
             only_convolutionals = FALSE,
             output_width = 3,
             num_features = 3,
             vol_layers_pattern = list(dense(25)),
             vol_dropout = 0.15,
             feature_layers = list(dense(10)),
             feature_dropout = 0.15,
             common_layers = list(dense(20)),
             common_dropout = 0.25,
             last_hidden_layers = list(dense(10)),
             optimizer = "adadelta",
             scale = "z",
             scale_y = "none")

  scheme$add(memory_limit = "2G")

  # Create new flow
  flow <- DLflow$new(name = "brain_extraction", inputs = c("T1"))

  # Scale the T1 image
  flow$add(what = scale_z,
           inputs = list("T1"),
           output = "T1_scaled")

  # Starting from a T1, add a trainable model which computes the brain_mask
  flow$add(what = scheme,
           inputs = list("T1_scaled"),
           output = "brain_mask")

  # To compute the brain extracted image, we multiply the T1 and the brain_mask
  flow$add(what = function(T1, brain_mask) {T1 * brain_mask},
           output = "only_brain")

  # Plot
  expect_works(flow$plot())
  expect_warning(flow$plot(interactive = TRUE))

  # Subset
  expect_works(new_flow <- flow$subset(outputs = "brain_mask"))
  expect_works(new_flow$plot())

  # Reset outputs
  expect_works(new_flow$reset(outputs = "brain_mask"))


  # Save and load
  path <- tempdir()
  file_prefix <- basename(tempfile())
  expect_works(new_flow$save(path = path, file_prefix = file_prefix))

  expect_works(new_flow$load(filename = file.path(path, paste0(file_prefix, ".zip"))))

})

test_that("A DLflow works for a fully-connected model", {

  # We'll use a modified BET (non-convolutional) demo
  load_keras()

  # Get the dataset
  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  info_bet <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)

  info_bet %>% split_train_test_sets()

  # Model scheme
  scheme <- DLscheme$new()

  scheme$add(width = 7,
             only_convolutionals = FALSE,
             output_width = 3,
             num_features = 3,
             vol_layers_pattern = list(dense(25)),
             vol_dropout = 0.15,
             feature_layers = list(dense(10)),
             feature_dropout = 0.15,
             common_layers = list(dense(20)),
             common_dropout = 0.25,
             last_hidden_layers = list(dense(10)),
             optimizer = "adadelta",
             scale = "z",
             scale_y = "none")

  scheme$add(memory_limit = "1G")

  # Create new flow
  flow <- DLflow$new(name = "brain_extraction", inputs = c("T1"))

  # Scale the T1 image
  flow$add(what = scale_z,
           inputs = list("T1"),
           output = "T1_scaled")

  # Starting from a T1, add a trainable model which computes the brain_mask
  flow$add(what = scheme,
           inputs = list("T1_scaled"),
           output = "brain_mask")

  # To compute the brain extracted image, we multiply the T1 and the brain_mask
  flow$add(what = function(T1, brain_mask) {T1 * brain_mask},
           output = "only_brain")

  # Train BET
  expect_works(flow$train(output = "brain_mask",
                          input_filenames = info_bet$inputs,
                          output_filenames = info_bet$outputs,
                          epochs = 1))

  expect_works(flow$save(path = getwd(), file_prefix = "test"))

  test_index <- sample(info_bet$test$subject_indices, size = 1)

  # Starting from original image
  file <- info_bet$inputs$T1[1]
  expect_works(result <- flow$execute(inputs = list(T1 = file),
                                      desired_outputs = c("only_brain")) )

  expect_named(result, expected = c("only_brain"))

})

test_that("DLflow trains correctly", {

  load_keras()

  # Basic scheme for all networks in the flow
  scheme_bigger <- DLscheme$new()

  scheme_bigger$add(width = 7,
                    output_width = 3,
                    num_features = 3,
                    vol_layers_pattern = list(dense(10)),
                    vol_dropout = 0.1,
                    feature_layers = list(dense(10)),
                    feature_dropout = 0.15,
                    common_layers = list(dense(10)),
                    common_dropout = 0.1,
                    last_hidden_layers = list(dense(2)),
                    optimizer = "adadelta",
                    scale = "none",
                    scale_y = "none")

  scheme_bigger$add(memory_limit = "1G")

  # Create new flow
  flow <- DLflow$new(name = "parcellation", inputs = c("T1"))

  # Scale the T1 image
  flow$add(what = scale_z,
           inputs = list("T1"),
           output = "T1_scaled")

  # Starting from a T1, add a trainable model which computes the brain_mask
  flow$add(what = scheme_bigger,
           inputs = list("T1_scaled"),
           output = "brain_mask")

  # To compute the brain extracted image, we multiply the T1 and the brain_mask
  flow$add(what = function(T1, brain_mask) {T1 * brain_mask},
           output = "only_brain")

  # Scale the brain extracted image
  flow$add(what = scale_z,
           inputs = list("only_brain"),
           output = "only_brain_scaled")

  # Starting form the brain extracted image ("only_brain"), add a trainable model which computes the
  # segmentation
  flow$add(what = scheme_bigger,
           inputs = list("only_brain_scaled"),
           output = "segmentation")

  # Using brain extracted and scaled image ("only_brain_scaled") and the segmentation, add a trainable model
  # to compute the parcellation
  cortex <- c(6, 45, 630:3000)
  scgm_labels <- c(10, 11, 12, 13, 17, 18, 49:54)
  spinal_cord_labels <- 16
  ventricles_labels <- c(4, 5, 14, 15, 24, 43, 44, 72)
  flow$add(what = scheme_bigger,
           inputs = list("only_brain_scaled", "segmentation"),
           output = "parcellation",
           subset = list(subset_classes = scgm_labels,
                         unify_classes = list(cortex,
                                              spinal_cord_labels,
                                              ventricles_labels)))

  # Let's train the different models:
  # Define the training sets:

  # First, the brain_mask
  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  info_bet <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)

  # Now, segmentation
  problem <- "segmentation"
  problem_path <- problem %>% get_dataset()
  info_seg <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)

  # To end, parcellation
  problem <- "parcellation"
  problem_path <- problem %>% get_dataset()
  info_parc <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)

  # This should fail. Not all previous processes are trained.
  # Train parcellation
  expect_error(flow$train(output = "parcellation",
                          input_filenames = list("only_brain" = info_parc$inputs$T1),
                          output_filenames = info_parc$outputs,
                          epochs = 1,
                          verbose = FALSE))

  # This should work.
  # Train segmentation
  expect_works(flow$train(output = "segmentation",
                          input_filenames = list("only_brain" = info_seg$inputs$T1),
                          output_filenames = info_seg$outputs,
                          epochs = 1,
                          verbose = FALSE))

  path <- tempdir()
  file_prefix <- basename(tempfile())
  expect_works(flow$save(path = path, file_prefix = file_prefix))
  expect_works(flow$load(file.path(path, paste0(file_prefix, ".zip"))))

  # Test the inference
  # Starting from betted image
  file <- info_seg$inputs$T1[1]
  expect_error(result <- flow$execute(inputs = list(only_brain = file),
                                      desired_outputs = c("segmentation", "parcellation")))

  expect_works(result <- flow$execute(inputs = list(only_brain = file),
                                      desired_outputs = c("segmentation")))

  expect_named(result, expected = c("segmentation"))
  expect_works(original_image <- read_nifti_to_array(file))
  expect_works(ortho_plot(x = original_image,
                          interactiveness = FALSE,
                          text = "Original Image"))

  col.y <- scales::alpha(colour = scales::viridis_pal()(3), alpha = 0.25)

  expect_works(ortho_plot(x = original_image,
                          y = result[["segmentation"]],
                          col.y = col.y,
                          interactiveness = FALSE,
                          text = paste0("Predicted: segmentation")))


})
