addImages <- function(A, B) {A + B}
subtractImages <- function(A, B) {A - B}
multiplyImages <- function(A, B) {A * B}

flow <- create_flow(name = "demo_flow", 
                    inputs = list("T1", "FLAIR"))

flow %>% add_process(proc = addImages, 
                     inputs = c("T1", "FLAIR"),
                     output = "T1_FLAIR")

flow %>% add_process(proc = subtractImages,
                     inputs = c("T1_FLAIR", "T1"), 
                     output = "only_FLAIR")

flow %>% add_process(proc = subtractImages,
                     inputs = c("T1_FLAIR", "FLAIR"), 
                     output = "only_T1")

flow %>% add_process(proc = multiplyImages,
                     inputs = c("only_T1", "only_FLAIR"), 
                     output = "T1xFLAIR")

flow %>% add_inputs(inputs = list("T1c"))

flow %>% add_process(proc = subtractImages,
                     inputs = c("T1xFLAIR", "T1c"), 
                     output = "final_output")


flow %>% plot_flow()

info <- get_problem_info("brats_lgg", num_subjects = 1)

results <- flow %>% execute_flow(inputs = list("T1" = info$inputs$T1[1], 
                                               "FLAIR" = info$inputs$FLAIR[1], 
                                               "T1c" = info$inputs$T1c[1]), 
                                 desired_outputs = c("only_FLAIR", 
                                                     "T1xFLAIR", 
                                                     "only_T1", 
                                                     "final_output"))
