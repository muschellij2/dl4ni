#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param name      (character) PARAM_DESCRIPTION, Default: ''
#' @param inputs    (call) PARAM_DESCRIPTION, Default: list()
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.initialize
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.name
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.get_inputs
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.get_outputs
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param output    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.get_model
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param output    (name) PARAM_DESCRIPTION
#' @param with      (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.replace
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param what      (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param inputs    (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param output    (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param subset    (NULL) PARAM_DESCRIPTION, Default: NULL
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.add
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param inputs                (call) PARAM_DESCRIPTION, Default: list()
#' @param desired_outputs       (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param initialize_outputs    (logical) PARAM_DESCRIPTION, Default: TRUE
#' @param mode                  (call) PARAM_DESCRIPTION, Default: c("debug", "faster", "medium", "slower")
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.execute
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param ...    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.run
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param output                     (name) PARAM_DESCRIPTION
#' @param input_filenames            (name) PARAM_DESCRIPTION
#' @param output_filenames           (name) PARAM_DESCRIPTION
#' @param train_split                (numeric) PARAM_DESCRIPTION, Default: 0.75
#' @param epochs                     (numeric) PARAM_DESCRIPTION, Default: 10
#' @param target_windows_per_file    (numeric) PARAM_DESCRIPTION, Default: 1024
#' @param mode                       (call) PARAM_DESCRIPTION, Default: c("debug", "faster", "medium", "slower")
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.train
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.graph
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param interactive    (logical) PARAM_DESCRIPTION, Default: FALSE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.plot
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param outputs    (character) PARAM_DESCRIPTION, Default: 'all'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.reset
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param path           (call) PARAM_DESCRIPTION, Default: tempdir()
#' @param file_prefix    (call) PARAM_DESCRIPTION, Default: self$name()
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.save
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param filename    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.load
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param outputs    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.subset
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param deep    (logical) PARAM_DESCRIPTION, Default: FALSE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' @name DLflow.clone
NULL

