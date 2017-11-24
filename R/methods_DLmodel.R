#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param ...    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import tidyverse
#' @import ggvis#'
#' @name DLmodel.initialize
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
#'
#' @name DLmodel.update
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
#'
#' @name DLmodel.summary
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param level      (call) PARAM_DESCRIPTION, Default: c("DEBUG", "INFO", "WARNING", "ERROR")
#' @param message    (character) PARAM_DESCRIPTION, Default: '...'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.log
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param epoch       (numeric) PARAM_DESCRIPTION, Default: 0
#' @param subepoch    (numeric) PARAM_DESCRIPTION, Default: 0
#' @param time        (call) PARAM_DESCRIPTION, Default: Sys.time()
#' @param loss        (logical) PARAM_DESCRIPTION, Default: NA
#' @param val_loss    (logical) PARAM_DESCRIPTION, Default: NA
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.add_to_history
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import ggvis#'
#' @name DLmodel.plot_history
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
#'
#' @name DLmodel.reset_history
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param initialize    (logical) PARAM_DESCRIPTION, Default: TRUE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import googleVis
#' @import rstudioapi#'
#' @name DLmodel.render_history
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
#'
#' @name DLmodel.update_render
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
#'
#' @name DLmodel.get_model
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
#'
#' @name DLmodel.get_width
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param width    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.set_width
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
#'
#' @name DLmodel.get_loss
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param loss    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.set_loss
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
#'
#' @name DLmodel.get_encoder
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
#'
#' @name DLmodel.get_decoder
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
#'
#' @name DLmodel.get_config
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
#'
#' @name DLmodel.get_history
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param level    (call) PARAM_DESCRIPTION, Default: c("DEBUG", "WARNING", "INFO", "ERROR")
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.print_log
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
#'
#' @name DLmodel.errors
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
#'
#' @name DLmodel.warnings
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param filename    (name) PARAM_DESCRIPTION
#' @param level       (call) PARAM_DESCRIPTION, Default: c("DEBUG", "WARNING", "INFO", "ERROR")
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.save_log
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[prettyunits]{pretty_bytes}}
#' @export 
#' @importFrom prettyunits pretty_bytes
#' @import #'
#' @name DLmodel.check_memory
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
#'
#' @name DLmodel.graph
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param to_file    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.plot
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param path      (name) PARAM_DESCRIPTION
#' @param prefix    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.load
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param path       (name) PARAM_DESCRIPTION
#' @param prefix     (name) PARAM_DESCRIPTION
#' @param comment    (character) PARAM_DESCRIPTION, Default: ''
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.save
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param use                        (call) PARAM_DESCRIPTION, Default: c("train", "test")
#' @param x_files                    (name) PARAM_DESCRIPTION
#' @param y_files                    (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param target_windows_per_file    (numeric) PARAM_DESCRIPTION, Default: 1024
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.use_data
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param epochs            (numeric) PARAM_DESCRIPTION, Default: 10
#' @param keep_best         (logical) PARAM_DESCRIPTION, Default: TRUE
#' @param metrics_viewer    (logical) PARAM_DESCRIPTION, Default: FALSE
#' @param ...               (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.fit
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
#'
#' @name DLmodel.reset
NULL

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param V        (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param speed    (call) PARAM_DESCRIPTION, Default: c("faster", "medium", "slower")
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#'
#' @name DLmodel.infer
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
#'
#' @name DLmodel.clone
NULL

