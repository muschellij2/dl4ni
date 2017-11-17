#' @title Convert a Character String to Bytes
#'
#' @description This function converts a string in the format "number unit" to the corresponding number of bytes.
#'
#' @param x    (character) The string to convert
#'
#' @return The number of bytes corresponding to the character expression: "1 K" is 1024, "1 M" is 1024 ^ 2, ...
#'
#' @export 
#' 
convert_to_bytes <- function(x) {
  
  # Split the character string into parts
  ptn <- "(\\d*(.\\d+)*)(.*)"
  
  # This is the number part of the string (first)
  num  <- as.numeric(sub(ptn, "\\1", x))
  
  # The units (last)
  unit <- sub(ptn, "\\3", x)             
  unit[unit == ""] <- "1" 
  
  # Unit multipliers
  mult <- c("1" = 1, 
            "K" = 1024, 
            "KB" = 1024, 
            "Kb" = 1024, 
            "M" = 1024 ^ 2, 
            "MB" = 1024 ^ 2, 
            "Mb" = 1024 ^ 2, 
            "G" = 1024 ^ 3,
            "GB" = 1024 ^ 3,
            "Gb" = 1024 ^ 3)
  
  # The conversion
  num * unname(mult[unit])
  
}
