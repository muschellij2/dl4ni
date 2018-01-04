map_images <- function(source, target) {
 
  if (!require("histmatch")) {
    
    devtools::install_github("krlmlr/histmatch")
    require(histmatch)
    
  }
  
  transformed <- histmatch(source, target)
  dim(transformed) <- dim(source)
  
  return(transformed)
   
}
