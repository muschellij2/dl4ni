get_dataset_dir <- function() dl4ni_globals$dataset_cache_dir

set_dataset_dir <- function(dir) dl4ni_globals$dataset_cache_dir <- suppressWarnings(normalizePath(dir))

get_dataset <- function(dataset, 
                        hosts = c("github", "bitbucket"),
                        force_download = FALSE) {
  
  # Where the dataset is stored  
  repo_name <- paste0("neuroimaginador/dl4ni.", dataset)
  destination_folder <- file.path(get_dataset_dir(), dataset)
  
  # What happens when the output directory exists?
  if (dir.exists(destination_folder)) {
    
    if (force_download) {
      
      unlink(destination_folder, recursive = TRUE)
      message("Deleting previous folder.")
      
    } else {
      
      return(destination_folder)
      
    }
    
  }
  
  # Let's check possible hosts
  for (host in hosts) {
    
    if (host == "github") {
      
      remote <- devtools:::github_remote(repo_name)
      
      path <- try(devtools:::remote_download(remote), silent = TRUE)
      
      if (!inherits(path, "try-error")) {
        
        break
        
      }
      
    }
    
    if (host == "bitbucket") {
      
      remote <- devtools:::bitbucket_remote(repo_name)
      
      path <- try(devtools:::remote_download(remote), silent = TRUE)
      
      if (!inherits(path, "try-error")) {
        
        break
        
      }
      
    }
    
  }
  
  if (!inherits(path, "try-error")) {
    
    # Ok, unzip the dataset and create destination folder if it does not exist.
    dir.create(get_dataset_dir(), recursive = TRUE, showWarnings = FALSE)
    L <- unzip(zipfile = path, exdir = get_dataset_dir(), list = TRUE)
    unzip(zipfile = path, exdir = get_dataset_dir())
    extracted_folder <- L$Name[1]
    
    file.rename(file.path(get_dataset_dir(), extracted_folder), to = destination_folder)
    
    cat("Dataset ", paste0("'", dataset, "'"), " downloaded to ", destination_folder, "\n")
    return(invisible(destination_folder))
    
  } else {
    
    stop("Dataset not found in repositories.")
    
  }
  
}

installed_datasets <- function() {
  
  list.dirs(get_dataset_dir(), recursive = FALSE, full.names = FALSE)
  
}
