# -------------------------------------------------------------------
# Utility functions for file context and project directories
# -------------------------------------------------------------------

# Extract the stem (base name without extension) of the currently running
# R script or R Markdown document. This is used to programmatically name
# outputs (e.g. logs, figures).
get_file_stem <- function() {
  
  # 1. If running inside an R Markdown document, use knitr's current_input().
  if (!is.null(knitr::current_input())) {
    
    file_path <- knitr::current_input()
    
  } else {
    
    # 2. If running as an R script via Rscript, inspect the command arguments
    #    for a '--file=' entry to get the script's path.
    args <- commandArgs(trailingOnly = FALSE)
    script_path <- sub("--file=", "", args[grep("--file=", args)])
    
    if (length(script_path) > 0) {
      
      file_path <- script_path
      
    } else {
      
      # 3. Fallback for interactive execution in RStudio.
      #    rstudioapi returns the path of the currently open source file.
      file_path <- rstudioapi::getSourceEditorContext()$path
      
      # If the file has never been saved, RStudio returns an empty string.
      # In that case we cannot determine a meaningful file stem, so return NULL.
      if (file_path == "") {
        return(NULL)
      }
    }
  }
  
  # Extract the base name (e.g. "analysis.R") from the full path.
  base_name <- basename(file_path)
  
  # Remove the file extension to get just the stem (e.g. "analysis").
  file_stem <- tools::file_path_sans_ext(base_name)
  
  return(file_stem)
}


# Extract both the file stem and directory of the currently running
# R script / Rmd and assign them to the global environment.
# This is used so that other scripts can refer to 'file_stem' and
# 'file_dir' when constructing output paths.
get_file_info <- function() {
  
  # 1. If running in an R Markdown document, use knitr's current_input().
  if (!is.null(knitr::current_input())) {
    
    file_path <- knitr::current_input()
    
  } else {
    
    # 2. Check whether the script is being run via Rscript, using --file.
    args <- commandArgs(trailingOnly = FALSE)
    script_path <- sub("--file=", "", args[grep("--file=", args)])
    
    if (length(script_path) > 0) {
      
      file_path <- script_path
      
    } else {
      
      # 3. Fallback for interactive execution in RStudio.
      file_path <- rstudioapi::getSourceEditorContext()$path
      
      # If no source file is associated with the session, abort gracefully.
      if (file_path == "") {
        return(NULL)
      }
    }
  }
  
  # Extract the base name and directory of the file.
  base_name <- basename(file_path)
  file_stem <- tools::file_path_sans_ext(base_name)
  file_dir  <- dirname(file_path)
  
  # Export both to the global environment so other scripts can access them.
  assign("file_stem", file_stem, envir = .GlobalEnv)
  assign("file_dir",  file_dir,  envir = .GlobalEnv)
}


# Dynamically identify the project root directory (by searching for a
# specified folder name, default "code") and the subdirectory one level
# below it, and construct the corresponding absolute project path.
# This supports running code from different working directories while
# still resolving a consistent project root.
get_directory <- function(root_dir_name = "code") {
  
  # Get the current working directory as a character string.
  current_dir <- getwd()
  
  # Split the path into its individual components.
  # Example: "/home/user/project/code/analysis"
  # becomes c("", "home", "user", "project", "code", "analysis")
  dir_components <- strsplit(current_dir, "/")[[1]]
  
  # Identify the position of the root directory (e.g. "code") in the path.
  root_index <- which(dir_components == root_dir_name)
  
  # If the specified root directory is not found, stop with a clear error.
  if (length(root_index) == 0) {
    stop(paste("Root directory", root_dir_name, "not found in the current path"))
  }
  
  # Reconstruct the path up to and including the root directory.
  root_dir <- do.call(file.path, as.list(dir_components[1:root_index]))
  
  # Identify the project subfolder directly below the root directory.
  # Example: if root_dir_name = "code", this might be the project name.
  project_folder <- dir_components[root_index + 1]
  
  # Full project directory is root_dir/project_folder.
  dir <- file.path(root_dir, project_folder)
  
  # Export key paths to the global environment so that other scripts
  # can use them without recomputing:
  # - dir_root       : path up to and including 'code'
  # - project_folder : name of the folder immediately under 'code'
  # - dir            : absolute path to the project folder
  assign("dir_root",       root_dir,       envir = .GlobalEnv)
  assign("project_folder", project_folder, envir = .GlobalEnv)
  assign("dir",            dir,            envir = .GlobalEnv)
  
  # Return the project directory invisibly for convenience if used directly.
  invisible(dir)
}
