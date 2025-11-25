# function to extract the name of an R/Rmd file
get_file_stem <- function() {
  # Check if running in an R Markdown document
  if (!is.null(knitr::current_input())) {
    file_path <- knitr::current_input()
  } else {
    # Attempt to retrieve the script name from the commandArgs
    args <- commandArgs(trailingOnly = FALSE)
    script_path <- sub("--file=", "", args[grep("--file=", args)])
    if (length(script_path) > 0) {
      file_path <- script_path
    } else {
      # Fallback for interactive execution
      file_path <- rstudioapi::getSourceEditorContext()$path
      if (file_path == "") {
        return(NULL)
      }
    }
  }
  
  # Extract the base name of the file
  base_name <- basename(file_path)
  # Remove the file extension
  file_stem <- tools::file_path_sans_ext(base_name)
  
  return(file_stem)
}

# Function to extract the name and directory of an R/Rmd file
get_file_info <- function() {
  # Check if running in an R Markdown document
  if (!is.null(knitr::current_input())) {
    file_path <- knitr::current_input()
  } else {
    # Attempt to retrieve the script name from the commandArgs
    args <- commandArgs(trailingOnly = FALSE)
    script_path <- sub("--file=", "", args[grep("--file=", args)])
    if (length(script_path) > 0) {
      file_path <- script_path
    } else {
      # Fallback for interactive execution
      file_path <- rstudioapi::getSourceEditorContext()$path
      if (file_path == "") {
        return(NULL)
      }
    }
  }
  
  # Extract the base name and directory of the file
  base_name <- basename(file_path)
  file_stem <- tools::file_path_sans_ext(base_name)
  file_dir <- dirname(file_path)
  
  # Export the base name and directory of the file to the global environment
  assign("file_stem", file_stem, envir = .GlobalEnv)
  assign("file_dir", file_dir, envir = .GlobalEnv)
}

# Function to dynamically identify the root directory and the subdirectory one level below
get_directory <- function(root_dir_name = "code") {
  # Get the current working directory
  current_dir <- getwd()
  
  # Split the current directory into its components
  dir_components <- strsplit(current_dir, "/")[[1]]
  
  # Identify the root directory dynamically based on the provided root directory name
  root_index <- which(dir_components == root_dir_name)
  if (length(root_index) == 0) {
    stop(paste("Root directory", root_dir_name, "not found in the current path"))
  }
  root_dir <- do.call(file.path, as.list(dir_components[1:root_index]))
  
  # Identify the subdirectory one level below the root and construct its absolute path
  project_folder <- dir_components[root_index + 1]
  dir <- file.path(root_dir, project_folder)
  
  # Export the root and subdirectory to the global environment
  assign("dir_root", root_dir, envir = .GlobalEnv)
  assign("project_folder", project_folder, envir = .GlobalEnv)
  assign("dir", dir, envir = .GlobalEnv)
}

