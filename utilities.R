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

