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

# functions for data processing #

# function to determine outliers
is_outlier_iqr <- function(x) {
  # +/- 1.5*IQR
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}

# rbind all columns
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

# functions for data display #

table_desc <- function(data = df, group_var = "group", dep_var = "variable"){
  
  out <- rbind(
    psych::describe(data[, dep_var]), # get descriptives whole sample
    do.call("rbind",psych::describeBy(data[, dep_var], group = data[, group_var])) # get descriptives per group
  )
  # edit output
  out$vars <- NULL
  rownames(out)[1] <- "all"
  out <- round(out, 3)
  # print output
  kbl(out, caption = paste0("Descriptives of variable '", dep_var,"' for whole sample and within each group")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T) %>% 
    print()
  cat("\n")
}

