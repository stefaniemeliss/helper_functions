# functions for data processing #

# function to determine outliers
is_outlier_iqr <- function(x, show.bounds = F) {
  # compute boundaries
  # +/- 1.5*IQR
  bound_lo <- quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T)
  bound_up <- quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T)
  
  if(show.bounds) return(list("bound_lo" = bound_lo, "bound_up" = bound_up))
  else return(x < bound_lo | x > bound_up)
}

is_outlier_3sd <- function(x, show.bounds = F) {
  # compute boundaries
  # +/- 3*SD
  bound_lo <- mean(x, na.rm = T) - 3 * sd(x, na.rm = T)
  bound_up <- mean(x, na.rm = T) + 3 * sd(x, na.rm = T)
  
  if(show.bounds) return(list("bound_lo" = bound_lo, "bound_up" = bound_up))
  else return(x < bound_lo | x > bound_up)
}

# rbind all columns
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

# Define function to make split a collapsed column into separate columns
create_element_columns <- function(data, column_name, separator = "|", drop = F) {
  
  # Escape special characters in the separator for splitting
  special_chars <- c(":", ";", ",", ".", "_", "-", "|", "/")
  escaped_separator <- separator
  for (char in special_chars) {
    escaped_separator <- gsub(char, paste0("\\", char), escaped_separator, fixed = TRUE)
  }
  
  # Split the column values using the provided separator
  elements <- unique(unlist(strsplit(paste(data[[column_name]], collapse = separator), split = escaped_separator)))
  elements <- unique(trimws(elements))  # Remove leading/trailing spaces
  elements <- elements[elements != ""]  # Remove empty elements
  
  # Create columns for each element dynamically
  for (element in elements) {
    col_name <- paste0(column_name, "_", tolower(gsub("[^a-zA-Z0-9]", "_", element)))
    data <- data %>%
      mutate(!!col_name := grepl(element, .data[[column_name]]))
  }
  
  if (drop) data[[column_name]] <- NULL
  
  return(data)
}

calculate_snr <- function(data, window_size) {
  # Calculate the signal using moving average
  signal <- zoo::rollapply(data, width = window_size, FUN = mean, fill = NA, align = "center")
  
  # Calculate the noise
  noise <- data - signal
  
  # Estimate power of signal and noise
  signal_power <- var(na.omit(signal))
  noise_power <- var(na.omit(noise))
  
  # Calculate SNR
  snr <- signal_power / noise_power
  
  return(snr)
}

# Improved SNR function with robust error handling and power calculation
calculate_snr <- function(data, window_size) {
  # Check inputs
  if(!is.numeric(data)) stop("Data must be numeric")
  if(window_size < 1 || window_size > length(data)) 
    stop("Window size must be between 1 and length of data")
  
  # Calculate the signal using moving average
  signal <- zoo::rollapply(data, width = window_size, FUN = mean, 
                           fill = NA, align = "center")
  
  # Calculate the noise
  noise <- data - signal
  
  # Remove NA values
  valid_indices <- !is.na(signal)
  clean_signal <- signal[valid_indices]
  clean_noise <- noise[valid_indices]
  
  # Estimate power of signal and noise
  signal_power <- mean(clean_signal^2)
  noise_power <- mean(clean_noise^2)
  
  # Calculate SNR with safety check
  if(noise_power < .Machine$double.eps) {
    warning("Noise power near zero, returning Inf")
    return(Inf)
  }
  
  snr <- signal_power / noise_power
  
  return(snr)
  
  # Return SNR in decibels for easier interpretation
  snr_db <- 10 * log10(snr)
  # snr_db > 20 - "Very strong trend signal - workforce changes follow highly predictable patterns"
  # snr_db > 10 - "Strong trend signal - workforce changes are largely predictable with minor fluctuations"
  # snr_db >  3 - "Moderate trend signal - workforce shows clear trends with some volatility"
  # snr_db >  0 - "Weak trend signal - workforce changes have trends but significant volatility")
  # else "Very weak trend signal - workforce changes appear largely random or highly volatile")
  
}
