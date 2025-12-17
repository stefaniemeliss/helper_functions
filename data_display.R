#### AMBITION THEME AND PALETTE ###############################################

# Source the project-specific ggplot theme and colour definitions.
# This script defines colours (e.g. coral, teal) and the 'ambition_theme'
# object used across all plots.
source(file = list.files(pattern = "ambition_theme.R",
                         recursive = TRUE,
                         full.names = TRUE))

# Construct colour palettes used in figures.
# - 'bright' palette: main colours with reduced use of cyan.
# - 'accent' palette: highlight colours with reduced use of yellow.
ambition_palette_bright <- c(coral, teal, purple, orange, cyan)   # de-prioritise cyan
ambition_palette_accent <- c(blue, red, yellow)                   # de-prioritise yellow
ambition_palette        <- c(coral, teal, purple, orange,
                             blue, red, cyan, yellow)             # full palette

# Declare dominant and non-dominant colours for use across plots.
# These are used by default in line/point geoms etc.
dominant_col    <- coral
nondominant_col <- navy


#### DESCRIPTIVE TABLES ########################################################

# Create a descriptive statistics table (overall and by group) for inclusion
# in R Markdown outputs. Uses psych::describe and knitr::kable.
table_desc <- function(data = df,
                       group_var = "group",
                       dep_var   = "variable") {
  
  # Combine:
  # 1. Overall descriptives for the dependent variable.
  # 2. Group-specific descriptives, stacked row-wise.
  out <- rbind(
    psych::describe(data[, dep_var]),                                # whole sample
    do.call("rbind",
            psych::describeBy(data[, dep_var],
                              group = data[, group_var]))           # by group
  )
  
  # Remove the 'vars' column (index) from the psych output.
  out$vars <- NULL
  
  # Rename the first row to "all" to indicate whole-sample descriptives.
  rownames(out)[1] <- "all"
  
  # Round statistics to three decimal places for readability.
  out <- round(out, 3)
  
  # Print the table as a nicely formatted HTML/markdown table.
  kbl(
    out,
    caption = paste0(
      "Descriptives of variable '", dep_var,
      "' for whole sample and within each group"
    )
  ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      fixed_thead       = TRUE
    ) %>%
    print()
  
  # Add a blank line after the table in the markdown output.
  cat("\n")
}
