# Trimming The Fat  -------------------------------------------------------
# Read the original R script
original_code <- readLines("compressing_code_june_25.R")
# Remove blank lines and lines with only whitespace
trimmed_code <- original_code[!grepl("^\\s*$", original_code)]
# Write the cleaned code to a new file
writeLines(trimmed_code, "your_script_trimmed.R")