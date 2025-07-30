code <- readLines("Dashboard_backup_july_9.R")
# Remove blank or whitespace-only lines
code_no_blank <- code[!grepl("^\\s*$", code)]

writeLines(code_no_blank, "Dashboard_backup_july_9.R")
