library(stringr)

# Function to process a single HTML file
process_html <- function(file_path) {
  # Read the HTML file
  html_content <- readLines(file_path, warn = FALSE)
  html_content <- paste(html_content, collapse = "\n")
  
  # Pattern to match <a href="URL"><img...base64...></a>
  pattern <- '<a href="([^"]+)"><img[^>]+src="data:image/[^>]+></a>'
  
  # Replace with just <img src="URL">
  processed_content <- str_replace_all(html_content, pattern, '<img src="\\1">')
  
  # Write back to file
  writeLines(processed_content, file_path)
  
  cat("Processed:", file_path, "\n")
}

# Find all HTML files with "TN" in the name in the current directory and subdirectories
html_files <- list.files(
  path = "~/R/newsletter/2024/", 
  pattern = ".*TN.*\\.html$", 
  recursive = TRUE,
  full.names = TRUE
)

html_files <- html_files[31:37]

# Process each file
if(length(html_files) > 0) {
  lapply(html_files, process_html)
  cat("\nProcessed", length(html_files), "files\n")
} else {
  cat("No matching HTML files found\n")
}
`