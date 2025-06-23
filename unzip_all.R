unzip_all <- function(directory) {
  # Create a temporary directory for recursive unzipping
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Use a queue to keep track of zip files to unzip
  zip_queue <- list.files(directory, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
  
  while (length(zip_queue) > 0) {
    zip_file <- zip_queue[[1]]
    zip_queue <- zip_queue[-1]  # dequeue
    
    # Define extraction path
    extract_path <- file.path(dirname(zip_file), tools::file_path_sans_ext(basename(zip_file)))
    dir.create(extract_path, showWarnings = FALSE)
    
    # Unzip
    unzip(zip_file, exdir = extract_path)
    
    # Optional: delete the original zip file (uncomment below to enable)
    # file.remove(zip_file)
    
    # Look for new zip files inside the extracted folder
    new_zips <- list.files(extract_path, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
    zip_queue <- c(zip_queue, new_zips)
  }
  
  cat("All zip files (including nested) have been extracted.\n")
}

# Usage
# unzip_all("path/to/your/folder")
