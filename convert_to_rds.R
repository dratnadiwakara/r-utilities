library(data.table)

convert_to_rds_fread <- function(directory, extension = "csv", sep = ",", encoding = "UTF-8") {
  # Ensure extension starts with a dot
  ext_pattern <- paste0("\\.", extension, "$")
  
  # Find all matching files recursively
  files <- list.files(directory, pattern = ext_pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  
  if (length(files) == 0) {
    message("No files found with extension: ", extension)
    return(invisible(NULL))
  }
  
  for (file_path in files) {
    tryCatch({
      dt <- fread(file_path, sep = sep, encoding = encoding, showProgress = FALSE)
      
      rds_path <- sub(paste0("\\.", extension, "$"), ".rds", file_path, ignore.case = TRUE)
      
      saveRDS(dt, rds_path)
      
      message("Converted: ", basename(file_path), " -> ", basename(rds_path))
    }, error = function(e) {
      warning("Failed to process: ", file_path, "\n", e)
    })
  }
}
