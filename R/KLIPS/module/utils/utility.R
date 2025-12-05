
save_overwrite_rds <- function(data, dir_path, file_name, overwrite = TRUE) {
  assertthat::assert_that(!is.null(data), msg = "[ERROR] data input not found")
  
  if (!dir.exists(here::here(dir_path))) {
    message("[INFO] Creating directory ", dir_path)
    dir.create(here::here(dir_path), recursive = TRUE)
  }
  
  file_path <- file.path(dir_path, file_name)
  
  if (!file.exists(here::here(file_path))) {
    message("[SAVE] Saving data at ", file_path)
  } else if (file.exists(here::here(file_path)) & overwrite == TRUE) {
    message("[SAVE] Overwriting file with data at ", file_path)
  } else {
    stop ("[ERROR] File already exists at current path. Set overwrite = TRUE to proceed")
  }
  
  saveRDS(data, file = file_path)
  
  invisible(NULL)
}


load_or_use <- function(data, dir_path, env = parent.frame()) {
  on.exit(gc())
  data_name <- deparse(substitute(data))
  
  if (!exists(data_name, envir = environment())) {
    file_path <- file.path(dir_path, paste0(data_name, '.rds'))
    
    if (!file.exists(here::here(file_path))) {
      stop(paste("[ERROR] File", file_path, "not found."))
    } else {
      data <- readRDS(here::here(file_path))
      assign(data_name, data, envir = env)
      message("[INFO] Loading ", file_path)
    }
  }
  
  return (data)
}
