#' Save processed data to a directory. 
#'
#' 
#' @param  data The data to save.
#' @param  filename The local filename.
#'
#' @return  return 
save_cache <- function(dat, filename, prefix, sep = "_", ext = ".rds") {
  root <- rprojroot::find_root("DESCRIPTION")
  loc <- file.path(root, "output", paste0(prefix, sep, filename, ext))
  cat(loc, "\n")
  saveRDS(dat, loc)
  invisible(loc)
}

#' Save most recent image using ggsave
#' 
#' @param  filename The local file name.
#'
#' @return  return 
save_plot <- function(filename, prefix, sep = "_", ext = ".pdf" ){
  root <- rprojroot::find_root("DESCRIPTION")
  loc <- file.path(root, "figures", paste0(prefix, sep, filename, ext))
  ggsave(loc)
  invisible(loc)
}


#' Load file from data directory. 
#'
#' @param name The data to save.
#' @param prefix Analysis prefix for data
#' @param sep Separator (prefix sep filename)
#' @return  return 
cache_file <- function(filename, prefix, sep = "_", ext = ".rds") {
  root <- rprojroot::find_root("DESCRIPTION")
  file.path(root, "output", paste0(prefix, sep, filename, ext))
}

#' Get list of cached data files 
#'
#' @return  return 
cur_cache <- function() {
  root <- rprojroot::find_root("DESCRIPTION")
  dir(file.path(root, "output"))
}


#' Return the absolue path to a file from data directory. 
#'
#' @param filename The filename
#'
#' @return  return 
data_file <- function(filename, prefix, sep = "_", ext = ".txt") {
  root <- rprojroot::find_root("DESCRIPTION")
  file.path(root, "data", paste0(prefix, sep, filename, ext))
}

# update to system.file("extdata", "name", package = "eegcomplex")
#' Return the absolue path to a file from data directory. 
#'
#' @param filename The filename
#'
#' @return  return 
data_file2 <- function(filename, prefix, sep = "_", ext = ".txt") {
  system.file("data", paste0(prefix, sep, filename, ext), package = "eegcomplex")  
}


