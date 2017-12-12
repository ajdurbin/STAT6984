options(stringsAsFactors = FALSE)
suppressWarnings(library(stringr, quietly = TRUE))


# function declarations ---------------------------------------------------


process_file_types <- function(root = "/Users/alexanderdurbin/Google Drive/vt/STAT-git/asc_durbin/") {
  
  # all files
  my_files <- list.files(path = root, recursive = TRUE, full.names = TRUE)
  # get everything after last / now
  my_files <- str_extract(string = my_files, pattern = "[^/]*$")
  # make lower case
  my_files <- str_to_lower(string = my_files)
  # get extensions
  extensions <- str_extract(string = my_files, pattern = "[^.]*$")
  extensions <- unique(extensions)
  
  # print by types
  cat("\n\nextension | filename")
  for (i in 1:length(extensions)) {
    
    # grab extension
    ext <- extensions[i]
    # grab for those files ending in .ext
    my_pattern <-  paste0("\\b", ext, "\\b$")
    good_files <- str_detect(string = my_files, pattern = my_pattern)
    good_files <- my_files[good_files]
    # remove the extensions
    my_pattern <- paste0("\\.", ext, "$")
    good_files <- str_replace(string = good_files, pattern = my_pattern,
                              replacement = "")
    # print
    cat("\n-----\n")
    cat(ext, "|", paste0(good_files, collapse = ", "))
    
  }
  
  cat("\n-----")
  
}

process_libraries <- function(root = "/Users/alexanderdurbin/Google Drive/vt/STAT-git/asc_durbin/") {
  
  # get all files
  my_files <- list.files(path = root, recursive = TRUE, full.names = FALSE)
  # look for R/Rmd files - is this cheating?
  my_pattern <- paste0("\\.R$")
  r_files <- str_detect(string = my_files, pattern = my_pattern)
  my_pattern <- paste0("\\.Rmd$")
  rmd_files <- str_detect(string = my_files, pattern = my_pattern)
  good_files <- c(my_files[r_files], my_files[rmd_files])
  
  # read all the files and grab the library statements
  for (i in 1:length(good_files)) {
    the_file <- good_files[i]
    contents <- readLines(paste0(root, "/", the_file), warn = FALSE)
    good_lines <- str_detect(string = contents, pattern = "library\\(")
    if (sum(good_lines) == 0) {
      next
    } else{
      good_lines <- contents[good_lines]
    }
    if (exists("libs")) {
      libs <- c(libs, good_lines)
    } else {
      libs <- good_lines
    }
  }
  
  # remove beginning/trailing whitespace
  libs <- str_trim(string = libs, side = "both")
  # locate the library statement and remove  it from the front
  locs <- str_locate(string = libs, pattern = "library\\(")[, 2]
  str_sub(string = libs, start = 1, end = locs) <- ""
  # locate end ) or , and remove it from the back
  locs <- str_locate(string = libs, pattern = ",|\\)")[, 2]
  str_sub(string = libs, start = locs, end = -1L) <- ""
  
  # read all the files and grab the require statements
  for (i in 1:length(good_files)) {
    the_file <- good_files[i]
    contents <- readLines(paste0(root, "/", the_file), warn = FALSE)
    good_lines <- str_detect(string = contents, pattern = "require\\(")
    if (sum(good_lines) == 0) {
      next
    } else{
      good_lines <- contents[good_lines]
    }
    if (exists("reqs")) {
      reqs <- c(reqs, good_lines)
    } else {
      reqs <- good_lines
    }
  }
  
  # need to check if reqs still not exit for printing
  if (exists("reqs")) {
    # remove beginning/trailing whitespace
    reqs <- str_trim(string = reqs, side = "both")
    # locate the library statement and remove  it from the front
    locs <- str_locate(string = reqs, pattern = "library\\(")[, 2]
    str_sub(string = reqs, start = 1, end = locs) <- ""
    # locate end ) or , and remove it from the back
    locs <- str_locate(string = reqs, pattern = ",|\\)")[, 2]
    str_sub(string = reqs, start = locs, end = -1L) <- ""
    # combine for printing
    libs <- c(libs, reqs)
    libs <- unique(libs)
    # printing
    cat("\nlibraries | ", paste0(libs, collapse = ", "), "\n-----\n")
  } else {
    libs <- unique(libs)
    # printing
    cat("\nlibraries | ", paste0(libs, collapse = ", "), "\n")
  }
  
}


# run ---------------------------------------------------------------------


process_file_types()
cat("\n\n")
process_libraries()
cat("\n\n")
