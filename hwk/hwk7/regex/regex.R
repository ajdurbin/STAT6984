# regex 
rm(list = ls())
options(stringsAsFactors = FALSE)
library(stringr)

# top of asc directory
root <- "/Users/alexanderdurbin/Google Drive/vt/STAT-git/asc_durbin/"
# all files
my_files <- list.files(path = root, recursive = TRUE, full.names = FALSE)
extensions <- str_extract(string = my_files, pattern = "[^.]*$")
extensions <- unique(extensions)
