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
# make logical of whatever ends with this extension
# then make a vector of extensions and then str_extact the corresponding extension 
# off of the other ones and then concatenate them with a comma for printing
# easy peasy

# library statement searching is a little more complication, but nothing i cant do
