# script for number 4 before including in markdown solution

web <- htmltab::htmltab("https://www.hockey-reference.com/leagues/NHL_2017_games.html",
                        colNames = c("Date", "Visitor", "VG", "Home", "HG", "T",
                                     "Att.", "LOG", "Notes"),
                        rm_nodata_cols = FALSE)
key <- read.table(file = "nhlteams.csv", header = TRUE, sep = ",")

# this appends home team information to columns, still want visitor stuff
total <- merge(x = web, y = key, by.x = "Home", by.y = "Team")
total <- merge(x = total, y = key, by.x = "Visitor", by.y = "Team")
colnames(total) <- c("Visitor", "Home", "Date", "VG", "HG", "T",
                     "Att.", "LOG", "Notes", "HABR", "HCONF", "HDIV",
                     "VABR", "VCONF", "VDIV")
head(total)
