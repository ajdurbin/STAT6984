# script for number 4 before including in markdown solution

web <- htmltab::htmltab("https://www.hockey-reference.com/leagues/NHL_2017_games.html",
                        which = 1,
                        colNames = c("Date", "Visitor", "VG", "Home", "HG", "T",
                                     "Att.", "LOG", "Notes"),
                        rm_nodata_cols = FALSE)
key <- read.table(file = "nhlteams.csv", header = TRUE, sep = ",")

# transform VG, HG into numeric for computation later
web$HG <- as.numeric(web$HG)
web$VG <- as.numeric(web$VG)

# this appends home team information to columns, still want visitor stuff
# so need to merge again with different key
total <- merge(x = web, y = key, by.x = "Home", by.y = "Team")
total <- merge(x = total, y = key, by.x = "Visitor", by.y = "Team")
# rename columns since second merge causes .1 for repeated names
colnames(total) <- c("Visitor", "Home", "Date", "VG", "HG", "TYPE",
                     "Att.", "LOG", "Notes", "HABR", "HCONF", "HDIV",
                     "VABR", "VCONF", "VDIV")
# quick double check
head(total)
tail(total)

# transform NA values in type to 'reg'
total$TYPE[is.na(total$TYPE)] <- "REG"

# add new columns for wins, goal diff based on home/away
total <- transform(total, HW = ifelse(HG>VG, 1, 0))
total <- transform(total, VW = ifelse(VG>HG, 1, 0))
# add new columns for goals for, goals against by home/away
total <- transform(total, HGF = 0)
total <- transform(total, IGA = 0)
total <- transform(total, VGF = 0)
total <- transform(total, VGA = 0)



# this is all of the shootout, so need to treat these separately for gf, ga
head(total[total$TYPE == "SO", ])
tail(total[total$TYPE == "SO", ])



total <- transform(total, VGD = -HGD)
