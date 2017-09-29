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
total <- transform(total, TYPE = ifelse(is.na(TYPE), "REG", TYPE))
total <- transform(total, TYPE = ifelse(is.na(TYPE), "REG", TYPE))

# add new columns for wins based on home/away
total <- transform(total, HW = ifelse(HG>VG, 1, 0))
total <- transform(total, VW = ifelse(VG>HG, 1, 0))

# add new columns for goals for, goals against by home/away
total$HGF <- 0
total$HGA <- 0
total$VGF <- 0
total$VGA <- 0

# nonshootout games
# goals for and against are unchaged
total <- transform(total, HGF = ifelse(TYPE != "SO", HG, 0))
total <- transform(total, HGA = ifelse(TYPE != "SO", VG, 0))
total <- transform(total, VGF = ifelse(TYPE != "SO", VG, 0))
total <- transform(total, VGA = ifelse(TYPE != "SO", HG, 0))

head(total)
tail(total)

# shootout has two cases
# for wins
total <- transform(total, HGF = ifelse(HW == 1 & TYPE == "SO", HG - 1, HGF))
total <- transform(total, HGA = ifelse(HW == 1 & TYPE == "SO", VG, HGA))
total <- transform(total, VGF = ifelse(HW == 1 & TYPE == "SO", VG, VGF))
total <- transform(total, VGA = ifelse(HW == 1 & TYPE == "SO", HG - 1, VGA))

# for losses
total <- transform(total, HGF = ifelse(HW == 0 & TYPE == "SO", HG, HGF))
total <- transform(total, HGA = ifelse(HW == 0 & TYPE == "SO", VG - 1, HGA))
total <- transform(total, VGF = ifelse(HW == 0 & TYPE == "SO", VG - 1, VGF))
total <- transform(total, VGA = ifelse(HW == 0 & TYPE == "SO", HG, VGA))

head(total[total$TYPE == 'SO', ], n = 2)





# scratch work
# head(total[total$TYPE == "SO", ])
# tail(total[total$TYPE == "SO", ])
# total <- transform(total, VGD = -HGD)
# head(total[total$HW == 1 & total$TYPE != "SO",])
# tail(total[total$HW == 1 & total$TYPE != "SO",])
