suppressWarnings(suppressMessages(
  library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)))

options(stringsAsFactors = FALSE)

# function declarations ---------------------------------------------------

# combine all games data and append new columns 
combo_all <- function(web, key){
  
  # transform VG, HG into numeric for computation later
  web$HG <- as.numeric(web$HG)
  web$VG <- as.numeric(web$VG)
  
  # this appends home team information to columns, still want visitor stuff
  # so need to merge again with different key
  all_data <- merge(x = web, y = key, by.x = "Home", by.y = "Team")
  all_data <- merge(x = all_data, y = key, by.x = "Visitor", by.y = "Team")
  # rename columns since second merge causes .1 for repeated names
  colnames(all_data) <- c("Visitor", "Home", "Date", "VG", "HG", "TYPE",
                       "Att.", "LOG", "Notes", "HABR", "HCONF", "HDIV",
                       "VABR", "VCONF", "VDIV")
  
  # transform NA values in type to 'reg'
  all_data <- transform(all_data, TYPE = ifelse(is.na(TYPE), "REG", TYPE))
  all_data <- transform(all_data, TYPE = ifelse(is.na(TYPE), "REG", TYPE))
  
  # add new columns for wins based on home/away
  all_data <- transform(all_data, HW = ifelse(HG>VG, 1, 0))
  all_data <- transform(all_data, VW = ifelse(VG>HG, 1, 0))
  
  # add new columns for goals for, goals against by home/away
  all_data$HGF <- 0
  all_data$HGA <- 0
  all_data$VGF <- 0
  all_data$VGA <- 0
  
  # nonshootout games
  # goals for and against are unchaged
  all_data <- transform(all_data, HGF = ifelse(TYPE != "SO", HG, 0))
  all_data <- transform(all_data, HGA = ifelse(TYPE != "SO", VG, 0))
  all_data <- transform(all_data, VGF = ifelse(TYPE != "SO", VG, 0))
  all_data <- transform(all_data, VGA = ifelse(TYPE != "SO", HG, 0))
  
  # shootout has two cases
  # for wins
  all_data <- transform(all_data, HGF = ifelse(HW == 1 & TYPE == "SO", HG - 1, HGF))
  all_data <- transform(all_data, HGA = ifelse(HW == 1 & TYPE == "SO", VG, HGA))
  all_data <- transform(all_data, VGF = ifelse(HW == 1 & TYPE == "SO", VG, VGF))
  all_data <- transform(all_data, VGA = ifelse(HW == 1 & TYPE == "SO", HG - 1, VGA))
  
  # for losses
  all_data <- transform(all_data, HGF = ifelse(HW == 0 & TYPE == "SO", HG, HGF))
  all_data <- transform(all_data, HGA = ifelse(HW == 0 & TYPE == "SO", VG - 1, HGA))
  all_data <- transform(all_data, VGF = ifelse(HW == 0 & TYPE == "SO", VG - 1, VGF))
  all_data <- transform(all_data, VGA = ifelse(HW == 0 & TYPE == "SO", HG, VGA))
  
  # goal diff
  all_data <- transform(all_data, HGD = HGF - HGA)
  all_data <- transform(all_data, VGD = VGF - VGA)
  
  
  # now add points for ot/so wins
  all_data$HPTS <- 0
  all_data$VPTS <- 0
  
  all_data <- transform(all_data, HPTS = ifelse(HW == 1, 2, HPTS))
  all_data <- transform(all_data, VPTS = ifelse(VW == 1, 2, VPTS))
  all_data <- transform(all_data, HPTS = ifelse(HW == 0 & (TYPE == "SO" | TYPE == "OT"), 1, HPTS))
  all_data <- transform(all_data, VPTS = ifelse(VW == 0 & (TYPE == "SO" | TYPE == "OT"), 1, VPTS))
  
  return(all_data)
  
}

# take all game data and reduce to team data
combo_by_team <- function(all_data){
  
  names <- unique(cbind(all_data$Visitor, all_data$VABR, all_data$VCONF,
                        all_data$VDIV))
  
  by_team <- data.frame(team = names[, 1],
                        abr = names[, 2],
                        conf = names[, 3],
                        div = names[ ,4],
                        gp = rep(0),
                        w = rep(0),
                        l = rep(0),
                        pts = rep(0),
                        gf = rep(0),
                        ga = rep(0),
                        diff = rep(0))
  
  # fill in data
  for (tm in names[, 1]){
    
    by_team[by_team$team == tm, ]$gp = nrow(all_data[all_data$Visitor == tm, ]) +
      nrow(all_data[all_data$Home == tm, ])
    
    by_team[by_team$team == tm, ]$w = nrow(all_data[all_data$Visitor == tm & all_data$VW == 1, ]) +
      nrow(all_data[all_data$Home == tm & all_data$HW == 1, ])
    
    by_team[by_team$team == tm, ]$l = nrow(all_data[all_data$Visitor == tm & all_data$VW == 0, ]) +
      nrow(all_data[all_data$Home == tm & all_data$HW == 0, ])
    
    by_team[by_team$team == tm, ]$pts = sum(all_data[all_data$Visitor == tm, ]$VPTS) +
      sum(all_data[all_data$Home == tm, ]$HPTS)
    
    by_team[by_team$team == tm, ]$gf = sum(all_data[all_data$Visitor == tm, ]$VGF) +
      sum(all_data[all_data$Home == tm, ]$HGF)
    
    by_team[by_team$team == tm, ]$ga = sum(all_data[all_data$Visitor == tm, ]$VGA) +
      sum(all_data[all_data$Home == tm & all_data$HW == 1, ]$HGA)
    
    # by_team[by_team$team == tm, ]$diff = sum(all_data[all_data$Visitor == tm, ]$VGD) +
    #   sum(all_data[all_data$Home == tm & all_data$HW == 1, ]$HGD)
    
    by_team[by_team$team == tm, ]$diff = by_team[by_team$team == tm, ]$gf -
      by_team[by_team$team == tm, ]$ga
    
  }
  
  return(by_team)
  
}

# print team data nicely
team_print <- function(team_data, abr = "", conf = "", div = ""){
  
  conference <- unique(team_data$conf)
  for (c in conference){
    
    the_conf <- team_data %>% 
      filter(conf == c)
    
    division <- unique(the_conf$div)
    
    for (d in division){
      
      the_div <- the_conf %>% 
        filter(div == d) %>% 
        select(team, abr, gp, w, l, pts, gf, ga, diff) 
      
      cat("-----", "\nConference: ", c, "\nDivision: ", d, "\nStandings: \n")
      print(the_div[with(the_div, order(pts, -l, w, diff, decreasing = TRUE)), ], row.names = FALSE)
      cat("-----\n")
      
    }
    
  }
  
}


# test functions ----------------------------------------------------------

web <- htmltab::htmltab("https://www.hockey-reference.com/leagues/NHL_2017_games.html",
                        which = 1,
                        colNames = c("Date", "Visitor", "VG", "Home", "HG", "T",
                                     "Att.", "LOG", "Notes"),
                        rm_nodata_cols = FALSE)
key <- read.table(file = "nhlteams.csv", header = TRUE, sep = ",")

# transform VG, HG into numeric for computation later
web$HG <- as.numeric(web$HG)
web$VG <- as.numeric(web$VG)

all_data <- combo_all(web = web, key = key)
team_data <- combo_by_team(all_data = all_data)

# note that vegas golden knights are new team in 2017

team_print(team_data = team_data)
