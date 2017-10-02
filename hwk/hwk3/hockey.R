library(tidyverse)

# function declarations ---------------------------------------------------

# combine all games data and append new columns 
combo_all <- function(web, key){
  
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
  
  # goal diff
  total <- transform(total, HGD = HGF - HGA)
  total <- transform(total, VGD = VGF - VGA)
  
  
  # now add points for ot/so wins
  total$HPTS <- 0
  total$VPTS <- 0
  
  total <- transform(total, HPTS = ifelse(HW == 1, 2, HPTS))
  total <- transform(total, VPTS = ifelse(VW == 1, 2, VPTS))
  total <- transform(total, HPTS = ifelse(HW == 0 & (TYPE == "SO" | TYPE == "OT"), 1, HPTS))
  total <- transform(total, VPTS = ifelse(VW == 0 & (TYPE == "SO" | TYPE == "OT"), 1, VPTS))
  
  return(total)
  
}

# take all game data and reduce to team data
combo_by_team <- function(all_data){
  
  names <- unique(cbind(total$Visitor, total$VABR, total$VCONF, total$VDIV))
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
    
    by_team[by_team$team == tm, ]$gp = nrow(total[total$Visitor == tm, ]) +
      nrow(total[total$Home == tm, ])
    
    by_team[by_team$team == tm, ]$w = nrow(total[total$Visitor == tm & total$VW == 1, ]) +
      nrow(total[total$Home == tm & total$HW == 1, ])
    
    by_team[by_team$team == tm, ]$l = nrow(total[total$Visitor == tm & total$VW == 0, ]) +
      nrow(total[total$Home == tm & total$HW == 0, ])
    
    by_team[by_team$team == tm, ]$pts = sum(total[total$Visitor == tm, ]$VPTS) +
      sum(total[total$Home == tm, ]$HPTS)
    
    by_team[by_team$team == tm, ]$gf = sum(total[total$Visitor == tm, ]$VGF) +
      sum(total[total$Home == tm, ]$HGF)
    
    by_team[by_team$team == tm, ]$ga = sum(total[total$Visitor == tm, ]$VGA) +
      sum(total[total$Home == tm & total$HW == 1, ]$HGA)
    
    by_team[by_team$team == tm, ]$diff = sum(total[total$Visitor == tm, ]$VGD) +
      sum(total[total$Home == tm & total$HW == 1, ]$HGD)
    
  }
  
  return(by_team)
  
}

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
      cat("-----")
      
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
