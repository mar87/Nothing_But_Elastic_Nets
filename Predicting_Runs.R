# load libraries:
library(data.table); library(dplyr); library(tidyr); library(plyr); library(XML)
library(lmer); library(plyr)



# set working directory:
#setwd("C:/Users/Derek/Documents/2017/summer/NBA/hackathon/data/Basketball Data-selected")
setwd('/Users/meganrobertson/Desktop/Basketball_Data/') #change to your data location


# create function to find opponenent for each possession:
find_opponent <- function(possess_log, team_map){
  game_ids = sort(unique(possess_log$GAME_ID))
  game_2_possess = lapply(game_ids, function(g) return(which(possess_log$GAME_ID == g)))
  names(game_2_possess) = game_ids
  game_2_teams = lapply(1:length(game_ids), function(ind) return(sort(unique(possess_log$TEAM[which(possess_log$GAME_ID==game_ids[ind])]))))
  names(game_2_teams) = game_ids
  opponent = unlist(lapply(1:dim(possess_log)[1], function(n) return(team_map$Team[match(game_2_teams[[possess_log$GAME_ID[n]]][which(game_2_teams[[possess_log$GAME_ID[n]]]!=possess_log$TEAM[n])],team_map$Team_ID)])))
  return(opponent)
}

# create dumb function to classify possessions as part of a run:
find_runs <- function(possess_log, threshold, half_window){
  num_pos = dim(possess_log)[1]
  is_streak = rep(0, num_pos)
  for (i in 1:num_pos){
    point_diff = 0
    if ((i>half_window) & (i<(dim(possess_log)[1]-half_window))){
      window = seq(i-half_window, i+half_window, 1)
      points_scored = possess_log$PTS[window]
      team_1_points = sum(points_scored[c(2,4,6,8,10)])
      team_2_points = sum(points_scored[c(1,3,5,7,11)])
      point_diff = team_1_points - team_2_points
      #print(point_diff)
      if (abs(point_diff) > threshold){
        is_streak[i] = 1
      }
    }
  }
  return(is_streak)
}


# read in possession logs:
#possess_log_16_17 = data.frame(fread('2016-17_nba_possession_log.txt'))
possess_log_16_17 = data.frame(fread('/Users/meganrobertson/Desktop/Nothing_But_Elastic_Nets/possession_1617_labeled.csv'))
possess_log_16_17[["SEASON"]] = "2016-2017"
possess_log = possess_log_16_17 # rbind(possess_log_14_15, possess_log_15_16, possess_log_16_17)
possess_log$PERIOD = as.factor(possess_log$PERIOD)
possess_log$TEAM_ID[which(possess_log$TEAM_ID=="(null)")] = "1610612766" # Change "(null)" to Hornets
#possess_log = possess_log[1:10000, ] # Take a subset of first 10000 observations
#possess_log = cbind(possess_log, model.matrix( ~ PERIOD-1, data=possess_log)) # add variables for each period 

#reading in other logs 
reb = data.frame(fread('2016-17_nba_reb_log.txt'))
shot = data.frame(fread('2016-17_nba_shot_log.txt'))
shot['good_shot'] = 0
shot[which(shot$SHOT_DIST <= 6), 'good_shot'] = 1

#plots to determine reasonable cut-offs for good shots
#ggplot(shot, aes(x=SHOT_DIST, fill=SHOT_RESULT)) + geom_density(alpha=0.5)
#ggplot(shot, aes(x=CLOSE_DEF_DIST, fill=SHOT_RESULT)) + geom_density(alpha=0.5)


# read in team map: 
#team map - conference, team, sportvuID
team_map = data.frame(fread('Team_map.csv'))

# read in game map:
#game map - ID, sportvuID, season, date
game_map = data.frame(fread('Game_Map.csv'))



# change possession log to have team label:
possess_log["TEAM"] = team_map$Team[match(possess_log$TEAM_ID, team_map$Team_ID)]
#opponent = find_opponent(possess_log, team_map)
#possess_log["OPPONENT"] = find_opponent(possess_log, team_map)

# include a run variable: 
#threshold = 5 
#half_window = 5
#possess_log["RUN"] = find_runs(possess_log, threshold, half_window)

#adding Megan's modeling variables
#function to add  information to a row, apply to the possession logs for the three seasons
add_info <- function(poss){
  
  #information from the shot log
  shots = shot[which(shot$GAME_ID == poss$GAME_ID & shot$PERIOD == poss$PERIOD),]
  range_shots = shots[which(shots$GAME_CLOCK >= poss$GAME_CLOCK_END & shots$GAME_CLOCK <= poss$GAME_CLOCK_START & shots$PERIOD == poss$PERIOD), ] #shots in the time range of possession
  poss['num_shots'] = sum(range_shots$FGA) #number of attempted shots during the possession
  poss['made_shots'] = sum(range_shots$FGM) #number of made shots during the possession
  poss['num_good_shots'] = sum(range_shots$good_shot) #how many good shots attempted
  
  
  #information from the rebound log, does a rebound start the possession?
  rebs = reb[which(reb$GAME_ID == poss$GAME_ID & reb$PERIOD == poss$PERIOD & reb$GAME_CLOCK >= poss$GAME_CLOCK_END & reb$GAME_CLOCK <= poss$GAME_CLOCK_START),] #all rebounds in the game
  poss['num_reb'] = nrow(rebs)
  
  return(poss)
}

model_data = adply(possess_log_16_17, 1, add_info)



# Run a logistic regression to predict run:
#formula = as.formula(paste("RUN ~", paste(paste(names(possess_log)[c(9,10,11,12,14,16,17,18,19,20,21,22,23,24)],sep=""), collapse="+")))
formula = as.formula(paste("RUN ~", paste(paste(names(possess_log)[c(6,9,10,11,12,14,24)],sep=""), collapse="+")))
formula
run_pred_1 = glm(formula, data=possess_log)
run_pred_1
summary(run_pred_1)

