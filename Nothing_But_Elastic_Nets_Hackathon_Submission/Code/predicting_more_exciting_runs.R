# load libraries:
library(data.table); library(dplyr); library(tidyr); library(plyr); library(XML)
library(lmer); library(plyr)
#Package preload
library(dotwhisker)
library(broom)
library(dplyr)


# set working directory:
#setwd("C:/Users/Derek/Documents/2017/summer/NBA/hackathon/data/Basketball Data-selected")
#setwd('/Users/meganrobertson/Desktop/Basketball_Data/') #change to your data location
setwd("C:/Users/Derek/Documents/2017/summer/NBA/hackathon/data/Basketball Data-selected/getting_closer")



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
possess_log_16_17 = data.frame(fread('possession_1617_labeled_directed_team.csv'))
possess_log_16_17[["SEASON"]] = "2016-2017"
possess_log = possess_log_16_17 # rbind(possess_log_14_15, possess_log_15_16, possess_log_16_17)
possess_log_16_17$PERIOD = as.factor(possess_log_16_17$PERIOD)
possess_log$TEAM_ID[which(possess_log$TEAM_ID=="(null)")] = "1610612766" # Change "(null)" to Hornets
#possess_log = possess_log[1:10000, ] # Take a subset of first 10000 observations
#possess_log_16_17 = cbind(possess_log_16_17, model.matrix( ~ PERIOD-1, data=possess_log_16_17)) # add variables for each period 

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
possess_log_16_17["TEAM"] = team_map$Team[match(possess_log_16_17$TEAM_ID, team_map$Team_ID)]
#opponent = find_opponent(possess_log, team_map)
#possess_log["OPPONENT"] = find_opponent(possess_log, team_map)

# include a run variable: 
#threshold = 5 
#half_window = 5
#possess_log["RUN"] = find_runs(possess_log, threshold, half_window)

#adding Megan's modeling variables
model_data = data.frame(fread('poss_model_data.csv'))

# add Megan's all star model data:
all_star_model_data = data.frame(fread('all_star_model_data.csv'))
all_star_model_data = all_star_model_data[which(!is.na(all_star_model_data$POSSESSION_ID)),]
NUM_ALLSTARS = all_star_model_data$num_all_stars

# change -1 to 1 and call offensive streak:
OFFENSIVE_RUN = as.numeric(possess_log_16_17$STREAK.TEAM==1)
DEFENSIVE_RUN = as.numeric(possess_log_16_17$STREAK.TEAM==-1)
TEAM = possess_log_16_17$TEAM
model_data = cbind(cbind(cbind(cbind(cbind(model_data, OFFENSIVE_RUN),TEAM),model.matrix( ~ PERIOD-1, data=possess_log_16_17)),DEFENSIVE_RUN),NUM_ALLSTARS)
#model_data = model_data[, c(8,10,11,12,13,15,16,17,21,26,29,32,33,34,35,36,37,38,39)] # choose a subset of variables
#model_data = model_data[, c(8,10,11,12,13,15,16,17,21,26,29)] # choose a subset of variables
model_data = model_data[, c(8,10,11,12,13,15,16,21,40,41)] # choose a subset of variables



# read in data all stars data:
#all_stars = data.frame(fread('data_all_stars.csv'))
#new_all_stars = all_stars[apply(all_stars, 1, function(n) return(any(is.na(n)))),]


# Run a logistic regression to predict run:
# with
formula = as.formula(paste("OFFENSIVE_RUN ~", paste(paste(names(model_data),sep=""), collapse="+")))
#formula = as.formula(paste("OFFENSIVE_RUN ~", paste(paste(names(model_data)[c(7,8,15)],sep=""), collapse="+")))
#formula = as.formula( paste(paste("OFFENSIVE_RUN ~", paste(paste(names(model_data)[c(7,8,15)],sep=""), collapse="+")),"+1",sep=""))
formula
run_pred_1 = glm(formula, data=model_data)
run_pred_1
run_sum_1 = summary(run_pred_1)
#run_sum_1 = summary(step(run_pred_1, direction = "backward", k = 2, trace = 0))
#run_sum_1 = summary(step(run_pred_1, direction = "backward", k = log(32), trace = 0))
run_sum_1



# Show a boxplot of coefficients:
dwplot(run_pred_1)



#formula2 = as.formula(paste("OFFENSIVE_RUN ~", paste(paste(names(model_data)[c(7,8,15)],sep=""), collapse="+")))
formula2 = as.formula(paste("DEFENSIVE_RUN ~", paste(paste(names(model_data),sep=""), collapse="+")))
formula2
run_pred_2 = glm(formula2, data=model_data)
run_pred_2
run_sum_2 = summary(run_pred_2)
#run_sum_2 = summary(step(run_pred_2, direction = "backward", k = log(32), trace = 0))
#run_sum_2 = summary(step(run_pred_2, direction = "backward", k = 2, trace = 0))
run_sum_2

#formula3 = as.formula(paste("OFFENSIVE_RUN ~", paste(paste(names(model_data)[c(7,8,15)],sep=""), collapse="+")))
#formula3 = as.formula(paste("OFFENSIVE_RUN ~", paste(paste(names(model_data),sep=""), collapse="+")))
#formula3
#run_pred_3 = glm(formula3, data=model_data)
#run_pred_3
#run_sum_3 = summary(run_pred_3)
#run_sum_3 = summary(step(run_pred_3, direction = "backward", k = 2, trace = 0))
#run_sum_3


# show some boxplots for coefficients in model to predict a run for the team with the ball
# and a run against the team with the ball:
#dwplot(list(run_pred_1, run_pred_2, run_pred_3)) +
#  facet_grid(~model, scales="free_y")
dwplot(list(run_pred_1, run_pred_2)) +
  facet_grid(~model, scales="free_y") 

