# Predicting the winner of an NHL game using statistics available at NHL.com/stats

#loading required packages
library(lubridate)
library(jsonlite)
library(Hmisc)
library(corrplot)
library(e1071)
library(nnet)
library(dummies)
library(devtools)
library(tidyverse)

#URLs to JSON data from page details
team_summary_file <- "http://www.nhl.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=true&reportName=teamsummary&sort=[{%22property%22:%22gameDate%22,%22direction%22:%22DESC%22}]&factCayenneExp=gamesPlayed%3E=1&cayenneExp=gameDate%3E=%222008-10-01%22%20and%20gameDate%3C=%222018-04-30%22%20and%20gameTypeId=2%20and%20gameLocationCode=%22H%22"
penalties_file <- "http://www.nhl.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=true&reportName=penalties&sort=[{%22property%22:%22penaltyMinutes%22,%22direction%22:%22DESC%22}]&factCayenneExp=gamesPlayed%3E=1&cayenneExp=gameDate%3E=%222008-10-01%22%20and%20gameDate%3C=%222018-04-30%22%20and%20gameTypeId=2%20and%20gameLocationCode=%22H%22"
hits_file <- "http://www.nhl.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=true&reportName=realtime&sort=[{%22property%22:%22hits%22,%22direction%22:%22DESC%22}]&factCayenneExp=gamesPlayed%3E=1&cayenneExp=gameDate%3E=%222008-10-01%22%20and%20gameDate%3C=%222018-04-30%22%20and%20gameTypeId=2%20and%20gameLocationCode=%22H%22"
faceoffs_file <- "http://www.nhl.com/stats/rest/team?isAggregate=false&reportType=basic&isGame=true&reportName=faceoffsbystrength&sort=[{%22property%22:%22faceoffWinPctg%22,%22direction%22:%22DESC%22}]&factCayenneExp=gamesPlayed%3E=1&cayenneExp=gameDate%3E=%222008-10-01%22%20and%20gameDate%3C=%222018-04-30%22%20and%20gameTypeId=2%20and%20gameLocationCode=%22H%22"

#import the JSON files
team_summary <- fromJSON(team_summary_file)
penalties <- fromJSON(penalties_file)
hits <- fromJSON(hits_file)
faceoffs <- fromJSON(faceoffs_file)

#extract the data frames from the lists
team_summary <- team_summary$data
penalties <- penalties$data
hits <- hits$data
faceoffs <- faceoffs$data

#view summaries for initial exploration
summary(team_summary)
summary(penalties)
summary(hits)
summary(faceoffs)

#Begin selecting columns and creating full dataset called nhl_wins
nhl_wins <- select(team_summary, gameId, gameDate, teamAbbrev, opponentTeamAbbrev, faceoffWinPctg, faceoffsLost, faceoffsWon, penaltyKillPctg, ppGoalsAgainst, ppGoalsFor, ppOpportunities, ppPctg, shNumTimes, shotsAgainst, shotsFor, wins)

#select columns to keep and trim remaining data sets
penalties_keep <- c(2,8:14)
hits_keep <- c(1, 7, 10, 12, 17, 19)
faceoffs_keep <- c(1,2,4,8)

penalties_trim <- select(penalties,penalties_keep)
hits_trim <- select(hits,hits_keep)
faceoffs_trim <- select(faceoffs,faceoffs_keep)

#combine into the nhl_wins data frame
nhl_wins <- left_join(nhl_wins,penalties_trim, by="gameId")
nhl_wins <- left_join(nhl_wins,hits_trim, by="gameId")
nhl_wins <- left_join(nhl_wins,faceoffs_trim, by="gameId")

#review nhl_wins data frame
glimpse(nhl_wins)
summary(nhl_wins)

#convert date string to class date
nhl_wins$gameDate <- ymd(str_sub(nhl_wins$gameDate,1,10))

#convert wins (class variable) to a factor and view the balance of wins to losses
nhl_wins$wins <- as.factor(nhl_wins$wins)
summary.factor(nhl_wins$wins)

#Explore variable correlations
nhl_corr <- rcorr(as.matrix(nhl_wins[,5:31]), type="pearson") #looking at only numeric values
corrplot(nhl_corr$r, type="upper",order="hclust", tl.col="black")

#Remove correlated variables that can be calculated from other variables
col_remove <- c("faceoffWinPctg", "faceoffs", "penalties", "penaltyMinutes", "penaltyKillPctg", "ppPctg")
nhl_wins <- nhl_wins[ ,-which(names(nhl_wins) %in% col_remove)]

#Explore team abbrev column
ggplot(data=nhl_wins, mapping = aes(teamAbbrev,wins==1)) + geom_col(color='darkblue') + theme(axis.text.x = element_text(angle=90)) + xlab("Home Team") + ylab("Win Count")

#Correct labels for re-located franchises to current abbreviation
ari_phx <- which(nhl_wins$teamAbbrev=="ARI"|nhl_wins$teamAbbrev=="PHX")
atl_wpg <- which(nhl_wins$teamAbbrev=="ATL"|nhl_wins$teamAbbrev=="WPG")
nhl_wins[ari_phx,]$teamAbbrev <- "ARI"
nhl_wins[atl_wpg,]$teamAbbrev <- "WPG"
#update visiting team column
ari_phx2 <- which(nhl_wins$opponentTeamAbbrev=="ARI"|nhl_wins$opponentTeamAbbrev=="PHX")
atl_wpg2 <- which(nhl_wins$opponentTeamAbbrev=="ATL"|nhl_wins$opponentTeamAbbrev=="WPG")
nhl_wins[ari_phx2,]$opponentTeamAbbrev <- "ARI"
nhl_wins[atl_wpg2,]$opponentTeamAbbrev <- "WPG"
#check work with bar charts
ggplot(data=nhl_wins, mapping = aes(teamAbbrev,wins==1)) + geom_col() + theme(axis.text.x = element_text(angle=90)) + xlab("Home Team") + ylab("Win Count")
ggplot(data=nhl_wins, mapping = aes(opponentTeamAbbrev,wins==1)) + geom_col() + theme(axis.text.x = element_text(angle=90)) + xlab("Away Team") + ylab("Win Count")

#see if early, mid or late season has different win percentages
nhl_wins$seasonPeriod <- ifelse(month(nhl_wins$gameDate) == 10 | month(nhl_wins$gameDate) == 11, "Early", ifelse(month(nhl_wins$gameDate) == 12 | month(nhl_wins$gameDate) == 1,"Mid", "Late"))
nhl_group <- nhl_wins %>% group_by(seasonPeriod)
summarise(nhl_group, win_pct = mean(as.numeric(wins)-1))
#check months (not grouped into early, mid and late)
nhl_group <- nhl_wins %>% group_by(month(gameDate))
nhl_group %>% summarise(win_pct = mean(as.numeric(wins)-1))
#create gameMonth variable and remove seasonPeriod variable
nhl_wins$gameMonth <- month(nhl_wins$gameDate)
nhl_wins <- nhl_wins[,-26]

#create train and test sets and verify the balance of the wins variable in each set
set.seed(9876)
ind <- sample(2,nrow(nhl_wins),replace=TRUE, prob=c(0.7,0.3))
nhl_train <- nhl_wins[ind==1,]
nhl_test <- nhl_wins[ind==2,]
summary.factor(nhl_train$wins)
summary.factor(nhl_test$wins)

#create and test SVM model 
nhl_svm <- svm(wins~., data=nhl_train,kernel="radial", cost=1, gamma=1/ncol(nhl_train))
svm_prediction <- predict(nhl_svm, nhl_test)
table(svm_prediction, nhl_test$wins)
sum(svm_prediction==nhl_test$wins)/length(svm_prediction)

#create neural network model, first converting to numeric data frame and normalizing using a custom function
#normalize function
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

nhl_num <- nhl_wins[,-c(1:4,13)]
nhl_num <- as.data.frame(sapply(nhl_num,normalize))
nhl_num$wins <- nhl_wins$wins

nhl_ann_train <- nhl_num[ind==1,]
nhl_ann_test <- nhl_num[ind==2,]
nhl_ann <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.5,decay=5e-4,maxit=200)
mse_nn <- sum(nhl_ann$residuals^2)/nrow(nhl_ann$residuals)
mse_nn
nn_predict <- predict(nhl_ann, nhl_ann_test, type="class")
table(nn_predict,nhl_ann_test$wins)
sum(nn_predict==nhl_ann_test$wins)/length(nn_predict)

#changing parameters to find the optimal setup
#lower rang value
nhl_ann2 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.2,decay=5e-4,maxit=200)
mse_nn2 <- sum(nhl_ann2$residuals^2)/nrow(nhl_ann2$residuals)
mse_nn2
nn2_predict <- predict(nhl_ann2, nhl_ann_test, type="class")
table(nn2_predict,nhl_ann_test$wins)
sum(nn2_predict==nhl_ann_test$wins)/length(nn2_predict)

#increase rang value
nhl_ann3 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.7,decay=5e-4,maxit=200)
mse_nn3 <- sum(nhl_ann3$residuals^2)/nrow(nhl_ann3$residuals)
mse_nn3
nn3_predict <- predict(nhl_ann3, nhl_ann_test, type="class")
table(nn3_predict,nhl_ann_test$wins)
sum(nn3_predict==nhl_ann_test$wins)/length(nn3_predict)

#higher again
nhl_ann4 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.9,decay=5e-4,maxit=200)
mse_nn4 <- sum(nhl_ann4$residuals^2)/nrow(nhl_ann4$residuals)
mse_nn4
nn4_predict <- predict(nhl_ann4, nhl_ann_test, type="class")
table(nn4_predict,nhl_ann_test$wins)
sum(nn4_predict==nhl_ann_test$wins)/length(nn4_predict)

#increase size
nhl_ann5 <- nnet(wins~., data=nhl_ann_train, size=10,rang=0.5,decay=5e-4,maxit=200)
mse_nn5 <- sum(nhl_ann5$residuals^2)/nrow(nhl_ann5$residuals)
mse_nn5
nn5_predict <- predict(nhl_ann5, nhl_ann_test, type="class")
table(nn5_predict,nhl_ann_test$wins)
sum(nn5_predict==nhl_ann_test$wins)/length(nn5_predict)
#decrease size
nhl_ann6 <- nnet(wins~., data=nhl_ann_train, size=2,rang=0.5,decay=5e-4,maxit=200)
mse_nn6 <- sum(nhl_ann6$residuals^2)/nrow(nhl_ann6$residuals)
mse_nn6
nn6_predict <- predict(nhl_ann6, nhl_ann_test, type="class")
table(nn6_predict,nhl_ann_test$wins)
sum(nn6_predict==nhl_ann_test$wins)/length(nn6_predict)

#increase maxit
nhl_ann7 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.5,decay=5e-4,maxit=300)
mse_nn7 <- sum(nhl_ann7$residuals^2)/nrow(nhl_ann7$residuals)
mse_nn7
nn7_predict <- predict(nhl_ann7, nhl_ann_test, type="class")
table(nn7_predict,nhl_ann_test$wins)
sum(nn7_predict==nhl_ann_test$wins)/length(nn7_predict)

#increase decay
nhl_ann8 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.5,decay=0.5,maxit=300)
mse_nn8 <- sum(nhl_ann8$residuals^2)/nrow(nhl_ann8$residuals)
mse_nn8
nn8_predict <- predict(nhl_ann8, nhl_ann_test, type="class")
table(nn8_predict,nhl_ann_test$wins)
sum(nn8_predict==nhl_ann_test$wins)/length(nn8_predict)
#adjust decay
nhl_ann9 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.5,decay=0.01,maxit=300)
mse_nn9 <- sum(nhl_ann9$residuals^2)/nrow(nhl_ann9$residuals)
mse_nn9
nn9_predict <- predict(nhl_ann9, nhl_ann_test, type="class")
table(nn9_predict,nhl_ann_test$wins)
sum(nn9_predict==nhl_ann_test$wins)/length(nn9_predict)
#zero out decay
nhl_ann10 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.5,decay=0,maxit=300)
mse_nn10 <- sum(nhl_ann10$residuals^2)/nrow(nhl_ann10$residuals)
mse_nn10
nn10_predict <- predict(nhl_ann10, nhl_ann_test, type="class")
table(nn10_predict,nhl_ann_test$wins)
sum(nn10_predict==nhl_ann_test$wins)/length(nn10_predict)

#Now testing to see if adding additional variables will increase accuracy
#add team abbreviation as factor and one-hot encode the values
nhl_num$teamAbbrev <- as.factor(nhl_wins$teamAbbrev)
nhl_num <- dummy.data.frame(nhl_num[,-22],sep="_")
nhl_num$wins <- nhl_wins$wins

#using the parameters from nhl_ann, retrain using the new data
nhl_ann_train <- nhl_num[ind==1,]
nhl_ann_test <- nhl_num[ind==2,]
nhl_ann11 <- nnet(wins~., data=nhl_ann_train, size=4,rang=0.5,decay=5e-4,maxit=200)
mse_nn11 <- sum(nhl_ann11$residuals^2)/nrow(nhl_ann11$residuals)
mse_nn11
nn11_predict <- predict(nhl_ann11, nhl_ann_test, type="class")
table(nn11_predict,nhl_ann_test$wins)
sum(nn11_predict==nhl_ann_test$wins)/length(nn11_predict)

#attempt to remove the months variable and see what happens to the accuracy
nhl_num2 <- nhl_wins[,-c(1:4,13,26)]
nhl_num2 <- as.data.frame(sapply(nhl_num2,normalize))
nhl_num2$wins <- nhl_wins$wins
nhl_ann_train2 <- nhl_num2[ind==1,]
nhl_ann_test2 <- nhl_num2[ind==2,]
nhl_ann12 <- nnet(wins~., data=nhl_ann_train2, size=4,rang=0.5,decay=5e-4,maxit=200)
mse_nn12 <- sum(nhl_ann12$residuals^2)/nrow(nhl_ann12$residuals)
mse_nn12
nn12_predict <- predict(nhl_ann12, nhl_ann_test2, type="class")
table(nn12_predict,nhl_ann_test2$wins)
sum(nn12_predict==nhl_ann_test2$wins)/length(nn12_predict)

#download plot.nnet function
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")
#plot the neural network
plot.nnet(nhl_ann)
plot.nnet(nhl_ann5)

#download gar.fun function to find importance of variables
source_url("https://raw.githubusercontent.com/fawda123/gar_fun/master/gar_fun.r")
#create plot and view raw data for importance of variables
nhl_imp <- gar.fun('wins',nhl_ann)
nhl_imp
nhl_imp$data