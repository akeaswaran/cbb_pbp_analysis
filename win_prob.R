library(ncaahoopR)
library(stringr)
library(data.table)

data("ids")
data("dict")

generate_league_pbp <- function(team_ids) {
    table = data.table()
    message(paste("Staring to load PbP for number of teams: ", length(team_ids), sep = ""))
    for(name in team_ids) {
        message(paste("Getting PbPs for team: ", name, sep = ""))
        games = get_game_ids(name)
        for(game in games) {
            box_score = generate_box_score(game_id = game)
            if (!is.null(box_score)) {
                table = rbind(table, select(box_score, FFDiff, AvgWinProb))
            }
        }
    }
    message(paste("Done getting PbP for number of teams: ", length(team_ids), sep = ""))
    return(table)
}

all_teams = dplyr::pull(ids, team)
total <- generate_league_pbp(all_teams[1:10])

# scatter.smooth(x=total$FFDiff, y=total$AvgWinProb, main="Four Factors Diff Correlated to Win Prob")

# cor(total$FFDiff, total$AvgWinProb) # correlation
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(total), 0.8*nrow(total))
trainingData <- total[trainingRowIndex, ]  # model training data
testData  <- total[-trainingRowIndex, ]   # test data

lmMod <- lm(AvgWinProb ~ FFDiff, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

summary(lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$AvgWinProb, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
# => 38.00%, min_max accuracy

# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

# WinProb <- (max(box$FFDiff) * 2.9498) + 50
#message(paste('Winning team had exp win prob of: ', predict(linearMod, max(box$FFDiff)), sep=""))
