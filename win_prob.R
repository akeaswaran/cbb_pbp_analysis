library(ncaahoopR)
library(stringr)
library(data.table)
source("./adv_box_score.R")

data("ids")
data("dict")

generate_league_pbp <- function(team_ids) {
    total_df = data.table()
    message(paste("Staring to load PbP for number of teams: ", length(team_ids), sep = ""))
    for(i in 1:length(team_ids)) {
        name = team_ids[i]
        if (name != 'Campbell' && name != 'C. Carolina' && name != 'Delaware State' && name != 'Charleston So' && name != 'Columbia') {
            message(paste0("[",i,"/",length(team_ids),"]"," Getting PbPs for team: ", name, sep = ""))
            games = get_game_ids(name)
            for(game in games) {
                box_score = generate_box_score(game_id = game)
                if (!is.null(box_score)) {
                    total_df = rbind(total_df, select(box_score, FFDiff, PointDiff))
                }
            }
            message(paste0("[",i,"/",length(team_ids),"]"," Done getting PbPs for team: ", name, sep = ""))
        } else {
            message(paste0("[",i,"/",length(team_ids),"]"," Skipping PbP for team: ", name, sep = ""))
        }
    }
    message(paste0("-----\n","[",i,"/",length(team_ids),"]"," Done getting PbP for number of teams: ", length(team_ids), sep = ""))
    message(paste0("-----\n","[",i,"/",length(team_ids),"]"," Parsed ", length(total_df)," records", sep = ""))
    return(total_df)
}

if (!exists("last_team_index")) {
    last_team_index = 1
}

generate_pbp_data <- function() {
    all_teams = dplyr::pull(ids, team)
    interval = 60
    end = (min(last_team_index+interval - 1, length(all_teams) - 1))
    range = last_team_index:end
    last_team_index = end
    dataframe <- generate_league_pbp(all_teams[range])
    return(dataframe)
}

if (!exists("total")) {
    total = data.table()
}
total <- rbind(total, generate_pbp_data())

if (!exists("proj_score_diff") || !exists("linear_model")) {

    message(paste("Correlation: ",cor(total$FFDiff, total$PointDiff),sep=""))

    set.seed(100)  # setting seed to reproduce results of random sampling
    trainingRowIndex <- sample(1:nrow(total), 0.8*nrow(total))
    trainingData <- total[trainingRowIndex, ]  # model training data
    testData  <- total[-trainingRowIndex, ]   # test data

    linear_model <- lm(PointDiff ~ FFDiff, data=trainingData)  # build the model
    distPred <- predict(linear_model, testData)  # predict distance
    message("Linear Regression model summary: ")
    summary(linear_model)

    proj_score_diff <- data.frame(cbind(actuals=testData$PointDiff, predicteds=distPred))  # make actuals_predicteds dataframe.
    message("Correlation accuracy: ")
    cor(proj_score_diff)
    head(proj_score_diff)

    # Min-Max Accuracy Calculation
    message(paste("Min/Max Accuracy: ", mean(apply(proj_score_diff, 1, min) / apply(proj_score_diff, 1, max)), sep=""))

    # MAE calculation
    message(paste("MAE: ", mean(abs((proj_score_diff$predicteds - proj_score_diff$actuals))), sep=""))

    # MdAE calculation
    message(paste("MdAE: ", median(abs((proj_score_diff$predicteds - proj_score_diff$actuals))), sep=""))

    # MAPE Calculation
    message(paste("MAPE: ", mean(abs((proj_score_diff$predicteds - proj_score_diff$actuals))/proj_score_diff$actuals), sep=""))
}

generate_win_prob <- function(espn_game_id) {
    box_score = generate_box_score(game_id = espn_game_id)

    # Take projected score diff and calculate win prob
    mu = mean(proj_score_diff$predicteds)
    std = sd(proj_score_diff$predicteds)
    #actuals_preds$WinProb = pnorm(actuals_preds$predicteds, mu, std)
    #actuals_preds$FFDiff = testData$FFDiff

    model_summary <- summary(linear_model)
    model_coeffs <- model_summary$coefficients
    beta.slope <- model_coeffs["FFDiff", "Estimate"]
    beta.intercept <- model_coeffs["(Intercept)", "Estimate"]

    WinProb <- (max(box_score$FFDiff) * beta.slope) + beta.intercept
    ff_max_team = box_score[which(max(box_score$FFDiff) == box_score$FFDiff)]
    message(paste('FF Max team: ', ff_max_team$Name, sep=""))
    message(paste('FF Max won by: ', ff_max_team$PointDiff, sep=""))
    message(paste('FF Max had four factors rating diff of: ', ff_max_team$FFDiff, sep=""))
    message(paste('FF Max team should have won by: ', WinProb, sep=""))
    message(paste('Proj win prob for FF Max team: ', pnorm(WinProb, mu, std), sep=""))
}

# generate_win_prob(espn_game_id = 401168157)
