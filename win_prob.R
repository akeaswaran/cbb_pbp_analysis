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
        if (name != 'Central Arkansas' && name != 'Columbia' && name != 'High Point' && name != 'Duquesne') {
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
    message(paste0("-----\n","[",i,"/",length(team_ids),"]"," Parsed ", length(total_df$FFDiff)," records", sep = ""))
    return(total_df)
}

if (!exists("last_team_index")) {
    last_team_index = 1
}

if (!exists("total")) {
    total = data.table()
}

refresh_data <- function(interval) {
    all_teams = dplyr::pull(ids, team)
    end <- (min(last_team_index+interval, length(all_teams) - 1))
    range = last_team_index:end
    dataframe <- generate_league_pbp(all_teams[range])
    last_team_index <<- end
    total <<- rbind(total, dataframe)
}

refresh_model <- function(should_refresh_data, interval) {
    if (should_refresh_data == TRUE) {
        refresh_data(interval = intvl)
    }
    message(paste("Correlation: ",cor(total$FFDiff, total$PointDiff),sep=""))

    set.seed(1024)  # setting seed to reproduce results of random sampling
    trainingRowIndex <- sample(1:nrow(total), 0.8*nrow(total))
    training_data <- total[trainingRowIndex, ]  # model training data
    test_data  <- total[-trainingRowIndex, ]   # test data

    linear_model <- lm(PointDiff ~ FFDiff, data=training_data)  # build the model
    dist_pred <- predict(linear_model, test_data)  # predict distance
    # message("Linear Regression model summary: ")
    # summary(linear_model)

    proj_score_diff <- data.frame(cbind(actuals = test_data$PointDiff,
                                        predicteds = dist_pred))
    message("Correlation of predictions to actual: ")
    print(cor(proj_score_diff))

    model_summary <- summary(linear_model)
    model_coeffs <- model_summary$coefficients

    return(list(slope = model_coeffs["FFDiff", "Estimate"], intercept = model_coeffs["(Intercept)", "Estimate"], model = linear_model, proj_scores = proj_score_diff))
}

generate_win_prob <- function(espn_game_id) {
    box_score = generate_box_score(game_id = espn_game_id)
    metadata <- refresh_model(should_refresh_data = FALSE, interval = 50)
    proj_score_diff <- metadata[['proj_scores']]

    # MAE calculation
    message(paste("Mean Abs Error: +/- ", signif(mean(abs((proj_score_diff$predicteds - proj_score_diff$actuals))), digits = 3), ' points', sep=""))

    # MdAE calculation
    message(paste("Mdn Abs Error: +/- ", signif(median(abs((proj_score_diff$predicteds - proj_score_diff$actuals))), digits = 3), ' points', sep=""))

    # MAPE Calculation
    message(paste("Mean Abs % Error: ", signif(abs(mean(abs((proj_score_diff$predicteds - proj_score_diff$actuals))/proj_score_diff$actuals)) * 100, digits = 3), '%', sep=""))

    # Take projected score diff and calculate win prob
    mu = mean(proj_score_diff$predicteds)
    std = sd(proj_score_diff$predicteds)

    WinProb <- (max(box_score$FFDiff) * metadata[["slope"]]) + metadata[["intercept"]]
    ff_max_team = box_score[which(max(box_score$FFDiff) == box_score$FFDiff)]
    message(paste('Actual Winner: ', box_score[which(max(box_score$Points) == box_score$Points)]$Name, sep=""))
    message(paste('Actual MOV: ', box_score[which(max(box_score$Points) == box_score$Points)]$PointDiff, sep=""))
    message(paste('Winner by Four Factors Rating: ', ff_max_team$Name, sep=""))
    message(paste('Four Factors Rating Margin: ', signif(ff_max_team$FFDiff, digits = 3), sep=""))
    message(paste(ff_max_team$Name,' projected MOV: ', WinProb, sep=""))
    message(paste(ff_max_team$Name,' projected post-game win expectancy: ', signif(pnorm(WinProb, mu, std) * 100, digits = 3), '%', sep=""))
}

# generate_win_prob(espn_game_id = 401168533)
