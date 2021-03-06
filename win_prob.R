library(ncaahoopR)
library(stringr)
library(data.table)
setwd("~/Documents/Apps/cbb_pbp_analysis")
source("./adv_box_score.R")

data("ids")
data("dict")

generate_league_pbp <- function(team_ids) {
    total_df = data.table()
    message(paste("Staring to load PbP for number of teams: ", length(team_ids), sep = ""))
    for(i in 1:length(team_ids)) {
        name = team_ids[i]
        if (name != 'Central Arkansas' && name != 'Columbia' && name != 'High Point' && name != 'Duquesne' && name != 'Lafayette' && name != 'Lehigh' && name != 'Monmouth' && name != 'Incarnate Word') {
            message(paste0("[",i,"/",length(team_ids),"]"," Getting PbPs for team: ", name, sep = ""))
            games = get_game_ids(name)
            for(game in games) {
                if (!any(game==total_df$GameID)) {
                    box_score = generate_box_score(game_id = game)
                    if (!is.null(box_score)) {
                        total_df = rbind(total_df, select(box_score, GameID, FFDiff, PointDiff))
                    }
                } else {
                    message(paste0("[",i,"/",length(team_ids),"]"," Already parsed GameID ",game,", skipping",sep=""))
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
    last_team_index = 0
}

if (!exists("total")) {
    total = data.table()
    if (file.exists('data/base.csv')) {
        message(paste0("[Startup] Reading existing model data from local CSV."))
        total = read.table('data/base.csv', header = TRUE, row.names = NULL, sep = ",")
    } else {
        message(paste0("[Startup] Could not find local CSV; will have to load data remotely."))
    }
} else {
    message(paste0("[Startup] 'total' already exists in workspace, skipping file loading."))
}

refresh_data <- function(interval) {
    all_teams = dplyr::pull(ids, team)
    end <- (min(last_team_index + interval, length(all_teams) - 1))
    range = (last_team_index + 1):end
    message(paste0("-----\n","[Data Loading]"," Retrieving ", interval," more teams' worth of game records for model training", sep = ""))
    dataframe <- generate_league_pbp(all_teams[range])
    last_team_index <<- end
    total <<- rbind(total, dataframe)
    total <<- na.omit(total)
    message(paste0("-----\n","[Data Loading]"," Wrote ", length(dataframe$GameID)," new game records to local file", sep = ""))
    write.csv(total, 'data/base.csv', row.names = FALSE)
}

refresh_model <- function(should_refresh_data, interval) {
    if (should_refresh_data == TRUE || length(total) == 0) {
        refresh_data(interval = interval)
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
    message(paste0("% of data covered by model: " ,summary(linear_model)$r.squared, sep=""))

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

    # MAPE Calculation -- can't really use this because the actual differentials will all add up to 0
    # message(paste("Mean Abs % Error: ", signif(abs(mean(abs((proj_score_diff$predicteds - proj_score_diff$actuals))/proj_score_diff$actuals)) * 100, digits = 3), '%', sep=""))

    # Take projected score diff and calculate win prob
    mu = mean(proj_score_diff$predicteds)
    std = sd(proj_score_diff$predicteds)

    message(paste0("Projected MOV mean: ", signif(mu, digits = 3)))
    message(paste0("Projected MOV standard deviation: ", signif(std, digits = 3)))

    proj_MOV <- (max(box_score$FFDiff) * metadata[["slope"]]) + metadata[["intercept"]]
    ff_max_team <- box_score[which(max(box_score$FFDiff) == box_score$FFDiff)]
    actual_winner <- box_score[which(max(box_score$Points) == box_score$Points)]
    message(paste('Actual Winner: ', actual_winner$Name, sep=""))
    message(paste('Actual MOV: ', actual_winner$PointDiff, sep=""))
    message(paste('Winner by Four Factors Rating: ', ff_max_team$Name, sep=""))
    message(paste('Four Factors Rating Margin: ', signif(ff_max_team$FFDiff, digits = 3), sep=""))
    message(paste(ff_max_team$Name,' projected MOV: ', signif(proj_MOV, digits = 3), sep=""))
    win_prob <- pnorm(proj_MOV, mu, std) * 100
    message(paste(ff_max_team$Name,' projected post-game win expectancy: ', signif(win_prob, digits = 3), '%', sep=""))
}

predict_matchup <- function(team1, team2) {
    team1_ids <- get_game_ids(team1)
    team2_ids <- get_game_ids(team2)

    container <- data.table()
    for (i in 1:length(team1_ids)) {
        game = team1_ids[i]
        message(paste0("[",i,"/",length(team1_ids),"]"," Parsing box score for GameID ",game,sep=""))
        if (!any(game==container$GameID)) {
            box_score = generate_box_score(game_id = game)
            if (!is.null(box_score)) {
                tmp = box_score #[which(box_score$Name == team1)]
                container = rbind(container, select(tmp, GameID,Name, FFSum))
            }
        } else {
            message(paste0("[",i,"/",length(team1_ids),"]"," Already parsed GameID ",game,", skipping",sep=""))
        }
    }

    for (i in 1:length(team2_ids)) {
        game = team2_ids[i]
        message(paste0("[",i,"/",length(team2_ids),"]"," Parsing box score for GameID ",game,sep=""))
        if (!any(game==container$GameID)) {
            box_score = generate_box_score(game_id = game)
            if (!is.null(box_score)) {
                tmp = box_score #[which(box_score$Name == team2)]
                container = rbind(container, select(tmp, GameID,Name, FFSum))
            }
        } else {
            message(paste0("[",i,"/",length(team2_ids),"]"," Already parsed GameID ",game,", skipping",sep=""))
        }
    }

    metadata <- refresh_model(should_refresh_data = FALSE, interval = 50)
    proj_score_diff <- metadata[['proj_scores']]

    team1_mu = mean(tail(container[(container$Name == team1)]$FFSum, 4))
    team1_sos = sum(container[(container$GameID %in% team1_ids) & (container$Name != team1)]$FFSum)

    team2_mu = mean(tail(container[(container$Name == team2)]$FFSum, 4))
    team2_sos = sum(container[(container$GameID %in% team2_ids) & (container$Name != team2)]$FFSum)

    if (team1_sos > team2_sos) {
        team2_mu = team2_mu * (team2_sos / team1_sos)
    } else if (team2_sos > team1_sos) {
        team1_mu = team1_mu * (team1_sos / team2_sos)
    }

    diff = team1_mu - team2_mu

    mu = mean(proj_score_diff$predicteds)
    std = sd(proj_score_diff$predicteds)

    proj_MOV <- (diff * metadata[["slope"]]) + metadata[["intercept"]]
    win_prob <- pnorm(proj_MOV, mu, std) * 100
    return(list(win_prob, proj_MOV))
}

# Example: generate a post-game win probability for ESPN game 401168533: Syracuse 97, Georgia Tech 63
# generate_win_prob(espn_game_id = 401168533)

# Example: predict a matchup between Georgia Tech and Houston based on their game-by-game post-game win probabilities
# predict_matchup("Georgia Tech", "Houston")
