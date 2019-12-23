library(ncaahoopR)
library(stringr)
library(data.table)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))
data("ids")
data("dict")

shot_types = c('Jumper','Dunk','Layup','Three Point Jumper')
rebound_types = c('Defensive Rebound','Offensive Rebound')

analyze_player_plays <- function(pbp, player, team) {
    message(paste("Beginning player analysis for player: ", player, sep = ""))
    if (is.null(pbp) || length(pbp) == 0) {
        message(paste("Bailing out, did not provide play by play data", sep = ""))
        return(NULL)
    }

    cleaned_team_name <- dict[which(dict$ESPN_PBP == team), ]
    if (is.null(cleaned_team_name) || length(cleaned_team_name$ESPN) == 0) {
        message(paste("Bailing out, could not find roster information for ", team, sep = ""))
        return(NULL)
    }
    team_roster <- get_roster(cleaned_team_name$ESPN)
    player_finder <- team_roster[which(team_roster$name == player), ]
    if (is.null(player_finder) || length(player_finder$name) == 0) {
        message(paste("Bailing out, could not find personal information for ", player, sep = ""))
        return(NULL)
    }

    team_plays <- pbp %>% filter(grepl(player, description))

    team_shots <- team_plays %>% filter(grepl(paste(shot_types, collapse="|"), description))
    made_shots <- team_shots %>% filter(grepl('made', description))

    team_3pt <- team_shots %>% filter(grepl('Three Point', description))
    made_3pt <- team_3pt %>% filter(grepl('made', description))

    team_rebounds <- team_plays %>% filter(grepl(paste(rebound_types, collapse="|"), description))
    off_rebounds <- team_rebounds %>% filter(grepl('Offensive', description))

    team_fts <- team_plays %>% filter(grepl('Free Throw', description))
    made_fts <- team_fts %>% filter(grepl('made', description))

    team_tos <- team_plays %>% filter(grepl('Turnover', description))

    team_assists <- team_plays %>% filter(grepl('Assist', description))
    team_steals <- team_plays %>% filter(grepl('Steal', description))

    team_pts <- (nrow(made_shots) - nrow(made_3pt)) * 2.0 + nrow(made_3pt) * 3.0 + nrow(made_fts)

    true_shooting_pct <- 100 * (team_pts / (2 * (nrow(team_shots) + 0.44 * nrow(team_fts))))

    eff_fg_pct <- 100 * ((nrow(made_shots) + (0.5 * nrow(made_3pt))) / nrow(team_shots))

    tov_pct <- 100 * (nrow(team_tos) / (nrow(team_shots) + (0.44 * nrow(team_fts)) + nrow(team_tos)))

    ft_to_fga <- (nrow(made_fts) / nrow(team_shots))

    opponent <- if (cleaned_team_name$ESPN_PBP == last(team_plays$home)) last(team_plays$away) else last(team_plays$home)

    table = data.table(
        Name = c(player),
        Team = c(team),
        Opponent = c(opponent),
        Points = team_pts,
        PPS = team_pts / nrow(team_shots),
        Assists = nrow(team_assists),
        Steals = nrow(team_steals),
        FGA = nrow(team_shots),
        FGM = nrow(made_shots),
        '2PA' = nrow(team_shots) - nrow(team_3pt),
        '2PM' = nrow(made_shots) - nrow(made_3pt),
        '3PA' = nrow(team_3pt),
        '3PM' = nrow(made_3pt),
        FTA = nrow(team_fts),
        FTM = nrow(made_fts),
        TSP = true_shooting_pct,
        eFG = eff_fg_pct,
        TRB = nrow(team_rebounds),
        ORB = nrow(off_rebounds),
        DRB = nrow(team_rebounds) - nrow(off_rebounds),
        TO = nrow(team_tos),
        TOV = tov_pct,
        FTpFGA = ft_to_fga
    )
    message(paste("Completed player analysis for player: ", player, sep = ""))
    return(table)
}

retrieve_player_game_data <- function(player, team) {
    message(paste("Beginning game by game analysis for player: ", player, sep = ""))

    cleaned_team_name <- dict[which(dict$ESPN_PBP == team), ]
    if (is.null(cleaned_team_name) || length(cleaned_team_name$ESPN) == 0) {
        message(paste("Bailing out, could not find roster information for ", team, sep = ""))
        return(NULL)
    }

    game_ids <- get_game_ids(cleaned_team_name$ESPN)
    player_data <- data.table()
    for(i in 1:length(game_ids)) {
        game = game_ids[i]
        message(paste0("[",i,"/",length(game_ids),"]"," Getting PbPs for game: ", game, sep = ""))
        if (!any(game==player_data$GameID)) {
            pbp_data = get_pbp_game(game)
            tmp = analyze_player_plays(pbp_data, player, team)
            if (!is.null(tmp)) {
                tmp$GameID = game
                player_data = rbind(player_data, tmp)
            }
        } else {
            message(paste0("[",i,"/",length(game_ids),"]"," Already parsed GameID ",game,", skipping",sep=""))
        }
    }
    message(paste("Finished game by game analysis for player: ", player, sep = ""))
    return(player_data)
}

generate_player_stat_chart <- function(player, team, stat1, stat1_title, stat2, stat2_title, line_color) {
    message(paste("Generating ", stat1, " vs ", stat2, " chart for player: ", player, sep = ""))
    stats <- retrieve_player_game_data(player, team)
    stats[[stat1]] <- factor(stats[[stat1]], levels = stats[[stat1]])
    base = ggplot(stats, aes_string(x=stat1, y=stat2, group=1))

    base + geom_line(color=line_color) + geom_point(color=line_color) + ggtitle(paste0(player," (2019-20)",sep="")) + ylab(stat2_title) + xlab(stat1_title) + geom_hline(yintercept = mean(stats[[stat2]]), color="navy", linetype="dashed")
}

# example: Tracking Michael Devoe's TSP% so far in 2019-20
generate_player_stat_chart("Michael Devoe", "Georgia Tech", "Opponent","Opponent", "TSP","True Shooting %", "#B3A369")
