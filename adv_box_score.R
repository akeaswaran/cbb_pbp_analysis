library(ncaahoopR)
library(stringr)
library(data.table)
data("ids")
data("dict")

shot_types = c('Jumper','Dunk','Layup','Three Point Jumper')
rebound_types = c('Defensive Rebound','Offensive Rebound')

calculate_four_factors_sum <- function(box_row) {
    shoot_factor = 0.4
    turnover_factor = -0.25
    rebound_factor = 0.2
    ft_factor = 0.15

    shoot = (shoot_factor * box_row$eFG)
    turnover = (turnover_factor * box_row$TOV)
    rebound = ((rebound_factor / 2) * box_row$ORB) + ((rebound_factor / 2) * box_row$DRB)
    fts = (ft_factor * box_row$FTpFGA)

    weighted_sum = sum(shoot, turnover, rebound, fts)
    return(weighted_sum)
}

analyze_team_plays <- function(team, pbp) {
    message(paste("Beginning game analysis for team: ", team, sep = ""))

    cleaned_team_name <- dict[which(dict$ESPN_PBP == team), ]
    if (is.null(cleaned_team_name) || length(cleaned_team_name$ESPN) == 0) {
        message(paste("Bailing out, could not find roster information for ", team, sep = ""))
        return(NULL)
    }
    team_roster <- get_roster(cleaned_team_name$ESPN)
    team_plays <- pbp %>% filter(grepl(paste(team_roster$name, collapse="|"), description))
    
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
    
    table = data.table(
        Name = c(team),
        # plays = nrow(team_plays),
        Points = team_pts,
        PPS = team_pts / nrow(team_shots),
        Assists = nrow(team_assists),
        Steals = nrow(team_steals),
        FGA = nrow(team_shots),
        FGM = nrow(made_shots),
        TPA = nrow(team_3pt),
        TPM = nrow(made_3pt),
        FTA = nrow(team_fts),
        FTM = nrow(made_fts),
        TSP = true_shooting_pct,
        eFG = eff_fg_pct,
        TRB = nrow(team_rebounds),
        ORB = nrow(off_rebounds),
        DRB = nrow(team_rebounds) - nrow(off_rebounds),
        TO = nrow(team_tos),
        TOV = tov_pct,
        FTpFGA = ft_to_fga,
        AssistRate = 100 * (nrow(team_assists) / nrow(team_shots))
    )
    message(paste("Completed game analysis for team: ", team, sep = ""))
    return(table)
}

generate_box_score <- function(game_id, home_team = NULL, away_team = NULL) {
    if (is.null(game_id)) {
        return(NULL)
    }
    plays <- get_pbp_game(game_id)
    if (is.null(plays) || nrow(plays) == 0) {
        return(NULL)
    }

    first_play = first(plays)
    selected_team <- ifelse(is.null(home_team), first_play$home, home_team)
    selected_opponent <- ifelse(is.null(away_team), first_play$away, away_team)

    selected_team_stats <- analyze_team_plays(selected_team, plays)
    selected_opponent_stats <- analyze_team_plays(selected_opponent, plays)

    if (is.null(selected_team_stats) || is.null(selected_opponent_stats)) {
        return(NULL)
    }

    game_stats = rbind(selected_team_stats, selected_opponent_stats)

    sel_team_stat_pack = (selected_team_stats$FGM + (0.4 * selected_team_stats$FTA) - ((1.07 * (selected_team_stats$ORB / (selected_team_stats$ORB + selected_opponent_stats$DRB))) * (selected_team_stats$FGA - selected_team_stats$FGM)) + selected_team_stats$TO)
    sel_opp_stat_pack = (selected_opponent_stats$FGM + (0.4 * selected_opponent_stats$FTA) - ((1.07 * (selected_opponent_stats$ORB / (selected_opponent_stats$ORB + selected_team_stats$DRB))) * (selected_opponent_stats$FGA - selected_opponent_stats$FGM)) + selected_opponent_stats$TO)

    game_stats$Possessions = round(0.5 * (sel_team_stat_pack + sel_opp_stat_pack))
    game_stats$PPP = game_stats$Points / game_stats$Possessions
    game_stats$PPM = game_stats$Points / (max(plays$secs_remaining_absolute) / 60)

    sel_team_orb = 100 * (selected_team_stats$ORB / (selected_team_stats$ORB + selected_opponent_stats$DRB))
    sel_team_drb = 100 * (selected_team_stats$DRB / (selected_team_stats$DRB + selected_opponent_stats$ORB))

    sel_opp_orb = 100 * (selected_opponent_stats$ORB / (selected_opponent_stats$ORB + selected_team_stats$DRB))
    sel_opp_drb = 100 * (selected_opponent_stats$DRB / (selected_opponent_stats$DRB + selected_team_stats$ORB))

    game_stats$ORBP = c(sel_team_orb,sel_opp_orb)
    game_stats$DRBP = c(sel_team_drb,sel_opp_drb)

    game_stats$StealRate = 100 * (game_stats$Steals / game_stats$Possessions)

    game_stats$BCI = (game_stats$Assists + game_stats$Steals) / game_stats$TO

    avg_win_prob <- average_win_prob(game_id) * 100
    game_stats$AvgWinProb <- c(avg_win_prob, 100 - avg_win_prob)

    ff_home = calculate_four_factors_sum(selected_team_stats)
    ff_away = calculate_four_factors_sum(selected_opponent_stats)
    game_stats$FFSum <- c(ff_home, ff_away)
    game_stats$FFDiff <- c(ff_home - ff_away, ff_away - ff_home)

    game_stats$PointDiff <- c(selected_team_stats$Points - selected_opponent_stats$Points, selected_opponent_stats$Points - selected_team_stats$Points)

    return(game_stats)
}

box <- generate_box_score(game_id = 401168216)

