library(ncaahoopR)
library(stringr)
library(data.table)

shot_types = c('Jumper','Dunk','Layup','Three Point Jumper')
rebound_types = c('Defensive Rebound','Offensive Rebound')

analyze_team_plays <- function(team, pbp) {
    team_roster <- get_roster(team)
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
        name = c(team),
        plays = nrow(team_plays),
        points = team_pts,
        PPS = team_pts / nrow(team_shots),
        assists = nrow(team_assists),
        steals = nrow(team_steals),
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
    
    return(table)
}

generate_box_score <- function(game_id, home_team, away_team) {
    plays <- get_pbp_game(game_id)
    selected_team <- home_team
    selected_opponent <- away_team
    
    selected_team_stats <- analyze_team_plays(selected_team, plays)
    selected_opponent_stats <- analyze_team_plays(selected_opponent, plays)
    
    game_stats = rbind(selected_team_stats, selected_opponent_stats)
    
    sel_team_stat_pack = (selected_team_stats$FGM + (0.4 * selected_team_stats$FTA) - ((1.07 * (selected_team_stats$ORB / (selected_team_stats$ORB + selected_opponent_stats$DRB))) * (selected_team_stats$FGA - selected_team_stats$FGM)) + selected_team_stats$TO)
    sel_opp_stat_pack = (selected_opponent_stats$FGM + (0.4 * selected_opponent_stats$FTA) - ((1.07 * (selected_opponent_stats$ORB / (selected_opponent_stats$ORB + selected_team_stats$DRB))) * (selected_opponent_stats$FGA - selected_opponent_stats$FGM)) + selected_opponent_stats$TO)
    
    game_stats$possessions = round(0.5 * (sel_team_stat_pack + sel_opp_stat_pack))
    game_stats$PPP = game_stats$points / game_stats$possessions
    
    sel_team_orb = 100 * (selected_team_stats$ORB / (selected_team_stats$ORB + selected_opponent_stats$DRB))
    sel_team_drb = 100 * (selected_team_stats$DRB / (selected_team_stats$DRB + selected_opponent_stats$ORB))
    
    sel_opp_orb = 100 * (selected_opponent_stats$ORB / (selected_opponent_stats$ORB + selected_team_stats$DRB))
    sel_opp_drb = 100 * (selected_opponent_stats$DRB / (selected_opponent_stats$DRB + selected_team_stats$ORB))
    
    game_stats$ORBP = c(sel_team_orb,sel_opp_orb)
    game_stats$DRBP = c(sel_team_drb,sel_opp_drb)
    
    game_stats$StealRate = 100 * (game_stats$steals / game_stats$possessions)
    
    game_stats$BCI = (game_stats$assists + game_stats$steals) / game_stats$TO
    
    return(game_stats)
}

box <- generate_box_score(game_id = 401168216, home_team = 'Georgia Tech', away_team = 'Arkansas')
