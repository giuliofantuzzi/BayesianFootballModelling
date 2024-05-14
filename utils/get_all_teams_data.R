library(rstan)
get_all_teams_data <- function(teams_list, start = 19, end = 35,models_dir_path=NULL) {
  
  if(is.null(models_dir_path) || !file.exists(models_dir_path)){
    stop("Error! Models directory path not provided or does not exist")
  }
  
  all_teams_data <- data.frame(Team=character(),
                               Matchday=integer(),
                               Mean=double(),
                               Sd=double(),
                               Lower=double(),
                               Upper=double(),
                               Type=character())
  for (m in start:end) {
    load(paste0(models_dir_path,"/matchday", m, "/KN_matchday", m, ".rds"))
    posterior <- as.array(KN_model)
    for (t in teams_list) {
      team_idx <- match(t, teams_list)
      att <- posterior[, , paste0("att[", team_idx, "]")]
      def <- posterior[, , paste0("def[", team_idx, "]")]
      team_data <- data.frame(
        "Team" = rep(t,2),
        "Matchday" =rep(m,2),
        "Mean" = c(mean(att), mean(def)),
        "Sd" =  c(sd(att), sd(def)),
        "Lower" = c(quantile(att,0.025), quantile(def,0.025)),
        "Upper" = c(quantile(att,0.975), quantile(def,0.975)),
        "Type" = c("att", "def")
      )
      all_teams_data <- rbind(all_teams_data, team_data)
    }
  }
  return(all_teams_data)
}
