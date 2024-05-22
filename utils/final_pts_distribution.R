final_pts_distribution<- function(teams,start=20,end=38,
                             n_chains=4,n_iters=11000,n_warmup=1000,
                             models_dir=NULL) {
  "
  Function to simulate the points at the end of the season basing on coef estimates
  Returns a df s.t.
    - each column is associated to a team
    - nrows: n_chains*(n_iters-n_warmup) to be friendly with the MCMC posteriors
  "
  if(is.null(models_dir) || !file.exists(models_dir)){
    stop("Error! Models directory path not provided or does not exist")
  }
  #-----------------------------------------------------------------------------
  # Rankings after the first half of the league
  n_teams=length(teams)
  teams_pts <- data.frame(matrix(NA,nrow = n_chains*(n_iters-n_warmup),ncol=n_teams))
  colnames(teams_pts)=teams
  for (t in 1:n_teams){
    n_wins= SerieA_data[1:190,] %>% 
      filter(((ht==t) &(FTHG>FTAG)) | ((at==t) & (FTHG<FTAG)))%>% nrow() 
    n_draws= SerieA_data[1:190,] %>% 
      filter(((ht==t) | (at==t)) & (FTHG == FTAG))%>% nrow() 
    teams_pts[,t]= 3*n_wins+n_draws
  }
  #-----------------------------------------------------------------------------
  # Iteration over the 2nd half
  for (m in start:end){
    cat(paste0("...Simulation of matchday n. ",m,"...\n"))
    #...........................................................................
    # (1) Get the current matchday to predict
    test_set=SerieA_data[(10*m -9):(10*m),]
    test_set=na.omit(test_set) #just to manage postponed matches in last batch
    #...........................................................................
    # (2) Load the most recent model and some of its stuff
    cat("...Loading the model and retrieving useful info...\n")
    load(paste0(models_dir,"matchday",m-1,"/KN_matchday",m-1,".rds"))
    posterior<- as.array(KN_model)
    n_iters=dim(posterior)[1]
    n_chains=dim(posterior)[2]
    mu = posterior[,,"mu"]
    home=posterior[,,"home_advantage"]
    p = posterior[,,"p"]
    #...........................................................................
    # (3) Make predictions
    cat("...Computing predictions and updating rankings...\n")
    for(mm in 1:nrow(test_set)){
      ht=test_set$ht[mm]
      at=test_set$at[mm]
      attH=posterior[,,paste0("att[",ht,"]")]
      defH=posterior[,,paste0("def[",ht,"]")]
      attA=posterior[,,paste0("att[",at,"]")]
      defA=posterior[,,paste0("def[",at,"]")]
      theta_H = exp(mu+home+attH+defA)
      theta_A = exp(mu+attA+defH)
      GD<- mapply(my_rzeroinflatedskellam, 1,theta_H, theta_A,p)
      #.........................................................................
      # (4) Calculate points according to predicted goal diffs
      pts_ht = ifelse(GD > 0, 3, ifelse(GD < 0, 0, 1))
      pts_at = ifelse(GD < 0, 3, ifelse(GD > 0, 0, 1))
      #.........................................................................
      # (5) Sum them to the previous points
      teams_pts[,ht] = teams_pts[,ht]+pts_ht
      teams_pts[,at] = teams_pts[,at]+pts_at
      #.........................................................................
    }
    cat("-------------------------------------------------\n")
  }
  #-----------------------------------------------------------------------------
  # return df
  return(teams_pts)
}
