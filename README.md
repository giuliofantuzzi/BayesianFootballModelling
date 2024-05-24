# Bayesian Statistics Final Exam

Repository for the "Bayesian Statistics" course, MSc degree in Data Science & AI (University of Trieste)

| Name | Surname | Student ID | UniTs email | Personal email | Course |
|:----:|:-------:|:----------:|:-----------:|:--------------:|:------:|
| Giulio | Fantuzzi | SM3800012 | GIULIO.FANTUZZI@studenti.units.it | giulio.fantuzzi01@gmail.com | DSAI|
---

# About

Predicting football match outcomes has long intrigued statisticians and sports analysts. The inherently random nature of sports events, coupled with various influencing factors like team strength, players performance, and home advantage, presents a challenging yet fascinating problem for statistical modeling.

In previous projects, I have explored goal-based models such as those proposed by *Maher* and *Dixon and Coles*. Maher's model describes the outcome of a match by modeling the goals of the two teams independently using Poisson distributions. Dixon and Coles enhanced this basic model by introducing correlation between the teams' goal-scoring processes and accounting for the home advantage effect. While these models have been widely used due to their simplicity and effectiveness, they rely on the assumption that goals follow a Poisson distribution. This assumption, however, can be questionable in certain leagues where overdispersion —in which the sample variance exceeds the sample mean— has been observed in the number of goals.

To address this limitation, *Karlis and Ntzoufras* proposed an alternative approach by <u>focusing on the goal difference rather than the individual goal counts of each team</u>. Their method utilizes the **Skellam distribution**, which eliminates the need to account for the correlation between teams' scores directly and avoids the assumption of Poisson marginals. Although the Skellam-based model cannot predict exact match scores, it offers valuable insights into the likely goal difference, thereby simplifying the complexity associated with modeling individual goal counts. The application of Bayesian methods in this context allows for the incorporation of prior knowledge and continuous updating of model parameters as new data becomes available, enhancing the predictive power and adaptability of the model.

---

# Project structure

```
📂 BayesianExam/
│ 
├── 📂 data/
│   └──  📂 season_2122/
│         ├── 📊 Bundesliga1_2122.csv
│         ├── 📊 Bundesliga2_2122.csv
│         ├── 📊 Championship_2122.csv
│         ├── 📊 LaLiga1_2122.csv
│         ├── 📊 LaLiga2_2122.csv
│         ├── 📊 Ligue1_2122.csv
│         ├── 📊 Ligue2_2122.csv
│         ├── 📊 PremierLeague_2122.csv
│         ├── 📊 SerieA_2122.csv
│         └── 📊 SerieB_2122.csv         
│ 
├── 📂 estimated_models/ 
│   └──  📂 season_2122/
│        ├── 📂 offline_models/
│        │   ├── 📂 matchday19/
|        |   ├── ...
│        │   └── 📂 matchday38/
│        │
│        └── 📂 online_models/
│            ├── 📂 matchday19/
|            ├── ...
│            └── 📂 matchday38/
│
├── 📂 report/ 
│
├── 📂 stan/ 
|   ├── ⚙️ karlis-ntzoufras.stan
│   └── ⚙️️️ online.stan
|
├── 📂 utils/ 
|   ├── HDA_probabilities.R
|   ├── final_pts_distribution.R
|   ├── get_all_teams_data.R
|   ├── plot_confusionmatrix.R
|   └── plot_parameters_ts.R
|
├── 📂 web-scraping/ 
│   └── 🔍 scraper.py
|
├── BrierScore.R
├── ConfusionMatrix.R
├── DataAnalysis.R
├── DynamicEstimation.R
├── ExploratoryDataAnalysis.R
├── FinalRankingSimulation.R
├── ModelChecking.R
├── OnlineLearning.R
└── Predictions.R
```

This report presents the findings of a comprehensive analysis of <u>Serie A football league data for the 2021-2022 season</u>. Given the inherent complexity of the analysis at hand, my project is structured into several directories, each serving a specific purpose in the data collection, analysis, and modeling process.

1. `data/` is organised in subfolders for specific seasons, containing the raw data collected from [football-data.co.uk](www.football-data.co.uk). 

2. `web-scraping/` contains a python web scraper (`scraper.py`) to automatically retrieve and download datasets for all major European teams. By executing the scraper from the terminal and providing the desired season, users can download datasets for analysis. Example of usage:
    ```bash
    python3 scraper.py <season> #e.g 2122
    ```


3. `stan/` contains the Stan models used for statistical modeling. Two models are included:
    - `karlis-ntzoufras.stan`: the standard version proposed by the authors. From now on in the report, we will refer to it as the "offline" model;
    - `online.stan`: the online-learning framework version of the model.
    
4. `utils/` simply contains some custom helper functions utilized throughout the analysis process

5. `report/` contains the final report file, where the results of the analysis are documented and presented.

6. `estimated_models/` contains the fitted models as `.rds` files. Like the `data/` directory, it is organized in season-specific subfolders. Since I wanted to store the "offline" models separately from the "online" ones, I further increase the depth of my subfolders, and I also stored the models by matchday (I hope it will be intuitive)

In addition to these directories, various `R` scripts are available, each corresponding to different stages of the analysis pipeline. These scripts facilitate data cleaning, exploration, modeling, and visualization, contributing to a structured and reproducible workflow. The following sections detail the methodology employed, the results obtained from statistical modeling, and insights derived from the analysis of Serie A 2021-2022 data.
