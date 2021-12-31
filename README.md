# Big Data Bowl 2022
Repo for the NFL Big Data Bowl 2022 competition

Need to have downloaded the 2022 NFL Big Data Bowl data from [Kaggle](https://www.kaggle.com/c/nfl-big-data-bowl-2022)

PROCESS DATA:

Rscript R/load_data.R --season 2018 

FIELD CONTROL MODEL:

Rscript R/field_control.R --season 2018 --rerun_cones TRUE --rerun_control TRUE

COMBINE TRACKING DATA:

Rscript R/punts.R --start_season 2018 --end_season 2020

PLAY-LEVEL RETURN PROBABILITY:

Rscript R/model_train/play_level_model_train.R

PLAYER-LEVEL RETURN OUTCOME:

Rscript R/model_train/punt_outcome_model_train.R --tune_lin TRUE --tune_xgb TRUE --blend TRUE

PREDICT:

Rscript R/model_predict.R --retrain TRUE

RETURN YARDS:

return_yards.R


