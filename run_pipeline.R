

library(yaml)
library(dplyr)
library(readr)
library(glmnet)

cfg <- yaml::read_yaml("config.yml")

source("scripts/00_fetch.R")
source("scripts/01_clean_data.R")
source("scripts/02_encode.R")
source("scripts/03_train.R")
source("scripts/04_save_Model.R")

df_raw    <- fetch_fda_data(cfg)
df_clean  <- clean_data(df_raw, cfg)
bal       <- encode_and_balance(df_clean, cfg)
glm_model <- train_models(bal, cfg)
finalaize(cfg)