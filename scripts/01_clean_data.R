
library(httr)
library(jsonlite)
library(tidyverse)
library(caret)
library(smotefamily)

# 2. Clean data
clean_data <- function(df, cfg) {
  df2 = df %>%
    rename(
      Classification   = status,
      State            = state,
      Country_Area     = country,
      Posted_Citations = voluntary_mandated,
      Project_Area     = product_description,
      Product_Type     = product_type,
      Report_Date      = report_date
    ) %>%
    transmute(
      Classification   = factor(Classification),
      State            = factor(State),
      Country_Area     = factor(Country_Area),
      Posted_Citations = factor(Posted_Citations),
      Project_Area     = factor(Project_Area),
      Product_Type     = factor(Product_Type),
      Fiscal_Year      = as.integer(substr(Report_Date, 1, 4))
    ) %>%
    drop_na() %>%
    group_by(Classification) %>%
    filter(n() >= 2) %>%
    ungroup() -> df_clean

  levels(df_clean$Classification) <- make.names(levels(df_clean$Classification))
  df_clean

    dir.create(dirname(cfg$paths$clean_data), recursive=TRUE, showWarnings=FALSE)
    readr::write_csv(df2, cfg$paths$clean_data)
    df2
}

