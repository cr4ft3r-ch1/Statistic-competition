#install.packages("tidyverse")
library(tidyverse)
library(readr)


raw_data_2023 <- read_csv(
  "SSDSE-A-2023.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)
raw_data_2024 <- read_csv(
  "SSDSE-A-2024.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)
raw_data_2025 <- read_csv(
  "SSDSE-A-2025.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)


data_2023 <- raw_data_2023 |> 
 dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数")

data_2024 <- raw_data_2024 |> 
  dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数")

data_2025 <- raw_data_2025 |> 
  dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数")

