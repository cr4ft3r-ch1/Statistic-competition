#install.packages("tidyverse")
#install.packages("modelsummary")
library(tidyverse)
library(readr)
library(modelsummary)


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

#　列名の変更、県ごとのグループ分け
data_2023 <- raw_data_2023 |> 
 dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数") |> 
  dplyr::rename(
    region_code = `地域コード`,
    prefecture = `都道府県`,
    municipality = `市区町村`,
    ordinary_balance_ratio = `経常収支比率（市町村財政）`,
    real_debt_service_ratio = `実質公債費比率（市町村財政）`,
    total_revenue = `歳入決算総額（市町村財政）`,
    local_tax = `地方税（市町村財政）`,
    total_expenditure = `歳出決算総額（市町村財政）`,
    welfare_expense = `民生費（市町村財政）`,
    public_works_expense = `土木費（市町村財政）`,
    education_expense = `教育費（市町村財政）`,
    disaster_recovery_expense = `災害復旧費（市町村財政）`,
    kindergarten_count = `幼稚園数`,
    kindergarten_students = `幼稚園在園者数`,
    elementary_school_count = `小学校数`,
    elementary_school_teachers = `小学校教員数`,
    elementary_school_students = `小学校児童数`,
    junior_high_school_count = `中学校数`,
    junior_high_school_teachers = `中学校教員数`,
    junior_high_school_students = `中学校生徒数`,
    compulsory_education_school_count = `義務教育学校数`,
    compulsory_education_school_teachers = `義務教育学校教員数`,
    compulsory_education_school_primary_students = `義務教育学校前期課程児童数`,
    compulsory_education_school_secondary_students = `義務教育学校後期課程生徒数`,
    high_school_count = `高等学校数`,
    high_school_students = `高等学校生徒数`
  ) |> 
  dplyr::group_by(prefecture) |>
  dplyr::summarise(dplyr::across(c(ordinary_balance_ratio,real_debt_service_ratio),
                   ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"
                  ),
            dplyr::across(where(is.numeric)&
                                    !c(
                                      ordinary_balance_ratio,
                                      real_debt_service_ratio
                                    ),
                                  ~sum(.x, na.rm = TRUE),
                                  .names = "pre_{.col}"
                  )
  )
 modelsummary::datasummary(
   All(data_2023)~ N + Mean + SD + Min + Max,
   data = data_2023,
   title = "表1：記述統計量一覧"
   )
# 教育費の分布をプロット
 ggplot2::ggplot() +
   ggplot2::geom_histogram(data = data_2023, 
                           mapping = aes(x = pre_education_expense)) +
   xlim(3000000,800000000) +
   ylim(0,12)
 
   

data_2024 <- raw_data_2024 |> 
  dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数")

data_2025 <- raw_data_2025 |> 
  dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数")

