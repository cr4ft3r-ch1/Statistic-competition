#install.packages("tidyverse")
#install.packages("modelsummary")
#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("maps")
#install.packages("GGally")
#install.packages("fixest")
library(tidyverse)   
library(modelsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(GGally)
library(patchwork)
library(fixest)

# 1. 住基人口データの読み込みとコードの抽出・加工
pop_data_2016 <- read_csv("市区町村別人口_2016.csv", skip = 3)|>
  dplyr::mutate(
    # 団体コード（6桁）の「1文字目から5文字目まで」を抽出し、先頭に"R"を付ける
    "地域コード" = paste0("R", stringr::str_sub(`団体コード`, 1, 5))
  ) |>
  dplyr::select(
    "地域コード", 
    "population" = `人`  # 列名は「人」
  )
pop_data_2017 <- read_csv("市区町村別人口_2017.csv", skip = 3)|>
  dplyr::mutate(
    # 団体コード（6桁）の「1文字目から5文字目まで」を抽出し、先頭に"R"を付ける
    "地域コード" = paste0("R", stringr::str_sub(`団体コード`, 1, 5))
  ) |>
  dplyr::select(
    "地域コード", 
    "population" = `人`  # 列名は「人」
  )
pop_data_2018 <- read_csv("市区町村別人口_2018.csv", skip = 3)|>
  dplyr::mutate(
    # 団体コード（6桁）の「1文字目から5文字目まで」を抽出し、先頭に"R"を付ける
    "地域コード" = paste0("R", stringr::str_sub(`団体コード`, 1, 5))
  ) |>
  dplyr::select(
    "地域コード", 
    "population" = `人`  # 列名は「人」
  )
pop_data_2019 <- read_csv("市区町村別人口_2019.csv", skip = 3)|>
  dplyr::mutate(
    # 団体コード（6桁）の「1文字目から5文字目まで」を抽出し、先頭に"R"を付ける
    "地域コード" = paste0("R", stringr::str_sub(`団体コード`, 1, 5))
  ) |>
  dplyr::select(
    "地域コード", 
    "population" = `人`  # 列名は「人」
  )
pop_data_2020 <- read_csv("市区町村別人口_2020.csv", skip = 3)|>
  dplyr::mutate(
    # 団体コード（6桁）の「1文字目から5文字目まで」を抽出し、先頭に"R"を付ける
    "地域コード" = paste0("R", stringr::str_sub(`団体コード`, 1, 5))
  ) |>
  dplyr::select(
    "地域コード", 
    "population" = `人`  # 列名は「人」
  )
pop_data_2021 <- read_csv("市区町村別人口_2021.csv", skip = 5) |>
  dplyr::mutate(
    # 団体コード（6桁）の「1文字目から5文字目まで」を抽出し、先頭に"R"を付ける
    "地域コード" = paste0("R", stringr::str_sub(`団体コード`, 1, 5))
  ) |>
  dplyr::select(
    "地域コード", 
    "population" = `人`  # 列名は「人」
  )

# 本データの読み込み

# data_2018 <- read_csv(
#   "SSDSE-A-2018.csv",
#   locale = locale(encoding = "CP932"),
#   skip = 2
# )|> dplyr::mutate(year = 2018,
#                   education_year = 2015)

data_2019 <- read_csv(
  "SSDSE-A-2019.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)|> dplyr::mutate(year = 2019,
                  education_year = 2016,
                  student_year = 2017)|>
  dplyr::inner_join(pop_data_2016, by = "地域コード")
data_2020 <- read_csv(
  "SSDSE-A-2020.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)|> dplyr::mutate(year = 2020,
                  education_year = 2017,
                  student_year = 2018)|>
  dplyr::inner_join(pop_data_2017, by = "地域コード")
data_2021 <- read_csv(
  "SSDSE-A-2021.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)|> dplyr::mutate(year = 2021,
                  education_year = 2018,
                  student_year = 2019)|>
  dplyr::inner_join(pop_data_2018, by = "地域コード")
data_2022 <- read_csv(
  "SSDSE-A-2022.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)|> dplyr::mutate(year = 2021,
                  student_year = 2020)
data_2023 <- read_csv(
  "SSDSE-A-2023.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)|> dplyr::mutate(year = 2023,
                  education_year = 2019,
                  student_year = 2021)|>
  dplyr::inner_join(pop_data_2019, by = "地域コード")
data_2024 <- read_csv(
  "SSDSE-A-2024.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)|> dplyr::mutate(year = 2024,
                  education_year = 2020,
                  student_year = 2022)|>
  dplyr::inner_join(pop_data_2020, by = "地域コード")

data_2025 <- read_csv(
  "SSDSE-A-2025.csv",
  locale = locale(encoding = "CP932"),
  skip = 2
)|> dplyr::mutate(year = 2025,
                  education_year = 2021,
                  student_year = 2023)|>
  dplyr::inner_join(pop_data_2021, by = "地域コード")


# # --- 年度ごとの設定を tibble で定義 ---
# ssdse_config <- tibble::tribble(
#   ~file_year, ~year, ~education_year, ~student_year, ~pop_year,
#   2019,       2019,  2016,            2017,          "2016",
#   2020,       2020,  2017,            2018,          "2017",
#   2021,       2021,  2018,            2019,          "2018",
#   2022,       2021,  NA,              2020,          NA,
#   2023,       2023,  2019,            2021,          "2019",
#   2024,       2024,  2020,            2022,          "2020",
#   2025,       2025,  2021,            2023,          "2021"
# )


# 関数の定義（パッケージ化）
add_student_number <- function(base_data, target_year_data) {
  
  # 必要な列だけを計算して切り出す
  clean_target_data <- target_year_data |> 
    dplyr::mutate(student_number = 小学校児童数 + 中学校生徒数,
                  teacher_number = 小学校教員数 + 中学校教員数) |> 
    dplyr::select(地域コード, student_number, teacher_number)
  
  # ベースとなるデータに結合して返す
  result_data <- base_data |> 
    dplyr::left_join(clean_target_data, by = "地域コード") |> 
    dplyr::select(-student_year) |> 
    dplyr::rename(new_year = education_year)
  
  return(result_data)
}

data_2021_merged <- add_student_number(data_2025,data_2023)
data_2020_merged <- add_student_number(data_2024,data_2022)
data_2019_merged <- add_student_number(data_2023,data_2021)
data_2018_merged <- add_student_number(data_2021,data_2020)
data_2017_merged <- add_student_number(data_2020,data_2019)
#data_2016_merged <- add_student_number(data_2019,data_2018)






#パネルデータ化
# 1. 関数 f の定義（市区町村レベル・パネルデータ対応版）
f_muni <- function(x) {
  x |> 
    dplyr::select(
      population,new_year, year, student_number,teacher_number,# 追加: 結合時に付与した年度列を残す
      "地域コード":"市区町村",
      "経常収支比率（市町村財政）":"高等学校生徒数"
    ) |> 
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
      high_school_count = `高等学校数`,
      high_school_students = `高等学校生徒数`
    ) |> 
    # summariseを削除し、直接mutateで指標を計算する
    dplyr::mutate(
      region = dplyr::case_when(
        prefecture %in% c("北海道", "青森県", "岩手県", "宮城県", "秋田県", "山形県", "福島県") ~ "北海道・東北地方",
        prefecture %in% c("新潟県", "富山県", "石川県", "福井県") ~ "北陸地方",
        prefecture %in% c("茨城県", "栃木県", "群馬県", "埼玉県", "千葉県", "東京都", "神奈川県") ~ "関東地方",
        prefecture %in% c("山梨県", "長野県", "岐阜県", "静岡県", "愛知県") ~ "中部地方",
        prefecture %in% c("三重県", "滋賀県", "京都府", "大阪府", "兵庫県", "奈良県", "和歌山県") ~ "近畿地方",
        prefecture %in% c("鳥取県", "島根県", "岡山県", "広島県", "山口県") ~ "中国地方",
        prefecture %in% c("徳島県", "香川県", "愛媛県", "高知県") ~ "四国地方",
        prefecture %in% c("福岡県", "佐賀県", "長崎県", "熊本県", "大分県", "宮崎県", "鹿児島県", "沖縄県") ~ "九州・沖縄地方",
        TRUE ~ NA_character_
      ),
      metro_area = dplyr::case_when(
        prefecture %in% c("東京都", "神奈川県", "埼玉県", "千葉県") ~ "首都圏",
        prefecture %in% c("大阪府", "京都府", "兵庫県", "奈良県") ~ "近畿都市圏",
        prefecture %in% c("愛知県", "岐阜県", "三重県") ~ "中京圏",
        prefecture %in% c("茨城県", "栃木県", "群馬県", "滋賀県", "和歌山県", "静岡県") ~ "都市圏近郊",
        TRUE ~ "地方圏"
      )
    ) |>
    dplyr::relocate(metro_area, .after = region) |> 
    dplyr::mutate(
      education_expenses_perstudents = if_else(
        student_number == 0, 
        NA_real_,
        education_expense / (student_number)
    )
    )
}

# 縦方向への結合後に、整形関数を一括適用する
#panel_data_muni <- bind_rows(data_2016_merged, data_2017_merged,data_2018_merged, data_2019_merged, data_2020_merged,data_2021_merged) |> 
#  f_muni()
panel_data_muni <- bind_rows(data_2017_merged,data_2018_merged, data_2019_merged, data_2020_merged,data_2021_merged) |> 
  f_muni()
# 県で分ける
panel_data_pre <- panel_data_muni |> 
  dplyr::group_by(prefecture,new_year) |> 
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
                
  ),.groups = "drop"
  ) |> 
  dplyr::mutate(
    region = dplyr::case_when(
      prefecture %in% c("北海道", "青森県", "岩手県", "宮城県", "秋田県", "山形県", "福島県") ~ "北海道・東北地方",
      prefecture %in% c("新潟県", "富山県", "石川県", "福井県") ~ "北陸地方",
      prefecture %in% c("茨城県", "栃木県", "群馬県", "埼玉県", "千葉県", "東京都", "神奈川県") ~ "関東地方",
      prefecture %in% c("山梨県", "長野県", "岐阜県", "静岡県", "愛知県") ~ "中部地方",
      prefecture %in% c("三重県", "滋賀県", "京都府", "大阪府", "兵庫県", "奈良県", "和歌山県") ~ "近畿地方",
      prefecture %in% c("鳥取県", "島根県", "岡山県", "広島県", "山口県") ~ "中国地方",
      prefecture %in% c("徳島県", "香川県", "愛媛県", "高知県") ~ "四国地方",
      prefecture %in% c("福岡県", "佐賀県", "長崎県", "熊本県", "大分県", "宮崎県", "鹿児島県", "沖縄県") ~ "九州・沖縄地方",
      TRUE ~ NA_character_
    ),
    metro_area = dplyr::case_when(
      prefecture %in% c("東京都", "神奈川県", "埼玉県", "千葉県") ~ "首都圏",
      prefecture %in% c("大阪府", "京都府", "兵庫県", "奈良県") ~ "近畿都市圏",
      prefecture %in% c("愛知県", "岐阜県", "三重県") ~ "中京圏",
      prefecture %in% c("茨城県", "栃木県", "群馬県", "滋賀県", "和歌山県", "静岡県") ~ "都市圏近郊",
      TRUE ~ "地方圏"
    )
  ) |>
  dplyr::relocate(metro_area, .after = region)|>
  dplyr::mutate(
    pre_education_expenses_perstudents = pre_education_expense / pre_student_number,
    metro_dummy = dplyr::case_when(
      metro_area %in% "地方圏" ~ 0,
      TRUE ~ 1
    )
  )

# final_data は前回作成した縦持ち(Long型)のパネルデータと仮定
diff_data <- panel_data_pre |>
  # 1. 必ず地域（市区町村や都道府県）ごとにグループ化する
  dplyr::group_by(prefecture) |>
  # 2. 年度順に昇順で並び替える（.by_group = TRUE でグループ内ソート）
  dplyr::arrange(new_year, .by_group = TRUE) |>
  dplyr::mutate(
    # 単純差分（今年の生徒数 - 昨年の生徒数）
    diff_student = pre_student_number - dplyr::lag(pre_student_number),
    
    # 対数差分による変化率の計算
    # log() を用いることで、計量経済学的に扱いやすい変化率になる
    log_diff_student = log(pre_student_number) - log(dplyr::lag(pre_student_number))
  ) |>
  # 3. グループ化を解除して安全な状態に戻す
  dplyr::ungroup()

# 空間的な要素を足す

# 1. Shapefileの読み込み
# ※日本の公的機関（国土交通省やe-Stat）のデータは文字コードがShift-JIS(CP932)であることが多いため、エンコーディングを指定して文字化けを防ぐ。
map_data <- sf::st_read("N03-20250101_14.shp", options = "ENCODING=CP932")

# 2. 読み込んだデータの中身（列名）を確認する
# ここで「市町村コード（5桁）」が入っている列の名前を特定する
print(colnames(map_data))

# 3. 【仮説】市町村コードの列名が "N03_007" だった場合の整形処理
# ※国土数値情報のデータ等では "N03_007" や "KEY_CODE" といった列名になっていることが多い。実際の列名に合わせて変更すること。
map_data_formatted <- map_data |>
  dplyr::mutate(
    # パネルデータ（final_panel_data）の仕様に合わせて、先頭に "R" を付与する
    region_code = paste0("R", as.character(N03_007))
  ) |>
  # 必要な列（作成した region_code と、地図情報である geometry）だけを残して軽くする
  dplyr::select(region_code, geometry)




