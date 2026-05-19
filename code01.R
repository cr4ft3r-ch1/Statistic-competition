#install.packages("tidyverse")
#install.packages("modelsummary")
#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
install.packages("maps")
library(tidyverse)
library(readr)
library(modelsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


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

 data_2023 <- data_2023 |>
   dplyr::mutate(
     
     region = dplyr::case_when(
       
       prefecture %in% c(
         "北海道",
         "青森県", "岩手県", "宮城県",
         "秋田県", "山形県", "福島県"
       ) ~ "北海道・東北地方",
       
       prefecture %in% c(
         "新潟県", "富山県", "石川県", "福井県"
       ) ~ "北陸地方",
       
       prefecture %in% c(
         "茨城県", "栃木県", "群馬県",
         "埼玉県", "千葉県", "東京都",
         "神奈川県"
       ) ~ "関東地方",
       
       prefecture %in% c(
         "山梨県", "長野県", "岐阜県",
         "静岡県", "愛知県"
       ) ~ "中部地方",
       
       prefecture %in% c(
         "三重県", "滋賀県", "京都府",
         "大阪府", "兵庫県", "奈良県",
         "和歌山県"
       ) ~ "近畿地方",
       
       prefecture %in% c(
         "鳥取県", "島根県", "岡山県",
         "広島県", "山口県"
       ) ~ "中国地方",
       
       prefecture %in% c(
         "徳島県", "香川県", "愛媛県",
         "高知県"
       ) ~ "四国地方",
       
       prefecture %in% c(
         "福岡県", "佐賀県", "長崎県",
         "熊本県", "大分県", "宮崎県",
         "鹿児島県", "沖縄県"
       ) ~ "九州・沖縄地方",
       
       TRUE ~ NA_character_
     ),
     
     metro_area = dplyr::case_when(
       
       prefecture %in% c(
         "東京都", "神奈川県",
         "埼玉県", "千葉県"
       ) ~ "首都圏",
       
       prefecture %in% c(
         "大阪府", "京都府",
         "兵庫県", "奈良県"
       ) ~ "近畿都市圏",
       
       prefecture %in% c(
         "愛知県", "岐阜県",
         "三重県"
       ) ~ "中京圏",
       
       prefecture %in% c(
         "茨城県", "栃木県", "群馬県",
         "滋賀県", "和歌山県",
         "静岡県"
       ) ~ "都市圏近郊",
       
       TRUE ~ "地方圏"
     )
     
   ) |>
   dplyr::relocate(
     metro_area,
     .after = region
   )
 

 
view# 教育費の分布をプロット
education_distribution <- ggplot2::ggplot() +
   ggplot2::geom_histogram(data = data_2023, 
                           mapping = aes(x = log10(pre_education_expense)), 
                          binwidth = 0.1)
""" 
+
  xlim(3000000,800000000) +
  ylim(0,12)
"""
#予算に占める教育費の割合をプロット
education_percentage <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = data_2023, 
                          mapping = aes(x = (pre_education_expense/pre_total_expenditure)), 
                          binwidth = 0.005)

education_point <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                   mapping = aes(x = log(pre_total_expenditure), 
                                 y = log(pre_education_expense)))

# 教育費の割合(色:地方)
education_rate_vs_total_by_region <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure), 
                                    y = log(pre_education_expense)/log(pre_total_expenditure),
                                    color =region))

# 教育費(対数)の割合(色:都市圏かどうか)
education_rate_vs_total_by_metro_area <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure), 
                                    y = log(pre_education_expense)/log(pre_total_expenditure),
                                    color =metro_area))+
                                  facet_wrap( ~ metro_area )

# 教育費の割合 地方別
education_rate_vs_total_by_region <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure), 
                                    y = log(pre_education_expense)/log(pre_total_expenditure),
                                    color =region) )+ 
                        facet_wrap( ~ region )

#生徒1人当たりの教育費をまとめた列を作る
data_2023 <- data_2023 |>
 dplyr::mutate(
   pre_education_expences_perstudents = (pre_education_expense)/(pre_elementary_school_students +  pre_junior_high_school_students + pre_high_school_students))

# 教育費の割合 地方別
education_perstudent_vs_total_by_region <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure) ,
                                    y = pre_education_expences_perstudents,
                                    color =region) )



# コロプレス図に描画
# 日本地図データ取得
japan_map <- rnaturalearth::ne_states(
  country = "japan",
  scale = 50,
  returnclass = "sf"
)

# 都道府県名を合わせる
japan_map <- japan_map |>
  dplyr::rename(
    prefecture = name_ja
  )

# データ結合
japan_map_data <- japan_map |>
  dplyr::left_join(
    map_data,
    by = "prefecture"
  )

# コロプレス図
education_map <- ggplot2::ggplot(
  japan_map_data
) +
  ggplot2::geom_sf(
    ggplot2::aes(
      fill = education_ratio
    ),
    color = "black",
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "plasma",
    labels = scales::percent
  ) +
  ggplot2::labs(
    title = "Prefectural Education Expenditure Ratio",
    fill = "Education\nRatio"
  ) +
  ggplot2::theme_minimal()

education_map
#いったん諦め






# 中学校に関連して
#　人的資源(教員1人当たり生徒数等を見る)
students_vs_teachers <- ggplot2::ggplot()+
  ggplot2::geom_histogram(data = data_2023,
                          mapping = aes(x = pre_junior_high_school_students/pre_junior_high_school_teachers),
                          binwidth = 0.3)

students_vs_teachers_plot <- ggplot2::ggplot()+
  ggplot2::geom_point(data = data_2023,
                          mapping = aes(x = pre_junior_high_school_students/pre_junior_high_school_teachers,
 y = pre_education_expense/pre_total_expenditure                                       ))




data_2024 <- raw_data_2024 |> 
  dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数")

data_2025 <- raw_data_2025 |> 
  dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
                :"高等学校生徒数")

