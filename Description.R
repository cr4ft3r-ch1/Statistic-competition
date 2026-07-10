source("data_cleaning.R")


# 2. グラフの作成(折れ線グラフ)
ggplot2::ggplot(panel_data_pre, aes(x = new_year, y = log(pre_education_expenses_perstudents), color = prefecture)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "地域別の推移", x = "年度", y = "値")+
  facet_wrap( ~ region )

ggplot2::ggplot(panel_data_muni, aes(x = year, y = log(education_expenses_perstudents), color = prefecture)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "地域別の推移", x = "年度", y = "値")

#差分を計算する
ggplot2::ggplot(diff_data, aes(x = new_year, y = diff_student, color = prefecture)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "地域別の推移", x = "年度", y = "値")+
  facet_wrap( ~ region )


# 時間固定効果を除去したトレンドを見る

#　市区町村単位

# 1. 全期間の総合平均を計算
overall_mean <- mean(panel_data_muni$education_expenses_perstudents, na.rm = TRUE)

# 2. 時間固定効果を除去した新しい列を作成
plot_data <- panel_data_muni |>
  # 必ず「年（year）」でグループ化する
  dplyr::group_by(new_year) |>
  dplyr::mutate(
    # その年の全国平均を計算
    year_mean = mean(education_expenses_perstudents, na.rm = TRUE),
    
    # 時間固定効果を除去（元の値 - 年平均 + 全体平均）
    edu_exp_adjusted = education_expenses_perstudents - year_mean + overall_mean
  ) |>
  dplyr::ungroup()

# 3. グラフ描画（Before / After の比較）

# Before: 元のデータ（右肩上がりのマクロトレンドが見えるはず）
plot_before <- ggplot(plot_data, aes(x = factor(new_year), y = education_expenses_perstudents)) +
  geom_boxplot(fill = "lightpink", outlier.shape = NA) + # 外れ値は非表示にして箱を見やすくする
  coord_cartesian(ylim = quantile(plot_data$education_expenses_perstudents, c(0.05, 0.95), na.rm=TRUE)) + # 上下5%をカットしてズーム
  labs(title = "Before: 元の1人当たり教育費", x = "年度", y = "1人当たり教育費")

# After: 時間固定効果除去後（トレンドが平坦化され、純粋な分散だけが残るはず）
plot_after <- ggplot(plot_data, aes(x = factor(new_year), y = edu_exp_adjusted)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  coord_cartesian(ylim = quantile(plot_data$edu_exp_adjusted, c(0.05, 0.95), na.rm=TRUE)) +
  labs(title = "After: 時間固定効果 除去後", x = "年度", y = "調整後 1人当たり教育費")

# patchworkを使って左右に並べて表示
plot_before + plot_after





# 県単位
# 1. 全期間の総合平均を計算
overall_mean <- mean(panel_data_pre$pre_education_expenses_perstudents, na.rm = TRUE)

# 2. 時間固定効果を除去した新しい列を作成
plot_data <- panel_data_pre |>
  # 必ず「年（year）」でグループ化する
  dplyr::group_by(new_year) |>
  dplyr::mutate(
    # その年の全国平均を計算
    year_mean = mean(pre_education_expenses_perstudents, na.rm = TRUE),
    
    # 時間固定効果を除去（元の値 - 年平均 + 全体平均）
    pre_edu_exp_adjusted = pre_education_expenses_perstudents - year_mean + overall_mean
  ) |>
  dplyr::ungroup()

# 3. グラフ描画（Before / After の比較）

# Before: 元のデータ（右肩上がりのマクロトレンドが見えるはず）
plot_before <- ggplot(plot_data, aes(x = factor(new_year), y = pre_education_expenses_perstudents)) +
  geom_boxplot(fill = "lightpink", outlier.shape = NA) + # 外れ値は非表示にして箱を見やすくする
  #coord_cartesian(ylim = quantile(plot_data$education_expenses_perstudents, c(0.05, 0.95), na.rm=TRUE)) + # 上下5%をカットしてズーム
  coord_cartesian(ylim = c(350, 900)) +
  theme_minimal() +
  labs(title = "Before: 元の1人当たり教育費", x = "年度", y = "1人当たり教育費")

# After: 時間固定効果除去後（トレンドが平坦化され、純粋な分散だけが残るはず）
plot_after <- ggplot(plot_data, aes(x = factor(new_year), y = pre_edu_exp_adjusted)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  #coord_cartesian(ylim = quantile(plot_data$edu_exp_adjusted, c(0.05, 0.95), na.rm=TRUE)) +
  coord_cartesian(ylim = c(350, 900)) +
  theme_minimal() +
  labs(title = "After: 時間固定効果 除去後", x = "年度", y = "調整後 1人当たり教育費")

# patchworkを使って左右に並べて表示
plot_before + plot_after


# 固定効果を除去したグラフ(県単位)を見るとなにかありそうなのでそれをみたい
ggplot2::ggplot(plot_data, aes(x = new_year, y = log(pre_edu_exp_adjusted), color = prefecture)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "地域別の推移", x = "年度", y = "値") +
  facet_wrap( ~ region )







  








modelsummary::datasummary(
   All(data_2023)~ N + Mean + SD + Min + Max,
   data = data_2023,
   title = "表1：記述統計量一覧"
 )
 
# 教育費の分布をプロット
education_distribution <- ggplot2::ggplot() +
   ggplot2::geom_histogram(data = data_2023, 
                           mapping = aes(x = log10(pre_education_expense)), 
                          binwidth = 0.1)
                          

#予算に占める教育費の割合をプロット
education_percentage <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = data_2023, 
                          mapping = aes(x = (pre_education_expense/pre_total_expenditure)), 
                          binwidth = 0.005)
plot(education_percentage)

education_point <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                   mapping = aes(x = log(pre_total_expenditure), 
                                 y = log(pre_education_expense)))
plot(education_point)
# 教育費の割合(色:地方)
education_rate_vs_total_by_region <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure), 
                                    y = log(pre_education_expense)/log(pre_total_expenditure),
                                    color =region))
plot(education_rate_vs_total_by_region)

# 教育費の割合 地方別(facetで分割)
education_rate_vs_total_by_region <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure), 
                                    y = log(pre_education_expense)/log(pre_total_expenditure),
                                    color =region) )+ 
  facet_wrap( ~ region )
plot(education_rate_vs_total_by_region)

# 教育費(対数)の割合(色:都市圏かどうか)
education_rate_vs_total_by_metro_area <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure), 
                                    y = log(pre_education_expense)/log(pre_total_expenditure),
                                    color =metro_area))+
                                  facet_wrap( ~ metro_area )
plot(education_rate_vs_total_by_metro_area)


#生徒1人当たりの教育費をまとめた列を作る
data_2023 <- data_2023 |>
 dplyr::mutate(
   pre_education_expenses_perstudents = (pre_education_expense)/(pre_elementary_school_students +  pre_junior_high_school_students + pre_high_school_students))

# 教育費の割合 地方別
education_perstudent_vs_total_by_region <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure) ,
                                    y = pre_education_expenses_perstudents,
                                    color =region) )

plot(education_perstudent_vs_total_by_region)
# 教育費の割合 地方別(facetで分割)
education_perstudent_vs_total_by_region2 <-ggplot2::ggplot()+
  ggplot2::geom_point(data=data_2023,
                      mapping = aes(x = log(pre_total_expenditure) ,
                                    y = pre_education_expenses_perstudents,
                                    color =region) )+ 
  facet_wrap( ~ region )
plot(education_perstudent_vs_total_by_region2)
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



# 相関を散布図で見る

# ---------------------------------------------------------
# 前準備：分析に投入する変数群に絞り込む（必須のプロセス）
# 例として、data_mergedから目的変数(Y)と説明変数(X1, X2...)を抽出
# ---------------------------------------------------------
target_data <- panel_data_pre %>%
  select(pre_teacher_number, pre_student_number, pre_population,pre_mean_ordinary_balance_ratio,pre_education_expenses_perstudents) # 分析に使う変数を指定
# 欠損値(NA)が含まれていると相関係数が計算できない場合があるため、
# 必要に応じて tidyr::drop_na() 等で処理する


# =========================================================
# 選択肢1: GGally::ggpairs() 【推奨：情報量最大】
# =========================================================
# メリット: 下三角に散布図、対角線にヒストグラム、上三角に相関係数が表示され、ggplot2ベースで美しい。
# デメリット: 変数が増えると描画処理が非常に重くなる。

# 散布図マトリックスの描画
ggpairs(target_data, 
        title = "変数間の散布図マトリックスおよび相関係数",
        lower = list(continuous = wrap("points", alpha = 0.5, size = 1))) # 点を半透明にして重なりを見やすくする


#ここから回帰分析

# 財的資源の地方格差
model_1 <- estimatr::lm_robust(data = panel_data_pre, pre_education_expenses_perstudents ~ metro_dummy + pre_population + pre_mean_ordinary_balance_ratio)

summary(model_1)

modelsummary(model_1)

# 人口ではスケールが大きいので対数をとった
model_2 <- estimatr::lm_robust(data = panel_data_pre, pre_education_expenses_perstudents ~ metro_dummy + log(pre_population) + pre_mean_ordinary_balance_ratio)

summary(model_2)

# 時系列のトレンドを除去した固定効果を含める
model_3 <- fixest::feols(pre_education_expenses_perstudents ~ metro_dummy + log(pre_population) + pre_mean_ordinary_balance_ratio | new_year, data = plot_data)
summary(model_3)

model_4 <- fixest::feols(pre_education_expenses_perstudents ~ log(pre_population) + pre_mean_ordinary_balance_ratio | new_year, data = plot_data)
summary(model_4)





#人的と財政的のうちどちらの影響が大きいか比較
model_teacher <- estimatr::lm_robust(data = panel_data_pre, pre_teacher_perstudents ~ metro_dummy + log(pre_population) + pre_mean_ordinary_balance_ratio)

summary(model_teacher)

# 時系列のトレンドを除去した固定効果を含める
model_teacher_2 <- fixest::feols(pre_teacher_perstudents ~ metro_dummy + log(pre_population) + pre_mean_ordinary_balance_ratio | new_year, data = panel_data_pre)
summary(model_teacher_2)

model_teacher_3 <- fixest::feols(pre_teacher_perstudents ~ log(pre_population) + pre_mean_ordinary_balance_ratio | new_year, data = panel_data_pre)
summary(model_teacher_3)

