source("data_cleaning.R")

# 教育費割合

summary(panel_data_muni$education_expenses_perexpenditure)

ggplot2::ggplot(panel_data_muni) +
  ggplot2::geom_histogram(
    aes(x = education_expenses_perexpenditure),
    bins = 50
  ) + 
  ggplot2::labs(title = "都市区分による教育費割合の分布(市区町村単位)",
                x = "教育費割合",
                y = "市区町村数") +
  ggplot2::facet_wrap( ~ metro_area) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5), 
    axis.title.x = ggplot2::element_text(size = 12))
                                         


#　生徒一人当たり教育費
ggplot2::ggplot(panel_data_muni) +
  ggplot2::geom_histogram(
    aes(x = education_expenses_perstudents),
    bins = 50
  ) + 
  ggplot2::labs(title = "年別の生徒一人当たり教育費の分布(市区町村単位)",
                x = "生徒一人当たり教育費",
                y = "市区町村数") +
  ggplot2::facet_wrap( ~ new_year) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5), 
    axis.title.x = ggplot2::element_text(size = 12))



ggplot2::ggplot(panel_data_muni) +
  ggplot2::geom_histogram(
    aes(x = log(education_expenses_perstudents)),
    bins = 50
  ) + 
  ggplot2::labs(title = "年別の対数を取った生徒一人当たり教育費の分布(市区町村単位)",
                x = "log(生徒一人当たり教育費)",
                y = "市区町村数") +
  ggplot2::facet_wrap( ~ new_year) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5), 
    axis.title.x = ggplot2::element_text(size = 12))

# 地方税
ggplot2::ggplot(panel_data_muni) +
  ggplot2::geom_histogram(
    aes(x = local_tax_perpop),
    bins = 50
  )
ggplot2::ggplot(panel_data_muni) +
  ggplot2::geom_histogram(
    aes(x = log(local_tax_perpop)),
    bins = 50
  ) + 
  ggplot2::labs(title = "年別の対数を取った一人当たり地方税年別分布(市区町村単位)",
                x = "log(一人当たり地方税)",
                y = "市区町村数") +
  ggplot2::facet_wrap( ~ new_year) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5), 
    axis.title.x = ggplot2::element_text(size = 12))






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
  labs(title = "Before: 元の生徒一人当たり教育費", x = "年度", y = "生徒一人当たり教育費") +
  ggplot2::theme_minimal() 
  

# After: 時間固定効果除去後（トレンドが平坦化され、純粋な分散だけが残るはず）
plot_after <- ggplot(plot_data, aes(x = factor(new_year), y = edu_exp_adjusted)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  coord_cartesian(ylim = quantile(plot_data$edu_exp_adjusted, c(0.05, 0.95), na.rm=TRUE)) +
  labs(title = "After: 時間固定効果 除去後", x = "年度", y = "調整後 生徒一人当たり教育費") +
  ggplot2::theme_minimal() 
  

# patchworkを使って左右に並べて表示
plot_before + plot_after

# data_2024 <- raw_data_2024 |> 
#   dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
#                 :"高等学校生徒数")
# 
# data_2025 <- raw_data_2025 |> 
#   dplyr::select("地域コード":"市区町村","経常収支比率（市町村財政）"
#                 :"高等学校生徒数")


#ここから回帰分析

# 市区町村単位の分析
# プールドOLS
muni_pooled_0 <- estimatr::lm_robust(
  data = panel_data_muni,
  education_expenses_perstudents ~ log(population),
  clusters = region_code,
  se_type = "stata"
)
summary(muni_pooled_0)


muni_pooled_1 <- estimatr::lm_robust(
  data = panel_data_muni,
  log(education_expenses_perstudents) ~ log(population)+ designated_dummy,
  clusters = region_code,
  se_type = "stata"
)
summary(muni_pooled_1)

muni_pooled_2 <- estimatr::lm_robust(
  data = panel_data_muni,
  log(education_expenses_perstudents) ~ log(population)+ designated_dummy + metro_dummy,
  clusters = region_code,
  se_type = "stata"
)
summary(muni_pooled_2)

muni_pooled_3 <- estimatr::lm_robust(
  data = panel_data_muni,
  log(education_expenses_perstudents) ~ log(population)+ designated_dummy + metro_dummy + log(local_tax_perpop),
  clusters = region_code,
  se_type = "stata"
)
summary(muni_pooled_3)

muni_pooled_4 <- estimatr::lm_robust(
  data = panel_data_muni,
  log(education_expenses_perstudents) ~ log(population)+ designated_dummy + metro_dummy + log(local_tax_perpop) + ordinary_balance_ratio,
  clusters = region_code,
  se_type = "stata"
)
summary(muni_pooled_4)

# pooled_5 <- estimatr::lm_robust(
#   data = panel_data_muni,
#   education_expenses_perstudents ~ log(population)+ designated_dummy + metro_dummy + local_tax_perpop + ordinary_balance_ratio + teacher_perstudents,
#   clusters = region_code,
#   se_type = "stata"
# )
# summary(pooled_5)


modelsummary(
  list(
    "muni_pooled_0" = muni_pooled_0,
    "muni_pooled_1" = muni_pooled_1,
    "muni_pooled_2" = muni_pooled_2,
    "muni_pooled_3" = muni_pooled_3,
    "muni_pooled_4" = muni_pooled_4
  )
)

# vif_model5 <- lm(
#   education_expenses_perstudents ~
#     log(population) +
#     designated_dummy +
#     metro_dummy +
#     local_tax_perpop +
#     ordinary_balance_ratio +
#     teacher_perstudents,
#   data = panel_data_muni
# )
# 
# car::vif(vif_model5)
# 
# 
# pooled_5_log <- estimatr::lm_robust(
#   data = panel_data_muni,
#   log(education_expenses_perstudents) ~ log(population) + designated_dummy + metro_dummy +
#     log(local_tax_perpop) + ordinary_balance_ratio + log(teacher_perstudents),
#   clusters = region_code)
# summary(pooled_5_log)
# 
# pooled_5_decomp <- estimatr::lm_robust(
#   data = panel_data_muni,
#   log(education_expenses_perstudents) ~
#     log(population) +
#     designated_dummy +
#     metro_dummy +
#     log(local_tax_perpop) +
#     ordinary_balance_ratio +
#     log(teacher_number) +
#     log(student_number),
#   clusters = region_code,
#   se_type = "stata"
# )
# summary(pooled_5_decomp)


# 年固定効果
muni_year_fixed <- fixest::feols(education_expenses_perstudents ~ log(population)+ designated_dummy + metro_dummy + log(local_tax_perpop) + ordinary_balance_ratio | new_year, data = panel_data_muni)
summary(muni_year_fixed)

# tw固定効果P1
muni_tw_fixed_1 <- fixest::feols(education_expenses_perstudents ~ log(population)| region_code + new_year, data = panel_data_muni)
summary(muni_tw_fixed_1)

# tw固定効果P2
muni_tw_fixed_2 <- fixest::feols(education_expenses_perstudents ~ log(population)+ log(local_tax_perpop)| region_code + new_year, data = panel_data_muni)
summary(muni_tw_fixed_2)

# tw固定効果P3
muni_tw_fixed_3 <- fixest::feols(education_expenses_perstudents ~ log(population)+ log(local_tax_perpop) + ordinary_balance_ratio| region_code + new_year, data = panel_data_muni)
summary(muni_tw_fixed_3)

modelsummary(
  list(
    "muni_year_fixed" = muni_year_fixed,
    "muni_tw_fixed_1" = muni_tw_fixed_1,
    "muni_tw_fixed_2" = muni_tw_fixed_2,
    "muni_tw_fixed_3" = muni_tw_fixed_3
)
)







