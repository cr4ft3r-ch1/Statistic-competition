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



# コロプレス図(神奈川県のみ) 

# 3. パネルデータ（属性データ）からの神奈川県抽出
# ※ panel_data_muni はこれまでの工程で作成済みのデータフレームと仮定
test_data_2 <- panel_data_muni |> 
  dplyr::filter(prefecture == "神奈川県") |>
  # 複数年のパネルデータの場合、一旦特定の年（例：2019年）だけに絞るか、
  # 全期間の平均をとって1行/1自治体にする必要がある。ここでは2019年に絞る例。
  dplyr::filter(new_year == 2019)

# 4. 空間データと属性データの完全結合
complete_data_test <- map_data_formatted |> 
  dplyr::inner_join(test_data_2, by = "region_code")

# 5. コロプレス図（色分け地図）の作成
# fillに色分けしたい変数（例：一人当たり教育費）を指定する
ggplot(data = complete_data_test) +
  geom_sf(aes(fill = education_expenses_perstudents), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "生徒数") + # 見やすいカラーパレット
  theme_minimal() +
  labs(
    title = "神奈川県：1人当たり教育費の空間的分布（2019年）",
    subtitle = "ヤードスティック競争の視覚的確認",
    caption = "※政令指定都市は市単位に集約済み"
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank() # 地図上の経度緯度の数値を消す
  )




# 1. 隣接関係のリストを作成（ポリゴンデータから抽出）
# complete_data_test は前のステップで作成した神奈川県のsfオブジェクト
kanagawa_nb <- spdep::poly2nb(complete_data_test$geometry, queen = TRUE)

# 2. 隣接関係の重み付け（行標準化: style = "W"）
# これにより、「隣接する全自治体の平均値」を計算するための重みができる
kanagawa_listw <- spdep::nb2listw(kanagawa_nb, style = "W", zero.policy = TRUE)

# 確認: 各自治体が平均して何個の自治体と接しているか等のサマリーを表示
summary(kanagawa_nb)

# 一人当たり教育費に空間的自己相関があるか検定
moran_test_result <- spdep::moran.test(
  complete_data_test$education_expenses_perstudents, 
  listw = kanagawa_listw, 
  zero.policy = TRUE
)

print(moran_test_result)
# ※ p-value < 0.05 であれば、「教育費は空間的にランダムではなく、隣接地域と似通っている」と結論づけられる。
# 比較用のベースラインOLSモデル（神奈川県単年度版）
ols_model <- lm(
  education_expenses_perstudents ~  log(population) + ordinary_balance_ratio,
  data = complete_data_test
)

# 空間自己回帰モデル（SAR）の推計
# lagsarlm関数を用いて、被説明変数に空間ラグを組み込む
sar_model <- spatialreg::lagsarlm(
  education_expenses_perstudents ~ log(population) + ordinary_balance_ratio,
  data = complete_data_test,
  listw = kanagawa_listw,
  zero.policy = TRUE
)

# 結果の表示
summary(sar_model)
summary(ols_model)

# 1. 空間パネル推計用のデータ前処理（絶対条件のクリア）
kanagawa_panel <- panel_data_muni |>
  # 神奈川県のみを抽出
  dplyr::filter(prefecture == "神奈川県") |>
  # 【最重要】空間（自治体コード） -> 時間（年）の順に完全にソートする
  dplyr::arrange(region_code, year)

# 3. 空間パネル自己回帰モデル（SAR）の推計
# spml関数を用いて、双方向固定効果（個体・時間）を統制しつつ空間ラグを組み込む
sar_panel_model <- splm::spml(
  formula = education_expenses_perstudents ~ log(population) + ordinary_balance_ratio,
  data = kanagawa_panel,
  index = c("region_code", "new_year"),
  listw = kanagawa_listw,      # ステップ1で作成したN×Nの重み行列をそのまま投入
  model = "within",            # 個体固定効果モデル（Fixed Effects）
  effect = "twoways",          # 空間（個体）と時間の双方向固定効果を指定
  spatial.error = "none",      # 空間誤差モデルではなく
  lag = TRUE                   # 空間ラグモデル（SAR）を指定
)

# 結果の表示
# 下部に出力される「Spatial autoregressive coefficient (lambda)」がヤードスティック効果を示す
summary(sar_panel_model)



# 全国で県単位の分析
# ※ panel_data_muni はこれまでの工程で作成済みのデータフレームと仮定
test_data_3 <- pre_complete_data |> 
  # 複数年のパネルデータの場合、一旦特定の年（例：2019年）だけに絞るか、
  # 全期間の平均をとって1行/1自治体にする必要がある。ここでは2019年に絞る例。
  dplyr::filter(new_year == 2019)

# 5. コロプレス図（色分け地図）の作成
# fillに色分けしたい変数（例：一人当たり教育費）を指定する
ggplot2::ggplot(data = test_data_3) +
  ggplot2::geom_sf(ggplot2::aes(fill = pre_education_expenses_perstudents), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "1人あたり教育費") + # 見やすいカラーパレット
  theme_minimal() +
  labs(
    title = "全国：1人当たり教育費の空間的分布（2019年）"
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank() # 地図上の経度緯度の数値を消す
  )

# 5. コロプレス図（色分け地図）の作成
# fillに色分けしたい変数（例：一人当たり教育費）を指定する
pre_complete_data |> 
  dplyr::group_by(prefecture) |> 
  dplyr::mutate(
  mean_pre_education_expenses_perstudents = mean(pre_education_expenses_perstudents)
)


ggplot2::ggplot(data = pre_complete_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = mean_pre_education_expenses_perstudents), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", name = "1人あたり教育費") + # 見やすいカラーパレット
  theme_minimal() +
  labs(
    title = "全国：1人当たり教育費の空間的分布（5年平均）"
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank() # 地図上の経度緯度の数値を消す
  )
  
  
  
  
  
# # 1. 隣接関係のリストを作成（ポリゴンデータから抽出）
# # complete_data_test は前のステップで作成した神奈川県のsfオブジェクト
#prefecture_nb <- spdep::poly2nb(test_data_3$geometry, queen = TRUE)
# 
# # 2. 隣接関係の重み付け（行標準化: style = "W"）
# # これにより、「隣接する全自治体の平均値」を計算するための重みができる
#prefecture_listw <- spdep::nb2listw(prefecture_nb, style = "W", zero.policy = TRUE)

# 確認: 各自治体が平均して何個の自治体と接しているか等のサマリーを表示
summary(prefecture_nb)

# 一人当たり教育費に空間的自己相関があるか検定
moran_test_result_2 <- spdep::moran.test(
  test_data_3$pre_education_expenses_perstudents, 
  listw = prefecture_listw, 
  zero.policy = TRUE
)

print(moran_test_result_2)
# ※ p-value < 0.05 であれば、「教育費は空間的にランダムではなく、隣接地域と似通っている」と結論づけられる。
# 比較用のベースラインOLSモデル（神奈川県単年度版）
ols_model_2 <- estimatr::lm_robust(
  pre_education_expenses_perstudents ~  log(pre_population) + mean_ordinary_balance_ratio,
  data = test_data_3
)

# 空間自己回帰モデル（SAR）の推計
# lagsarlm関数を用いて、被説明変数に空間ラグを組み込む
sar_model_2 <- spatialreg::lagsarlm(
  pre_education_expenses_perstudents ~ log(pre_population) + mean_ordinary_balance_ratio,
  data = test_data_3,
  listw = prefecture_listw,
  zero.policy = TRUE
)

# 結果の表示
summary(sar_model_2)
summary(ols_model_2)

# 1. 空間パネル推計用のデータ前処理（絶対条件のクリア）
prefecture_panel <- panel_data_pre |>
  # 【最重要】 -> 時間（年）の順に完全にソートする
  dplyr::arrange(prefecture, new_year)

clean_panel <- prefecture_panel |> 
  dplyr::filter(
    !is.na(pre_education_expenses_perstudents),
    !is.na(pre_population),
    !is.na(mean_ordinary_balance_ratio)
  )
# ===================================================
# ステップ3: 空間重み行列 W の次元と並び順をデータに完全同期させる
# ===================================================
# データに存在する一意な都道府県のリスト（ソート済み）
final_pref_list <- unique(final_panel$prefecture)

# 既存のlistw（prefecture_listw）から、データに存在する都道府県だけを抽出し、
# かつデータの並び順と完全に一致させた「新しい重み行列」を再作成する
# ※注: もし現在作成しているlistwの元オブジェクト（nbオブジェクト等）があれば
# そこからサブセットを作成するのが最も安全です。
# 以下は、listwの名称（attr(prefecture_listw, "region.id")）が都道府県名と一致している場合のコード例です。

# 例: nbオブジェクト（prefecture_nb）からデータの並び順で抽出し直す場合：
# final_nb <- spdep::subset.nb(prefecture_nb, attr(prefecture_nb, "region.id") %in% final_pref_list)
# final_listw <- spdep::nb2listw(final_nb, style = "W")



# 3. 空間パネル自己回帰モデル（SAR）の推計
# spml関数を用いて、双方向固定効果（個体・時間）を統制しつつ空間ラグを組み込む
sar_panel_model_2 <- splm::spml(
  formula = pre_education_expenses_perstudents ~ log(pre_population) + mean_ordinary_balance_ratio,
  data = final_panel,
  index = c("prefecture", "new_year"),
  listw = prefecture_listw,      # ステップ1で作成したN×Nの重み行列をそのまま投入
  model = "within",            # 個体固定効果モデル（Fixed Effects）
  effect = "twoways",          # 空間（個体）と時間の双方向固定効果を指定
  spatial.error = "none",      # 空間誤差モデルではなく
  lag = TRUE                   # 空間ラグモデル（SAR）を指定
)

# 結果の表示
# 下部に出力される「Spatial autoregressive coefficient (lambda)」がヤードスティック効果を示す
summary(sar_panel_model_2)