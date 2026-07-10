source("data_cleaning.R")

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

# # 5. コロプレス図（色分け地図）の作成
# # fillに色分けしたい変数（例：一人当たり教育費）を指定する
# pre_complete_data |> 
#   dplyr::group_by(prefecture) |> 
#   dplyr::mutate(
#     mean_pre_education_expenses_perstudents = mean(pre_education_expenses_perstudents)
#   )
# 
# 
# ggplot2::ggplot(data = pre_complete_data) +
#   ggplot2::geom_sf(ggplot2::aes(fill = mean_pre_education_expenses_perstudents), color = "black", size = 0.1) +
#   scale_fill_viridis_c(option = "plasma", name = "1人あたり教育費") + # 見やすいカラーパレット
#   theme_minimal() +
#   labs(
#     title = "全国：1人当たり教育費の空間的分布（5年平均）"
#   ) +
#   theme(
#     legend.position = "bottom",
#     axis.text = element_blank() # 地図上の経度緯度の数値を消す
#   )





# # 1. 隣接関係のリストを作成（ポリゴンデータから抽出）
#prefecture_nb <- spdep::poly2nb(test_data_3$geometry, queen = TRUE)
#saveRDS(prefecture_nb, "prefecture_nb.rds")
# 
# # 2. 隣接関係の重み付け（行標準化: style = "W"）
# # これにより、「隣接する全自治体の平均値」を計算するための重みができる
#prefecture_listw <- spdep::nb2listw(prefecture_nb, style = "W", zero.policy = TRUE)
#saveRDS(prefecture_listw, "prefecture_listw.rds")
# 確認: 各自治体が平均して何個の自治体と接しているか等のサマリーを表示
prefecture_nb <- readRDS("prefecture_nb.rds")
prefecture_listw <- readRDS("prefecture_listw.rds")
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