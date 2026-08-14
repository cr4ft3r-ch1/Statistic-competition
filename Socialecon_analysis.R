source("data_cleaning.R")


# 市区町村データから地図データを作成
socialecon_muni_data_2019 <- muni_complete_data |>
  dplyr::filter(new_year == 2019)


# 1. データフレームから類似団体区分の文字型ベクトルを抽出
fits_classification <- socialecon_muni_data_2019$classification

# 2. outer関数で全自治体ペア(N x N)の同区分判定(TRUE/FALSE)を行い、1/0の数値行列に変換
W_fits_matrix <- outer(fits_classification, fits_classification, "==") * 1

# 3. 対角成分(自分自身とのペア)を0に設定して自己参照を排除
diag(W_fits_matrix) <- 0


# ---------------------------------------------------------
# Step 3: 行標準化と listw オブジェクトへの変換
# ---------------------------------------------------------

# 1. 数値行列(matrix)から spdep の listw オブジェクトへ変換(行標準化: style = "W")
fits_listw <- spdep::mat2listw(
  W_fits_matrix,
  style = "W",
  zero.policy = TRUE
)

# 2. 作成した重み行列のサマリー(結合数や孤立地域の有無)を確認
summary(fits_listw, zero.policy = TRUE)
