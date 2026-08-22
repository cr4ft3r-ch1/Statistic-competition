source("data_cleaning.R")

muni_clean_panel <- muni_complete_data |>
  dplyr::filter(
    !is.na(education_expenses_perstudents),
    !is.na(population),
    !is.na(ordinary_balance_ratio),
    !is.na(teacher_perstudents),
    !is.na(local_tax_perpop)
  )

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

# 各年のGlobal Morans's
calc_global_moran <- function(df_year, listw_obj, year) {
  
  moran <- spdep::moran.test(
    log(df_year$education_expenses_perstudents),
    listw_obj,
    zero.policy = TRUE
  )
  
  tibble(
    year = year,
    moran_I = unname(moran$estimate["Moran I statistic"]),
    p_value = moran$p.value
  )
}

global_moran_results <- muni_clean_panel |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = purrr::map2(
      data,
      new_year,
      ~ calc_global_moran(.x, fits_listw, .y)
    )
  ) |>
  unnest(result) |>
  select(-data)

print(global_moran_results)


# 線形回帰に対する残差Moran's I

calc_ols_moran <- function(df_year, listw_obj) {
  
  mod_A <- lm(
    log(education_expenses_perstudents) ~
      log(population) +
      ordinary_balance_ratio,
    data = df_year
  )
  
  moran_A <- spdep::moran.test(
    resid(mod_A),
    listw_obj,
    zero.policy = TRUE
  )
  
  mod_B <- lm(
    log(education_expenses_perstudents) ~
      log(population) +
      ordinary_balance_ratio + 
      log(local_tax_perpop),
    data = df_year
  )
  
  moran_B <- spdep::moran.test(
    resid(mod_B),
    listw_obj,
    zero.policy = TRUE
  )
  
  # mod_C <- lm(
  #   log(education_expenses_perstudents) ~
  #     log(population) + I(log(population)^2) +
  #     ordinary_balance_ratio + log(local_tax_perpop),
  #   data = df_year
  # )
  # 
  # moran_C <- spdep::moran.test(
  #   resid(mod_C),
  #   listw_obj,
  #   zero.policy = TRUE
  # )
  
  tibble(
    `OLS Model A I` = unname(
      moran_A$estimate["Moran I statistic"]
    ),
    `OLS Model A p-value` = moran_A$p.value,
    `OLS Model B I` = unname(
      moran_B$estimate["Moran I statistic"]
    ),
    `OLS Model B p-value` = moran_B$p.value,
    # `OLS Model C I` = unname(
    #   moran_C$estimate["Moran I statistic"]
    # ),
    # `OLS Model C p-value` = moran_C$p.value
  )
}


ols_moran_results <- muni_clean_panel |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_ols_moran(.x, fits_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)


print(ols_moran_results)




# 年固定効果モデルに対する残差Moran's I
# 年固定効果
model_year_fe <- fixest::feols(
  log(education_expenses_perstudents) ~
    log(population) +
    ordinary_balance_ratio +
    log(local_tax_perpop)|
    new_year,
  
  data = muni_clean_panel
)

# 残差を保存
muni_clean_panel <- muni_clean_panel |>
  mutate(
    year_fe_residual = resid(model_year_fe)
  )


calc_year_fe_moran <- function(df_year, listw_obj) {
  
  moran <- spdep::moran.test(
    df_year$year_fe_residual,
    listw_obj,
    zero.policy = TRUE
  )
  
  tibble(
    `Year FE I` =
      unname(moran$estimate["Moran I statistic"]),
    `Year FE p-value` =
      moran$p.value
  )
}

year_fe_moran_results <- muni_clean_panel |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_year_fe_moran(.x, fits_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

year_fe_moran_results

# 自治体固定効果モデルに対する残差Moran's I
# 自治体固定効果のみ
model_muni_fe <- fixest::feols(
  log(education_expenses_perstudents) ~
    log(population) +
    ordinary_balance_ratio +
    log(local_tax_perpop) |
    region_code,
  data = muni_clean_panel
)

# 残差を保存
muni_clean_panel <- muni_clean_panel |>
  mutate(
    muni_fe_residual = resid(model_muni_fe)
  )


# 自治体FEモデルに対する残差Moran's I
calc_muni_fe_moran <- function(df_year, listw_obj) {
  
  moran <- spdep::moran.test(
    df_year$muni_fe_residual,
    listw_obj,
    zero.policy = TRUE
  )
  
  tibble(
    `Municipality FE I` =
      unname(moran$estimate["Moran I statistic"]),
    `Municipality FE p-value` =
      moran$p.value
  )
}


# 年ごとにMoran's Iを計算
muni_fe_moran_results <- muni_clean_panel |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_muni_fe_moran(.x, fits_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)


muni_fe_moran_results

# 2-way FEモデルに対する残差Moran's I
model_twoway_fe <- fixest::feols(
  log(education_expenses_perstudents) ~
    log(population) +
    ordinary_balance_ratio +
    log(local_tax_perpop) |
    region_code + new_year,
  data = muni_clean_panel
)

# 残差を保存
muni_clean_panel <- muni_clean_panel |>
  mutate(
    twoway_fe_residual = resid(model_twoway_fe)
  )

calc_twoway_fe_moran <- function(df_year, listw_obj) {
  
  moran <- spdep::moran.test(
    df_year$twoway_fe_residual,
    listw_obj,
    zero.policy = TRUE
  )
  
  tibble(
    `2-way FE I` =
      unname(moran$estimate["Moran I statistic"]),
    `2-way FE p-value` =
      moran$p.value
  )
}

twoway_fe_moran_results <- muni_clean_panel |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_twoway_fe_moran(.x, fits_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

twoway_fe_moran_results

socialecon_moran_results <- ols_moran_results |>
  left_join(
    year_fe_moran_results,
    by = "new_year"
  ) |>
  left_join(
    muni_fe_moran_results,
    by = "new_year"
  ) |> 
  left_join(
    twoway_fe_moran_results,
    by = "new_year"
  )

print(socialecon_moran_results)

socialecon_moran_I_table <- socialecon_moran_results |>
  select(
    new_year,
    `OLS Model A I`,
    `OLS Model B I`,
    `Year FE I`,
    `Municipality FE I`,
    `2-way FE I`
  ) |>
  tidyr::pivot_longer(
    cols = -new_year,
    names_to = "Model",
    values_to = "Moran's I"
  ) |>
  tidyr::pivot_wider(
    names_from = new_year,
    values_from = `Moran's I`
  )

socialecon_moran_I_table


# calc_global_moran_mc <- function(df_year, listw_obj, nsim = 999) {
#   mc <- spdep::moran.mc(
#     df_year$education_expenses_perstudents,
#     listw_obj,
#     nsim = nsim,
#     zero.policy = TRUE
#   )
#   tibble(
#     moran_I_mc = mc$statistic,
#     p_value_mc = mc$p.value  # 最小でも 1/(nsim+1) = 0.001
#   )
# }
# global_moran_mc_results <- muni_clean_panel |>
#   group_by(new_year) |>
#   nest() |>
#   mutate(
#     result = map(
#       data,
#       ~ calc_global_moran_mc(.x, fits_listw, nsim = 9999)
#     )
#   ) |>
#   unnest(result) |>
#   select(-data)
# 
# print(global_moran_mc_results)




# SAR

sar_panel_model <- splm::spml(
  formula = log(education_expenses_perstudents) ~ log(population) + ordinary_balance_ratio + log(local_tax_perpop),
  data = muni_clean_panel,
  index = c("region_code", "new_year"),
  listw = fits_listw,      # ステップ1で作成したN×Nの重み行列をそのまま投入
  model = "within",            # 個体固定効果モデル（Fixed Effects）
  effect = "twoways",          # 空間（個体）と時間の双方向固定効果を指定
  spatial.error = "none",      # 空間誤差モデルではなく
  lag = TRUE,                   # 空間ラグモデル（SAR）を指定
  zero.policy = TRUE
)

# 結果の表示
# 下部に出力される「Spatial autoregressive coefficient (lambda)」がヤードスティック効果を示す
summary(sar_panel_model)
