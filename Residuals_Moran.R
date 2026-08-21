source("data_cleaning.R")
prefecture_nb <- readRDS("prefecture_nb.rds")
prefecture_listw <- readRDS("prefecture_listw.rds")
muni_nb <- readRDS("muni_nb.rds")
muni_listw <- readRDS("muni_listw.rds")


muni_clean_panel <- muni_complete_data |>
  dplyr::filter(
    !is.na(education_expenses_perstudents),
    !is.na(population),
    !is.na(ordinary_balance_ratio),
    !is.na(teacher_perstudents),
    !is.na(local_tax_perpop)
  )


# 各年のGlobal Morans's
calc_global_moran <- function(df_year, listw_obj) {
  
  moran <- spdep::moran.test(
    df_year$education_expenses_perstudents,
    listw_obj,
    zero.policy = TRUE
  )
  
  tibble(
    year = unique(df_year$new_year),
    moran_I = unname(moran$estimate["Moran I statistic"]),
    p_value = moran$p.value
  )
}

global_moran_results <- muni_clean_panel |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_global_moran(.x, muni_listw)
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
  # estimatr::lm_robust(
  #   data = df_year,
  #   log(education_expenses_perstudents) ~ 
  #     ordinary_balance_ratio + 
  #     log(local_tax_perpop),,
  #   clusters = region_code,
  #   se_type = "stata"
  # )
  moran_B <- spdep::moran.test(
    resid(mod_B),
    listw_obj,
    zero.policy = TRUE
  )
  
  tibble(
    `OLS Model A I` = unname(
      moran_A$estimate["Moran I statistic"]
    ),
    `OLS Model A p-value` = moran_A$p.value,
    `OLS Model B I` = unname(
      moran_B$estimate["Moran I statistic"]
    ),
    `OLS Model B p-value` = moran_B$p.value
   )
}


ols_moran_results <- muni_clean_panel |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_ols_moran(.x, muni_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)


print(ols_moran_results)


# mod_lm <- lm(
#   log(education_expenses_perstudents) ~
#     log(population) +
#     ordinary_balance_ratio +
#     log(local_tax_perpop),
#   data = df_year
# )
# 
# mod_robust <- estimatr::lm_robust(
#   log(education_expenses_perstudents) ~
#     log(population) +
#     ordinary_balance_ratio +
#     log(local_tax_perpop),
#   data = df_year
# )
# 
# max(abs(
#   residuals(mod_lm) - mod_robust$residuals
# ))


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
      ~ calc_year_fe_moran(.x, muni_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

year_fe_moran_results

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
      ~ calc_twoway_fe_moran(.x, muni_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

twoway_fe_moran_results

moran_results <- ols_moran_results |>
  left_join(
    year_fe_moran_results,
    by = "new_year"
  ) |>
  left_join(
    twoway_fe_moran_results,
    by = "new_year"
  )

print(moran_results)

moran_I_table <- moran_results |>
  select(
    new_year,
    `OLS Model A I`,
    `OLS Model B I`,
    `Year FE I`,
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

moran_I_table




# ============================================================
# 生徒一人当たり教員数を含めなかった場合の残差に対するMorans I
# ============================================================



# 年固定効果モデルに対する残差Moran's I
# 年固定効果
model_year_fe <- fixest::feols(
  education_expenses_perstudents ~
    log(population) +
    ordinary_balance_ratio  |
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
      ~ calc_year_fe_moran(.x, muni_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

year_fe_moran_results

# 2-way FEモデルに対する残差Moran's I
model_twoway_fe <- fixest::feols(
  education_expenses_perstudents ~
    log(population) +
    ordinary_balance_ratio |
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
      ~ calc_twoway_fe_moran(.x, muni_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

twoway_fe_moran_results

moran_results <- ols_moran_results |>
  left_join(
    year_fe_moran_results,
    by = "new_year"
  ) |>
  left_join(
    twoway_fe_moran_results,
    by = "new_year"
  )

print(moran_results)

moran_I_table <- moran_results |>
  select(
    new_year,
    `OLS Model A I`,
    # `OLS Model B I`,
    `Year FE I`,
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

moran_I_table


#固定効果モデルのMoran's I
model_year_fe <- fixest::feols(
  log(education_expenses_perstudents) ~
    log(population) +
    ordinary_balance_ratio 
  |
    new_year,
  data = panel_data_muni)


# 残差を保存
panel_data_muni <- panel_data_muni |>
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

year_fe_moran_results <-panel_data_muni |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_year_fe_moran(.x, muni_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

year_fe_moran_results

# 2-way FEモデルに対する残差Moran's I
model_twoway_fe <- fixest::feols(
  log(education_expenses_perstudents) ~
    log(population) +
    ordinary_balance_ratio 
   |
    region_code + new_year,
  data = panel_data_muni
)

# 残差を保存
panel_data_muni <-panel_data_muni |>
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

twoway_fe_moran_results <- panel_data_muni |>
  group_by(new_year) |>
  nest() |>
  mutate(
    result = map(
      data,
      ~ calc_twoway_fe_moran(.x, muni_listw)
    )
  ) |>
  unnest(result) |>
  select(-data)

twoway_fe_moran_results

moran_results <- ols_moran_results |>
  left_join(
    year_fe_moran_results,
    by = "new_year"
  ) |>
  left_join(
    twoway_fe_moran_results,
    by = "new_year"
  )

print(moran_results)

moran_I_table <- moran_results |>
  select(
    new_year,
    `OLS Model A I`,
    # `OLS Model B I`,
    `Year FE I`,
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

moran_I_table

# 空間自己回帰モデル（SAR）の推計
# lagsarlm関数を用いて、被説明変数に空間ラグを組み込む
sar_model <- spatialreg::lagsarlm(
  log(education_expenses_perstudents) ~ log(population) + ordinary_balance_ratio + log(local_tax_perpop),
  data = muni_clean_panel,
  listw = muni_listw,
  zero.policy = TRUE
)

# 結果の表示
summary(sar_model)


# # ① 自治体数
# n_distinct(muni_clean_panel$region_code)
# 
# # ② Wのサイズ
# length(muni_listw$neighbours)
# dim(spdep::listw2mat(muni_listw))
# 
# # ③ パネルの観測数
# nrow(muni_clean_panel)
# 
# # ④ 各自治体が4年間存在するか
# table(table(muni_clean_panel$region_code))
# 
# # ⑤ 欠損
# muni_clean_panel |>
#   summarise(
#     y_na = sum(is.na(education_expenses_perstudents)),
#     pop_na = sum(is.na(population)),
#     balance_na = sum(is.na(ordinary_balance_ratio)),
#     region_na = sum(is.na(region_code)),
#     year_na = sum(is.na(new_year))
#   )
# 
# # ⑥ Wの自治体ID
# head(muni_listw$region.id)
# 
# # ⑦ データの自治体ID
# head(unique(muni_clean_panel$region_code))
# 
# # ⑧ 孤立自治体
# sum(spdep::card(muni_listw$neighbours) == 0)
# 
# muni_clean_panel <- muni_clean_panel |>arrange(region_code, new_year)
# length(muni_listw$neighbours)
# length(muni_listw$weights)
# listw2mat(muni_listw) |> dim()
# 
# muni_clean_panel <- muni_clean_panel |>
#   arrange(region_code, new_year)
# 
# # 自治体IDの順番
# muni_ids <- muni_clean_panel |>
#   distinct(region_code) |>
#   arrange(region_code) |>
#   pull(region_code)
# 
# length(muni_ids)
# muni_listw$region.id <- muni_ids
# 
# head(muni_listw$region.id)
# 1741

# 3. 空間パネル自己回帰モデル（SAR）の推計
# spml関数を用いて、双方向固定効果（個体・時間）を統制しつつ空間ラグを組み込む
sar_panel_model <- splm::spml(
  formula = log(education_expenses_perstudents) ~ log(population) + ordinary_balance_ratio + log(local_tax_perpop),
  data = muni_clean_panel,
  index = c("region_code", "new_year"),
  listw = muni_listw,      # ステップ1で作成したN×Nの重み行列をそのまま投入
  model = "within",            # 個体固定効果モデル（Fixed Effects）
  effect = "twoways",          # 空間（個体）と時間の双方向固定効果を指定
  spatial.error = "none",      # 空間誤差モデルではなく
  lag = TRUE,                   # 空間ラグモデル（SAR）を指定
  zero.policy = TRUE
)

# 結果の表示
# 下部に出力される「Spatial autoregressive coefficient (lambda)」がヤードスティック効果を示す
summary(sar_panel_model)
