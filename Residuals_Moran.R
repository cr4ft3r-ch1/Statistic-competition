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
    !is.na(teacher_perstudents)
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
    education_expenses_perstudents ~
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
    education_expenses_perstudents ~
      log(population) +
      ordinary_balance_ratio +
      teacher_perstudents,
    data = df_year
  )
  
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



# 年固定効果モデルに対する残差Moran's I
# 年固定効果
model_year_fe <- fixest::feols(
  education_expenses_perstudents ~
    log(population) +
    ordinary_balance_ratio +
    teacher_perstudents |
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
    ordinary_balance_ratio +
    teacher_perstudents |
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
