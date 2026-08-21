#2023年のデータでとりあえずデータの特徴をつかもう`
modelsummary::datasummary(
  All(data_2023)~ N + Mean + SD + Min + Max,
  data = data_2023,
  title = "表1：記述統計量一覧"
)

# 教育費の分布をプロット
education_distribution <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = data_2023, 
                          mapping = aes(x = log10(pre_education_expences)), # なぜlog10
                          binwidth = 0.1)


#予算に占める教育費の割合をプロット
education_percentage <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = data_2023, 
                          mapping = aes(x = (pre_education_expences/pre_total_expenditure)), 
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







