# created Juy 17, 2024

# See https://lse-dsi.github.io/DS202/2023/autumn-term/weeks/week04/lab.html

uk_hpi <- readr::read_csv( here::here("slides/data","UK-HPI-full-file-2023-06.csv"), show_col_types = FALSE )

# Select just the UK countries
df <-
  uk_hpi %>%
  dplyr::filter(RegionName %in% c("England", "Wales", "Scotland", "Northern Ireland")) %>%
  dplyr::select(Date, RegionName, SalesVolume) %>%
  dplyr::mutate(Date = dmy(Date))

df %>%
  dplyr::group_by(RegionName) %>%
  dplyr::tally()

df %>%
  tidyr::drop_na() %>%
  dplyr::group_by(RegionName) %>%
  dplyr::tally()

df %>%
  drop_na() %>%
  group_by(RegionName) %>%
  summarise(min_date = min(Date), max_date = max(Date))

# USE THIS AS THE DATA
df <-
  uk_hpi %>%
  dplyr::filter(RegionName %in% c("England", "Wales", "Scotland", "Northern Ireland")) %>%
  dplyr::select(Date, RegionName, SalesVolume) %>%
  dplyr::mutate(Date = dmy(Date)) %>%
  dplyr::filter(Date >= dmy("01-01-2005"))

df |> readr::write_csv( here::here("labs/data","UK_house_prices.csv") )

# Say our goal is to predict SalesVolume using the sales volume from the past month,
# similar to what we’ve been doing so far in this course,
# we could use dplyr functions mutate and lag to achieve that. That is:

rec <-
  recipes::recipe(SalesVolume ~ ., data = df) %>%
  recipes::step_arrange(Date) %>% # We need to sort the data by date before we can use step_lag()
  recipes::step_lag(SalesVolume, lag=1) %>%                  # step_lag is a wrapper for dplyr::lag()
  recipes::prep() # we need to 'prep' the recipe before we can use it. This is always the final step of a recipe

summary(rec)

rec %>%
  bake(df) %>%
  filter(Date <= ymd(20050201)) %>%
  head(8)

rec <-
  recipe(SalesVolume ~ ., data = df) %>%
  step_arrange(RegionName, Date) %>% # Adding RegionName to the arrange call achieves the same as the group_by
  step_lag(SalesVolume, lag=1) %>%
  prep()
rec

rec %>%
  bake(df) %>%
  filter(Date <= ymd(20050201)) %>%
  head(8)



# !!!!!!!!!!!!
rec <-
  recipe(sales_volume ~ ., data = df |> janitor::clean_names()) |>
  step_arrange(region_name, date) |>
  step_lag(sales_volume, lag=1) |>
  step_naomit(lag_1_sales_volume, skip=FALSE) |>  # This line is new
  update_role(date, region_name, new_role = "id") |>
  prep()

rec |> bake(df |> janitor::clean_names()) %>%
  filter(date <= ymd(20050301))
# !!!!!!!!!!!!


rec <-
  recipe(SalesVolume ~ ., data = df) |>
  step_arrange(RegionName, Date) |>
  step_lag(SalesVolume, lag=1) |>
  step_naomit(lag_1_SalesVolume, skip=FALSE) |>
  update_role(Date, RegionName, new_role = "id") |>
  prep()

rec %>%
  bake(df) %>%
  filter(Date <= ymd(20050201)) %>%
  head(8)
