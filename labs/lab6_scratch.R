# created October 19, 2023

require(magrittr)
require(ggplot2)


# Q1 ----

data <-
  readr::read_csv("data/Telco-Customer-Churn.csv") %>%
  dplyr::mutate(churn = as.factor(churn))

skimr::skim(data)

data %>%
  ggplot(aes(x=monthly_charges)) + geom_histogram()

data %>%
  ggplot(aes(x=tenure)) + geom_histogram()

# Q2 ----
set.seed(8740)

# split data
data_split    <- rsample::initial_split(data, prop = 0.7)
default_train <- rsample::training(data_split)
default_test  <- rsample::testing(data_split)

# create a recipe
default_recipe <- default_train %>%
  recipes::recipe(formula = churn ~ .) %>%
  recipes::step_normalize(recipes::all_numeric_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors())

default_recipe %>% recipes::prep(default_train)

# Q3 ----

# create a linear regression model
default_model <- parsnip::logistic_reg() %>%
  parsnip::set_engine("glm") %>%
  parsnip::set_mode("classification")

# create a workflow
default_workflow <- workflows::workflow() %>%
  workflows::add_recipe(default_recipe) %>%
  workflows::add_model(default_model)

lm_fit <-
  default_workflow %>%
  parsnip::fit(default_train)

# training dataset
training_results <-
  broom::augment(lm_fit , default_train)

training_results %>%
  yardstick::conf_mat(estimate=.pred_class, truth = churn)

training_results %>%
  yardstick::roc_auc(.pred_No, truth = churn)

training_results %>%
  yardstick::accuracy(estimate=.pred_class, truth = churn)

training_results %>%
  yardstick::roc_curve(.pred_No, truth = churn) %>% autoplot()

m_set <-
  yardstick::metric_set(
    yardstick::accuracy
    , yardstick::precision
    , yardstick::recall
    , yardstick::f_meas
    , yardstick::spec
    , yardstick::sens
    , yardstick::ppv
    , yardstick::npv
)
training_results %>% m_set(truth = churn, estimate = .pred_class)

m_set_roc <-
  yardstick::metric_set(
    yardstick::roc_curve
    , yardstick::roc_auc
)
training_results %>% m_set_roc(truth = churn, estimate = .pred_No)

# testing dataset
testing_results <-
  broom::augment(lm_fit , default_test)

testing_results %>%
  yardstick::accuracy(estimate=.pred_class, truth = churn)

testing_results %>%
  yardstick::roc_auc(.pred_No, truth = churn)

testing_results %>%
  yardstick::conf_mat(estimate=.pred_class, truth = churn)

testing_results %>%
  yardstick::roc_curve(truth = churn, .pred_No) %>% autoplot()



# Q4 ----
lm_fit %>% broom::tidy() %>%
  dplyr::arrange(desc(abs(estimate)))

exp(-1.9) # [1] 0.1495686

# Q5 ----

default_model_knn <- parsnip::nearest_neighbor(neighbors = 3) %>%
  parsnip::set_engine("kknn") %>%
  parsnip::set_mode("classification")

# create a workflow
default_workflow_knn <- default_workflow %>%
  workflows::update_model(default_model_knn)

lm_fit_knn <-
  default_workflow_knn %>%
  parsnip::fit(default_train)

# augment the data with the predictions using the model fit

# train
training_results_knn <-
  broom::augment(lm_fit_knn , default_train)

training_results_knn %>%
  yardstick::accuracy(estimate=.pred_class, truth = churn)

training_results_knn %>%
  yardstick::roc_auc(.pred_No, truth = churn)

training_results_knn %>%
  yardstick::roc_curve(.pred_No, truth = churn) %>% autoplot()

# test
testing_results_knn <-
  broom::augment(lm_fit_knn , default_test)

testing_results_knn %>%
  yardstick::accuracy(estimate=.pred_class, truth = churn)

testing_results_knn %>%
  yardstick::roc_auc(.pred_No, truth = churn)

testing_results %>%
  yardstick::roc_curve(.pred_No, truth = churn) %>% autoplot()

# Q6 ----

# v-fold cross validation data
data_vfold_cv <- data %>% rsample::vfold_cv(v=5)

rf_fit_rs <-
  default_workflow_knn %>%
  tune::fit_resamples(data_vfold_cv, control = tune::control_resamples(save_pred = TRUE))

rf_fit_rs %>% tune::collect_metrics()

rf_fit_rs %>% tune::collect_predictions() %>%
  yardstick::roc_curve(.pred_No, truth = churn) %>% autoplot()

# Q7 ----

default_model_knn_tuned <- parsnip::nearest_neighbor(neighbors = tune::tune()) %>%
  parsnip::set_engine("kknn") %>%
  parsnip::set_mode("classification")

# create a workflow
default_workflow_knn <- default_workflow %>%
  workflows::update_model(default_model_knn_tuned)

# a grid for tuning
clust_num_grid <-
  dials::grid_regular(dials::neighbors(), levels = 10)

tune_results <- tune::tune_grid(
  default_workflow_knn,
  resamples = data_vfold_cv,
  grid = clust_num_grid,
  control = tune::control_grid(save_pred = TRUE)
  , metrics =
    yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)
)

tune_results

# Q8 ----
tune_results %>%
  tune::collect_metrics()

tune_results %>%
  tune::collect_metrics() %>%
  ggplot(aes(neighbors,mean)) +
  geom_line(linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2)

# Q9 ----
tune_results %>%
  tune::show_best("roc_auc")

best_nn <- tune_results %>%
  tune::select_best("roc_auc")

final_wf <- default_workflow_knn %>%
  tune::finalize_workflow(best_nn)

final_fit <-
  final_wf %>%
  tune::last_fit(data_split)

final_fit %>%
  tune::collect_metrics()

final_fit %>%
  tune::collect_predictions() %>%
  yardstick::roc_curve(.pred_No, truth = churn) %>%
  autoplot()

# Q10 ----
# https://www.tidymodels.org/learn/statistics/k-means/

set.seed(8740)

# generate data (save?)

centers <- tibble::tibble(
  cluster = factor(1:3),
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(5, 0, -3),              # x1 coordinate of cluster center
  x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)

labelled_points <-
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>%
  select(-num_points) %>%
  unnest(cols = c(x1, x2))

labelled_points %>% readr::write_csv('data/lab_6_clusters.csv')

ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.3)

# clusters
points <-
  labelled_points %>%
  select(-cluster)

kclust <- stats::kmeans(points, centers = 3)
kclust

broom::augment(kclust, points)
broom::tidy(kclust)
broom::glance(kclust)

#

kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

clusters <-
  kclusts %>%
  tidyr::unnest(cols = c(tidied))

assignments <-
  kclusts %>%
  tidyr::unnest(cols = c(augmented))

clusterings <-
  kclusts %>%
  tidyr::unnest(cols = c(glanced))

p <- assignments %>% ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
  facet_wrap(~ k)

p + geom_point(data = clusters, size = 10, shape = "x")

clusterings %>% ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# $$$$$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
set.seed(8740)

dat <- readr::read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

skimr::skim(dat)

data %>%
  ggplot(aes(x=monthly_charges)) + geom_histogram()

data %>%
  ggplot(aes(x=tenure)) + geom_histogram()


data <- dat %>% dplyr::mutate(
  tenure_interval =
    dplyr::case_when(
      (tenure >= 0 & tenure <= 6) ~ '0-6 Month'
      , (tenure > 6 & tenure <= 12) ~ '6-12 Month'
      , (tenure > 12 & tenure <= 24) ~ '12-24 Month'
      , (tenure > 24 & tenure <=36) ~ '24-36 Month'
      , (tenure > 36 & tenure <=48) ~ '36-48 Month'
      , (tenure > 48 & tenure <= 62) ~ '48-62 Month'
      , (tenure > 62) ~ '> 62 Month'
      , TRUE ~ 'inadmissable'
    )
  , .after = tenure
) %>%
  dplyr::mutate(
    SeniorCitizen = dplyr::case_when(SeniorCitizen == 1 ~ 'Yes', TRUE ~ "No")
    , Churn = as.factor(Churn)) %>%
  dplyr::select(-customerID) %>%
  janitor::clean_names() %>%
  stats::na.omit()

data %>% readr::write_csv('data/Telco-Customer-Churn.csv')

data <-
  readr::read_csv('labs/data/Telco-Customer-Churn.csv', show_col_types = FALSE) %>%
  dplyr::mutate(churn=as.factor(churn))


skimr::skim(data)

data %>%
  ggplot(aes(x=monthly_charges)) + geom_histogram()

data %>%
  ggplot(aes(x=tenure)) + geom_histogram()


# split data
data_split    <- rsample::initial_split(data)
default_train <- rsample::training(data_split)
default_test  <- rsample::testing(data_split)

# create a recipe
default_recipe <- default_train %>%
  recipes::recipe(formula = churn ~ .) %>%
  recipes::step_normalize(recipes::all_numeric_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors())

default_recipe %>% recipes::prep(default_train) %>% recipes::bake(default_train)

# create a linear regression model
default_model <- parsnip::logistic_reg() %>%
  parsnip::set_engine("glm") %>%
  parsnip::set_mode("classification")

# create a workflow
default_workflow <- workflows::workflow() %>%
  workflows::add_recipe(default_recipe) %>%
  workflows::add_model(default_model)

lm_fit <-
  default_workflow %>%
  parsnip::fit(default_train)

# augment the data with the predictions using the model fit
training_results <-
  broom::augment(lm_fit , default_train)

training_results %>% dplyr::glimpse()

auc_roc_tbl <- training_results %>%
  dplyr::select(churn:.pred_Yes) %>%
  # order prediction probability from high to low
  dplyr::arrange( desc(.pred_Yes) ) %>%
  # make new variable for cumulative % of 'Yes' category
  dplyr::mutate(
    # scale to percent (# of all 'Yes' categories)
    y = ifelse(churn == "Yes", 1/sum(churn == "Yes"),0)
    # accumulate the values
    , y = cumsum(y)
  ) %>%
  # keep the 'No' category values
  dplyr::filter(churn == "No") %>%
  # number rows & scale to % of total; compute incremental areas
  tibble::rowid_to_column("ID") %>%
  dplyr::mutate(
    auc_inc = y / max(ID) # multiply the height by the width
    , ID = ID / max(ID)   # scale to percent (# of all 'No' categories)
  )

auc_roc_tbl %>%
  ggplot(aes(x=ID, y = y)) +
  geom_line() +
  # xlim(c(0,1)) +
  geom_abline(slope=1) +
  coord_fixed() +
  labs(
    title = "ROC curve for customer churn prediction",
    subtitle =
      stringr::str_glue("Logistic Regression AUC = {scales::label_number(accuracy = 10^-7)(sum(auc_roc_tbl$auc_inc) )}")
  )

# NEW Nov 3
auc_roc_tbl <- training_results %>%
  dplyr::select(churn:.pred_Yes) %>%
  # order prediction probability from high to low
  dplyr::arrange( desc(.pred_Yes) ) %>%
  # make new variable for cumulative % of 'Yes' category
  dplyr::mutate(
    # scale to percent (# of all 'Yes' categories)
    y = ifelse(churn == "Yes", 1/sum(churn == "Yes"),0)
    # accumulate the values (TRUE POSITIVE RATE)
    , y = cumsum(y)
    , x = ifelse(churn == "No", 1/sum(churn == "No"),0)
    , x = cumsum(x)
  ) %>%
  # keep the 'No' category values
  dplyr::filter(churn == "No") %>%
  # number rows & scale to % of total; compute incremental areas
  tibble::rowid_to_column("ID") %>%
  dplyr::mutate(
    auc_inc = y / max(ID) # multiply the height by the width
    , ID = ID / max(ID)   # scale to percent (# of all 'No' categories)
  )

auc_roc_tbl %>% dplyr::summarize(auc = sum(auc_inc))

auc_roc_tbl %>%
  ggplot(aes(x=x, y = y)) +
  geom_line() +
  geom_abline(slope=1) +
  coord_fixed() +
  labs(
    title = "ROC curve for customer churn prediction"
    , subtitle =
      stringr::str_glue("Logistic Regression AUC = {scales::label_number(accuracy = 10^-7)(sum(auc_roc_tbl$auc_inc) )}")
    , x = "false positive rate"
    , y = 'true positive rate'
  ) +
  theme_bw()

training_results %>%
  yardstick::roc_auc(.pred_No, truth = churn)

training_results %>%
  yardstick::specificity(estimate=.pred_class, truth = churn)

training_results %>%
  yardstick::accuracy(estimate=.pred_class, truth = churn)
training_results %>%
  yardstick::recall(estimate=.pred_class, truth = churn)
training_results %>%
  yardstick::f_meas(estimate=.pred_class, truth = churn)
training_results %>%
  yardstick::conf_mat(estimate=.pred_class, truth = churn)
training_results %>%
  yardstick::conf_mat(estimate=.pred_class, truth = churn) %>% summary()
training_results %>%
  vip::vi
training_results %>%
  yardstick::j_index(estimate=.pred_class, truth = churn)

# %%%%%%%%%%%%%
# https://howtolearnmachinelearning.com/articles/roc-machine-learning/
# see https://probably.tidymodels.org/articles/where-to-use.html
# https://jmsallan.netlify.app/blog/a-workflow-for-binary-classification-with-tidymodels/
# %%%%%%%%%%%%%

threshold_data <- training_results %>%
  dplyr::select(churn:.pred_Yes) %>%
  probably::threshold_perf(estimate=.pred_Yes, truth = churn, thresholds = seq(0.5, 1, by = 0.0025))
threshold_data_metrics <- threshold_data %>%
  dplyr::filter(.threshold %in% c(0.5, 0.6, 0.7, 0.8, .9, 0.95, 0.99))

auc <- training_results %>%
  yardstick::roc_auc(.pred_No, truth = churn) %>%
  dplyr::pull(.estimate)

training_results %>%
  dplyr::arrange( desc(.pred_Yes) ) %>%
  # dplyr::mutate(churn=as.factor(churn)) %>%
  dplyr::select(churn:.pred_Yes) %>%
  # yardstick::roc_curve(.pred_class, truth = churn) %>%
  yardstick::roc_curve(.pred_Yes, truth = churn) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()
  autoplot() +
  labs(
    title = "ROC curve for customer churn prediction"
    , subtitle =
      stringr::str_glue(
      "Logistic Regression AUC = {scales::label_number(accuracy = 10^-7)( auc[1] )}")
  )

# ---
data %<>%
  stats::na.omit()

# churn_recipe <- data %>%
#   recipes::recipe(~ ., data = .) %>%
#   recipes::step_naomit() %>%
#   recipes::step_dummy(recipes::all_nominal_predictors())

kmeans_spec <- tidyclust::k_means( num_clusters = 3 )

wflow <- workflows::workflow() %>%
  workflows::add_model(kmeans_spec) %>%
  workflows::add_recipe(default_recipe)

set.seed(8740)
data_resamples <- data %>% rsample::bootstraps(apparent = TRUE)

wflow %>%
  parsnip::fit(data) %>%
  broom::tidy()

wflow %>%
  parsnip::fit(data) %>% tidyclust::extract_centroids()

wflow %>%
  parsnip::fit(data) %>% tidyclust::sse_ratio()

wflow %>%
  parsnip::fit(data) %>% tidyclust::sse_total()

all_workflows <-
  workflowsets::workflow_set(
    preproc = list(base = churn_recipe),
    models = list(tidyclust::k_means( num_clusters = parsnip::tune() ) )
  )

all_workflows <- all_workflows %>%
  workflowsets::workflow_map(
    verbose = TRUE                # enable logging
    , fn = "tune_cluster"
    , grid = tibble(num_clusters = 7:15)
    , resamples = data_resamples # a parameter passed to tune::tune_grid()
  )

all_workflows %>% workflowsets::rank_results()

# KNN ----
# default_model_knn <- parsnip::nearest_neighbor(neighbors = tune::tune()) %>%
default_model_knn <- parsnip::nearest_neighbor(neighbors = 3) %>%
  parsnip::set_engine("kknn") %>%
  parsnip::set_mode("classification")

# create a workflow
default_workflow_knn <- workflows::workflow() %>%
  workflows::add_recipe(default_recipe) %>%
  workflows::add_model(default_model_knn)

lm_fit_knn <-
  default_workflow_knn %>%
  parsnip::fit(default_test)

# augment the data with the predictions using the model fit
testing_results_knn <-
  broom::augment(lm_fit_knn , default_test)

testing_results_knn %>% dplyr::glimpse()

p <- testing_results_knn %>% yardstick::roc_curve(.pred_No, truth = churn) %>% autoplot()

testing_results_knn %>% yardstick::roc_auc(.pred_No, truth = churn)


# v-fold cross validation data
data_vfold_cv <- data %>% rsample::vfold_cv(v=5)

# fixed number ----
rf_fit_rs <-
  default_workflow_knn %>%
  tune::fit_resamples(data_vfold_cv, control = tune::control_resamples(save_pred = TRUE))

rf_fit_rs %>% tune::collect_metrics()

rf_fit_rs %>%
 yardstick::roc_curve(.pred_No, truth = churn) %>% autoplot()

rf_fit_rs %>%
  workflows::extract_fit_parsnip()

rf_fit_rs %>% tune::collect_predictions() %>%
  yardstick::roc_curve(.pred_No, truth = churn) %>% autoplot()

# end fixed number ----

# a grid for tuning
clust_num_grid <-
  dials::grid_regular(dials::neighbors(), levels = 10)

tidyclust::tune_cluster(
  default_workflow_knn,
  resamples = data_vfold_cv,
  grid = clust_num_grid,
  # control = tune::control_grid(save_pred = TRUE, extract = identity)
  # , metrics =
  #   tidyclust::cluster_metric_set(
  #     #yardstick::roc_auc
  #     tidyclust::sse_within_total,
  #     # tidyclust::sse_total,
  #     # tidyclust::sse_ratio
  #   )
)

tune_results <- tune::tune_grid(
  default_workflow_knn,
  resamples = data_vfold_cv,
  grid = clust_num_grid,
  control = tune::control_grid(save_pred = TRUE, extract = identity)
  , metrics =
    yardstick::metric_set(yardstick::accuracy, yardstick::roc_auc)
)

tune_results %>%
  tune::collect_metrics()

tune_results %>%
  tune::collect_metrics() %>%
  # dplyr::mutate(neighbors = factor(neighbors)) %>%
  ggplot(aes(neighbors,mean)) +
  geom_line(linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

tune_results %>%
  tune::show_best("accuracy")

best_nn <- tune_results %>%
  tune::select_best("accuracy")

final_wf <- default_workflow_knn %>%
  tune::finalize_workflow(best_nn)

final_fit <-
  final_wf %>%
  tune::last_fit(data_split)

final_fit %>%
  tune::collect_metrics()

final_fit %>%
  tune::collect_predictions() %>%
  yardstick::roc_curve(.pred_No, truth = churn) %>%
  autoplot()



