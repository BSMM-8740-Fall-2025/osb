# created November 26, 2023

require(magrittr)
require(ggplot2)

# Q1 ----

# (1)
dag_1 <-
ggdag::dagify(
  A ~ D
  , Y ~ A
  , B ~ A
  , coords = ggdag::time_ordered_coords(
    list(
      "D"             # time point 1
      , c("A","B")    # time point 2
      , "Y"           # time point 3
    )
  ),
  exposure = "D",
  outcome = "Y"
)

# (2)
dag_2 <-
ggdag::dagify(
  D ~ A
  , E ~ D
  , Y ~ E
  , E ~ F
  , B ~ F
  , B ~ A
  , C ~ B
  , coords = ggdag::time_ordered_coords(
    list(
      c("A","D")             # time point 1
      , c("B","F","E")    # time point 2
      , c("C","Y")           # time point 3
    )
  ),
  exposure = "D",
  outcome = "Y"
)

# (3)
dag_3 <-
ggdag::dagify(
  A ~ D
  , Y ~ D
  , D ~ B
  , A ~ B
  , Y ~ B
  , coords = ggdag::time_ordered_coords(
    list(
      "D"             # time point 1
      , c("B","A")    # time point 2
      , "Y"           # time point 3
    )
  ),
  exposure = "D",
  outcome = "Y"
)

# (4)
dag_4 <-
  ggdag::dagify(
    Y ~ D
    , Y ~ C
    , D ~ B
    , D ~ A
    , B ~ C
    , B ~ A
    , coords = ggdag::time_ordered_coords(
      list(
        c("A","D")             # time point 1
        , "B"    # time point 2
        , c("C","Y")           # time point 3
      )
    ),
    exposure = "D",
    outcome = "Y"
  )
# %>%
#   ggdag::ggdag(text = TRUE) +
#   ggdag::theme_dag()

dag_flows <-
  purrr::map(
    list(dag_1 = dag_1, dag_2 = dag_2, dag_3 = dag_3, dag_4 = dag_4)
    , ggdag::tidy_dagitty
  ) |>
  purrr::map("data") |>
  purrr::list_rbind(names_to = "dag") |>
  dplyr::mutate(dag = factor(dag, levels = c("dag_1", "dag_2", "dag_3", "dag_4")))

dag_flows |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  ggdag::geom_dag_edges(edge_width = 1) +
  ggdag::geom_dag_point() +
  ggdag::geom_dag_text() +
  facet_wrap(~ dag) +
  ggdag::expand_plot(
    expand_x = expansion(c(0.2, 0.2)),
    expand_y = expansion(c(0.2, 0.2))
  ) +
  ggdag::theme_dag()

dag_4 %>% ggdag::ggdag_paths()


dag <- ggdag::dagify(y ~ x + z2 + w2 + w1,
              x ~ z1 + w1,
              z1 ~ w1 + v,
              z2 ~ w2 + v,
              w1 ~ ~w2,
              exposure = "x",
              outcome = "y"
)

ggdag::tidy_dagitty(dag_4) %>% ggdag::dag_adjustment_sets()

ggdag::ggdag_adjustment_set(dag_4)

paths <- dagitty::paths(dag_4)
open_paths <- glue::glue_collapse(glue::glue('`{paths$paths[paths$open]}`'), sep = ", ", last = ", and ")
adj_sets <- unclass( dagitty::adjustmentSets(dag_4) ) |>
  purrr::map_chr(\(.x) glue::glue('{unlist(glue::glue_collapse(.x, sep = " + "))}'))
adj_sets <- glue::glue("`{adj_sets}`")


# Q2 ----
set.seed(8740)
ggdag::dagify(
  demand ~ price + economy + c_price
  , price ~ c_price
  , c_price ~ wholesale
  , wholesale ~ economy
  , exposure = "price"
  , outcome = "demand"
  , labels = c(
      price = "price",
      c_price = "c_price",
      economy = "economy",
      demand = "demand",
      wholesale = "wholesale"
    )
) %>%
  ggdag::ggdag(use_labels = "label", text = FALSE) +
  ggdag::theme_dag()

Q3 ----

