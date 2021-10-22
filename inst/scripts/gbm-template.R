pacman::p_load(mirr, tidymodels, mirviemodels)


# Get data ---------------------------------------------------------------------
## Create a data frame called `data` with columns `mirvie_id`, your predictors
## and outcome only.




# Split data -------------------------------------------------------------------

set.seed(1)
data_split <- initial_split(data, strata = INSERT_OUTCOME_HERE)
training_data <- training(data_split)
testing_data <- testing(data_split)


# Create a model specification

mod_spec <- boost_tree(mode = "INSERT_MODE_HERE",
                       trees = tune(), tree_depth = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost")


# Create a recipe and a workflow -----------------------------------------------

gene_names <- get_gene_names()
rec <- recipe(training_data) %>%
  update_role(everything(), new_role = "predictor") %>%
  update_role(INSERT_OUTCOME_HERE, new_role = "outcome") %>%
  update_role(mirvie_id, new_role = "ID") %>%
  step_select_genes(any_of(gene_names), condition = "INSERT_OUTCOME_HERE")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(mod_spec)


# Set up the resampling --------------------------------------------------------

set.seed(1)
resamp <- vfold_cv(training_data, v = 10, repeats = 1,
                   strata = INSERT_OUTCOME_HERE)


# Set up the parallel processing if you want it --------------------------------

doParallel::registerDoParallel(4)


# Do Bayesian (Gaussian process) tuning ----------------------------------------

set.seed(1)
tb <- tune_bayes(
  wf,
  resamp,
  metrics = metric_set(INSERT_METRICS_HERE),
  iter = 200, initial = 50,
  param_info = update(parameters(wf),
                      tree_depth = tree_depth(c(1L, 2L))),
  control = control_bayes(verbose = TRUE)
)

finwf <- finalize_workflow(
  wf,
  select_by_one_std_err(tb, trees)
)

fitted_mod <- fit(finwf, training_data)
