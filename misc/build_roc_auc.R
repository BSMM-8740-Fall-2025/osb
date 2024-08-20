training_results


training_results$.pred_Yes

# positives
log_preds <- ifelse(training_results$.pred_Yes > 0.5, 1, 0)

ref <- factor(training_results$churn, levels = c('Yes','No'), labels = c('Yes', 'No'))
log_preds <- factor(log_preds, levels = c(1,0), labels = c('Yes', 'No'))
(caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$table)

accuracy <- caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$overall[['Accuracy']]
accuracy

classification_error <- 1 - accuracy
classification_error

caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$byClass[['Precision']]
caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$byClass[['Sensitivity']]
caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$byClass[['Specificity']]



build_roc_aus <- function(threshold, dat = training_results){
  # threshold on predictions
  log_preds <- ifelse(dat$.pred_Yes > threshold, 1, 0)
  # compute predictions and reference/truth
  ref <- factor(dat$churn, levels = c('Yes','No'), labels = c('Yes', 'No'))
  log_preds <- factor(log_preds, levels = c(1,0), labels = c('Yes', 'No'))

  cm <- caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')

  # return( c(threshold, 1-cm$byClass[['Specificity']], cm$byClass[['Sensitivity']]) )

  return(
    tibble::tibble(
      threshold = threshold
      , "1-Specificity" = 1-cm$byClass[['Specificity']]
      , Sensitivity = cm$byClass[['Sensitivity']]
    )
  )

  # accuracy <- caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$overall[['Accuracy']]
  # accuracy
  #
  # classification_error <- 1 - accuracy
  # classification_error
  #
  # caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$byClass[['Precision']]
  # caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$byClass[['Sensitivity']]
  # caret::confusionMatrix(data = log_preds, reference = ref, positive = 'Yes')$byClass[['Specificity']]
}


((0:100)/100) |> purrr::map(~build_roc_aus(.x)) |>
  dplyr::bind_rows() |>
    ggplot(aes(x=`1-Specificity`, y=Sensitivity)) +
    geom_line() + coord_fixed() +
    geom_abline(intercept=0, slope=1)

