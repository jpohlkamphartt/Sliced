rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth + 1) - log(estimate + 1))^2))
  }
  
  metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

rmsle <- function(data, ...) {
  UseMethod("rmsle")
}
rmsle <- new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}

rmsle_vec<-function (truth, estimate, na_rm = TRUE, ...) 
{
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth+1) - log(estimate+1))^2))
  }
  metric_vec_template(
    metric_impl = rmsle_impl, 
    truth = truth, 
    estimate = estimate, 
    na_rm = na_rm, 
    cls = "numeric",
    ...)
}

rmsle<-function(data,...){
  UseMethod("rmsle")
}

rmsle<-new_numeric_metric(rmsle,direction= "minimize")

rmsle.data.frame<-function (data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "rmsle", 
    metric_fn = rmsle_vec, 
    data = data, 
    truth = !!enquo(truth), 
    estimate = !!enquo(estimate), 
    na_rm = na_rm,
    ...)
}