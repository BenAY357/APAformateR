multiple_regression_write_up <- function(mod){
  require(broom)
  require(tidyverse)
  # output model
  coeffs <- tidy(mod) %>% mutate_at(vars("estimate","std.error", "statistic"), funs(round(., 2)))
  fit <- glance(mod) # get rest of stats as a data frame
  # print model fit
  fit_p_value <- ifelse(fit$p.value <= 0.001, " p < .001", paste("p = ",rd(fit$p.value, digits =3), sep = ""))
  print(paste("Fit: ","F", "(", fit$df %>% round(2), ", ", fit$df.residual %>% round(2),") = ", fit$statistic %>% round(2), ",",fit_p_value,sep= ""))
  # print adjusted r squared
  adj_r_squared <- fit$adj.r.squared %>% round(2) *100
  print(paste("adjR2:", adj_r_squared, "%", sep = ""))
  # apa format predictors
  for(i in 2:nrow(coeffs)){
    # retrieve predictor
    predictor <- coeffs[i,]
    # paste in apa format
    apa <- paste(predictor$term, ": ", "b = ", predictor$estimate %>% round(2),", SE = ", predictor$std.error,", t = ", predictor$statistic, ",", ifelse(predictor$p.value <= 0.001, paste(" p < .001"), paste("p = ",rd(predictor$p.value, digits = 3), sep = "")),sep = "")
    print(apa)
  }
}
