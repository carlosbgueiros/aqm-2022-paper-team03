
# Pooling all observations from all countries
m1_ord_logit <- polr(stmigrant ~ downward_mob + lrscale,  ## MASS package 
                     data = data_ineq, 
                     Hess = TRUE,
                     method = "logistic")

m1_ord_logit2 <- clm(stmigrant ~ downward_mob + lrscale, data = data_ineq, link = "logit") # Ordinal PAckage

m1_ord_logit2 <- clmm(stmigrant ~ downward_mob + lrscale, data = data_ineq, link = "logit") # Ordinal PAckage


## view a summary of the model
summary(m1_ord_logit)
summary(m1_ord_logit2)

m1_ord_logit2 |> 
  marginaleffects(newdata = "mean") |> 
  summary()

comparisons()

predictions(m1_ord_logit)
