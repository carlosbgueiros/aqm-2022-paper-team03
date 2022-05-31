
## Complete Pooling Models ####

# Pooling all observations from all countries
m1_ord_logit <- polr(stmigrant ~ downward_mob + lrscale,  ## MASS package 
                     data = data_ineq, 
                     Hess = TRUE,
                     method = "logistic")

M.plr <- arm::sim(m1_ord_logit, n.sims = 1000)
coef.sim <- coef(M.plr, slot="coef")
zeta.sim <- coef(M.plr, slot="zeta")
coefall.sim <- coef(M.plr)

ggpredict(m1_ord_logit, "downward_mob")
predictions(m1_ord_logit, variables = "downward_mob")

ggpredict(m1_ord_logit_clmm, c("downward_mob", "lrscale"))

mydf <- ggpredict(m1_ord_logit_clmm, c("downward_mob"))

## Dta grid for the quantities of interest?


m1_ord_logit_clm <- clm(stmigrant ~ downward_mob + lrscale, data = data_ineq, link = "logit") # Ordinal PAckage

m2_ord_logit_clm <- clm(stmigrant ~ downward_mob + lrscale + downward_mob * lrscale, data = data_ineq, link = "logit") # Ordinal PAckage


m3_formula <- formula(paste("stmigrant ~ downward_mob + lrscale + downward_mob * lrscale + incdec + forbrn + educcat", collapse = " "))
m3_ord_logit_clm <- clm(m3_formula, data = data_ineq, link = "logit") # Ordinal PAckage

m3_formula <- formula(paste("stmigrant ~ downward_mob_cens + lrscale + incdec + forbrn + educcat", collapse = " "))
m3_ord_logit_clm <- clm(m3_formula, data = data_ineq, link = "logit") # Ordinal PAckage

summary(m3_ord_logit_clm)
#incdec + # income decile
 # forbrn + # Dummy for foreign born
  #educcat # University degree

## Partial Pooling - Random Effects #####

#random effect for the state for country
m1_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + lrscale + (1|cntry) + (1|region), data = data_ineq, link = "logit") # Ordinal PAckage 

m2_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + lrscale + (1|cntry/region), data = data_ineq, link = "logit") # Ordinal PAckage 

m3_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + lrscale + downward_mob * lrscale + 
                            incdec + forbrn + educcat + (1|cntry),
                          data = data_ineq, link = "logit") # Ordinal PAckage

m3b_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + lrscale +
                            incdec + forbrn + educcat + (1|cntry/region),
                          data = data_ineq, link = "logit") # Ordinal PAckage

m3c_ord_logit_clmm <- clmm(stmigrant ~ downward_mob_cens + lrscale +
                             incdec + forbrn + educcat + (1|cntry/region),
                           data = data_ineq, link = "logit") # Ordinal PAckage

m4_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + lrscale + downward_mob * lrscale + 
                            incdec + forbrn + educcat + (1|cntry) + (1|region),
                          data = data_ineq, link = "logit") # Ordinal PAckage 

m5_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + lrscale + downward_mob * lrscale + 
                            incdec + forbrn + educcat + (1|cntry/region), # nest regions within countries  #(1|cntry) + (1|cntry:region),
                          data = data_ineq, link = "logit") # Ordinal PAckage 

# 
m6_ord_logit_clmm <- clmm(stmigrant ~ z_downward_mob_cens + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + forbrn + educcat + (1|cntry/region),
                          data = data_ineq, link = "logit") # Ordinal PAckage 

m7_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + forbrn + educcat + (1|cntry/region),
                          data = data_ineq, link = "logit") # Ordinal PAckage 

#ordinal::ranef(m7_ord_logit_clmm)

#summary(m3c_ord_logit_clmm) 

summary(m7_ord_logit_clmm)

predict(m1_ord_logit2, type="class")

## view a summary of the model
summary(m1_ord_logit)
summary(m2_ord_logit_clm)

m1_ord_logit2 |> 
  marginaleffects(newdata = "mean") |> 
  summary()

comparisons()

predictions(m1_ord_logit)


## Bayesian estimation ####
#options(mc.cores=parallel::detectCores()) #Speed up regressions with "brms"

m_ord_logit_brm <- brm(stmigrant ~ downward_mob + lrscale, #+ (1|cntry), 
                       data=data_ineq, family=cumulative("logit")) # bayesian multilevel ordinal logit

m_ord_logit_brm <- brm(stmigrant ~ downward_mob_cens + lrscale + (1|cntry), 
                       data=data_ineq, family=cumulative("logit")) # bayesian multilevel ordinal logit


