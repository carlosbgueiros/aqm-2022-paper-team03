
## Complete Pooling Models ####
# Pooling all observations from all countries

m1_ord_logit <- clm(stmigrant ~ downward_mob + lrscale + incdec + saliinequ + saliunempl + forbrn + educcat, 
                    data = data_ineq, link = "logit") # Ordinal PAckage 

# Variables - 2 standard deviations
m2_ord_logit_2sd <- clm(stmigrant ~ downward_mob + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + forbrn + educcat,   
                        data = data_ineq, link = "logit")

m3_ord_logit_2sd <- clm(stmigrant ~ downward_mob + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + forbrn + educcat + 
                          female,  # include female dummy
                        data = data_ineq, link = "logit")

m4_ord_logit_2sd <- clm(stmigrant ~ downward_mob + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + 
                          z_saliimmigr + #salience of immigration
                          forbrn + educcat,  
                        data = data_ineq, link = "logit")

## Partial Pooling - Random Effects - Multilevel #####

# Pooling all observations from all countries
# nest regions within countries and observations within regions  (1|cntry) + (1|cntry:region) is equivalent to (1|cntry/region)

m1_ord_logit_clmm <- clmm(stmigrant ~ downward_mob + lrscale + incdec + saliinequ + saliunempl + forbrn + educcat + (1|cntry/region),
                          data = data_ineq, link = "logit") # Ordinal PAckage 

# Variables - 2 standard deviations
m2_ord_logit_clmm_2sd <- clmm(stmigrant ~ downward_mob + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + forbrn + educcat + (1|cntry/region),
                          data = data_ineq, link = "logit") # Ordinal PAckage 




m3_ord_logit_clmm_2sd <- clmm(stmigrant ~ downward_mob + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + forbrn + educcat + 
                                female +
                                (1|cntry/region),
                              data = data_ineq, link = "logit") 


m4_ord_logit_clmm_2sd <- clmm(stmigrant ~ downward_mob + z_lrscale + z_incdec + z_saliinequ + z_saliunempl + 
                                z_saliimmigr + 
                                forbrn + educcat + (1|cntry/region),
                              data = data_ineq, link = "logit")  


## Put models inside lists for tables 

no_normalized_model_list <- list("Model 1" =  m1_ord_logit, 
                                 "Model 1 - MM" =  m1_ord_logit_clmm) 

pooled_models <- list(
  "Model 2" =  m2_ord_logit_2sd, 
  "Model 3" =  m3_ord_logit_2sd, 
  "Model 4" =  m4_ord_logit_2sd
    )

partial_pool_models <- list(
  "Model 2 - MM" =  m2_ord_logit_clmm_2sd, 
  "Model 3 - MM" =  m3_ord_logit_clmm_2sd, 
  "Model 4 - MM" =  m4_ord_logit_clmm_2sd
            )


