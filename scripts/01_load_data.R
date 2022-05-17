
#library(data.table)
library(dplyr)
#library(tidyr)
#library(purrr)
#library(glue)
library(haven)

ineq_pol_data <- read_dta("raw-data/swissubase_1323_1_0/1323_Ineq-pol_Data_v1.0.0.dta")

#dplyr::glimpse(ineq_pol_data)


data_ineq <- ineq_pol_data |> 
  select(cntry, yrbrn, stmigrant, posrich, prosprich, pospoor, prosppoor, jobsec, trustppl, trustpol, citiz, forbrn, forpar, 
         saliimmigr, saliinequ, saliunempl, salicrime) |> 
  mutate(mobility_rich = posrich - prosprich, .after = prosprich) |> # Main independent variable
  mutate(mobility_poor = pospoor - prosppoor, .after = prosppoor) |> 
  mutate(stmigrant = ifelse(stmigrant == 99, NA_integer_, stmigrant) |> 
           as.factor(levels(c("")))
         )
  


basic_ols <- lm(stmigrant ~ mobility_rich, data = data_ineq) 


summary(basic_ols)
