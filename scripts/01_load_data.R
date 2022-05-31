
#library(data.table)
#library(MASS)
#library(dplyr)
#library(tibble)
#library(tidyr)
#library(purrr)
#library(glue)
#library(haven)
#library(ordinal)
#library(ggeffects)
#library(effects)
#library(marginaleffects)
#library(rms)
#library(modelsummary)

# First you define which packages you need for your analysis and assign it to 
# the p_needed object. 
p_needed <-
  c("viridis", "knitr", "MASS", "ordinal", "arm", "tidyverse", "modelr", 
    "haven", "ggeffects", "marginaleffects", "emmeans", "modelsummary", "kableExtra", "brms")

# Now you check which packages are already installed on your computer.
# The function installed.packages() returns a vector with all the installed 
# packages.
packages <- rownames(installed.packages())
# Then you check which of the packages you need are not installed on your 
# computer yet. Essentially you compare the vector p_needed with the vector
# packages. The result of this comparison is assigned to p_to_install.
p_to_install <- p_needed[!(p_needed %in% packages)]
# If at least one element is in p_to_install you then install those missing
# packages.
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
# Now that all packages are installed on the computer, you can load them for
# this project. Additionally the expression returns whether the packages were
# successfully loaded.
sapply(p_needed, require, character.only = TRUE)

select <- dplyr::select # Avoid Clash between dplyr and MASS


ineq_pol_data <- read_dta("raw-data/swissubase_1323_1_0/1323_Ineq-pol_Data_v1.0.0.dta") |> 
  as_tibble()

#dplyr::glimpse(ineq_pol_data)

data_ineq <- ineq_pol_data |> 
  dplyr::select(cntry, region, yrbrn, stmigrant, posrich, prosprich, pospoor, prosppoor, 
                jobsec, trustppl, trustpol, citiz, forbrn, forpar, 
         saliimmigr, saliinequ, saliunempl, salicrime,
         gndr, educ, educcat, incdec, lrscale) |> 
  mutate(mobility_rich = posrich - prosprich, .after = prosprich) |> 
  mutate(mobility_poor = pospoor - prosppoor, .after = prosppoor) |> 
  mutate(downward_mob = ifelse(mobility_rich < 0, 1, 0), 
         downward_mob_cens = ifelse(mobility_rich < 0, mobility_rich, 0),
         .after = mobility_rich) |> # Main independent variable
  mutate(educcat = case_when(    # Recode University degree for binary
            educcat == 1 ~ 0,
            educcat == 2 ~ 1),
         forbrn = case_when(
         forbrn == 1 ~ 1,
         forbrn == 2 ~ 0),
         female = ifelse(gndr == 2, 1, 0)
         ) |> 
  filter(stmigrant != 99 & lrscale != 98 & lrscale != 99 & incdec != 98 & forbrn != 98 & educcat != 98 &
           saliunempl != 99 & saliinequ != 99 ) # Remove "I don't know" Answers
 #filter(if_all(everything(), ~ .x != 99))

data_ineq$stmigrant <- factor(data_ineq$stmigrant, 
                             levels = c(1,2,3,4,5),
                             labels = c("Strongly disagree", "Disagree", 
                                        "Nor agree nor disagree", "Agree", "Strongly Agree"), 
                             ordered = TRUE)   

data_ineq$cntry <- factor(data_ineq$cntry)
data_ineq$region <- factor(data_ineq$region)
#data_ineq$incdec <- factor(data_ineq$incdec, ordered = T)
#data_ineq$lrscale <- factor(data_ineq$lrscale)



