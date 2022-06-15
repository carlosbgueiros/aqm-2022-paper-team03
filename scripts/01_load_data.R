
# Define which packages you need for your analysis and assign it to 
# the p_needed object. 
p_needed <-
  c("viridis", "knitr", 
    "MASS", "ordinal", 
    "tidyverse", "modelr", 
    "haven", "marginaleffects", 
    "modelsummary", "kableExtra", "gridExtra", 
    "stevemisc", "stevetemplates")

# check which packages are already installed on your computer.
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

## load dataset
ineq_pol_data <- read_dta("raw-data/swissubase_1323_1_0/1323_Ineq-pol_Data_v1.0.0.dta") |> 
  as_tibble()

data_ineq <- ineq_pol_data |> 
  dplyr::select(cntry, region, yrbrn, stmigrant, posrich, prosprich, pospoor, prosppoor, 
                jobsec, trustppl, trustpol, citiz, forbrn, forpar, 
                 saliimmigr, saliinequ, saliunempl, salicrime,
                 gndr, educ, educcat, incdec, lrscale) |> 
                mutate(mobility_rich = posrich - prosprich, .after = prosprich) |> 
  mutate(mobility_poor = pospoor - prosppoor, .after = prosppoor) |> 
  mutate(downward_mob = ifelse(mobility_rich < 0, 1, 0), 
         mobility3 = case_when( # 3 categories for expect downward mobility, do not expect change or expect upward mobility.
           mobility_rich == 0 ~ 0,
           mobility_rich < 0 ~ -1,
           mobility_rich > 0 ~ 1),
         downward_mob_cens = ifelse(mobility_rich < 0, mobility_rich, 0),
         .after = mobility_rich) |> # Main independent variable
  mutate(educcat = case_when(    # Recode University degree for binary
            educcat == 1 ~ 0,
            educcat == 2 ~ 1),
         forbrn = case_when(  # Recode University foreign born for 1 and 0
         forbrn == 1 ~ 1,
         forbrn == 2 ~ 0),
         female = ifelse(gndr == 2, 1, 0) # Recode Gender foreign born for binary
         ) |> 
  filter(stmigrant != 99 & lrscale != 98 & lrscale != 99 & incdec != 98 & forbrn != 98 & educcat != 98 &
           saliunempl != 99 & saliinequ != 99 ) # Remove "I don't know" Answers

# Dependent Variable
data_ineq$stmigrant <- factor(data_ineq$stmigrant, 
                             levels = c(1,2,3,4,5),
                             labels = c("Strongly disagree", "Disagree", 
                                        "Nor agree nor disagree", 
                                        "Agree", "Strongly Agree"), 
                             ordered = TRUE)   

data_ineq$cntry <- factor(data_ineq$cntry)
data_ineq$region <- factor(data_ineq$region)

# Each variable that is not binary is rescaled by dividing 2 standard deviations
data_ineq <- data_ineq %>%
  # r2sd_at() is in {stevemisc} to rescale non-binary (categorical and continuous) variables by 2 standard deviations
  stevemisc::r2sd_at(c("lrscale","incdec", "saliinequ", "saliunempl", "saliimmigr"))

# data_ineq <- data_ineq |> 
#  mutate(quantile_perceived_mobility = ntile(mobility_rich, 2), .after = mobility_rich)


