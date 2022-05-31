
data_ineq <- data_ineq %>%
  # r2sd_at() is in {stevemisc}
  stevemisc::r2sd_at(c("downward_mob_cens", "lrscale","incdec", "saliinequ", "saliunempl"))


coef_m7_ord_logit_clmm <- coef(m7_ord_logit_clmm) # coef
vcov_m7_ord_logit_clmm <- vcov(m7_ord_logit_clmm) # vcov 

# Code below is repeated to make vcoc without country and region random effect
vcov_m7_ord_logit_clmm <- vcov_m7_ord_logit_clmm[-nrow(vcov_m7_ord_logit_clmm), -ncol(vcov_m7_ord_logit_clmm)] 
vcov_m7_ord_logit_clmm <- vcov_m7_ord_logit_clmm[-nrow(vcov_m7_ord_logit_clmm), -ncol(vcov_m7_ord_logit_clmm)]

set.seed(202206)
sim_m7_ord_logit_clmm <- MASS::mvrnorm(1000, coef_m7_ord_logit_clmm, vcov_m7_ord_logit_clmm) %>% as_tibble() %>% # 1,000 sims, convert matrix to tbl
  mutate(sim = seq(1:1000)) %>% # create simulation identifier
  select(sim, everything()) # make sim column first in the data

new_data_ineq <- data_ineq %>%
  # Let's keep it simple and obvious. The min of z_ideo is "very liberal."
  # The max of z_ideo is "very conservative."
  # We should expect to see large magnitude differences.
  modelr::data_grid(.model = m7_ord_logit_clmm, z_lrscale = c(min(z_lrscale, na.rm=T), max(z_lrscale, na.rm=T)), downward_mob = c(0,1)) %>%
  # because we got 1000 sims, we need to repeat this 1000 times
  # ordinal has a slice function that will clash with dplyr
  dplyr::slice(rep(row_number(), 1000)) 

sim_m7_ord_logit_clmm <- sim_m7_ord_logit_clmm %>%
  # repeat it 4 times because we have two values of z_lrscale and two values for downward_mob
  dplyr::slice(rep(row_number(), 4)) %>%
  # arrange by simulation number after we repeated the data twice
  arrange(sim) 

sim_m7_ord_logit_clmm 

sim_m7_ord_logit_clmm <- sim_m7_ord_logit_clmm %>% 
  # rename these to be clear they're simulated coefficients
  rename_at(vars("downward_mob", "z_lrscale" , "z_incdec" , "z_saliinequ" , "z_saliunempl" , "forbrn" , "educcat"),
            ~paste0("coef", .)) %>%
  bind_cols(., new_data_ineq) 


sim_m7_ord_logit_clmm <- sim_m7_ord_logit_clmm %>%
  mutate(xb = (downward_mob*coefdownward_mob) + (z_lrscale*coefz_lrscale) + (z_incdec*coefz_incdec) +
           (z_saliinequ*coefz_saliinequ) + (z_saliunempl*coefz_saliunempl) + (forbrn*coefforbrn) + (educcat*coefeduccat)) %>%
  select(sim, xb, everything()) 


sim_m7_ord_logit_clmm <- sim_m7_ord_logit_clmm %>%
  mutate(logit1 = `Strongly disagree|Disagree` - xb,
         logit2 = `Disagree|Nor agree nor disagree` - xb,
         logit3 = `Nor agree nor disagree|Agree` - xb,
         logit4 = `Agree|Strongly Agree` - xb)  %>%
  mutate_at(vars(contains("logit")), list(p = ~plogis(.))) 


sim_m7_ord_logit_clmm <- sim_m7_ord_logit_clmm %>%
  mutate(p1 = logit1_p,
         p2 = logit2_p - logit1_p,
         p3 = logit3_p - logit2_p,
         p4 = logit4_p - logit3_p,
         p5 = 1 - logit4_p,
         # sump should be 1. Let's check.
         sump = p1 + p2 + p3 + p4 + p5) 

sim_m7_ord_logit_clmm %>%
  select(sim, z_lrscale, downward_mob, p1:p5, sump)


#pivot_longer(c(-sim, -ideo), names_to = "var", values_to = "val")

sim_table_df <- sim_m7_ord_logit_clmm  %>% 
  mutate(
    lrscale = case_when(
      z_lrscale < 0 ~ "Left",
      z_lrscale > 0 ~ "Right" ),
    downward_mob = ifelse(downward_mob == 1, "Yes", "No")
    )  %>% 
#  mutate(lrscale = c("Left", "Right"),
   #      downward_mob = c("No", "Yes")) %>%
  select(sim, lrscale, downward_mob, p1:p5) %>%
  pivot_longer(c(-sim, -lrscale, -downward_mob), names_to = "var", values_to = "val")  %>%
  #gather(var, val, -sim, -lrscale) %>%
  group_by(downward_mob, lrscale, var) %>%
  summarize(mean = mean(val),
            lwr = quantile(val, .025),
            upr = quantile(val, .975)) %>%
  mutate(var = rep(c("Strongly\ndisagree", "Disagree",
                     "Nor agree\nnor disagree",
                     "Agree", "Strongly\nAgree"))) %>%
  mutate_if(is.numeric, ~round(., 3)) 

sim_table_df %>%
  kable(., format="html", table.attr='id="stevetable"',
        col.names=c("Ideology", "The inflow of immigrants is a major
reason for the rise of income inequality in the Country", "Mean(Probability)", "Lower Bound", "Upper Bound"),
        align = c("l","l","c","c","c"))


sim_table_df %>%
ggplot(.,aes(var, mean, ymin=lwr, ymax=upr, color=lrscale, shape=lrscale)) +
  #geom_hline(yintercept = .5, linetype ="dashed") +
  #theme_steve_web() + 
  theme_bw() +
  scale_color_manual(values = c("#377EB8", "#E41A1C")) +
  #geom_pointrange(size=.8) +
  geom_pointrange() +
 # scale_y_continuous(limits = c(0, 1)) +
 # facet_wrap(~downward_mob, labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
  #                                               #    "Yes" = "Expect Downward Mobility"))) + 
   labs(color = "", shape = "",
       x = "Responses",  y = "Predicted Probability of the Response (with 95% Intervals)",
       title = "The Effect of Perceived Downward Mobility and Political Ideology on the Attitudes Toward Immigration and Inequality",
       subtitle = "Dependent Variable: The inflow of immigrants is a major reason for the rise of income inequality in the Country",
       caption = "Data: Politics and Inequality Survey (2019). Partial Pooling: Multi-level Ordinal Logit with nested country and region random effects.") +
  facet_wrap(~downward_mob,  labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
                                                                    "Yes" = "Expect Downward Mobility"))) 


