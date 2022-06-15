
#ova_plots

ova_plot_model_2_MM <- ova_plots[[1]][[1]] # model 2 MM predicted probabilities
ova_plot_fd_model_2_MM <- ova_plots[[1]][[2]] # model 2 MM fd

ova_plot_model_3_MM <- ova_plots[[2]][[1]] # model 3 MM predicted probabilities
ova_plot_fd_model_3_MM <- ova_plots[[2]][[2]] # model 3 MM fd

ova_plot_model_4_MM <- ova_plots[[3]][[1]] # model 3 MM predicted probabilities
ova_plot_fd_model_4_MM <- ova_plots[[3]][[2]] # model 3 MM fd

# Main FD as tibble
df_ova_plot_fd_model_2_MM <- tibble(
  var  = levels(data_ineq$stmigrant), #unique(data_ineq$stmigrant) 
  mean_prob = ova_plot_fd_model_2_MM[,,1],
  lwr = ova_plot_fd_model_2_MM[1, , 2],
  upr = ova_plot_fd_model_2_MM[1, , 3]
)


# additional FD as tibble
df_ova_plot_fd_model_3_MM <- tibble(
  var  = levels(data_ineq$stmigrant), #unique(data_ineq$stmigrant) 
  mean_prob = ova_plot_fd_model_3_MM[,,1],
  lwr = ova_plot_fd_model_3_MM[1, , 2],
  upr = ova_plot_fd_model_3_MM[1, , 3]
)

# additional FD as tibble
df_ova_plot_fd_model_4_MM <- tibble(
  var  = levels(data_ineq$stmigrant), #unique(data_ineq$stmigrant) 
  mean_prob = ova_plot_fd_model_4_MM[,,1],
  lwr = ova_plot_fd_model_4_MM[1, , 2],
  upr = ova_plot_fd_model_4_MM[1, , 3]
)


plot_df_ova_plot_fd_model_2_MM <- df_ova_plot_fd_model_2_MM |> 
  ggplot(aes(0, mean_prob, ymin=lwr, ymax=upr, color=var)) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_color_manual(values = c("#377EB8", "#E41A1C")) +
  #geom_pointrange(size=.8) +
  geom_pointrange(position = position_dodge(width = 1)) +
  # scale_y_continuous(limits = c(0, 1)) +
  # facet_wrap(~downward_mob, labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
  #                                               #    "Yes" = "Expect Downward Mobility"))) + 
  labs(color = "", shape = "",
       x = "Responses",  y = "Predicted Probability of the Response (with 95% Intervals)",
       title = "First Difference in Predicted Probability: Model 2 MM",
       subtitle = "Dependent Variable: The inflow of immigrants is a major reason for the rise of income inequality in the Country",
       caption = "Data: Politics and Inequality Survey (2019). Partial Pooling: Multi-level Ordinal Logit with country and country-region random effects.") #+
# facet_wrap(~downward_mob,  labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
# "Yes" = "Expect Downward Mobility"))) 



plot_df_ova_plot_fd_model_3_MM <- df_ova_plot_fd_model_2_MM |> 
  ggplot(aes(0, mean_prob, ymin=lwr, ymax=upr, color=var)) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_color_manual(values = c("#377EB8", "#E41A1C")) +
  #geom_pointrange(size=.8) +
  geom_pointrange(position = position_dodge(width = 1)) +
  # scale_y_continuous(limits = c(0, 1)) +
  # facet_wrap(~downward_mob, labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
  #                                               #    "Yes" = "Expect Downward Mobility"))) + 
  labs(color = "", shape = "",
       x = "Responses",  y = "Predicted Probability of the Response (with 95% Intervals)",
       title = "First Difference in Predicted Probability: Model 3 MM",
       subtitle = "Dependent Variable: The inflow of immigrants is a major reason for the rise of income inequality in the Country",
       caption = "Data: Politics and Inequality Survey (2019). Partial Pooling: Multi-level Ordinal Logit with country and country-region random effects.") #+
# facet_wrap(~downward_mob,  labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
# "Yes" = "Expect Downward Mobility"))) 


plot_df_ova_plot_fd_model_4_MM <- df_ova_plot_fd_model_2_MM |> 
  ggplot(aes(0, mean_prob, ymin=lwr, ymax=upr, color=var)) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #scale_color_manual(values = c("#377EB8", "#E41A1C")) +
  #geom_pointrange(size=.8) +
  geom_pointrange(position = position_dodge(width = 1)) +
  # scale_y_continuous(limits = c(0, 1)) +
  # facet_wrap(~downward_mob, labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
  #                                               #    "Yes" = "Expect Downward Mobility"))) + 
  labs(color = "", shape = "",
       x = "Responses",  y = "Predicted Probability of the Response (with 95% Intervals)",
       title = "First Difference in Predicted Probability: Model 4 MM",
       subtitle = "Dependent Variable: The inflow of immigrants is a major reason for the rise of income inequality in the Country",
       caption = "Data: Politics and Inequality Survey (2019). Partial Pooling: Multi-level Ordinal Logit with country and country-region random effects.") #+
# facet_wrap(~downward_mob,  labeller=labeller(downward_mob = c("No" = "Do not Expect Downward Mobility",
# "Yes" = "Expect Downward Mobility"))) 




