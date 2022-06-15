
## Regression Table ####


modelsummary(c(no_normalized_model_list, pooled_models, partial_pool_models),
              coef_map = cm,
              stars = TRUE,
              title = "Regression - Ordinal Models",
              notes = table_notes)  |> 
        add_header_above(c(" " = 1, "No rescaled variables" = 2, "Complete Pooling" = 3,  "Partial Pooling" = 3)) |> 
        kable_styling(full_width = F, latex_options =  c("scale_down", "HOLD_position")) #|> 
        #landscape()

# modelsummary(models, escape = FALSE,
#              coef_map = cm,
#              stars = TRUE,
#              # gof_omit = ".*", 
#              title = "Regression - Multilevel Ordinal Models",
#              #   statistic = c("s.e. = {std.error}"),
#              notes = table_notes) |> 
#   #  add_header_above(c(" " = 1, "Proximity Model" = 3,  "Directional Model" = 3)) |> 
#   kable_styling(full_width = F, latex_options =  c("scale_down", "HOLD_position")) #|> 
# #landscape()


# modelplot(
#   no_normalized_model_list
# ) +
#   theme_bw() #+
#  # scale_color_viridis(type = 'qual')
# 
# modelplot(pooled_models) +
#   theme_bw()
# 
# modelplot(partial_pool_models)

