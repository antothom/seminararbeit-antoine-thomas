## Creating plot for visualization of the cost structure depending on the occupancy

cost_structure <- tibble(occ = c(.5,.7,1), fix_costs = c(.869,.8438,.813), var_costs = c(.131,.1563,.187))

lm_fix_costs <- lm(formula = fix_costs ~ occ, data = cost_structure)
lm_var_costs <- lm(formula = var_costs ~ occ, data = cost_structure)

tibble(occ = seq(0, 1.1, by = 0.1)) %>%
  mutate(fix_costs = predict(lm_fix_costs,.),
         var_costs = predict(lm_var_costs,.)) %>%
  mutate(tot_costs = fix_costs+var_costs) %>%
  gather("cost_cat", "value", 2:4) %>%
  mutate(cost_cat = replace(cost_cat, cost_cat == "var_costs", "Variable costs"),
         cost_cat = replace(cost_cat, cost_cat == "fix_costs", "Fixed costs"),
         cost_cat = replace(cost_cat, cost_cat == "tot_costs", "Total costs")) %>%
  ggplot(aes(x = occ, y = value, colour = cost_cat)) +
  geom_point() +
  geom_line(stat = "smooth", method = "lm", se = F, linetype = "dashed", alpha = .7) +
  stat_regline_equation(label.x.npc = .75, label.y.npc = .2, data = . %>% filter(cost_cat == "Variable costs"), key_glyph = draw_key_blank) +
  stat_regline_equation(label.x.npc = .75, label.y.npc = .72, data = . %>% filter(cost_cat == "Fixed costs"), key_glyph = draw_key_blank) +
  scale_colour_discrete(limits = c("Fixed costs", "Variable costs", "Total costs")) +
  labs(x = "Occupancy", y = "Costs", title = "Cost structures with differentiation of fixed and variable costs (in %)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)