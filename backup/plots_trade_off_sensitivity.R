## Dummies for 'methodology' chapter
read_csv2("backup/dummy_trade_off_analysis.csv") %>%
  gather(key = "Category", value = "Value", 2:4) %>%
  ggplot(aes(x = `Room rate`, y = Value, colour = Category)) +
  geom_point() +
  geom_line() +
  labs(y = "(EUR)", title = "Trade-Off Analysis (normal demand)") +
  theme_bw() +
  theme(legend.title = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom")

read_csv2("backup/dummy_sensitivity_analysis.csv") %>%
  gather(key = "Occupancy slope", value = "Profit", 2:13) %>%
  mutate(`Occupancy slope` = as.numeric(gsub(",",".",`Occupancy slope`))) %>%
  arrange(`Occupancy slope`, `Room rate`) %>%
  mutate(`Occupancy slope` = as.factor(`Occupancy slope`)) %>%
  ggplot(aes(x = `Room rate`, y = Profit, colour = `Occupancy slope`)) +
  geom_point() +
  geom_line() +
  labs(title = "Sensitivity Analysis (normal demand)") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

## Results
readxl::read_xlsx("backup/munich_trade_off_analysis.xlsx") %>%
  gather(key = "Type", value = "Value", 2:5) %>%
  filter(Type != "Occupancy",
         Demand == "Normal") %>%
  ggplot(aes(x = `Room rate`, y = Value, colour = Type)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "orange") +
  theme_bw() +
  labs(y = "EUR", title = "Trade-off Analysis (normal demand)") +
  theme(legend.title = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom")

readxl::read_xlsx("backup/munich_trade_off_analysis.xlsx") %>%
  gather(key = "Type", value = "Value", 2:5) %>%
  filter(Type != "Occupancy",
         Demand == "Oktoberfest") %>%
  ggplot(aes(x = `Room rate`, y = Value, colour = Type)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "orange") +
  theme_bw() +
  labs(y = "EUR", title = "Trade-off Analysis (Oktoberfest)") +
  theme(legend.title = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom")


readxl::read_xlsx("backup/sensitivity_analysis_normal_demand.xlsx") %>%
  gather(key = "Slope", value = "Profit", 2:18) %>%
  mutate(Slope = as.numeric(Slope)) %>%
  mutate(Slope = as.factor(Slope)) %>%
  ggplot(aes(x = `room rate`, y = Profit, colour = Slope)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw() +
  labs(x = "Room rate", title = "Sensitivity analysis (normal demand)", colour = "Occupancy slope") +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

readxl::read_xlsx("backup/sensitivity_analysis_oktoberfest.xlsx") %>%
  gather(key = "Slope", value = "Profit", 2:18) %>%
  mutate(Slope = as.numeric(Slope)) %>%
  mutate(Slope = as.factor(Slope)) %>%
  ggplot(aes(x = `room rate`, y = Profit, colour = Slope)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_bw() +
  labs(x = "Room rate", title = "Sensitivity analysis (Oktoberfest)", colour = "Occupancy slope") +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")