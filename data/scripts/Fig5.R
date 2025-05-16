this_file <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this_file)

library(tidyverse)

q25_data <- tribble(
  ~Factor, ~VeryLikely, ~Likely, ~NotVeryLikely, ~NotAtAllLikely,
  "Professional incentives 
  for reproducing (I1)", 40, 45, 9, 5,
  "More time to teach and su-
  pervise new researchers (I2)", 27, 48, 18, 5,
  "Financial incentive for 
  adopting 3R practices (I3)", 32, 41, 17, 9,
  "Adopting practices to 
  increase the 3R (I4)", 40, 44, 13, 1
)

q25_long <- q25_data %>%
  pivot_longer(cols = -Factor, names_to = "Rating", values_to = "Count") %>%
  mutate(
    Score = case_when(
      Rating == "NotAtAllLikely" ~ 1,
      Rating == "NotVeryLikely" ~ 2,
      Rating == "Likely" ~ 3,
      Rating == "VeryLikely" ~ 4
    )
  ) %>%
  uncount(weights = Count)

p <- ggplot(q25_long, aes(
  x = fct_reorder(Factor, Score, .fun = median), 
  y = Score
)) +
  geom_violin(fill = "#91bfdb", alpha = 0.6, color = NA, scale = "area") +
  geom_boxplot(
    width = 0.5,
    fill = "#4575b4", 
    color = "#2c3e50",
    outlier.shape = 21, 
    outlier.fill = "#e74c3c", 
    outlier.color = "#c0392b"
  ) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "grey", fatten = 1) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "white", fatten = 1) +
  coord_flip() +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("Not at all likely", "Not very likely", "Likely", "Very likely")
  ) +
  labs(
    x = NULL,
    y = "Likelihood to foster 3R"
  ) +
  theme_minimal(base_size = 11, base_family = "Times New Roman") +
  theme(
    axis.text.y = element_text(color = "black", size = 9),
    axis.text.x = element_text(size = 7),
    plot.title = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(size = 8),
    plot.margin = margin(t = 0, r = 9, b = 0, l = -0.2)
  )

ggsave("Fig5.png", plot = p, width = 3.8, height = 2)
getwd()
