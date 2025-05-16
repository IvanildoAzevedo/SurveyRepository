this_file <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this_file)


library(extrafont)
library(tidyverse)

q25_data <- tribble(
  ~Factor, ~Always, ~VeryOften, ~Sometimes, ~Rarely, ~Never,
  "Fraud (F10)", 16, 7, 23, 36, 2,
  "Pressure to publish (F3)", 8, 43, 35, 5, 2,
  "Lack of oversight/mentoring (F9)", 6, 31, 49, 7, 1,
  "Weak peer review (F7)", 8, 34, 41, 10, 3,
  "Selective reporting (F1)", 13, 37, 34, 12, 0,
  "Mistakes/inadequate expertise (F4)", 11, 49, 35, 2, 1,
  "Raw data unavailable (F2)", 18, 49, 27, 4, 0,
  "Code/protocols unavailable (F6)", 27, 49, 19, 3, 0,
  "Technical expertise needed (F8)", 6, 39, 36, 16, 2,
  "Human factors (F5)", 13, 36, 35, 8, 0
)

q25_long <- q25_data %>%
  pivot_longer(cols = -Factor, names_to = "Rating", values_to = "Count") %>%
  mutate(
    Score = case_when(
      Rating == "Never" ~ 1,
      Rating == "Rarely" ~ 2,
      Rating == "Sometimes" ~ 3,
      Rating == "VeryOften" ~ 4,
      Rating == "Always" ~ 5
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
    breaks = 1:5,
    labels = c("Never", "Rarely", "Sometimes", "Very Often", "Always")
  ) +
  labs(
    x = NULL,
    y = "Frequency of Contribution"
  ) +
  theme_minimal(base_size = 12, base_family = "Times New Roman") +
  theme(
    axis.text.y = element_text(color = "black", size = 9),
    axis.text.x = element_text(size = 5.5),
    plot.title = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(size = 8)
  )

ggsave("Fig4.png", plot = p, width = 4.2, height = 2.7)
getwd()