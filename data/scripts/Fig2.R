# Install if necessary
# install.packages("ggplot2")
# install.packages("tidyverse")

this_file <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this_file)

library(ggplot2)
library(tidyverse)

# Data
data <- tribble(
  ~Definition,       ~Response,               ~Count,
  "Repeatability",   "Strongly Agree",        33,
  "Repeatability",   "Agree",                 57,
  "Repeatability",   "Neither",               5,
  "Repeatability",   "Disagree",              6,
  "Repeatability",   "Strongly Disagree",     0,
  
  "Reproducibility", "Strongly Agree",        34,
  "Reproducibility", "Agree",                 53,
  "Reproducibility", "Neither",               7,
  "Reproducibility", "Disagree",              5,
  "Reproducibility", "Strongly Disagree",     2,
  
  "Replicability",   "Strongly Agree",        35,
  "Replicability",   "Agree",                 44,
  "Replicability",   "Neither",               12,
  "Replicability",   "Disagree",              9,
  "Replicability",   "Strongly Disagree",     1
)

# Categories
data$Response <- factor(data$Response, levels = c(
  "Strongly Disagree", "Disagree", "Neither", "Agree", "Strongly Agree"
  #"Strongly Agree", "Agree", "Neither", "Disagree", "Strongly Disagree"
))

# Percentages
data <- data %>%
  group_by(Definition) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Gráfico com barras horizontais empilhadas e normalizadas (100%)
p <- ggplot(data, aes(x = Definition, y = Count, fill = Response)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  geom_text(aes(
    label = ifelse(Count > 0, paste0(round(Percent, 1), "%"), ""),
    hjust = ifelse(Response == "Strongly Disagree", -0.4, 0.5)
  ),
  position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Strongly Disagree" = "#d73027",
      "Disagree" = "#fc8d59",
      "Neither" = "#ffffbf",
      "Agree" = "#91bfdb",
      "Strongly Agree" = "#4575b4"
    )
  ) +
  labs(
    x = NULL,
    y = "Percentage of Responses",
    fill = ""
  ) +
  coord_flip() +
  theme_minimal(base_size = 20, base_family = "Times New Roman") +
  theme(
  legend.position = "top",
  legend.direction = "horizontal",
  legend.justification = "right",
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 13),
  axis.text.x = element_text(size = 13),
  axis.text.y = element_text(color = "black"),
  axis.title.x = element_text(size = 13),
  plot.margin = margin(t = -8, r = 0, b = 0, l = 0)
) +
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave("Fig2.png", plot = p, width = 6.8, height = 3)
getwd()
