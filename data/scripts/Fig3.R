this_file <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this_file)

data <- data.frame(
  Category = c(
    "Since the last year",
    "In the last 3 years",
    "In the last 6 years",
    "In the last 10 years or more",
    "Since I started with my research group",
    "No established practices"
  ),
  Percent = c(2.0, 23.8, 32.7, 13.9, 7.9, 19.8)
)

library(ggplot2)
p <- ggplot(data, aes(x = reorder(Category, Percent), y = Percent)) +
  geom_col(fill = "#4575b4") +
  geom_text(aes(label = paste0(Percent, "%")), hjust = -0.09, size = 4.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +  # <- espaço extra à direita
  coord_flip() +
  labs(x = NULL, y = "Percentage") +
  theme_minimal(base_size = 18, base_family = "Times New Roman") +
  theme(plot.title = element_text(size = 10),axis.text.y = element_text(color = "black"),axis.title.x = element_text(size = 9))


ggsave("Fig3.png", plot = p, width = 6.9, height = 2.7)
getwd()