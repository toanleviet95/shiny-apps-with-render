library(ggplot2)

dices <- c(1:6)
n <- 50
sample_dice <- sample(dices, size = n*2, replace = TRUE)
results_dices <- factor(rowSums(matrix(sample_dice, ncol = 2)),
                        levels = c(2:12))
results_dices

data_dices <- data.frame(prop.table(table(results_dices)))
data_dices


ggplot(data = data_dices, mapping = aes(x = results_dices, y = Freq)) +
  geom_bar(stat = "identity", fill = "forestgreen", width = 0.6) +
  labs(x = "Số điểm", y = "Tỷ lệ") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
ggsave(filename = "barplot_dices.png", width = 9, height = 6)

