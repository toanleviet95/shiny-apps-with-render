library(ggplot2)

n <- 1000
sample_coins <- sample(x = 0:1, size = n, replace = TRUE)
coin_res <- cumsum(sample_coins)/(1:n)
dt_coin_res <- data.frame(trials = c(0:n), probs = c(0, coin_res))

ggplot(dt_coin_res, mapping = aes(x = trials, y = probs)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "blue") +
  labs(x = "Lần tung", y = "Xác suất mặt số") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
ggsave(filename = "probability_coins.png", width = 9, height = 6)
