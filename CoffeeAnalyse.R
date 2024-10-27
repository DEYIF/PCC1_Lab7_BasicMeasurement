#import dataset
library(openxlsx)
library(ggplot2)
# sheet 3 is the sheet with the after coffee data
data <- read.xlsx("dataset.xlsx", sheet = 3)

# plot the data
ggplot(data, aes(x = time)) +
  geom_line(aes(y = watch, color = "Watch HRV"), size = 0.8) +
  geom_point(aes(y = watch, color = "Watch HRV"), size = 2) +
  geom_line(aes(y = device, color = "PPG Device HRV"), size = 0.8) +
  geom_point(aes(y = device, color = "PPG Device HRV"), size = 2) +
  facet_wrap(~ testID, ncol = 2) +
  labs(title = "Heart Rate Values After Coffee Consumption Over Time",
       x = "time (s)",
       y = "Hear Rate Value (bpm)",
       color = "Measurement tool") +
  theme_minimal() +
  theme(legend.position = "top")#+

# analyse the data, using Kruskal-Wallis test
kruskal_result <- kruskal.test(watch ~ testID, data = data)
print(kruskal_result)
kruskal_result <- kruskal.test(device ~ testID, data = data)
print(kruskal_result)

# using Dunn's test for post-hoc analysis
library(dunn.test)
dunn_result <- dunn.test(data$watch, data$testID, method = "bonferroni")
print(dunn_result)

# Friedman test
friedman_result <- friedman.test(cbind(watch, device) ~ testID | time, data = data)
print(friedman_result)