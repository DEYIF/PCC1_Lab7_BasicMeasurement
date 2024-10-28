#import dataset
library(openxlsx)
library(ggplot2)
# sheet 3 is the sheet with the after coffee data
data <- read.xlsx("dataset.xlsx", sheet = 2)

# Step 1: Calculate the difference and mean of the two measurements
data$diff <- data$watch - data$device
data$mean <- (data$watch + data$device) / 2

# Step 2: Calculate the mean difference and standard deviation of the difference
mean_diff <- mean(data$diff)                  # 差值均值
sd_diff <- sd(data$diff)                      # 差值的标准差
upper_limit <- mean_diff + 1.96 * sd_diff     # 上限
lower_limit <- mean_diff - 1.96 * sd_diff     # 下限

# Step 3: plot Bland-Altman figure
plot(data$mean, data$diff, main="Bland-Altman Plot",
     xlab="Mean of Watch and Device", ylab="Difference (Watch - Device)",
     pch=19, col="blue")
abline(h=mean_diff, col="red", lwd=2)         # mean diff
abline(h=upper_limit, col="green", lty=2)     # consistency upper
abline(h=lower_limit, col="green", lty=2)     # consistency lower
legend("topright", legend=c("Mean Diff", "Upper Limit", "Lower Limit"),
       col=c("red", "green", "green"), lty=c(1, 2, 2))


library(dplyr)

precision_analysis <- data %>%
  group_by(observation) %>%
  summarise(
    mean_watch = mean(watch),
    sd_watch = sd(watch),
    cv_watch = (sd(watch) / mean(watch)) * 100,
    mean_device = mean(device),
    sd_device = sd(device),
    cv_device = (sd(device) / mean(device)) * 100  # 变异系数，以百分比表示
  )

print(precision_analysis)