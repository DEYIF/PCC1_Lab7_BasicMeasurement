#import dataset
library(openxlsx)
library(ggplot2)
# sheet 3 is the sheet with the after coffee data
data <- read.xlsx("dataset.xlsx", sheet = 3)

data$testID <- factor(data$testID, 
                      levels = c(4, 5, 6, 7), 
                      labels = c("Immediate", "After 10 minutes", "After 20 minutes", "After 30 minutes"))
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
  #scale_color_manual(values = c("Watch HRV" = "#96abdc", "PPG Device HRV" = "#ec5e57"))  # set color


# analyse the data, wheather the watch and the device have significant difference
t.test(data$watch, data$device, paired = TRUE, alternative = "two.sided")
# if the p-value is less than 0.05, then the difference is significant