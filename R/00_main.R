library (tidyverse)
library (here) 
results <- read.csv(here("data/raw/CTmax_assay_results.csv"))

# cleaning missing values
results1 <- results [-(72:151),]
results1$date <- as.character(results1$date)
results1 <- results1 %>%
  mutate(date = as_date(date, tz = NULL, format = NULL))

results1 <- results1 %>%
  mutate(across(c(fish_group, testing_group_number), factor))

summary (results1)

# Descriptive stats
## Cont variables 

### mean
mean_resC <- mean(results1$results_c)
mean_resC
### median
median_resC <- median (results1$results_c)
median_resC
### quartiles 
quart_resC <- quantile(results1$results_c, probs = c(0.25, 0.5, 0.75),
                     na.rm = TRUE)
### sample variance
sam_var_resC <- var(results1$results_c, na.rm =  TRUE) 
### sample standard deviation
sd_resC <- sd(results1$results_c, na.rm=TRUE)
### sample range 
range_resC <- range (results1$results_c, na.rm=TRUE)
### interquartile range (IQR)
IQR (results1$results_c, na.rm=TRUE)
#### show all the info at once
summary (results1$results_c)

# Distribution
mean_resCC <- mean(results1$results_c[results1$fish_group == "control "], na.rm = TRUE)
mean_resCR <- mean(results1$results_c[results1$fish_group == "recovery"], na.rm = TRUE)
median_resCC <- median (results1$results_c[results1$fish_group == "control "], na.rm = TRUE)
median_resCR <- median (results1$results_c[results1$fish_group == "recovery"], na.rm = TRUE)

hist(results1$results_c, freq = FALSE,
     xlab = "CT max, C", main = "Distribution of CT max in Danio rerio")
abline(v = mean_resC, col = "red") + abline(v = median_resC, col = "blue")
#### The distribution of CTmax is skewed, since the sample mean and the median are not equal to each other. 
#### Also the shape of the histogram indicates that here we're dealing not with the normal distribution.

# Graph comparison 
boxplot(results1$results_c~results1$fish_group, xlab= "Fish group", ylab= "CTmax results, C") 
points(results1$results_c~results1$fish_group) 

results2 <- results1 %>% group_by(fish_group) %>%  mutate(mean = mean(results_c))
results2 <- results2 %>% group_by(fish_group) %>%  mutate(median = median(results_c))
ggplot(results2, aes(x = results_c)) +
  geom_histogram(binwidth=0.5, colour= "black", fill="#CC99FF") +
  facet_grid(fish_group~.) +xlab("CTmax results, C") + ylab("")+
  geom_vline(aes(xintercept = mean, group = fish_group), colour = 'red', lwd = 1)+
  geom_vline(aes(xintercept = median, group = fish_group), colour = 'blue', lwd = 1)

# QQ plot 
results1 %>%
  ggplot(aes(sample = results_c)) + 
  geom_qq_line(distribution = stats::qnorm) +
  geom_qq(color = "steelblue", distribution = stats::qnorm) + 
  xlab("Quantiles") +
  ylab("CTmax results, C") +
  theme_bw() + facet_grid(~fish_group)

## Shapiro test 
shapiro_test <- results1 %>%
  group_by(fish_group) %>%
  rstatix::shapiro_test(results_c)
## p-value for control group = 0.00000336 -> not normally distributed 
##  p-value for recovery group = 0.0104 -> not normally distributed


# Kruskal-Wallis test
kruskal_test <- kruskal.test(results_c ~ fish_group, data = results1)
### p value is less than 0.05 -> there's a weak evidence of a difference between control and recovery group

# Wilcox test 
wilcox_test <- results1 %>%
  rstatix:: wilcox_test(results_c ~ fish_group)
### p value is less than 0.05 -> there's a weak evidence of a difference between control and recovery group

usethis::create_github_token()
gitcreds::gitcreds_set() 
