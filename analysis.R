setwd("/Users/silkedesmedt/Library/Mobile Documents/com~apple~CloudDocs/School/Master/1e master/Jaarvakken/Research Project/PsychoPy Experiment/data_SilkeDeSmedt")
data <- read.csv('combined_data.csv')
# Libraries
library(lme4)

#Take out the practice trial
data <- data[data$practice != 1, ]
head(data)
str(data)

## MAKE NEW VARIABLES ## 
#Make a new variable for re-click, novel click and high value click 
data$reclick <- ifelse(data$consecutive.distance == 0, 1, 0)
data$novel.click <- 1 - data$reclick
data$high.value.click<- ifelse(data$clicked.reward >= 76, 1, 0) # Define high value reward as a reward higher than 76 (top 10% of 85)

#Make a general exploration variable
data$exploration <- data$consecutive.distance + data$distance.to.top.10.

#New variable for even or uneven pp number
data$participant_even <- ifelse(data$Participant.Nummer %% 2 == 0, 1, 0)
data$participant_id <- factor(data$participant_number)

# Put everything in log scale
data$exploration <- data$exploration + 0.0001
data$consecutive.distance <- data$consecutive.distance + 0.0001
data$distance.to.top.10. <- data$distance.to.top.10. + 0.0001
data$log_exploration <- log(data$exploration)
data$log_consecutive <- log(data$consecutive.distance)
data$log_distance_to_top_10 <- log(data$distance.to.top.10.)

## Count the number of reclicks
# Count the number of reclicks
num_reclicks <- sum(data$reclick)

# Data subsets
mean_condition <- subset(data, block.type == "mean")
total_condition <- subset(data, block.type == "total")
head(mean_condition)

## Does doing the other experiment first matter? Does it result in an overall lower performance? ##
# Subset the data to include only the last trial (trial number 19)
last_trial_data <- data[data$trial.number == 19, ]
head(last_trial_data)

meantotallasttrial <- mean(last_trial_data$total)

# Split the last trial data into two groups based on participant number parity
even_participants_last_trial <- last_trial_data[last_trial_data$participant_number %% 2 == 0, ]
odd_participants_last_trial <- last_trial_data[last_trial_data$participant_number %% 2 == 1, ]

# Calculate the mean total score for each group
mean_score_even_last_trial <- mean(even_participants_last_trial$total, na.rm = TRUE)
mean_score_odd_last_trial <- mean(odd_participants_last_trial$total, na.rm = TRUE)

# Perform t-test
t_test_evenodd <- t.test(even_participants_last_trial$total, odd_participants_last_trial$total)
print(t_test_evenodd)

# Only first block
first_block_data <- data[data$block.number == 2, ]
mean_condition1 <- subset(first_block_data, block.type == "mean")
total_condition1 <- subset(first_block_data, block.type == "total")

mean_consec_dist1 <- mean(mean_condition1$consecutive.distance)
mean_dist_to_top_101 <- mean(mean_condition1$distance.to.top.10.)
mean_high_value1 <- mean(mean_condition1$high.value.click)
mean_novel1 <- mean(mean_condition1$novel.click)
mean_mean1 <- mean(mean_condition1$exploration)

total_consec_dist1 <- mean(total_condition1$consecutive.distance)
total_dist_to_top_101 <- mean(total_condition1$distance.to.top.10.)
total_high_value1 <- mean(total_condition1$high.value.click)
total_novel1 <- mean(total_condition1$novel.click)
total_mean1 <- mean(total_condition1$exploration)

## Mean v total over all blocks
# Mean condition
mean_consec_dist <- mean(mean_condition$consecutive.distance)
mean_dist_to_top_10 <- mean(mean_condition$distance.to.top.10.)
mean_high_value <- mean(mean_condition$high.value.click)
mean_novel <- mean(mean_condition$novel.click)
mean_mean <- mean(mean_condition$exploration)

# Total condition
total_consec_dist <- mean(total_condition$consecutive.distance)
total_dist_to_top_10 <- mean(total_condition$distance.to.top.10.)
total_high_value <- mean(total_condition$high.value.click)
total_novel <- mean(total_condition$novel.click)
total_mean <- mean(total_condition$exploration)

# t-test to compare means of exploration between mean_condition and total_condition
t_test_exploration <- t.test(mean_condition$exploration, total_condition$exploration)
t_test_consec <- t.test(mean_condition$consecutive.distance, total_condition$consecutive.distance)
t_test_high_value <- t.test(mean_condition$high.value.click, total_condition$high.value.click)
t_test_novel_clicks <- t.test(mean_condition$novel.click, total_condition$novel.click)
t_test_top10 <- t.test(mean_condition$distance.to.top.10., total_condition$distance.to.top.10.)

# Print the result
print(t_test_exploration)
print(t_test_consec)
print(t_test_high_value)
print(t_test_novel_clicks)
print(t_test_top10)

## Take only the first block
# t-test to compare means of exploration between mean_condition and total_condition
t_test_exploration1 <- t.test(mean_condition1$exploration, total_condition1$exploration)
t_test_consec1 <- t.test(mean_condition1$consecutive.distance, total_condition1$consecutive.distance)
t_test_high_value1 <- t.test(mean_condition1$high.value.click, total_condition1$high.value.click)
t_test_novel_clicks1 <- t.test(mean_condition1$novel.click, total_condition1$novel.click)
t_test_top101 <- t.test(mean_condition1$distance.to.top.10., total_condition1$distance.to.top.10.)

# Print the result
print(t_test_exploration1)
print(t_test_consec1)
print(t_test_high_value1)
print(t_test_novel_clicks1)
print(t_test_top101)

# LME's main effects
lme_consec <- lmer(consecutive.distance ~ block.type + (1 | participant_number), data = data)
lme_top10 <- lmer(distance.to.top.10. ~ block.type + (1 | participant_number), data = data)
lme_highvalue <- lmer(high.value.click ~ block.type+ (1 | participant_number), data = data)
lme_novelclick <- lmer(novel.click ~ block.type + (1 | participant_number), data = data)
lme_expl <- lmer(exploration ~ block.type + (1 | participant_number), data = data)
lme_blockorder <- lmer(exploration ~ block.number + (1 | participant_number), data = data )
lme_all <- lmer(exploration + high.value.click + novel.click + reclick ~ block.type + trial.number+ (1 | participant_number), data = data)

summary(lme_consec)
summary(lme_top10)
summary(lme_highvalue)
summary(lme_novelclick)
summary(lme_expl)
summary(lme_blockorder)
summary(lme_all)

## Calculations ##
# High value clicks per round
blockmean_high_value <- aggregate(high.value.click ~ block.number, data = mean_condition, FUN = mean)
blocktotal_high_value <- aggregate(high.value.click ~ block.number, data = total_condition, FUN = mean)

# Novel clicks per round
blockmean_novel_click <- aggregate(novel.click ~ block.number, data = mean_condition, FUN = mean)
blocktotal_novel_click <- aggregate(novel.click ~ block.number, data = total_condition, FUN = mean)

# High value clicks per trial
trialmean_high_value <- aggregate(high.value.click ~ trial.number, data = mean_condition, FUN = mean)
trialtotal_high_value <- aggregate(high.value.click ~ trial.number, data = total_condition, FUN = mean)

# Novel clicks per trial
trialmean_novel_click <- aggregate(novel.click ~ trial.number, data = mean_condition, FUN = mean)
trialtotal_novel_click <- aggregate(novel.click ~ trial.number, data = total_condition, FUN = mean)

# Log consecutive distance
trialmean_logconsecutive <- aggregate(log_consecutive ~ trial.number, data = mean_condition, FUN = mean)
trialtotal_logconsecutive <- aggregate(log_consecutive ~ trial.number, data = total_condition, FUN = mean)

# Exploration per trial
trialmean_logexploration <- aggregate(log_exploration ~ trial.number, data = mean_condition, FUN = mean)
trialtotal_logexploration <- aggregate(log_exploration ~ trial.number, data = total_condition, FUN = mean)

## LME's interaction effects 
# Interactions with trial number
lme_consect <- lmer(consecutive.distance ~ block.type * trial.number + (1 | participant_number), data = data)
lme_top10t <- lmer(distance.to.top.10. ~ block.type * trial.number + (1 | participant_number), data = data)
lme_highvaluet <- lmer(high.value.click ~ block.type * trial.number+ (1 | participant_number), data = data)
lme_novelclickt <- lmer(novel.click ~ block.type+ (1 | participant_number), data = data)
lme_explt <- lmer(exploration ~ block.type * trial.number+ (1 | participant_number), data = data)
lme_blockordert <- lmer(exploration ~ block.type * block.number + (1 | participant_number), data = data )
lme_allt <- lmer(exploration + high.value.click + novel.click + reclick ~ block.type + trial.number+ (1 | participant_number), data = data)

# Interactions with block number
lme_consecb <- lmer(consecutive.distance ~ block.type * block.number + (1 | participant_number), data = data)
lme_top10b <- lmer(distance.to.top.10. ~ block.type * block.number + (1 | participant_number), data = data)
lme_highvalueb <- lmer(high.value.click ~ block.type * block.number+ (1 | participant_number), data = data)
lme_novelclickb <- lmer(novel.click ~ block.type+ (1 | participant_number), data = data)
lme_explb <- lmer(exploration ~ block.type * block.number+ (1 | participant_number), data = data)
lme_blockorderb <- lmer(exploration ~ block.type * block.number + (1 | participant_number), data = data )

# LME that entails all variables
lme_all <- lmer(exploration + high.value.click + novel.click + reclick ~ block.type + trial.number+ (1 | participant_number), data = data)

# Mean high value clicks per block for mean and total condition
ggplot() +
  geom_line(data = blockmean_high_value, aes(x = block.number, y = high.value.click, color = "Mean Condition")) +
  geom_line(data = blocktotal_high_value, aes(x = block.number, y = high.value.click, color = "Total Condition")) +
  labs(x = "Block", y = "Mean high value clicks", title = "Mean high value clicks per block for mean and total condition") +
  scale_color_manual(values = c("Mean Condition" = "blue", "Total Condition" = "red")) +
  theme_minimal()

# Mean novel clicks per block for mean and total condition
ggplot() +
  geom_line(data = blockmean_novel_click, aes(x = block.number, y = novel.click, color = "Mean Condition")) +
  geom_line(data = blocktotal_novel_click, aes(x = block.number, y = novel.click, color = "Total Condition")) +
  labs(x = "Block", y = "Mean novel clicks", title = "Mean novel clicks per block for mean and total condition") +
  scale_color_manual(values = c("Mean Condition" = "blue", "Total Condition" = "red")) +
  theme_minimal()

# Mean high value clicks per trial for mean and total condition
ggplot() +
  geom_line(data = trialmean_high_value, aes(x = trial.number, y = high.value.click, color = "Mean Condition")) +
  geom_line(data = trialtotal_high_value, aes(x = trial.number, y = high.value.click, color = "Total Condition")) +
  labs(x = "Trial", y = "Mean high value", title = "Mean high value clicks per trial for mean and total condition") +
  scale_color_manual(values = c("Mean Condition" = "blue", "Total Condition" = "red")) +
  theme_minimal()

# Mean novel clicks per trial for mean and total condition
ggplot() +
  geom_line(data = trialmean_novel_click, aes(x = trial.number, y = novel.click, color = "Mean Condition")) +
  geom_line(data = trialtotal_novel_click, aes(x = trial.number, y = novel.click, color = "Total Condition")) +
  labs(x = "Trial", y = "Mean novel clicks", title = "Mean novel clicks per trial for mean and total condition") +
  scale_color_manual(values = c("Mean Condition" = "blue", "Total Condition" = "red")) +
  theme_minimal()

# Distance covered per trial in log scale
ggplot() +
  geom_line(data = trialmean_logconsecutive, aes(x = trial.number, y = log_consecutive, color = "Mean Condition")) +
  geom_line(data = trialtotal_logconsecutive, aes(x = trial.number, y = log_consecutive, color = "Total Condition")) +
  labs(x = "Trial", y = "Covered distance", title = "Mean log covered distance per trial for mean and total condition") +
  scale_color_manual(values = c("Mean Condition" = "blue", "Total Condition" = "red")) +
  theme_minimal()

# Exploration per block number
ggplot() +
  geom_line(data = trialmean_logexploration, aes(x = trial.number, y = log_exploration, color = "Mean Condition")) +
  geom_line(data = trialtotal_logexploration, aes(x = trial.number, y = log_exploration, color = "Total Condition")) +
  labs(x = "Trial", y = "Exploration", title = "Mean log exploration per trial for mean and total condition") +
  scale_color_manual(values = c("Mean Condition" = "blue", "Total Condition" = "red")) +
  theme_minimal()

## Mean drop vs total ##
# Create an empty vector to store mean_change values
mean_change <- numeric(nrow(data))

# Calculate mean_change per block, per participant
participants <- unique(data$participant_number)
blocks <- unique(data$block.number)

for (participant in participants) {
  for (block in blocks) {
    # Get indices of rows for the current participant and block
    block_indices <- which(data$block.number == block & data$participant_number == participant)
    
    if (length(block_indices) > 0) {  # Ensure there are rows for the current participant and block
      # Calculate mean_change within the block for the current participant
      mean_change_block <- c(0, diff(data$mean[block_indices]) < 0)
      # Assign mean_change values to the corresponding rows for the current participant and block
      mean_change[block_indices] <- mean_change_block
    }
  }
}

# Assign mean_change values to the data frame
data$mean_change <- mean_change
# Convert boolean values to numeric (0 for FALSE, 1 for TRUE)
data$mean_change <- as.numeric(data$mean_change)

# Subset data
down_mean_trials_indices <- which(data$mean_change == 1 & data$block.type == 'mean') + 1
down_mean_trials_indices <- down_mean_trials_indices[down_mean_trials_indices <= nrow(data)]
down_total_trials_indices <- which(data$mean_change == 1 & data$block.type == 'total') + 1
down_total_trials_indices <- down_total_trials_indices[down_total_trials_indices <= nrow(data)]

down_mean_trials_data <- data[down_mean_trials_indices,]
down_total_trials_data <- data[down_total_trials_indices,]

# Count how many of each we have
nrow(down_mean_trials_data)
nrow(down_total_trials_data)

# Find indices of trials immediately following down_mean_trials for block.type == 'mean'
down_mean_next_trial_indices <- which(data$mean_change == 1 & data$block.type == 'mean') + 1
down_mean_next_trial_indices <- down_mean_next_trial_indices[down_mean_next_trial_indices <= nrow(data)]
# Subset data for the trial immediately following down_mean_trials for block.type == 'mean'
next_trial_down_mean_data <- data[down_mean_next_trial_indices, ]

# Find indices of trials immediately following down_total_trials for block.type == 'total'
down_total_next_trial_indices <- which(data$mean_change == 1 & data$block.type == 'total') + 1
down_total_next_trial_indices <- down_total_next_trial_indices[down_total_next_trial_indices <= nrow(data)]
# Subset data for the trial immediately following down_total_trials for block.type == 'total'
next_trial_down_total_data <- data[down_total_next_trial_indices, ]

t.test(next_trial_down_mean_data$log_exploration, next_trial_down_total_data$log_exploration, data = data)
t.test(next_trial_down_mean_data$log_consecutive, next_trial_down_total_data$log_consecutive, data = data)
t.test(next_trial_down_mean_data$log_distance_to_top_10, next_trial_down_total_data$log_distance_to_top_10, data = data)

mean_log_exploration_down_mean <- mean(next_trial_down_mean_data$log_exploration)
mean_exploration_down_mean <- mean(next_trial_down_mean_data$exploration)
mean_log_exploration_down_total <- mean(next_trial_down_total_data$log_exploration)
mean_exploration_down_total <- mean(next_trial_down_total_data$exploration)
mean_log_all <- mean(data$log_exploration)
mean_exploration_all <- mean(data$exploration)

# Create a data frame for plotting
plot_data <- data.frame(
  Condition = c("All Trials", "Mean Decrease Mean Block", "Mean Decrease Total Block"),
  Mean_exploration = c(mean_exploration_all, mean_exploration_down_mean, mean_exploration_down_total)
)

# Create the bar plot
ggplot(plot_data, aes(x = Condition, y = Mean_exploration, fill = Condition)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Mean Exploration after a drop in mean score",
       x = "Condition",
       y = "Mean Exploration") +
  theme_minimal()

## Overarching plot ##
library(ggplot2)
install.packages("ggdist")
library(ggdist)
install.packages("ggpubr")
library(ggpubr)

ggplot(data, aes(x = factor(block.type), y = exploration, fill= block.type)) + #toon data
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2, .width = 0, show.legend = FALSE) + #toon een distributie van je data punten,
  scale_fill_manual(values=c("blue", "red")) + theme_classic() +  #lay-out dingetjes
  geom_boxplot(width = 0.15, outlier.color = NA, alpha = 0.5, show.legend = FALSE, position = position_dodge(width = 0.5)) + #boxplotje erbij
  geom_signif(comparisons = list(c("mean", "total")), map_signif_level=TRUE) +  #vergelijk of dit dezelfde significantie geeft als de t-test (normaal wel)
  ggdist::stat_dots(side = "left", justification = 1.12, binwidth = 1, dotsize = 0.03, overflow = "compress", show.legend = FALSE)  
