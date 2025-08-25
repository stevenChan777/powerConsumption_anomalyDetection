library(dplyr)
library(ggplot2)
library(depmixS4)

### Data scaling preprocessing ###
df <- read.csv("B:/Desktop/Code Stuff/TermProjectData.txt", header = TRUE, sep = ",")
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")

df$Week_Num <- strftime(df$Date, format = "%V")

df$Year <- as.numeric(format(df$Date, "%Y"))

cols_to_use <- c("Global_active_power", "Global_reactive_power",
                 "Voltage", "Global_intensity",
                 "Sub_metering_1", "Sub_metering_2",
                 "Sub_metering_3")

for (col in cols_to_use) {
  df[[col]] <- ifelse(is.na(df[[col]]),
                      approx(seq_along(df[[col]]), df[[col]],
                             seq_along(df[[col]]), rule = 2)$y, df[[col]])
}

df[cols_to_use] <- scale(df[cols_to_use]) # z-score scale
summary(df[cols_to_use])

# ### PCA data analysis ###
pca <- prcomp(df[, cols_to_use], scale = FALSE)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var / sum(pca.var) * 100, 1)
pc_labels <- paste0("PC", seq_along(pca.var.per))

barplot_heights <- barplot(pca.var.per,
                           main = "Scree Plot",
                           xlab = "Principal Component",
                           ylab = "Percent Variation",
                           names.arg = pc_labels,
                           las = 2,
                           ylim = c(0, max(pca.var.per) + 2))

lines(barplot_heights, pca.var.per, type = "b", col = "red", pch = 16, lwd = 2)

text(barplot_heights, pca.var.per, labels = pca.var.per,
     pos = 3, col = "black", cex = 0.8, font = 2)

pca.data <- data.frame(Sample = seq_len(nrow(pca$x)), X = pca$x[, 1], Y = pca$x[, 2])
ggplot(data = pca.data, aes(x = X, y = Y)) +
 geom_point(alpha = 0.5, color = "blue") +
 xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) +
 ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
 theme_bw() +
 ggtitle("PCA Projection (PC1 vs PC2)")

loading_scores <- pca$rotation[, 1]
pca_scores <- abs(loading_scores)
pca_scores_ranked <- sort(pca_scores, decreasing = TRUE)
pca_scores_ranked

loadings_df <- data.frame(
  Variable = rownames(pca$rotation),
  PC1 = abs(pca$rotation[, 1]),
  PC2 = abs(pca$rotation[, 2]),
  PC3 = abs(pca$rotation[, 3]),
  PC4 = abs(pca$rotation[, 4]),
  PC5 = abs(pca$rotation[, 5]),
  PC6 = abs(pca$rotation[, 6]),
  PC7 = abs(pca$rotation[, 7])
)

### partition data to test and train ###
train_data <- subset(df, df$Year >= 2006 & df$Year <= 2008)
test_data <- subset(df, df$Year == 2009)

# prepare time window 2 pm to 8 pm
one_day_train <- subset(train_data, Date == "2007-07-12")
one_day_test <- subset(test_data, Date == "2009-07-12")

# Define the three time windows

### #1 Option: Testing with differnt time window frame
# time_windows <- list(
#   time1 = c("14:00:00", "20:00:00"),
#   morning = c("06:00:00", "12:00:00"),
#   afternoon = c("12:00:00", "18:00:00"),
#   evening = c("18:00:00", "23:59:59"),
#   overnight = c("00:00:01", "06:00:00")
# )
# selected_window <- time_windows$time1
# 
# cat("Selected Time Window:", selected_window[1], "to", selected_window[2], "\n")
# 
# # Filter train and test data for the selected time window
# one_day_train$Time <- as.POSIXct(one_day_train$Time, format = "%H:%M:%S")
# time_window_train <- subset(one_day_train,
#                             format(one_day_train$Time, "%H:%M:%S") >= selected_window[1] &
#                               format(one_day_train$Time, "%H:%M:%S") < selected_window[2])
# 
# one_day_test$Time <- as.POSIXct(one_day_test$Time, format = "%H:%M:%S")
# time_window_test <- subset(one_day_test,
#                            format(one_day_test$Time, "%H:%M:%S") >= selected_window[1] &
#                              format(one_day_test$Time, "%H:%M:%S") < selected_window[2])

#### #2 Option: Testing with a day
one_day_data <- train_data[
format(train_data$Date, "%Y-%m-%d") == "2007-07-01",] #hardcoded a day, fix if its wrong T.T
one_day_data$Time <- as.POSIXct(one_day_data$Time, format = "%H:%M:%S")
time_window_train <- subset(one_day_data,
                      format(one_day_data$Time, "%H:%M:%S") >= "14:00:00"
                      & format(one_day_data$Time, "%H:%M:%S") <= "20:00:00")

test_day_data <- test_data[format(test_data$Date, "%Y-%m-%d") == "2009-07-01", ]
test_day_data$Time <- as.POSIXct(test_day_data$Time, format = "%H:%M:%S")
time_window_test <- subset(test_day_data,
                           format(test_day_data$Time, "%H:%M:%S") >= "14:00:00" &
                             format(test_day_data$Time, "%H:%M:%S") <= "20:00:00")
cat("Train time window rows:", nrow(time_window_train), "\n")
cat("Test time window rows:", nrow(time_window_test), "\n")

cat("Train NAs per column:\n")
print(colSums(is.na(time_window_train)))

cat("\nTest NAs per column:\n")
print(colSums(is.na(time_window_test)))

#### Discrete variables
# time_window_train <- time_window_train[, c("Global_active_power", "Global_intensity", "Global_reactive_power")]
# time_window_test <- time_window_test[, c("Global_active_power", "Global_intensity", "Global_reactive_power")]
# # Discretize Global_active_power
# time_window_train$Discrete_Global_active_power <- round(time_window_train$Global_active_power * 2) / 2
# time_window_train$Discrete_Global_active_power <- as.factor(time_window_train$Discrete_Global_active_power)
# time_window_test$Discrete_Global_active_power <- round(time_window_test$Global_active_power * 2) / 2
# time_window_test$Discrete_Global_active_power <- as.factor(time_window_test$Discrete_Global_active_power)
# 
# # Discretize Global_intensity
# time_window_train$Discrete_Global_intensity <- round(time_window_train$Global_intensity * 2) / 2
# time_window_train$Discrete_Global_intensity <- as.factor(time_window_train$Discrete_Global_intensity)
# time_window_test$Discrete_Global_intensity <- round(time_window_test$Global_intensity * 2) / 2
# time_window_test$Discrete_Global_intensity <- as.factor(time_window_test$Discrete_Global_intensity)
# 
# # Discretize Global_reactive_power
# time_window_train$Discrete_Global_reactive_power <- round(time_window_train$Global_reactive_power * 2) / 2
# time_window_train$Discrete_Global_reactive_power <- as.factor(time_window_train$Discrete_Global_reactive_power)
# time_window_test$Discrete_Global_reactive_power <- round(time_window_test$Global_reactive_power * 2) / 2
# time_window_test$Discrete_Global_reactive_power <- as.factor(time_window_test$Discrete_Global_reactive_power)


### Finding best number of states ###
log_likelihoods_train <- c()
log_likelihoods_test <- c()
bic_values <- c()
valid_states <- c()
states_range <- 4:10
results <- data.frame(State = integer(), Log_Likelihood = numeric(), BIC = numeric())

for (num_states in states_range) {
  cat("\nTraining HMM with", num_states, "states...\n")

  tryCatch({
    mod_train <- depmix(
      response = list(Global_active_power ~ 1,
                      Global_intensity ~ 1,
                      Global_reactive_power ~ 1),
      data = time_window_train,
      nstates = num_states,
      ntimes = nrow(time_window_train),
      family = list(gaussian(), gaussian(), gaussian())
    )
    fit_train <- fit(mod_train)
    ll_train <- as.numeric(logLik(fit_train))
    bic_val <- BIC(fit_train)
    
    mod_test <- depmix(
      response = list(Global_active_power ~ 1,
                      Global_intensity ~ 1,
                      Global_reactive_power ~ 1),
      data = time_window_test,
      nstates = num_states,
      ntimes = nrow(time_window_test),
      # family = list(multinomial(),multinomial(),multinomial()) ## comment this out if use with discrete
      family = list(gaussian(), gaussian(), gaussian())
    )
    fit_test <- fit(mod_test)
    ll_test <- as.numeric(logLik(fit_test))
    
    valid_states <- c(valid_states, num_states)
    log_likelihoods_train <- c(log_likelihoods_train, ll_train)
    log_likelihoods_test <- c(log_likelihoods_test, ll_test)
    bic_values <- c(bic_values, bic_val)
    
  }, error = function(e) {
    cat("Failed for", num_states, "states:", conditionMessage(e), "\n")
  })
}


norm_ll_train <- log_likelihoods_train / nrow(time_window_train)
norm_ll_test <- log_likelihoods_test / nrow(time_window_test)

plot(states_range, norm_ll_train, type = "b", col = "blue", pch = 19,
     ylim = range(c(norm_ll_train, norm_ll_test)),
     xlab = "Number of States", ylab = "Normalized Log-Likelihood",
     main = "Normalized Log-Likelihood: Train vs Test")
lines(states_range, norm_ll_test, type = "b", col = "red", pch = 17)
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), pch = c(19, 17))


plot(states_range, bic_values, type = "b", col = "darkgreen", pch = 19,
     xlab = "Number of States", ylab = "BIC", main = "BIC on Training Set")

## Training detecting model ###
best_state_count <- 7
best_model <- depmix(
  response = list(Global_active_power ~ 1,
                  Global_intensity ~ 1,
                  Global_reactive_power ~ 1),
  data = time_window_train,
  nstates = best_state_count,
  ntimes = nrow(time_window_train),
  # family = list(multinomial(),multinomial(),multinomial()) ## comment this out if use with discrete
  family = list(gaussian(), gaussian(), gaussian())
)
best_fit <- fit(best_model)
train_logLik <- as.numeric(logLik(best_fit))

### determining threshold ###

test_data$Time <- as.POSIXct(test_data$Time, format = "%H:%M:%S")

test_window <- subset(test_data,
                      format(test_data$Time, "%H:%M:%S") >= "14:00:00" &
                        format(test_data$Time, "%H:%M:%S") <= "20:00:00")

test_window$Week <- strftime(test_window$Date, format = "%Y-%W")

weekly_subsets <- split(test_window, test_window$Week)
weekly_subsets <- Filter(function(x) nrow(x) > 0, weekly_subsets)
weekly_subsets <- weekly_subsets[1:min(10, length(weekly_subsets))]
weekly_logLik <- c()

for (i in seq_along(weekly_subsets)) {
  week_df <- weekly_subsets[[i]]
  tryCatch({
    test_mod <- depmix(
      response = list(Global_active_power ~ 1,
                      Global_intensity ~ 1,
                      Global_reactive_power ~ 1),
      data = week_df,
      nstates = best_state_count,
      ntimes = nrow(week_df),
      family = list(gaussian(), gaussian(), gaussian())
    )
    fit_test <- fit(test_mod, verbose = FALSE)
    weekly_logLik[i] <- as.numeric(logLik(fit_test))
  }, error = function(e) {
    cat("Failed on week", i, ":", conditionMessage(e), "\n")
    weekly_logLik[i] <- NA
  })
}

valid_logLik <- na.omit(weekly_logLik)
deviations <- valid_logLik - train_logLik
threshold <- max(deviations)
cat("Train Log-Likelihood:", train_logLik, "\n")
cat("Weekly Test Log-Likelihoods:\n"); print(valid_logLik)
cat("Deviations from Train Log-Likelihood:\n"); print(deviations)
cat("Anomaly Threshold (Max Deviation):", threshold, "\n")

week_labels <- paste("Week", 1:length(deviations))

plot(deviations, type = "b", pch = 19, col = "darkorange",
     xaxt = "n", xlab = "Weekly Subsets", ylab = "Deviation",
     main = "Deviation from Train Log-Likelihood")
axis(1, at = 1:length(deviations), labels = week_labels, las = 2)

