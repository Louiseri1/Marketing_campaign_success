################################################################################
####################               LOAD PACKAGES              ##################
################################################################################
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(caret)
library(stargazer)



################################################################################
####################               QUESTION 1                 ##################
################################################################################

####################               TASK 1                     ##################
# Read data
df <- read_delim("subscribe_data.csv")

# Look at data
df %>% 
  head()

# COMMENT
# We notice that all the coumns that hold catagorical values (such as
# yes / no etc.) have the datatype <chr>. Hence, we must convert them to factors

# Convert character columns to factors
df <- df %>% 
  mutate_if(is.character, as.factor)

# For the column

# Check summary of dataframe
summary(df)

# COMMENT
# The summary show the minimum, 1st quartile, median, mean, 3rd wuarile and max
# values of the numeric columns. For the categorical columns, we see the number
# of times each category is counted

# Check if there are any NAs in the data
colSums(is.na(df))

# COMMENT
# Even though we do not have any NA´s, we have several "unknown"´s, which will
# add no predicting value to our data. Hence we assign every "unknown" the NA 
# value.

# Make "unknown" NA insted
df <- df %>% 
  na_if("unknown")

# COMMENT
# Also, in the column "pdays, 999 is supposed to mean the contacted was not 
# previously contacted, which we rather assign the NA value. 

# Replace values with NAs 
df <- df %>% 
  mutate(pdays = replace(pdays, pdays == 999, NA))

# Now we again check number of NA´s per column
colSums(is.na(df))

# COMMENT
# Note that the "default" column has 4329 NA´s. This is about 20% of the number
# of rows in the entire dataset. Some models handle NA´s, however, 
# some do not. We might remove the NA´s later. 

# We also notice that almost no customers was
# last contacted from last campaign. Hence, we find it more appropriate to
# let the column represent whether a customer was contacted or not, rather
# than showing the days since last contact.

# Let 1 represent contact, and 0 no contact
df <- df %>% 
  mutate(pdays = ifelse(is.na(pdays) != TRUE, 1, 0))


# Further we can look at a correlation heatmap of the numeric variables

# Calculate correlation between each pairwise combination of variables
cor_df <- round(cor(df %>% select(where(is.numeric))), 2)

# Melt the data frame
melted_cormat <- melt(cor_df)

# Plot heatmap
ggplot(data = melted_cormat, aes(x  =X1, y = X2, fill = value)) + 
  geom_tile() +
  geom_text(aes(X1, X2, label = value), size = 2) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name = "Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

# COMMENT
# We notice that several of the variables are highly correlated, and should not
# be included in the same linear models due to issues with multicolinearity
# Now we check for outliers in the relevant numeric columns 

# Box plot of "campaign"
df %>% 
  ggplot(aes(campaign)) +
  geom_boxplot() +
  theme_bw()

# COMMENT
# We notice several outliers (the dots), we update the dateframe removing them

# Remove outliers in the "campaign" column
df <- df %>% 
  mutate(zscore = (campaign - mean(campaign)) / sd(campaign)) %>% 
  filter(zscore > -3 & zscore < 3) %>% 
  select(-c(zscore))

# Box plot of "previous"
df %>% 
  ggplot(aes(previous)) +
  geom_boxplot() +
  theme_bw()

# COMMENT
# We notice several outliers (the dots), we update the dateframe removing them

# Remove outliers in the "previous" column
df <- df %>% 
  mutate(zscore = (previous - mean(previous)) / sd(previous)) %>% 
  filter(zscore > -3 & zscore < 3) %>% 
  select(-c(zscore))


# Box plot of "emp.var.rate"
df %>% 
  ggplot(aes(emp.var.rate)) +
  geom_boxplot() +
  theme_bw()

# COMMENT
# Looks good

# Box plot of "emp.var.rate"
df %>% 
  ggplot(aes(cons.price.idx)) +
  geom_boxplot() +
  theme_bw()

# COMMENT
# Looks good

# Box plot of "emp.var.rate"
df %>% 
  ggplot(aes(cons.conf.idx)) +
  geom_boxplot() +
  theme_bw()

# COMMENT
# Looks good

# Box plot of "emp.var.rate"
df %>% 
  ggplot(aes(euribor3m)) +
  geom_boxplot() +
  theme_bw()

# COMMENT
# Looks good

# Box plot of "emp.var.rate"
df %>% 
  ggplot(aes(nr.employed)) +
  geom_boxplot() +
  theme_bw()

# COMMENT
# Looks good


# Plot of outcome variable
df %>% 
  ggplot(aes(y)) +
  geom_histogram(stat = "count") +
  theme_bw()


####################               TASK 2a                    ##################

# COMMENT
# Our DV is "y" (whether a client has subscribed to a term deposit or not)
# We choose our modified "contact", "marital", "job", "loan" and "campaign". 
# The choice of IV´s is based on columns with few NA´s and not highly correlated
# variables. We want variables regarding both the campaign (campign, contact), 
# as well as personal information about the customer (marital, job and loan)


####################               TASK 2b                    ##################

# ** This part needs no coding as i read it



####################               TASK 2c                    ##################

# ** This part needs no coding as i read it




####################               TASK 2d                    ##################

# COMMENT
# First we create a biniomial logistic regression using the variables we chose 
# in a) as predictors. 

# Change outcome variable to binary
df_new <- df %>% 
  mutate(y = as_factor(ifelse(y == "yes", 1, 0)))


# Set seed for reproducibilty
set.seed(123)

# Split data into train and test (75% train)
data_split <- initial_split(df_new)
train <- training(data_split)
test <- testing(data_split)

# Create model from a)
model_a <- glm(y ~ campaign + contact + marital + job + loan, 
               family = "binomial",
               train)

# COMMENT
# For the other model, we can look "poutcome", "previous", "education", "age" 
# and "day_of_week", looking at more external effects

# Model with new variables 
model_b <- glm(y ~ poutcome + previous + education + age + day_of_week, 
               family = "binomial",
               train)
df %>% 
  select(c(poutcome, previous, education, age, day_of_week))

# Look at summary of models
stargazer(model_a, type = "text")
stargazer(model_b, type = "text")

# Predicting the test set with model a
pred_a <- predict(model_a, test, type = "response")

# We can set the threshold of predicting 1 (yes) to 0.2, as the cost of 
# predicting wrong is not very great
pred_a <- pred_a %>% 
  as_tibble() %>% 
  mutate(value = ifelse(value > 0.2, 1, 0))

# Look at confusion matrix for model a
conf_mat_a <- table(test$y, pred_a$value)

# Prediciting the test set with model b, using same threshold as a
# Predicting the test set with model a
pred_b <- predict(model_b, test, type = "response")

# We can set the threshold of predicting 1 (yes) to 0.2, as the cost of 
# predicting wrong is not very great
pred_b <- pred_b %>% 
  as_tibble() %>% 
  mutate(value = ifelse(value > 0.2, 1, 0))

# Look at confusion matrix for model a
conf_mat_b <- table(test$y, pred_b$value)

# COMMENT
# When evaluating, we can look at significant coeffisients, and AIC score from 
# the summary. When evalutating performance on the test data, we can use 
# accuracy (number of right predictions)

# Accuracy for a
sum(diag(conf_mat_a)) / sum(conf_mat_a)

# Accuracy for b
sum(diag(conf_mat_b)) / sum(conf_mat_b)

# We notice that b is actually more accurate, but as there are so many more 0s
# it might only be that it is predicting more 0s. Hence we can look at the model
# precision, to see the accuracy of the positive (1) predictions

# We create a function for calculating precision
precision_func <- function(matrix) {
  
  # True positive
  tp <- matrix[2, 2]
  
  # false positive
  fp <- matrix[1, 2]
  
  return (tp / (tp + fp))
}

# Precision for a
precision_func(conf_mat_a)

# Precision for b
precision_func(conf_mat_b)

# COMMENT
# b actually seems alot better lol

# Function for calculating recall
recall <- function(matrix) {
  
  # True positive
  tp <- matrix[2, 2]
  
  # False positive
  fn <- matrix[2, 1]
  
  return (tp / (tp + fn))
}

# Not to differnt in recall tho
recall(conf_mat_a)
recall(conf_mat_b)


####################               TASK 2e                    ##################

# COMMENT
# As b appeared the best (Lower AIC and better accuracy, precision and recall)
# we continue with this model

final_model <- glm(y ~ poutcome + previous + education + age + day_of_week, 
                   family = "binomial",
                   df_new)

# Model fit statistics
stargazer(final_model, type = "text")
