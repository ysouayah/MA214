# ==============================================================================
# MA 214 Milestone 4: Statistical Analysis Script
# ==============================================================================

# 1. Load Necessary Libraries
library(tidyverse)
library(car) # For VIF (Variance Inflation Factor)

# 2. Load and Clean the Data
# ------------------------------------------------------------------------------
# Read the CSV file (Make sure the file is in your working directory)
movies <- read_csv("/Users/ysfsouayah/Downloads/top-500-movies.csv", show_col_types = FALSE)

# Clean currency columns (remove '$' and ',') and convert to numeric
movies_clean <- movies %>%
  mutate(
    production_cost = as.numeric(gsub("[\\$,]", "", production_cost)),
    worldwide_gross = as.numeric(gsub("[\\$,]", "", worldwide_gross)),
    # Calculate ROI for the comparison analysis
    roi = (worldwide_gross - production_cost) / production_cost
  ) %>%
  # Remove rows with missing or zero values in key columns to avoid errors
  filter(
    !is.na(production_cost) & production_cost > 0,
    !is.na(worldwide_gross) & worldwide_gross > 0,
    !is.na(theaters) & theaters > 0,
    !is.na(runtime)
  ) %>%
  # Create the Log Transformed Variable
  mutate(log_gross = log(worldwide_gross))

# ==============================================================================
# Part 3: Exploratory Analysis & Transformations (Recap of Milestone 3)
# ==============================================================================

# Model 1: Base Linear Model (Gross vs Budget)
model_base <- lm(worldwide_gross ~ production_cost, data = movies_clean)

# Model 2: ROI Model (Gross vs ROI) - The one with extreme variance
model_roi <- lm(worldwide_gross ~ roi, data = movies_clean)

# Plotting Residuals for Base Model
par(mfrow=c(2,2)) # Set up 2x2 plot area
plot(model_base, which=1, main="Model 1 Base: Residuals vs Fitted")

# Plotting Residuals for ROI Model
# You should see the "fan shape" explode here, confirming your observation
plot(model_roi, which=1, main="Model 2 ROI: Residuals vs Fitted")

# ==============================================================================
# Part 4: Statistical Analysis
# ==============================================================================

# 4.1 Multicollinearity Check
# ------------------------------------------------------------------------------
# Calculate correlation matrix for quantitative variables
cor_matrix <- movies_clean %>%
  select(production_cost, theaters, runtime) %>%
  cor()

print("Correlation Matrix:")
print(cor_matrix)

# 4.2 Model Fit & Variable Selection (Backward Elimination)
# ------------------------------------------------------------------------------

# Step 1: Fit the Full Model with all variables (including Categorical)
# We use log_gross as the response variable
full_model <- lm(log_gross ~ production_cost + theaters + runtime + as.factor(mpaa) + as.factor(genre), 
                 data = movies_clean)

print("Full Model Summary:")
summary(full_model)
# Look at p-values for 'mpaa'. If they are > 0.05, we remove them.

# Step 2: Fit the Reduced/Final Model (Quantitative Variables Only)
final_model <- lm(log_gross ~ production_cost + theaters + runtime + as.factor(mpaa) + as.factor(genre), data = movies_clean)
print("Final Model Summary:")
summary(final_model)

# Check Variance Inflation Factors (VIF) to confirm no multicollinearity
print("VIF Values:")
print(vif(final_model))

# Generate the ANOVA table for the final model
print("Analysis of Variance (ANOVA) Table:")
anova(final_model)

# 4.3 Residual Analysis of Final Model
# ------------------------------------------------------------------------------
# Reset plot area
par(mfrow=c(2,2)) 

# Create standard diagnostic plots for the final model
# Plot 1 (Top-Left): Residuals vs Fitted -> Should look random (no fan shape)
# Plot 2 (Top-Right): Normal Q-Q -> Points should follow the line
plot(final_model, which = c(1, 2))

# ==============================================================================
# Part 6: Prediction (Wicked Example)
# ==============================================================================

# 1. Create the dataframe with Wicked's specific stats
wicked_data <- data.frame(
  production_cost = 150000000, # Estimated budget
  theaters = 3888,             # Widest release count
  runtime = 160,               # 2 hours 40 minutes
  mpaa = "PG",                 # Rated PG
  genre = "Musical"            # Best fit for your model's categories
)

# 2. Run the prediction using your existing 'final_model'
log_prediction_wicked <- predict(final_model, newdata = wicked_data, interval = "prediction")

# 3. Convert from Log back to Dollars
dollar_prediction_wicked <- exp(log_prediction_wicked)

# 4. Print the result
print("Predicted Worldwide Gross for Wicked (2024):")
print(dollar_prediction_wicked)