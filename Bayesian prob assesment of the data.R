library(brms)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load data
GBT <- read.csv("E:/suk B gurung sir folder/My Final arranged Data.csv", header = TRUE)

# Prepare data
GBT <- GBT %>%
  mutate(
    GEN = as.factor(GEN),
    across(c(FRS, AUDPC, ACI, r.value), as.numeric),
    AUDPC = ifelse(is.na(AUDPC), mean(AUDPC, na.rm = TRUE), AUDPC),
    FRS = ifelse(is.na(FRS), mean(FRS, na.rm = TRUE), FRS),
    ACI = ifelse(is.na(ACI), mean(ACI, na.rm = TRUE), ACI),
    r.value = ifelse(is.na(r.value), mean(r.value, na.rm = TRUE), r.value),
    APR_myn = as.numeric(scale(AUDPC)) +
      as.numeric(scale(FRS)) +
      as.numeric(scale(ACI)) +
      as.numeric(scale(r.value))
  )

# Fit Bayesian model
fit_apr <- brm(
  APR_myn ~ AUDPC + FRS + ACI + r.value + AUDPC:ACI + FRS:r.value + (1 | GEN),
  data = GBT,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 5), class = "b"),
    prior(cauchy(0, 2), class = "sd"),
    prior(cauchy(0, 5), class = "sigma")
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.95),
  seed = 123
)
plot(fit_apr)
# Summary of model fit
summary(fit_apr)

# Detailed convergence diagnostics
library(bayesplot)
mcmc_trace(fit_apr, pars = c("b_Intercept", "b_AUDPC", "b_FRS", "b_ACI", "b_r.value")) # Check trace plots for key parameters

# Prediction dataset
pred_data <- GBT %>%
  group_by(GEN) %>%
  summarise(
    AUDPC = mean(AUDPC, na.rm = TRUE),
    FRS = mean(FRS, na.rm = TRUE),
    ACI = mean(ACI, na.rm = TRUE),
    r.value = mean(r.value, na.rm = TRUE)
  ) %>%
  ungroup()
pred_data
# Predict APR
pred_matrix <- posterior_predict(fit_apr, newdata = pred_data, allow_new_levels = TRUE)

# Summarize predictions
pred_apr <- as.data.frame(pred_matrix) %>%
  setNames(pred_data$GEN) %>%
  pivot_longer(cols = everything(), names_to = "GEN", values_to = "APR_pred") %>%
  group_by(GEN) %>%
  summarise(
    Mean_APR = mean(APR_pred),
    Lower_CI = quantile(APR_pred, 0.025),
    Upper_CI = quantile(APR_pred, 0.975)
  ) %>%
  arrange(Mean_APR)

# Plot
ggplot(pred_apr, aes(x = reorder(GEN, Mean_APR), y = Mean_APR)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "brown") +
  coord_flip() +
  labs(x = "Genotype", y = "Predicted APR Mean", title = "APR by Genotype") +
  theme_minimal()
library(forcats)

APRrp<-ggplot(pred_apr, aes(x = fct_rev(fct_reorder(GEN, Mean_APR)), y = Mean_APR)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "brown") +
  coord_flip() +
  labs(x = "Genotype", y = "Predicted APR Mean", title = "APR by Genotype") +
  theme_minimal()
plot(APRrp)
library(gridExtra)
getwd()

ggsave("Final APR based on  Bayes.jpg", plot = APRrp, height = 12, width = 15, dpi = 600)
summary(fit_apr)

###For drawing the cleavland dot plot

# Define five breaks for Mean_APR (using quantiles)
apr_breaks <- quantile(pred_apr$Mean_APR, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Define five distinct colors
five_colors <- c("blue", "green", "yellow", "orange", "red") # Sequential palette

# Create the plot
cleveland_plot <- ggplot(pred_apr, aes(x = reorder(GEN, Mean_APR), y = Mean_APR)) +
  geom_point(aes(color = Mean_APR), size = 2) + # Color by Mean_APR
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "brown") +
  geom_hline(yintercept = mean(pred_apr$Mean_APR), linetype = "dashed", color = "black") + # Mean reference line
  coord_flip() +
  labs(x = "Genotype", y = "Interactive Inverse APR Mean", title = "Predicted APR by Genotype") +
  scale_color_gradientn(
    colors = five_colors, # Five distinct colors
    breaks = apr_breaks,  # Breaks for legend
    labels = round(apr_breaks, 2), # Round labels for clarity
    name = "Interactive traits APR Mean"     # Legend title
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6), # Smaller text for many labels
    panel.grid.major.y = element_blank(), # Remove grid lines
    legend.position = "right",            # Ensure legend is visible
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Display the plot
print(cleveland_plot)
getwd()
# Save the plot
ggsave("Cleveland for Bayesian interactive prediction.jpg", plot = cleveland_plot, height = 10, width = 10, dpi = 600)



# Define five breaks for Mean_APR (using quantiles)
library(ggplot2)
library(dplyr)
library(forcats)
apr_breaks <- quantile(pred_apr$Mean_APR, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Define five distinct colors
five_colors <- c("blue", "green", "yellow", "orange", "red") # Sequential palette

# Create the plot
cleveland_plot <- ggplot(pred_apr, aes(x = fct_rev(fct_reorder(GEN, Mean_APR)), y = Mean_APR)) +
  geom_point(aes(color = Mean_APR), size = 2) + # Color by Mean_APR
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "brown") +
  geom_hline(yintercept = mean(pred_apr$Mean_APR), linetype = "dashed", color = "black") + # Mean reference line
  coord_flip() +
  labs(x = "Genotype", y = "Interactive Inverse APR Mean", title = "Predicted APR by Genotype") +
  scale_color_gradientn(
    colors = five_colors, # Five distinct colors
    breaks = apr_breaks,  # Breaks for legend
    labels = round(apr_breaks, 2), # Round labels for clarity
    name = "Interactive Traits APR Mean" # Legend title
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6), # Smaller text for many labels
    panel.grid.major.y = element_blank(), # Remove grid lines
    legend.position = "right",            # Ensure legend is visible
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Display the plot
print(cleveland_plot)
