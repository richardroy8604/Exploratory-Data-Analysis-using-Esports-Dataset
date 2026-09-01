# ==============================================================================
# Exploratory Data Analysis (EDA) and Data Storytelling using R
# Case Study: Hotel Booking Demand & Cancellation Analytics
# ==============================================================================

# Automatic installation of missing R packages only
required_pkgs <- c("ggplot2", "dplyr", "tidyr", "corrplot", "plotly", "DT", "htmlwidgets", "e1071", "scales")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]

if(length(missing_pkgs) > 0) {
  cat("Installing missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
} else {
  cat("All required R packages are already installed!\n")
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(plotly)
library(DT)
library(htmlwidgets)
library(e1071)
library(scales)

if (!dir.exists("plots")) {
  dir.create("plots")
}

# 1. Load Data
cat("Loading Hotel Booking Demand Dataset...\n")
df_hotel <- read.csv("hotel_bookings.csv", stringsAsFactors = FALSE)

# 2. Preprocessing
df_hotel$children[is.na(df_hotel$children)] <- 0
df_hotel$hotel <- as.factor(df_hotel$hotel)
df_hotel$is_canceled_factor <- factor(df_hotel$is_canceled, levels = c(0, 1), labels = c("Not Canceled", "Canceled"))
df_hotel$total_stay <- df_hotel$stays_in_weekend_nights + df_hotel$stays_in_week_nights

cat("Dimensions:", dim(df_hotel), "\n")
cat("Missing values in children:", sum(is.na(df_hotel$children)), "\n")

# 3. Descriptive Statistics
num_vars <- c("lead_time", "adr", "stays_in_week_nights", "stays_in_weekend_nights", "total_of_special_requests")

calc_stats <- function(var_name) {
  x <- df_hotel[[var_name]]
  x <- x[!is.na(x)]
  q <- quantile(x, probs = c(0.25, 0.50, 0.75))
  data.frame(
    Variable = var_name,
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
    Min = min(x),
    Q1 = q[1],
    Q3 = q[3],
    IQR = IQR(x),
    Max = max(x),
    Skewness = skewness(x)
  )
}

stats_df <- bind_rows(lapply(num_vars, calc_stats))
cat("\n--- DESCRIPTIVE STATISTICS TABLE ---\n")
print(stats_df)

# 4. Univariate Visualizations
theme_custom <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = "#555555"))

df_adr_clean <- df_hotel %>% filter(adr > 0 & adr < 500)
p1 <- ggplot(df_adr_clean, aes(x = adr)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#2980b9", color = "white", alpha = 0.7) +
  geom_density(color = "#1b365d", linewidth = 1) +
  labs(title = "Task 4: Distribution of Average Daily Rate (ADR $)", x = "ADR ($)", y = "Density") +
  theme_custom

ggsave("plots/univariate_adr_hist.png", p1, width = 7, height = 4.5, dpi = 300)

hotel_counts <- df_hotel %>% count(hotel)
p2 <- ggplot(hotel_counts, aes(x = hotel, y = n, fill = hotel)) +
  geom_bar(stat = "identity", width = 0.6, color = "white") +
  geom_text(aes(label = comma(n)), vjust = -0.3, size = 4, fontface = "bold") +
  scale_y_continuous(labels = comma, limits = c(0, max(hotel_counts$n) * 1.15)) +
  scale_fill_manual(values = c("#3498db", "#e67e22")) +
  labs(title = "Task 4: Booking Counts by Hotel Type", x = "Hotel Type", y = "Total Bookings") +
  theme_custom + theme(legend.position = "none")

ggsave("plots/univariate_hotel_bar.png", p2, width = 6.5, height = 4.5, dpi = 300)

# 5. Bivariate Visualizations
cancel_hotel <- df_hotel %>% 
  group_by(hotel, is_canceled_factor) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  group_by(hotel) %>% 
  mutate(Pct = Count / sum(Count))

p3 <- ggplot(cancel_hotel, aes(x = hotel, y = Pct, fill = is_canceled_factor)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(round(Pct * 100, 1), "%")), 
            position = position_dodge(width = 0.6), vjust = -0.3, size = 3.8) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.75)) +
  scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
  labs(title = "Task 5: Cancellation Rate Comparison by Hotel Type",
       x = "Hotel Type", y = "Percentage of Bookings", fill = "Booking Status") +
  theme_custom

ggsave("plots/bivariate_cancellation_hotel.png", p3, width = 7, height = 4.8, dpi = 300)

p5 <- ggplot(df_hotel, aes(x = is_canceled_factor, y = lead_time, fill = is_canceled_factor)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1) +
  scale_fill_manual(values = c("#27ae60", "#c0392b")) +
  labs(title = "Task 5: Lead Time Distribution by Cancellation Status",
       x = "Booking Status", y = "Lead Time (Days)") +
  theme_custom + theme(legend.position = "none")

ggsave("plots/bivariate_lead_cancellation.png", p5, width = 7, height = 4.8, dpi = 300)

# 6. Correlation Analysis
num_cols <- df_hotel %>% select(lead_time, is_canceled, stays_in_weekend_nights, stays_in_week_nights, adults, children, adr, previous_cancellations, total_of_special_requests)
cor_mat <- cor(num_cols, use = "complete.obs")

cat("\n--- CORRELATION MATRIX ---\n")
print(round(cor_mat, 3))

png("plots/correlation_heatmap.png", width = 1800, height = 1500, res = 300)
corrplot(cor_mat, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Task 6: Correlation Heatmap of Hotel Attributes", mar = c(0,0,2,0))
dev.off()

# 7. Hypothesis Testing
cat("\n=== TEST 1: ONE-WAY ANOVA (ADR by Hotel Type) ===\n")
anova_adr <- aov(adr ~ hotel, data = df_hotel)
print(summary(anova_adr))

cat("\n=== TEST 2: CHI-SQUARE TEST (Cancellation by Hotel Type) ===\n")
chi_tbl <- table(df_hotel$hotel, df_hotel$is_canceled)
print(chi_tbl)
print(chisq.test(chi_tbl))

# 8. Interactive Widgets
p_int1 <- plot_ly(
  df_hotel %>% sample_n(1500), x = ~lead_time, y = ~adr, color = ~hotel,
  text = ~paste("Hotel:", hotel, "<br>Segment:", market_segment, "<br>Country:", country),
  type = 'scatter', mode = 'markers'
) %>% layout(title = "Interactive Lead Time vs ADR", xaxis = list(title = "Lead Time (Days)"), yaxis = list(title = "ADR ($)"))

saveWidget(p_int1, "plots/interactive_scatter.html", selfcontained = FALSE)

cat("\nAll Hotel Booking EDA tasks completed successfully!\n")
