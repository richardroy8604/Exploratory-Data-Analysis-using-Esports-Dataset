# ==============================================================================
# Exploratory Data Analysis (EDA) and Data Storytelling using R
# Case Study: Global Esports Performance & Historical Dynamics Dataset
# ==============================================================================

# Required Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(plotly)
library(DT)
library(htmlwidgets)
library(e1071)
library(scales)

# Create directory for output plots
if (!dir.exists("plots")) {
  dir.create("plots")
}

# ------------------------------------------------------------------------------
# Task 1 & Task 2: Data Import, Inspection, & Pre-processing
# ------------------------------------------------------------------------------
cat("Loading Esports Dataset...\n")
df_gen <- read.csv("Esports Dataset/GeneralEsportData.csv", stringsAsFactors = FALSE)

cat("Dimensions:", dim(df_gen), "\n")
cat("Structure of imported data:\n")
str(df_gen)

cat("Missing values count per variable:\n")
print(colSums(is.na(df_gen)))

# Pre-processing
df_gen$PercentOffline[is.na(df_gen$PercentOffline)] <- 0 # Impute NAs for 0 earnings
df_gen$Genre <- as.factor(df_gen$Genre)

# Feature Engineering
df_gen$LogTotalEarnings <- log10(df_gen$TotalEarnings + 1)
df_gen$LogTotalPlayers <- log10(df_gen$TotalPlayers + 1)
df_gen$EarningsPerPlayer <- ifelse(df_gen$TotalPlayers > 0, df_gen$TotalEarnings / df_gen$TotalPlayers, 0)

# ------------------------------------------------------------------------------
# Task 3: Descriptive Statistics
# ------------------------------------------------------------------------------
num_vars <- c("TotalEarnings", "OfflineEarnings", "PercentOffline", "TotalPlayers", "TotalTournaments")

get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

calc_stats <- function(var_name) {
  x <- df_gen[[var_name]]
  q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  data.frame(
    Variable = var_name,
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mode = get_mode(x),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Q1 = q[1],
    Q3 = q[3],
    IQR = IQR(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE)
  )
}

stats_summary <- bind_rows(lapply(num_vars, calc_stats))
cat("\n--- SUMMARY DESCRIPTIVE STATISTICS ---\n")
print(stats_summary)

# ------------------------------------------------------------------------------
# Task 4: Univariate Visualizations
# ------------------------------------------------------------------------------
theme_custom <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = "#555555"))

# Histogram & Density of Log Total Earnings
p1 <- ggplot(df_gen, aes(x = LogTotalEarnings)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#3498db", color = "white", alpha = 0.7) +
  geom_density(color = "#2c3e50", linewidth = 1) +
  labs(title = "Task 4: Distribution of Log10 Total Prize Earnings",
       x = "Log10(Total Earnings + 1)", y = "Density") +
  theme_custom

ggsave("plots/univariate_histogram.png", p1, width = 7, height = 4.5, dpi = 300)

# Boxplot of Active Pro Players
p2 <- ggplot(df_gen, aes(y = TotalPlayers + 1)) +
  geom_boxplot(fill = "#2ecc71", color = "#27ae60", outlier.color = "#e74c3c", outlier.alpha = 0.6) +
  scale_y_log10(labels = comma) +
  labs(title = "Task 4: Boxplot of Active Pro Players per Game",
       y = "Total Players + 1 (Log10 Scale)") +
  theme_custom

ggsave("plots/univariate_boxplot.png", p2, width = 6, height = 4.5, dpi = 300)

# Bar Chart of Game Frequencies by Genre
genre_counts <- df_gen %>% count(Genre) %>% arrange(desc(n))
p3 <- ggplot(genre_counts, aes(x = reorder(Genre, n), y = n)) +
  geom_bar(stat = "identity", fill = "#9b59b6", color = "white") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  ylim(0, max(genre_counts$n) * 1.1) +
  labs(title = "Task 4: Count of Esports Games by Genre", x = "Genre", y = "Count of Games") +
  theme_custom

ggsave("plots/univariate_genre_barchart.png", p3, width = 7.5, height = 4.5, dpi = 300)

# ------------------------------------------------------------------------------
# Task 5: Bivariate Visualizations
# ------------------------------------------------------------------------------
major_genres <- c("Fighting Game", "First-Person Shooter", "Multiplayer Online Battle Arena", 
                 "Battle Royale", "Strategy", "Sports", "Racing")
df_sub <- df_gen %>% filter(Genre %in% major_genres)

# Log Players vs Log Earnings Scatter Plot
p5 <- ggplot(df_sub, aes(x = LogTotalPlayers, y = LogTotalEarnings, color = Genre)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Task 5: Log Total Players vs Log Total Earnings",
       x = "Log10(Total Players + 1)", y = "Log10(Total Earnings + 1)") +
  scale_color_brewer(palette = "Dark2") +
  theme_custom

ggsave("plots/bivariate_scatter.png", p5, width = 7.5, height = 5, dpi = 300)

# Percent Offline Violin Plot by Genre
p7 <- ggplot(df_sub, aes(x = Genre, y = PercentOffline, fill = Genre)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = "Task 5: Proportion of Offline (LAN) Earnings by Genre",
       x = "Genre", y = "Percent Offline Earnings") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_custom +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

ggsave("plots/bivariate_violin_offline.png", p7, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# Task 6: Correlation Analysis
# ------------------------------------------------------------------------------
num_mat <- df_gen %>% select(ReleaseDate, TotalEarnings, OfflineEarnings, PercentOffline, TotalPlayers, TotalTournaments)
cor_mat <- cor(num_mat, use = "complete.obs")

cat("\n--- CORRELATION MATRIX ---\n")
print(round(cor_mat, 3))

png("plots/correlation_heatmap.png", width = 1800, height = 1500, res = 300)
corrplot(cor_mat, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Task 6: Correlation Heatmap of Esports Attributes", mar = c(0,0,2,0))
dev.off()

# ------------------------------------------------------------------------------
# Task 7: Hypothesis Testing (One-Way ANOVA)
# ------------------------------------------------------------------------------
df_major <- df_gen %>% group_by(Genre) %>% filter(n() >= 15) %>% ungroup()

cat("\n=== ONE-WAY ANOVA TEST (Log Earnings by Genre) ===\n")
anova_res <- aov(LogTotalEarnings ~ Genre, data = df_major)
print(summary(anova_res))

cat("\n=== WELCH ANOVA (Unequal Variances) ===\n")
print(oneway.test(LogTotalEarnings ~ Genre, data = df_major, var.equal = FALSE))

cat("\n=== KRUSKAL-WALLIS TEST ===\n")
print(kruskal.test(TotalEarnings ~ Genre, data = df_major))

# ------------------------------------------------------------------------------
# Task 8: Save Interactive Widgets
# ------------------------------------------------------------------------------
p_int1 <- plot_ly(df_gen, x = ~TotalPlayers, y = ~TotalEarnings, color = ~Genre,
                  text = ~paste("Game:", Game, "<br>Release:", ReleaseDate),
                  type = 'scatter', mode = 'markers') %>%
  layout(title = "Interactive Scatter: Players vs Earnings", xaxis = list(type = "log"), yaxis = list(type = "log"))

saveWidget(p_int1, "plots/interactive_scatter.html", selfcontained = FALSE)

dt_w <- datatable(df_gen %>% select(Game, ReleaseDate, Genre, TotalEarnings, OfflineEarnings, PercentOffline, TotalPlayers, TotalTournaments),
                  options = list(pageLength = 10), filter = 'top')
saveWidget(dt_w, "plots/interactive_table.html", selfcontained = FALSE)

cat("\nAll EDA tasks completed successfully!\n")
