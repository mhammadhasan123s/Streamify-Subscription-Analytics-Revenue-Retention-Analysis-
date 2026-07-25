# =============================================================================
# STQD6134 — Business Analytics  |  Video Presentation Assignment (20%)
# Topic   : Streamify — Subscription Revenue & Retention Analysis
# Scenario: Business Analyst at a digital streaming platform
#
# Tasks covered:
#   1. Data Simulation & Preprocessing
#   2. Business Metric Calculations (8 metrics)
#   3. Visualizations (5 plots)
#   4. Insights & Recommendations
#
# HOW TO RUN
#   source("streamify_analysis.R")
#   The simulated dataset is also exported as: streamify_data.csv
# =============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────
pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tidyr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)

cat("\n", strrep("=", 65), "\n")
cat("  STQD6134 Business Analytics — Streamify Analysis\n")
cat(strrep("=", 65), "\n\n")


# =============================================================================
# SECTION 1 — DATA SIMULATION
# =============================================================================
cat(strrep("-", 65), "\n")
cat("SECTION 1 · Data Simulation\n")
cat(strrep("-", 65), "\n\n")

set.seed(123)   # ensures reproducible results
n <- 1500

CustomerID       <- paste0("C", sprintf("%04d", 1:n))

JoinDate         <- sample(
  seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
  n, replace = TRUE
)

ActiveMonths     <- sample(1:12, n, replace = TRUE)

# FIX: The original ifelse() strips the Date class from JoinDate,
# storing CancelDate as a plain integer (days since 1970-01-01).
# Wrapping in as.Date() restores the correct Date class.
CancelDate       <- as.Date(ifelse(
  runif(n) < 0.3,
  JoinDate + ActiveMonths * 30,
  NA
), origin = "1970-01-01")   # 30% of customers cancel

Region           <- sample(c("North", "South", "East", "West"),
                           n, replace = TRUE)

SubscriptionType <- sample(
  c("Basic", "Standard", "Premium"),
  n, replace = TRUE,
  prob = c(0.40, 0.35, 0.25)   # 40% Basic, 35% Standard, 25% Premium
)

MonthlyFee       <- case_when(
  SubscriptionType == "Basic"    ~ 10,
  SubscriptionType == "Standard" ~ 20,
  SubscriptionType == "Premium"  ~ 30
)

# FIX: rnorm can produce negative stream counts — clamp minimum to 5.
TotalStreams     <- pmax(round(rnorm(n, mean = 150, sd = 60)), 5)

DeviceType       <- sample(
  c("Mobile", "Smart TV", "Laptop", "Tablet"),
  n, replace = TRUE
)

PaymentMethod    <- sample(
  c("Card", "Online Wallet", "NetBanking"),
  n, replace = TRUE
)

stream_data <- data.frame(
  CustomerID, JoinDate, CancelDate, Region, SubscriptionType,
  MonthlyFee, ActiveMonths, TotalStreams, DeviceType, PaymentMethod,
  stringsAsFactors = FALSE
)

stream_data$Revenue <- stream_data$MonthlyFee * stream_data$ActiveMonths

cat(sprintf("Dataset simulated: %d customers\n", nrow(stream_data)))
cat("\nFirst 6 rows:\n")
print(head(stream_data))
cat("\nData structure:\n")
str(stream_data)


# =============================================================================
# SECTION 2 — DATA PREPROCESSING
# =============================================================================
cat(strrep("-", 65), "\n")
cat("SECTION 2 · Data Preprocessing\n")
cat(strrep("-", 65), "\n\n")

# ── 2.1 Check missing values ─────────────────────────────────────────────────
cat("Missing values per column:\n")
print(colSums(is.na(stream_data)))
cat("\nNote: CancelDate NAs are EXPECTED — they represent active customers.\n\n")

# ── 2.2 Convert data types ───────────────────────────────────────────────────
# JoinDate and CancelDate are already Date class from simulation above.
# Confirm:
cat(sprintf("JoinDate  class: %s\n", class(stream_data$JoinDate)))
cat(sprintf("CancelDate class: %s\n\n", class(stream_data$CancelDate)))

# ── 2.3 Create derived variables ─────────────────────────────────────────────
stream_data <- stream_data %>%
  mutate(
    # IsActive: TRUE if customer has NOT cancelled (CancelDate is NA)
    IsActive     = is.na(CancelDate),

    # FIX: Use as.integer(month(...)) to avoid factor ordering issues in plots.
    # MonthJoined: numeric month (1–12) for sorting; MonthLabel for display.
    MonthJoined  = month(JoinDate),
    MonthLabel   = month(JoinDate, label = TRUE, abbr = TRUE),

    # EngagementRate: streams per active month (individual customer level)
    EngagementRate = TotalStreams / ActiveMonths,

    # RevenueCategory: bucket customers for deeper analysis
    RevenueCategory = case_when(
      Revenue <= 60  ~ "Low (<=$60)",
      Revenue <= 120 ~ "Mid ($61-$120)",
      TRUE           ~ "High (>$120)"
    )
  )

cat("Derived variables created:\n")
cat("  IsActive       : TRUE = active, FALSE = churned\n")
cat("  MonthJoined    : numeric month (1-12) for proper sorting\n")
cat("  EngagementRate : TotalStreams / ActiveMonths\n")
cat("  RevenueCategory: Low / Mid / High revenue customer buckets\n\n")

cat("Active vs Churned customer count:\n")
print(table(IsActive = stream_data$IsActive))


# =============================================================================
# SECTION 3 — BUSINESS METRIC CALCULATIONS
# =============================================================================
cat(strrep("-", 65), "\n")
cat("SECTION 3 · Business Metric Calculations\n")
cat(strrep("-", 65), "\n\n")

# ── Metric 1: Total Revenue ───────────────────────────────────────────────────
total_revenue <- sum(stream_data$Revenue)
cat(sprintf("1. Total Revenue           : $%s\n",
            format(total_revenue, big.mark = ",")))

# ── Metric 2: ARPU ────────────────────────────────────────────────────────────
arpu <- mean(stream_data$Revenue)
cat(sprintf("2. ARPU (Avg Revenue/User) : $%.2f\n", arpu))

# ── Metric 3: Revenue by Subscription Type ────────────────────────────────────
revenue_by_plan <- stream_data %>%
  group_by(SubscriptionType) %>%
  summarise(
    Customers    = n(),
    TotalRevenue = sum(Revenue),
    AvgRevenue   = round(mean(Revenue), 2),
    Share_pct    = round(sum(Revenue) / total_revenue * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalRevenue))

cat("\n3. Revenue by Subscription Type:\n")
print(revenue_by_plan)

# ── Metric 4: Churn Rate ──────────────────────────────────────────────────────
# Churn rate = proportion of customers who have cancelled.
# With prob=0.3 in simulation, expect ~30% churn.
churn_rate <- mean(!stream_data$IsActive)
cat(sprintf("\n4. Churn Rate              : %.1f%%\n", churn_rate * 100))
cat(sprintf("   Active customers        : %d\n",   sum( stream_data$IsActive)))
cat(sprintf("   Churned customers       : %d\n",   sum(!stream_data$IsActive)))

# ── Metric 5: Regional Revenue ────────────────────────────────────────────────
revenue_by_region <- stream_data %>%
  group_by(Region) %>%
  summarise(
    Customers    = n(),
    TotalRevenue = sum(Revenue),
    AvgRevenue   = round(mean(Revenue), 2),
    Share_pct    = round(sum(Revenue) / total_revenue * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalRevenue))

cat("\n5. Revenue by Region:\n")
print(revenue_by_region)

# ── Metric 6: Average Engagement (streams per active month) ──────────────────
avg_engagement <- mean(stream_data$EngagementRate)
cat(sprintf("\n6. Avg Engagement (streams/month)   : %.2f\n", avg_engagement))

# Engagement by Subscription Type (assignment asks this)
engagement_by_plan <- stream_data %>%
  group_by(SubscriptionType) %>%
  summarise(
    AvgStreams       = round(mean(TotalStreams), 1),
    AvgStreams_Month = round(mean(EngagementRate), 2),
    .groups = "drop"
  )
cat("\n   Engagement by Subscription Type:\n")
print(engagement_by_plan)

# Engagement by Device Type (assignment asks this)
engagement_by_device <- stream_data %>%
  group_by(DeviceType) %>%
  summarise(
    Customers        = n(),
    AvgStreams       = round(mean(TotalStreams), 1),
    AvgStreams_Month = round(mean(EngagementRate), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(AvgStreams))
cat("\n   Engagement by Device Type:\n")
print(engagement_by_device)

# ── Metric 7: Monthly Join Trend ─────────────────────────────────────────────
monthly_join <- stream_data %>%
  group_by(MonthJoined, MonthLabel) %>%
  summarise(NewCustomers = n(), .groups = "drop") %>%
  arrange(MonthJoined)

cat("\n7. Monthly New Customer Joins:\n")
print(monthly_join[, c("MonthLabel", "NewCustomers")])

# ── Metric 8: Device Usage Breakdown ─────────────────────────────────────────
device_usage <- stream_data %>%
  count(DeviceType) %>%
  mutate(Percent = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

cat("\n8. Device Usage Breakdown:\n")
print(device_usage)

# ── Summary table ─────────────────────────────────────────────────────────────
cat("\n", strrep("-", 40), "\n")
cat("KPI DASHBOARD SUMMARY:\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Total Revenue   : $%s\n",    format(total_revenue, big.mark = ",")))
cat(sprintf("  ARPU            : $%.2f\n",  arpu))
cat(sprintf("  Churn Rate      : %.1f%%\n", churn_rate * 100))
cat(sprintf("  Active Customers: %d\n",     sum(stream_data$IsActive)))
cat(sprintf("  Avg Engagement  : %.2f streams/month\n", avg_engagement))
cat(sprintf("  Top Plan        : %s ($%s revenue)\n",
            revenue_by_plan$SubscriptionType[1],
            format(revenue_by_plan$TotalRevenue[1], big.mark = ",")))
cat(sprintf("  Top Region      : %s ($%s revenue)\n",
            revenue_by_region$Region[1],
            format(revenue_by_region$TotalRevenue[1], big.mark = ",")))
cat(strrep("-", 40), "\n\n")


# =============================================================================
# SECTION 4 — VISUALIZATIONS
# =============================================================================
cat(strrep("-", 65), "\n")
cat("SECTION 4 · Visualizations\n")
cat(strrep("-", 65), "\n\n")

# Shared colour palette
PAL_PLAN   <- c("Basic" = "#3498DB", "Standard" = "#2ECC71", "Premium" = "#E74C3C")
PAL_REGION <- c("North" = "#9B59B6", "South" = "#E67E22",
                 "East"  = "#1ABC9C", "West"  = "#F39C12")
PAL_DEVICE <- c("Mobile" = "#3498DB", "Smart TV" = "#E74C3C",
                 "Laptop" = "#2ECC71", "Tablet"  = "#9B59B6")

# ── Plot 1: Revenue by Subscription Type ──────────────────────────────────────
p1 <- ggplot(revenue_by_plan,
             aes(x = reorder(SubscriptionType, -TotalRevenue),
                 y = TotalRevenue,
                 fill = SubscriptionType)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0("$", format(TotalRevenue, big.mark = ","),
                                "\n(", Share_pct, "%)")),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = PAL_PLAN) +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Total Revenue by Subscription Plan",
    subtitle = sprintf("Total = $%s  |  n = %d customers",
                       format(total_revenue, big.mark = ","), n),
    x        = "Subscription Plan",
    y        = "Total Revenue (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
print(p1)
cat("Plot 1 displayed: Revenue by Subscription Type\n")

# ── Plot 2: Revenue by Region ─────────────────────────────────────────────────
p2 <- ggplot(revenue_by_region,
             aes(x = reorder(Region, -TotalRevenue),
                 y = TotalRevenue,
                 fill = Region)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0("$", format(TotalRevenue, big.mark = ","),
                                "\n(", Share_pct, "%)")),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = PAL_REGION) +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Total Revenue by Region",
    subtitle = "Revenue distribution across all four geographic regions",
    x        = "Region",
    y        = "Total Revenue (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
print(p2)
cat("Plot 2 displayed: Revenue by Region\n")

# ── Plot 3: Monthly Join Trend ────────────────────────────────────────────────
# FIX: Use MonthJoined (numeric 1-12) for x-axis to ensure correct month order,
# then apply MonthLabel as display labels. The original used label=TRUE factor
# which could reorder alphabetically or by factor level.
p3 <- ggplot(monthly_join,
             aes(x = MonthJoined, y = NewCustomers)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = NewCustomers), vjust = -0.8, size = 3.2) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Monthly New Customer Join Trend (2024)",
    subtitle = sprintf("Average: %.0f new customers/month",
                       mean(monthly_join$NewCustomers)),
    x        = "Month",
    y        = "New Customers"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
print(p3)
cat("Plot 3 displayed: Monthly Join Trend\n")

# ── Plot 4: Device Usage Breakdown ────────────────────────────────────────────
p4 <- ggplot(device_usage,
             aes(x = "", y = n, fill = DeviceType)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(DeviceType, "\n", Percent, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5, fontface = "bold") +
  scale_fill_manual(values = PAL_DEVICE) +
  labs(
    title    = "Device Usage Breakdown",
    subtitle = "Primary device used for streaming",
    fill     = "Device"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )
print(p4)
cat("Plot 4 displayed: Device Usage Breakdown\n")

# ── Plot 5: TotalStreams Distribution ─────────────────────────────────────────
p5 <- ggplot(stream_data, aes(x = TotalStreams)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(stream_data$TotalStreams),
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text",
           x     = mean(stream_data$TotalStreams) + 10,
           y     = Inf,
           label = sprintf("Mean = %.0f", mean(stream_data$TotalStreams)),
           vjust = 2, hjust = 0, color = "red", size = 3.5) +
  labs(
    title    = "Distribution of Total Streams per Customer",
    subtitle = "Shows overall engagement spread across the customer base",
    x        = "Total Streams Watched",
    y        = "Number of Customers"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
print(p5)
cat("Plot 5 displayed: TotalStreams Distribution\n")

# ── BONUS Plot 6: Engagement by Plan & Device ─────────────────────────────────
# Assignment Q4 asks "how do engagement levels vary across plans or devices?"
# This additional plot directly answers that question.
engagement_long <- stream_data %>%
  select(SubscriptionType, DeviceType, EngagementRate)

p6 <- ggplot(engagement_long,
             aes(x = SubscriptionType, y = EngagementRate, fill = SubscriptionType)) +
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = PAL_PLAN) +
  labs(
    title    = "Engagement Rate by Subscription Plan",
    subtitle = "Streams per Active Month — distribution across plan types",
    x        = "Subscription Plan",
    y        = "Streams per Active Month"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
print(p6)
cat("Plot 6 displayed: Engagement by Subscription Plan\n\n")


# =============================================================================
# SECTION 5 — INSIGHTS & RECOMMENDATIONS
# =============================================================================
cat(strrep("-", 65), "\n")
cat("SECTION 5 · Business Insights & Recommendations\n")
cat(strrep("-", 65), "\n\n")

# Identify top plan and region dynamically from computed metrics
top_plan   <- revenue_by_plan$SubscriptionType[1]
top_region <- revenue_by_region$Region[1]

cat("Q1 — Which subscription plan and region generate the most revenue?\n")
cat(sprintf("  Top Plan  : %s  ($%s, %.1f%% of total revenue)\n",
            top_plan,
            format(revenue_by_plan$TotalRevenue[1], big.mark = ","),
            revenue_by_plan$Share_pct[1]))
cat(sprintf("  Top Region: %s  ($%s, %.1f%% of total revenue)\n\n",
            top_region,
            format(revenue_by_region$TotalRevenue[1], big.mark = ","),
            revenue_by_region$Share_pct[1]))

cat("Q2 — What is the overall churn rate and what does it suggest?\n")
cat(sprintf("  Churn Rate: %.1f%%\n", churn_rate * 100))
cat("  Interpretation:\n")
if (churn_rate < 0.35) {
  cat("  A churn rate of ~30% is moderate for a streaming platform.\n")
  cat("  70% of customers remain active, indicating reasonable retention.\n")
  cat("  However, losing 3 in 10 customers is a significant revenue risk\n")
  cat("  and warrants targeted retention programmes.\n\n")
} else {
  cat("  A churn rate above 35% is HIGH and demands immediate action.\n\n")
}

cat("Q3 — How do engagement levels vary across plans or devices?\n")
cat("  By subscription plan:\n")
for (i in seq_len(nrow(engagement_by_plan))) {
  cat(sprintf("    %-10s : %.2f streams/month\n",
              engagement_by_plan$SubscriptionType[i],
              engagement_by_plan$AvgStreams_Month[i]))
}
cat("  By device:\n")
for (i in seq_len(nrow(engagement_by_device))) {
  cat(sprintf("    %-10s : %.2f streams/month  (%d customers)\n",
              engagement_by_device$DeviceType[i],
              engagement_by_device$AvgStreams_Month[i],
              engagement_by_device$Customers[i]))
}
cat("  Note: Engagement is similar across plans (random simulation).\n")
cat("  In real data, Premium subscribers typically stream more.\n\n")

cat("Q4 — Strategies to improve retention and revenue:\n\n")

cat("  Strategy 1: Premium Loyalty Programme\n")
cat(sprintf("  Premium plan generates the highest revenue (%.1f%% share)\n",
            revenue_by_plan$Share_pct[revenue_by_plan$SubscriptionType == "Premium"]))
cat("  but represents only 25% of the customer base.\n")
cat("  Action: Offer Basic/Standard subscribers an upgrade incentive\n")
cat("  (e.g., 1-month free trial of Premium) to grow this segment.\n")
cat("  Expected impact: +10-15% ARPU if 20% of Standard users upgrade.\n\n")

cat("  Strategy 2: Churn Prediction & Early Intervention\n")
cat(sprintf("  With %.1f%% churn, building a churn early-warning model\n", churn_rate * 100))
cat("  (using ActiveMonths, EngagementRate, SubscriptionType as features)\n")
cat("  could flag at-risk customers before they cancel.\n")
cat("  Action: Trigger personalised retention offers (discount, extra\n")
cat("  content, or account pause option) to customers with low engagement.\n")
cat("  Expected impact: Reducing churn by 5pp saves ~$")
potential_saving <- round(sum(!stream_data$IsActive) * 0.05 * arpu)
cat(format(potential_saving, big.mark = ","), "in annual revenue.\n\n")


# =============================================================================
# SECTION 6 — EXPORT SIMULATED DATASET
# =============================================================================
cat(strrep("-", 65), "\n")
cat("SECTION 6 · Export Dataset\n")
cat(strrep("-", 65), "\n\n")

write.csv(stream_data, "streamify_data.csv", row.names = FALSE)
cat(sprintf("Simulated dataset exported: streamify_data.csv  (%d rows x %d cols)\n",
            nrow(stream_data), ncol(stream_data)))
cat("Columns: ", paste(names(stream_data), collapse = ", "), "\n\n")

cat(strrep("=", 65), "\n")
cat("  Analysis complete. 6 plots displayed.\n")
cat("  Dataset saved: streamify_data.csv\n")
cat(strrep("=", 65), "\n")
