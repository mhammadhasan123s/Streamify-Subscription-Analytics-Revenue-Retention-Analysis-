# Streamify Subscription Revenue & Retention Analysis

> **Course:** STQD6134 - Business Analytics
> **Assessment:** Video Presentation (20%)
> **Institution:** Universiti Kebangsaan Malaysia (UKM)
> **Scenario:** Business Analyst at Streamify, a digital streaming platform

---

## Business Questions Answered

1. Which subscription plan and region generate the most revenue?
2. What is the overall churn rate and what does it suggest?
3. How do engagement levels vary across plans or devices?
4. What strategies can improve retention or increase revenue?

---

## Repository Structure

```
stqd6134-streamify-analytics/
├── R/
│   └── streamify_analysis.R     ← single script: simulation + analysis + plots
├── data/
│   └── streamify_data.csv       ← generated when script runs (1,500 rows)
└── README.md
```

---

## Simulated Dataset Variables

| Variable | Type | Description | Values |
|---|---|---|---|
| `CustomerID` | character | Unique customer ID | C0001–C1500 |
| `JoinDate` | Date | Subscription start date | 2024-01-01 to 2024-12-31 |
| `CancelDate` | Date | Cancellation date (if applicable) | Date or NA |
| `Region` | character | Geographic region | North, South, East, West |
| `SubscriptionType` | character | Plan tier | Basic (40%), Standard (35%), Premium (25%) |
| `MonthlyFee` | numeric | Monthly charge (USD) | 10 / 20 / 30 |
| `ActiveMonths` | integer | Months subscribed | 1–12 |
| `TotalStreams` | integer | Videos watched (min 5) | ~150 mean |
| `DeviceType` | character | Primary viewing device | Mobile, Smart TV, Laptop, Tablet |
| `PaymentMethod` | character | Payment mode | Card, Online Wallet, NetBanking |
| `Revenue` | numeric | MonthlyFee × ActiveMonths | Derived |
| `IsActive` | logical | TRUE if not cancelled | Derived |
| `MonthJoined` | integer | Month number (1–12) | Derived |
| `EngagementRate` | numeric | TotalStreams / ActiveMonths | Derived |

---

## Business Metrics Computed

| # | Metric | Description |
|---|---|---|
| 1 | Total Revenue | Sum of all customer revenues |
| 2 | ARPU | Average Revenue Per User |
| 3 | Revenue by Plan | Breakdown + % share per subscription tier |
| 4 | Churn Rate | % of customers who cancelled (~30%) |
| 5 | Regional Revenue | Total revenue with % share per region |
| 6 | Avg Engagement | Streams/month by plan AND by device |
| 7 | Monthly Join Trend | New customers per month (2024) |
| 8 | Device Usage | Count + % per device type |

---

## Visualizations (6 plots)

| Plot | Type | Shows |
|---|---|---|
| 1 | Bar chart | Revenue by Subscription Plan (with $ labels + % share) |
| 2 | Bar chart | Revenue by Region (with $ labels + % share) |
| 3 | Line chart | Monthly New Customer Join Trend (Jan–Dec 2024) |
| 4 | Pie chart | Device Usage Breakdown (%) |
| 5 | Histogram | Total Streams Distribution (engagement spread) |
| 6 | Box plot | Engagement Rate by Subscription Plan |

---

## Key Findings

- **Top plan:** Premium generates highest revenue per customer ($30/month)
- **Churn rate:** ~30% — moderate; 70% customer retention achieved
- **Engagement:** Broadly similar across plans and devices in simulation
- **Mobile + Smart TV** account for ~50% of device usage

## Recommendations

1. **Premium upgrade campaign** — incentivise Basic/Standard users to trial Premium; a 20% upgrade rate would increase ARPU by ~10–15%
2. **Churn early-warning model** — use ActiveMonths + EngagementRate to predict at-risk customers and trigger personalised retention offers before cancellation

---

## How to Run

```r
setwd("path/to/stqd6134-streamify-analytics")
source("R/streamify_analysis.R")
```

Output: 6 plots + `streamify_data.csv` (1,500 rows)

---

## New Skills Demonstrated

| Skill | Detail |
|---|---|
| Business KPI computation | ARPU, Churn Rate, Revenue share |
| Data simulation | Controlled random data generation with `set.seed()` |
| ggplot2 advanced | Labels on bars, pie charts, boxplots, line charts |
| `scales` package | `dollar_format()` for professional axis labels |
| Business storytelling | Metrics → visualisations → actionable recommendations |
