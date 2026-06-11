# ---- Step 1: Gather & Align Data (R) ----
suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(lubridate)
  library(zoo)
  library(tidyr)
})

from <- as.Date("2005-01-01")
to   <- Sys.Date()

# === 1) Market series ===
# SPY adjusted close
SPY_xts <- getSymbols("SPY", src = "yahoo", from = from, to = to, auto.assign = FALSE)
spy_px  <- Ad(SPY_xts)
colnames(spy_px) <- "SPY"

# VIX level (Yahoo)
vix_xts <- getSymbols("^VIX", src = "yahoo", from = from, to = to, auto.assign = FALSE)
vix_lvl <- Cl(vix_xts)
colnames(vix_lvl) <- "VIX"
vix_lvl <- na.locf(vix_lvl, maxgap = 3, na.rm = FALSE)   # fill small gaps

# === 2) Macro series ===
# 10Y (^TNX) and 3M (^IRX) Treasury yields from Yahoo (in bps -> divide by 100)
y10_xts <- getSymbols("^TNX", src = "yahoo", from = from, to = to, auto.assign = FALSE)
y3m_xts <- getSymbols("^IRX", src = "yahoo", from = from, to = to, auto.assign = FALSE)

y10_xts <- Cl(y10_xts) / 100
y3m_xts <- Cl(y3m_xts) / 100
colnames(y10_xts) <- "Y10"
colnames(y3m_xts) <- "Y3M"

# forward-fill small holiday gaps (≤5 days)
y10_ff <- na.locf(y10_xts, maxgap = 5, na.rm = FALSE)
y3m_ff <- na.locf(y3m_xts, maxgap = 5, na.rm = FALSE)

# === 3) Merge all series on SPY trading days ===
idx <- index(spy_px)
merged_xts <- merge(spy_px, vix_lvl, y10_ff, y3m_ff, join = "right")
merged_xts <- merged_xts[idx]   # keep SPY trading days only

# === 4) Convert to tidy dataframe and add returns / term spread ===
df_raw <- merged_xts %>%
  na.omit() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date))

df_aligned <- df_raw %>%
  arrange(Date) %>%
  mutate(
    r_t = log(SPY / lag(SPY)),       # daily log return
    term_spread = Y10 - Y3M          # 10Y–3M spread (percentage points)
  ) %>%
  select(Date, SPY, r_t, VIX, Y10, Y3M, term_spread) %>%
  drop_na(r_t)

# === 5) quick sanity check ===
cat("Rows:", nrow(df_aligned),
    "\nRange:", min(df_aligned$Date), "to", max(df_aligned$Date), "\n")
summary(df_aligned[, c("r_t", "VIX", "Y10", "Y3M", "term_spread")])

# df_aligned is now ready for Step 2: feature engineering

#-------------------------------------------------------------------------------


# ---- Step 2: Feature Engineering ----
library(dplyr)
library(zoo)

# Start from the clean df_aligned from Step 1
df_feat <- df_aligned %>%
  mutate(
    # 1) 30-day rolling volatility (annualized)
    rv_30 = rollapply(r_t, width = 30, FUN = sd, fill = NA, align = "right") * sqrt(252),
    
    # 2) 5-day realized volatility (short-term risk)
    rv_5  = rollapply(r_t, width = 5,  FUN = sd, fill = NA, align = "right") * sqrt(252),
    
    # 3) 5-day mean return
    ret5d = rollapply(r_t, width = 5, FUN = mean, fill = NA, align = "right"),
    
    # 4) change in VIX (daily difference)
    dVIX = c(NA, diff(VIX)),
    
    # 5) 30-day rolling correlation between SPY returns and ΔVIX
    corr_spy_dvix = rollapply(
      data = data.frame(r_t, dVIX),
      width = 30,
      FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
      by.column = FALSE, align = "right", fill = NA
    )
  ) %>%
  select(Date, r_t, rv_30, rv_5, ret5d, VIX, dVIX, corr_spy_dvix, term_spread) %>%
  drop_na()

# Quick check
summary(df_feat[, -1])
head(df_feat)


library(ggplot2)

# (a) rolling volatility timeline
ggplot(df_feat, aes(Date, rv_30)) +
  geom_line(color="steelblue") +
  labs(title="30-Day Rolling Volatility (Annualized)", y="Volatility")

# (b) VIX vs SPY volatility relationship
ggplot(df_feat, aes(VIX, rv_30)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="VIX vs SPY Rolling Volatility", x="VIX Level", y="30-Day Volatility")

# (c) rolling correlation check
ggplot(df_feat, aes(Date, corr_spy_dvix)) +
  geom_line(color="darkorange") +
  labs(title="Rolling Correlation: SPY Returns vs ΔVIX", y="Correlation")


#-------------------------------------------------------------------------------
# ---- Step 3: Scaling + K-Means Clustering ----
library(dplyr)
library(recipes)
library(factoextra)

# Select numeric features for clustering
df_cluster <- df_feat %>%
  select(rv_30, rv_5, ret5d, VIX, corr_spy_dvix, term_spread)

# 1) Create a recipe to standardize (z-score)
rec <- recipe(~ ., data = df_cluster) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |>
  prep()

Z <- bake(rec, new_data = df_cluster)

# 2) Decide k using Elbow + Silhouette
fviz_nbclust(Z, kmeans, method = "wss") + theme_minimal() +
  labs(title = "Elbow Method for K")

fviz_nbclust(Z, kmeans, method = "silhouette") + theme_minimal() +
  labs(title = "Silhouette Method for K")

# 3) Fit K-Means (try k = 2 and k = 3)
set.seed(123)
k <- 2  # start with 2
km <- kmeans(Z, centers = k, nstart = 50, iter.max = 1000)

df_feat$cluster <- km$cluster

# 4) Profile each cluster
cluster_summary <- df_feat %>%
  group_by(cluster) %>%
  summarise(across(c(rv_30, rv_5, ret5d, VIX, term_spread), mean))

print(cluster_summary)

# 5) Map clusters → regime names
# Highest volatility & VIX = Turbulent; lowest = Calm
prof <- cluster_summary %>% arrange(rv_30)
mapping <- if (k == 2) {
  setNames(c("Calm", "Turbulent"), prof$cluster)
} else {
  setNames(c("Calm", "Neutral", "Turbulent"), prof$cluster)
}

df_feat$regime <- mapping[as.character(df_feat$cluster)]

# 6) Visualize SPY price with regime colors
library(ggplot2)
ggplot(df_feat, aes(x = Date, y = r_t)) +
  geom_line(color = "gray60") +
  geom_point(aes(color = regime), size = 0.6) +
  scale_color_manual(values = c("Calm" = "blue", "Turbulent" = "red", "Neutral" = "orange")) +
  labs(title = "Market Regimes via K-Means", y = "SPY Daily Return") +
  theme_minimal()

# 7) K=3 
k <- 3
km <- kmeans(Z, centers = k, nstart = 50, iter.max = 1000)
df_feat$cluster <- km$cluster
cluster_summary <- df_feat %>%
  group_by(cluster) %>%
  summarise(across(c(rv_30, rv_5, ret5d, VIX, term_spread), mean))
print(cluster_summary)
mapping <- c("Calm"="blue", "Neutral"="orange", "Turbulent"="red")
# ---- Visualize 3-cluster result ----
library(ggplot2)

# Map each cluster to its economic regime
mapping3 <- c("1" = "Calm", "2" = "Neutral", "3" = "Turbulent")
df_feat$regime3 <- mapping3[as.character(df_feat$cluster)]

# Choose colors
regime_colors <- c("Calm" = "blue", "Neutral" = "orange", "Turbulent" = "red")

# Plot SPY daily returns over time with regime color
ggplot(df_feat, aes(x = Date, y = r_t)) +
  geom_line(color = "gray70") +
  geom_point(aes(color = regime3), size = 0.6) +
  scale_color_manual(values = regime_colors) +
  labs(
    title = "Market Regimes via K-Means (k = 3)",
    y = "SPY Daily Return",
    color = "Regime"
  ) +
  theme_minimal()

write.csv(df_feat[, c("Date", "r_t", "VIX", "term_spread", "regime3")],
          "RegimeSense_KMeans_Output.csv", row.names = FALSE)
df_check <- read.csv("~/Desktop/SEM 3/FE 800/Project/RegimeSense_KMeans_Output.csv")

# Check date range and frequency
range(as.Date(df_check$Date))
length(unique(df_check$Date)) / ((as.numeric(diff(range(as.Date(df_check$Date)))))/365)
# ≈ 252 observations per year → confirms daily trading data

# Regime counts
table(df_check$regime3)


#-------------------------------------------------------------------------------
# ---- Step 4: Smooth the Regime Labels (Fix daily flipping issue) ----
#-------------------------------------------------------------------------------

library(zoo)
library(dplyr)

df_feat <- df_feat %>%
  mutate(
    # Convert regime to numeric for easier smoothing
    regime_num = case_when(
      regime3 == "Calm" ~ 1,
      regime3 == "Neutral" ~ 2,
      regime3 == "Turbulent" ~ 3
    )
  )

# ---- 4a) Rolling Majority Vote (7-day window) ----
roll_majority <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(as.numeric(names(sort(table(x), decreasing = TRUE)[1])))
}

df_feat$regime_smooth_num <- rollapply(
  df_feat$regime_num,
  width = 7,                # you can try 5, 7, 10
  FUN = roll_majority,
  fill = NA,
  align = "right"
)

# Map numbers back to labels
df_feat$regime_smooth <- case_when(
  df_feat$regime_smooth_num == 1 ~ "Calm",
  df_feat$regime_smooth_num == 2 ~ "Neutral",
  df_feat$regime_smooth_num == 3 ~ "Turbulent",
  TRUE ~ NA_character_
)

# Drop early NAs
df_feat_clean <- df_feat %>% drop_na(regime_smooth)

table(df_feat_clean$regime_smooth)


#-------------------------------------------------------------------------------
# ---- Step 5: Visualize SMOOTHED Regimes ----
#-------------------------------------------------------------------------------

ggplot(df_feat_clean, aes(x = Date, y = r_t)) +
  geom_line(color = "gray70") +
  geom_point(aes(color = regime_smooth), size = 0.6) +
  scale_color_manual(values = c(
    "Calm" = "blue",
    "Neutral" = "orange",
    "Turbulent" = "red"
  )) +
  labs(
    title = "Market Regimes via K-Means (Smoothed - 7 Day Rolling Majority Vote)",
    y = "SPY Daily Return",
    color = "Regime"
  ) +
  theme_minimal()

# ---- 14-day smoothing ----
df_feat$regime_smooth_num_14 <- rollapply(
  df_feat$regime_num,
  width = 14,
  FUN = roll_majority,
  fill = NA,
  align = "right"
)

df_feat$regime_smooth_14 <- case_when(
  df_feat$regime_smooth_num_14 == 1 ~ "Calm",
  df_feat$regime_smooth_num_14 == 2 ~ "Neutral",
  df_feat$regime_smooth_num_14 == 3 ~ "Turbulent"
)

df_feat_clean_14 <- df_feat %>% drop_na(regime_smooth_14)


ggplot(df_feat_clean_14, aes(Date, r_t)) +
  geom_point(aes(color = regime_smooth_14), size=0.6) +
  scale_color_manual(values=c("Calm"="blue","Neutral"="orange","Turbulent"="red")) +
  theme_minimal() +
  labs(title="Market Regimes (14-Day Smoothed)", y="Daily Return")


df_feat2 <- df_aligned %>%
  mutate(
    rv_30 = rollapply(r_t, 30, sd, fill=NA, align="right") * sqrt(252),
    VIX_30 = rollapply(VIX, 30, mean, fill=NA, align="right"),
    corr_30 = rollapply(
      data.frame(r_t, dVIX = c(NA, diff(VIX))),
      30,
      function(x) cor(x[,1], x[,2], use="complete.obs"),
      align="right",
      fill=NA
    )
  ) %>%
  select(Date, r_t, rv_30, VIX_30, corr_30, term_spread) %>%
  drop_na()
