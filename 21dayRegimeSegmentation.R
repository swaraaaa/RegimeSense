
#### STEP 0: Load Libraries
suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(lubridate)
  library(zoo)
  library(tidyr)
  library(recipes)
  library(factoextra)
  library(ggplot2)
})

from <- as.Date("2005-01-01")
to   <- Sys.Date()

# --- 1a) SPY Prices ---
SPY_xts <- getSymbols("SPY", src = "yahoo", from = from, to = to,
                      auto.assign = FALSE)
spy_px <- Ad(SPY_xts)
colnames(spy_px) <- "SPY"

# --- 1b) VIX ---
vix_xts <- getSymbols("^VIX", src = "yahoo", from = from, to = to,
                      auto.assign = FALSE)
vix_lvl <- Cl(vix_xts)
colnames(vix_lvl) <- "VIX"
vix_lvl <- na.locf(vix_lvl, maxgap = 3)

# --- 1c) Treasury Yields (TNX = 10y, IRX = 3m) ---
y10 <- getSymbols("^TNX", src = "yahoo", from = from, to = to,
                  auto.assign = FALSE) |> Cl() / 100
y3m <- getSymbols("^IRX", src = "yahoo", from = from, to = to,
                  auto.assign = FALSE) |> Cl() / 100
colnames(y10) <- "Y10"
colnames(y3m) <- "Y3M"

y10 <- na.locf(y10, maxgap = 5)
y3m <- na.locf(y3m, maxgap = 5)

# --- 1d) Merge on SPY trading days ---
idx <- index(spy_px)

merged <- merge(spy_px, vix_lvl, y10, y3m, join = "right")
merged <- merged[idx]

# Convert to tidy
df <- merged %>%
  na.omit() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Date") %>%
  mutate(
    Date = as.Date(Date),
    r_t = log(SPY / lag(SPY)),
    term_spread = Y10 - Y3M
  ) %>%
  drop_na(r_t)

cat("Data Loaded → Rows:", nrow(df), " Range:", range(df$Date), "\n")


df_feat21 <- df %>%
  arrange(Date) %>%
  mutate(
    # 21-day rolling volatility (annualized)
    rv_21 = rollapply(r_t, 21, sd, fill = NA, align = "right") * sqrt(252),
    
    # 21-day mean VIX
    VIX_21 = rollapply(VIX, 21, mean, fill = NA, align = "right"),
    
    # daily VIX change
    dVIX = c(NA, diff(VIX)),
    
    # 21-day rolling correlation (SPY return vs ΔVIX)
    corr_21 = rollapply(
      cbind(r_t, dVIX),                 # FIXED (always 2 columns)
      21,
      FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
      by.column = FALSE,
      align = "right",
      fill = NA
    )
  ) %>%
  select(Date, r_t, rv_21, VIX_21, corr_21, term_spread) %>%
  drop_na()

df_cluster21 <- df_feat21 %>%
  select(rv_21, VIX_21, corr_21, term_spread)

rec21 <- recipe(~ ., df_cluster21) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep()

Z21 <- bake(rec21, df_cluster21)

set.seed(123)
km21 <- kmeans(Z21, centers = 3, nstart = 50, iter.max = 1000)

df_feat21$cluster <- km21$cluster

# Profile clusters
cluster_summary21 <- df_feat21 %>%
  group_by(cluster) %>%
  summarise(across(c(rv_21, VIX_21, corr_21, term_spread), mean))

print(cluster_summary21)

# Map clusters → regimes by volatility ranking
prof21 <- cluster_summary21 %>% arrange(rv_21)
mapping21 <- setNames(c("Calm", "Neutral", "Turbulent"), prof21$cluster)

df_feat21$regime_raw <- mapping21[as.character(df_feat21$cluster)]



df_feat21$regime_num <- case_when(
  df_feat21$regime_raw == "Calm" ~ 1,
  df_feat21$regime_raw == "Neutral" ~ 2,
  df_feat21$regime_raw == "Turbulent" ~ 3
)

roll_majority <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
}

df_feat21$regime_smooth_num <- rollapply(
  df_feat21$regime_num,
  width = 21,
  FUN = roll_majority,
  fill = NA,
  align = "right"
)

df_feat21$regime_smooth <- case_when(
  df_feat21$regime_smooth_num == 1 ~ "Calm",
  df_feat21$regime_smooth_num == 2 ~ "Neutral",
  df_feat21$regime_smooth_num == 3 ~ "Turbulent"
)

df_feat21_clean <- df_feat21 %>% drop_na(regime_smooth)


ggplot(df_feat21_clean, aes(Date, r_t)) +
  geom_point(aes(color = regime_smooth), size = 0.6) +
  scale_color_manual(values = c(
    "Calm" = "blue",
    "Neutral" = "orange",
    "Turbulent" = "red"
  )) +
  labs(
    title = "Market Regimes via K-Means (Rolling Features + 21-Day Smoothed)",
    y = "SPY Daily Return",
    color = "Regime"
  ) +
  theme_minimal()

