
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


df_feat <- df %>%
  arrange(Date) %>%
  mutate(
    rv_30 = rollapply(r_t, 30, sd, fill = NA, align = "right") * sqrt(252),
    
    VIX_30 = rollapply(VIX, 30, mean, fill = NA, align = "right"),
    
    dVIX = c(NA, diff(VIX)),
    
    corr_30 = rollapply(
      data.frame(r_t, dVIX),
      30,
      FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
      by.column = FALSE,
      align = "right",
      fill = NA
    )
  ) %>%
  select(Date, r_t, rv_30, VIX_30, corr_30, term_spread) %>%
  drop_na()



# Select good-quality features only
df_cluster <- df_feat %>%
  select(rv_30, VIX_30, corr_30, term_spread)

# Standardize
rec <- recipe(~ ., df_cluster) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  prep()

Z <- bake(rec, df_cluster)

# Fit K-means (k=3)
set.seed(123)
km <- kmeans(Z, centers = 3, nstart = 50, iter.max = 1000)
df_feat$cluster <- km$cluster

# Profile clusters
cluster_summary <- df_feat %>%
  group_by(cluster) %>%
  summarise(across(c(rv_30, VIX_30, corr_30, term_spread), mean))

print(cluster_summary)

# Map clusters → regimes (sorted by volatility)
prof <- cluster_summary %>% arrange(rv_30)
mapping <- setNames(c("Calm", "Neutral", "Turbulent"), prof$cluster)
df_feat$regime_raw <- mapping[as.character(df_feat$cluster)]



# Convert labels to numeric
df_feat$regime_num <- case_when(
  df_feat$regime_raw == "Calm" ~ 1,
  df_feat$regime_raw == "Neutral" ~ 2,
  df_feat$regime_raw == "Turbulent" ~ 3
)

# Majority vote helper
roll_majority <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
}

df_feat$regime_smooth_num <- rollapply(
  df_feat$regime_num,
  width = 14,      # <-- can try 21 too
  FUN = roll_majority,
  fill = NA,
  align = "right"
)

df_feat$regime_smooth <- case_when(
  df_feat$regime_smooth_num == 1 ~ "Calm",
  df_feat$regime_smooth_num == 2 ~ "Neutral",
  df_feat$regime_smooth_num == 3 ~ "Turbulent"
)

df_feat_clean <- df_feat %>% drop_na(regime_smooth)



ggplot(df_feat_clean, aes(Date, r_t)) +
  geom_point(aes(color = regime_smooth), size = 0.6) +
  scale_color_manual(values = c(
    "Calm" = "blue",
    "Neutral" = "orange",
    "Turbulent" = "red"
  )) +
  labs(
    title = "Market Regimes via K-Means (Rolling Features + 14-Day Smoothed)",
    y = "SPY Daily Return",
    color = "Regime"
  ) +
  theme_minimal()
