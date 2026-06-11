

suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(depmixS4)
  library(ggplot2)
  library(TTR)
})


start_date <- "2005-01-01"
end_date   <- Sys.Date()

# SPY, VIX, 10Y Treasury, 3M Treasury
getSymbols(c("SPY", "^VIX", "^TNX", "^IRX"),
           src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Convert to data frame
spy_df <- data.frame(Date = index(SPY), coredata(SPY))
vix_df <- data.frame(Date = index(VIX), coredata(VIX))
tnx_df <- data.frame(Date = index(TNX), coredata(TNX))
irx_df <- data.frame(Date = index(IRX), coredata(IRX))

df <- spy_df %>%
  select(Date, SPY.Adjusted) %>%
  rename(SPY = SPY.Adjusted) %>%
  left_join(vix_df %>% select(Date, VIX.Close) %>% rename(VIX = VIX.Close), by="Date") %>%
  left_join(tnx_df %>% select(Date, TNX.Close) %>% rename(TNX = TNX.Close), by="Date") %>%
  left_join(irx_df %>% select(Date, IRX.Close) %>% rename(IRX = IRX.Close), by="Date")

# Remove NA
df <- df[complete.cases(df), ]


# Daily return
df$r_t <- c(NA, diff(log(df$SPY)))
df <- df[complete.cases(df), ]

# 21-day rolling volatility
df$vol21 <- runSD(df$r_t, n = 21)

# 21-day rolling average VIX
df$vix21 <- SMA(df$VIX, n = 21)

# ΔVIX
df$dVIX <- c(NA, diff(df$VIX))

# 21-day rolling correlation SPY ↔ dVIX
df$corr21 <- runCor(df$r_t, df$dVIX, n = 21)

# Term spread (10Y – 3M)  
# Yahoo format: TNX = 10-year yield * 10  
#               IRX = 3-month yield * 100  
df$term_spread <- (df$TNX / 10) - (df$IRX / 100)

# Clean NAs created by rolling windows
df <- df[complete.cases(df), ]


df_hmm <- df %>%
  select(Date, r_t, vol21, vix21, corr21, term_spread)



set.seed(123)

mod_full <- depmix(
  response = list(
    r_t ~ 1,
    vol21 ~ 1,
    vix21 ~ 1,
    corr21 ~ 1,
    term_spread ~ 1
  ),
  data    = df_hmm,
  nstates = 3,
  family  = list(
    gaussian(), gaussian(), gaussian(), gaussian(), gaussian()
  )
)

fit_mod <- fit(mod_full, verbose = FALSE, emcontrol = em.control(maxit = 1000))
post <- posterior(fit_mod)
df_hmm$state <- post$state



state_profile <- df_hmm %>%
  group_by(state) %>%
  summarise(mean_vix = mean(vix21, na.rm = TRUE)) %>%
  arrange(mean_vix)

lab_map <- setNames(c("Calm","Neutral","Turbulent"), state_profile$state)
df_hmm$regime_hmm <- lab_map[df_hmm$state]



st <- df_hmm$state
trans_counts <- table(st[-length(st)], st[-1])
trans_mat <- prop.table(trans_counts, 1)
diag_p <- diag(trans_mat)
expected_duration <- 1 / (1 - diag_p)

print("Transition Matrix:")
print(trans_mat)
print("Expected Durations:")
print(expected_duration)



reg_cols <- c(Calm="#2c7be5", Neutral="#f0ad4e", Turbulent="#d9534f")

ggplot(df_hmm, aes(Date, r_t)) +
  geom_line(color="grey70") +
  geom_point(aes(color = regime_hmm), size = 0.6) +
  scale_color_manual(values = reg_cols) +
  labs(
    title = "HMM Regimes (3 States) — Rolling Features + Macro",
    subtitle = "Emissions: r_t, vol21, vix21, corr21, term_spread",
    x = "Date",
    y = "SPY Daily Return",
    color = "Regime"
  ) +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Make sure df_hmm$regime_hmm is a factor in correct order
df_hmm$regime_hmm <- factor(df_hmm$regime_hmm,
                            levels = c("Calm", "Neutral", "Turbulent"))

reg_cols <- c(Calm="#2c7be5", Neutral="#f0ad4e", Turbulent="#d9534f")

ggplot(df_hmm, aes(x = Date, y = 1, fill = regime_hmm)) +
  geom_tile(height = 0.8) +
  scale_fill_manual(values = reg_cols) +
  labs(
    title = "HMM Regime State Transitions Over Time",
    subtitle = "Calm → Neutral → Turbulent (as detected by HMM)",
    x = "Date",
    y = "",
    fill = "Regime"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )


df_0809 <- df_hmm %>% 
  filter(Date >= "2007-01-01", Date <= "2009-12-31")

ggplot(df_0809, aes(x = Date, y = 1, fill = regime_hmm)) +
  geom_tile() +
  scale_fill_manual(values = reg_cols) +
  labs(
    title = "HMM Regime Transitions — 2007 to 2009 (GFC)",
    x = "Date",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())




df_2020 <- df_hmm %>% 
  filter(Date >= "2020-01-01", Date <= "2020-12-31")

ggplot(df_2020, aes(x = Date, y = 1, fill = regime_hmm)) +
  geom_tile() +
  scale_fill_manual(values = reg_cols) +
  labs(
    title = "HMM Regime Transitions — 2020 (COVID Crash)",
    x = "Date",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

df_2123 <- df_hmm %>% 
  filter(Date >= "2021-01-01", Date <= "2023-12-31")

ggplot(df_2123, aes(x = Date, y = 1, fill = regime_hmm)) +
  geom_tile() +
  scale_fill_manual(values = reg_cols) +
  labs(
    title = "HMM Regime Transitions — 2021 to 2023 (Fed Tightening)",
    x = "Date",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())


df_0507 <- df_hmm %>% 
  filter(Date >= "2005-01-01", Date <= "2007-12-31")

ggplot(df_0507, aes(x = Date, y = 1, fill = regime_hmm)) +
  geom_tile() +
  scale_fill_manual(values = reg_cols) +
  labs(
    title = "HMM Regime Transitions — 2005 to 2007 (Pre-GFC Calm)",
    x = "Date",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())


