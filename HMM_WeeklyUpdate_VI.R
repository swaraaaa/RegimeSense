

suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(depmixS4)
  library(ggplot2)
  library(TTR)
})

start_date <- "2005-01-01"
end_date   <- Sys.Date()

symbols <- c("SPY", "^VIX", "^TNX", "^IRX")
getSymbols(symbols, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

spy_df <- cbind(Date = as.Date(index(SPY)), data.frame(coredata(SPY)))
vix_df <- cbind(Date = as.Date(index(VIX)), data.frame(coredata(VIX)))
tnx_df <- cbind(Date = as.Date(index(TNX)), data.frame(coredata(TNX)))
irx_df <- cbind(Date = as.Date(index(IRX)), data.frame(coredata(IRX)))

df <- spy_df %>% 
  dplyr::select(Date, SPY = SPY.Adjusted) %>%
  left_join(vix_df %>% dplyr::select(Date, VIX = VIX.Close), by = "Date") %>%
  left_join(tnx_df %>% dplyr::select(Date, TNX = TNX.Close), by = "Date") %>%
  left_join(irx_df %>% dplyr::select(Date, IRX = IRX.Close), by = "Date")


df <- df[complete.cases(df), ]

df$r_t <- c(NA, diff(log(df$SPY)))
df <- df[complete.cases(df), ]

df$vol21  <- runSD(df$r_t, n = 21)
df$vix21  <- SMA(df$VIX, n = 21)
df$dVIX   <- c(NA, diff(df$VIX))
df$corr21 <- runCor(df$r_t, df$dVIX, n = 21)

# Term spread adjustment
df$term_spread <- (df$TNX / 10) - (df$IRX / 100)

df <- df[complete.cases(df), ]

df_hmm <- df %>% 
  dplyr::select(Date, r_t, vol21, vix21, corr21, term_spread)

set.seed(123)

mod <- depmix(
  response = list(
    r_t ~ 1,
    vol21 ~ 1,
    vix21 ~ 1,
    corr21 ~ 1,
    term_spread ~ 1
  ),
  data = df_hmm,
  nstates = 3,
  family = list(
    gaussian(), gaussian(), gaussian(), gaussian(), gaussian()
  )
)

fit_mod <- fit(mod, verbose = FALSE, emcontrol = em.control(maxit = 1000))
post <- posterior(fit_mod)
df_hmm$state <- post$state

regime_order <- df_hmm %>%
  group_by(state) %>%
  summarise(mean_vix = mean(vix21)) %>%
  arrange(mean_vix) %>%
  pull(state)

labels <- c("Calm", "Neutral", "Turbulent")
label_map <- setNames(labels, regime_order)

df_hmm$regime_hmm <- factor(label_map[df_hmm$state],
                            levels = c("Calm","Neutral","Turbulent"))

# Convert log-return to simple return
df_hmm$spy_ret <- exp(df_hmm$r_t) - 1

# Basic regime strategy:
# Calm   → Fully in SPY
# Neutral → 0.5 SPY
# Turbulent → Cash
df_hmm$strategy_ret <- ifelse(
  df_hmm$regime_hmm == "Calm", df_hmm$spy_ret,
  ifelse(df_hmm$regime_hmm == "Neutral", 0.5 * df_hmm$spy_ret, 0)
)

# Cumulative returns
df_hmm$cum_spy <- cumprod(1 + df_hmm$spy_ret)
df_hmm$cum_strategy <- cumprod(1 + df_hmm$strategy_ret)

ggplot(df_hmm, aes(x = Date)) +
  geom_line(aes(y = cum_spy, color = "SPY Buy & Hold"), size = 1) +
  geom_line(aes(y = cum_strategy, color = "Regime-Aware Strategy"), size = 1) +
  scale_color_manual(values = c(
    "SPY Buy & Hold" = "#2c7be5",
    "Regime-Aware Strategy" = "#d9534f"
  )) +
  labs(
    title = "Cumulative Return Comparison: SPY vs Regime-Aware Strategy",
    subtitle = "Rules: Calm → SPY, Neutral → 0.5 SPY, Turbulent → Cash",
    x = "Date",
    y = "Cumulative Return",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
