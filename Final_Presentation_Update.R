
suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(depmixS4)
  library(ggplot2)
  library(TTR)
})


start_date <- "2005-01-01"
end_date   <- Sys.Date()

symbols <- c("SPY", "^VIX", "^TNX", "^IRX", "IEF", "GLD")
getSymbols(symbols, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

spy_df <- cbind(Date = as.Date(index(SPY)), data.frame(coredata(SPY)))
vix_df <- cbind(Date = as.Date(index(VIX)), data.frame(coredata(VIX)))
tnx_df <- cbind(Date = as.Date(index(TNX)), data.frame(coredata(TNX)))
irx_df <- cbind(Date = as.Date(index(IRX)), data.frame(coredata(IRX)))
ief_df <- cbind(Date = as.Date(index(IEF)), data.frame(coredata(IEF)))
gld_df <- cbind(Date = as.Date(index(GLD)), data.frame(coredata(GLD)))

df <- spy_df %>% 
  dplyr::select(Date, SPY = SPY.Adjusted) %>%
  left_join(vix_df %>% dplyr::select(Date, VIX = VIX.Close), by = "Date") %>%
  left_join(tnx_df %>% dplyr::select(Date, TNX = TNX.Close), by = "Date") %>%
  left_join(irx_df %>% dplyr::select(Date, IRX = IRX.Close), by = "Date") %>%
  left_join(ief_df %>% dplyr::select(Date, IEF = IEF.Adjusted), by = "Date") %>%
  left_join(gld_df %>% dplyr::select(Date, GLD = GLD.Adjusted), by = "Date")

df <- df[complete.cases(df), ]

df$r_t      <- c(NA, diff(log(df$SPY)))
df$ief_ret  <- c(NA, diff(log(df$IEF)))
df$gld_ret  <- c(NA, diff(log(df$GLD)))

df <- df[complete.cases(df), ]

df$vol21  <- runSD(df$r_t, n = 21)
df$vix21  <- SMA(df$VIX, n = 21)
df$dVIX   <- c(NA, diff(df$VIX))
df$corr21 <- runCor(df$r_t, df$dVIX, n = 21)

df$term_spread <- (df$TNX / 10) - (df$IRX / 100)

df <- df[complete.cases(df), ]


df_hmm <- df %>% 
  dplyr::select(Date, r_t, vol21, vix21, corr21, term_spread,
                ief_ret, gld_ret)

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

df_hmm$spy_ret <- exp(df_hmm$r_t) - 1
df_hmm$ief_ret <- exp(df_hmm$ief_ret) - 1
df_hmm$gld_ret <- exp(df_hmm$gld_ret) - 1

# Calm       → 100% SPY
# Neutral    → 50% SPY / 30% IEF / 20% GLD
# Turbulent  → 60% IEF / 40% GLD

df_hmm$strategy_ret <- dplyr::case_when(
  df_hmm$regime_hmm == "Calm" ~ 
    df_hmm$spy_ret,
  
  df_hmm$regime_hmm == "Neutral" ~ 
    0.5 * df_hmm$spy_ret + 
    0.3 * df_hmm$ief_ret + 
    0.2 * df_hmm$gld_ret,
  
  df_hmm$regime_hmm == "Turbulent" ~ 
    0.6 * df_hmm$ief_ret + 
    0.4 * df_hmm$gld_ret
)

df_hmm$cum_spy      <- cumprod(1 + df_hmm$spy_ret)
df_hmm$cum_strategy <- cumprod(1 + df_hmm$strategy_ret)

ggplot(df_hmm, aes(x = Date)) +
  geom_line(aes(y = cum_spy, color = "SPY Buy & Hold"), size = 1) +
  geom_line(aes(y = cum_strategy, color = "Regime Strategy (SPY+IEF+GLD)"), size = 1) +
  scale_color_manual(values = c(
    "SPY Buy & Hold" = "#2c7be5",
    "Regime Strategy (SPY+IEF+GLD)" = "#d9534f"
  )) +
  labs(
    title = "Cumulative Returns: SPY vs Regime-Aware Strategy",
    subtitle = "Calm → SPY | Neutral → SPY + IEF + GLD | Turbulent → IEF + GLD",
    x = "Date", y = "Cumulative Return", color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


compute_cum_from <- function(data, start_date) {
  
  data %>% 
    filter(Date >= as.Date(start_date)) %>%
    mutate(
      cum_spy = cumprod(1 + spy_ret),
      cum_strategy = cumprod(1 + strategy_ret)
    )
}


start_pre_crisis  <- "2005-01-01"   # Pre-crisis / full sample
start_crisis      <- "2008-01-01"   # Financial crisis
start_post_crisis <- "2010-01-01"   # Post-crisis recovery

df_2005 <- compute_cum_from(df_hmm, start_pre_crisis)
df_2008 <- compute_cum_from(df_hmm, start_crisis)
df_2010 <- compute_cum_from(df_hmm, start_post_crisis)

ggplot(df_2005, aes(x = Date)) +
  geom_line(aes(y = cum_spy, color = "SPY Buy & Hold"), size = 1) +
  geom_line(aes(y = cum_strategy, color = "Regime Strategy (SPY+IEF+GLD)"), size = 1) +
  labs(
    title = "Cumulative Returns Starting in 2005",
    subtitle = "Full sample including pre- and post-crisis periods",
    x = "Date", y = "Cumulative Return", color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(df_2008, aes(x = Date)) +
  geom_line(aes(y = cum_spy, color = "SPY Buy & Hold"), size = 1) +
  geom_line(aes(y = cum_strategy, color = "Regime Strategy (SPY+IEF+GLD)"), size = 1) +
  labs(
    title = "Cumulative Returns Starting in 2008",
    subtitle = "Stress test during the Global Financial Crisis",
    x = "Date", y = "Cumulative Return", color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(df_2010, aes(x = Date)) +
  geom_line(aes(y = cum_spy, color = "SPY Buy & Hold"), size = 1) +
  geom_line(aes(y = cum_strategy, color = "Regime Strategy (SPY+IEF+GLD)"), size = 1) +
  labs(
    title = "Cumulative Returns Starting in 2010",
    subtitle = "Post-crisis recovery and expansion phase",
    x = "Date", y = "Cumulative Return", color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")






