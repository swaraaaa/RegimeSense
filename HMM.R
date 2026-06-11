# ---- Step 4: Hidden Markov Model (HMM) ----
suppressPackageStartupMessages({
  library(depmixS4)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
})
# 0) Load the K-means output
csv_path <- "/Users/watashideker/Library/Mobile Documents/com~apple~CloudDocs/Stevens/Fall 2025/FA 800 Project in Financial Analytics/Code/RegimeSense_KMeans_Output.csv"
df <- read.csv(csv_path)
df$Date <- as.Date(df$Date)

# (optional) keep only the columns we need
df <- df[, c("Date", "r_t", "VIX", "regime3")]

# 1) Train/Test split (fit on 2005-2019; validate 2020+)
train_idx <- df$Date < as.Date("2020-01-01")
df_tr  <- df[train_idx, ]
df_te  <- df[!train_idx, ]

# 2) Build a 3-state Gaussian HMM with two emissions: r_t and VIX
# --- Fit on the full dataset instead of splitting ---
set.seed(123)
mod_full <- depmix(
  response = list(
    r_t ~ 1,
    VIX ~ 1
  ),
  data = df,
  nstates = 3,
  family = list(gaussian(), gaussian())
)
fit_mod <- fit(mod_full, verbose = FALSE, emcontrol = em.control(maxit = 1000))

# posterior on the same data
post <- posterior(fit_mod)

# post contains: state (MAP), S1..S3 probabilities, logLik, etc.
df$state_hmm <- post$state
df$S1 <- post$S1; df$S2 <- post$S2; df$S3 <- post$S3

# 5) Give states economic names by ordering them using the average VIX (low→high)
state_profile <- df %>%
  group_by(state_hmm) %>%
  summarise(mean_VIX = mean(VIX, na.rm = TRUE),
            mean_ret = mean(r_t,  na.rm = TRUE),
            .groups = "drop") %>%
  arrange(mean_VIX)

# Map: lowest VIX -> Calm, middle -> Neutral, highest -> Turbulent
lab_map <- setNames(c("Calm","Neutral","Turbulent"), state_profile$state_hmm)
df$regime_hmm <- lab_map[as.character(df$state_hmm)]

# 6) Empirical transition matrix from the inferred state sequence
#    (simple, robust way to present transitions)
st <- df$state_hmm
trans_counts <- table(st[-length(st)], st[-1])
trans_mat <- prop.table(trans_counts, 1)  # row-normalized

# Expected duration (persistence) per state: 1 / (1 - p_ii)
diag_p <- diag(trans_mat)
exp_dur <- 1 / pmax(1 - diag_p, 1e-8)
trans_mat
exp_dur

# 7) Agreement with K-means (how close HMM is to your k=3 labels)
agree <- mean(df$regime_hmm == df$regime3, na.rm = TRUE)
conf_tab <- table(HMM = df$regime_hmm, KMeans = df$regime3)

agree
conf_tab

# 8) Plot: HMM regimes over time
reg_cols <- c(Calm="#2c7be5", Neutral="#f0ad4e", Turbulent="#d9534f")
ggplot(df, aes(Date, r_t)) +
  geom_line(color="grey70") +
  geom_point(aes(color = regime_hmm), size = 0.6) +
  scale_color_manual(values = reg_cols) +
  labs(title = "Hidden Markov Model Regimes (3 states)",
       subtitle = "Emissions: SPY daily returns (r_t) and VIX level; states ordered by mean VIX",
       y = "SPY Daily Return", color = "HMM Regime") +
  theme_minimal()

# 9) (Optional) Plot smoothed probability of the Turbulent state
df$P_Turb <- df$S1; df$P_Neut <- df$S2; df$P_Calm <- df$S3  # we'll remap to match lab_map
# Fix S1/S2/S3 ordering to match labels (by the same mean-VIX ordering)
ord_states <- state_profile$state_hmm
prob_cols <- paste0("S", match(1:3, 1:3))  # S1,S2,S3 exist; we need to realign:
# Create a vector of column names S{state_id}
df$P_Calm      <- df[[paste0("S", ord_states[1])]]
df$P_Neutral   <- df[[paste0("S", ord_states[2])]]
df$P_Turbulent <- df[[paste0("S", ord_states[3])]]

ggplot(df, aes(Date, P_Turbulent)) +
  geom_line() +
  labs(title = "Smoothed Probability: Turbulent State (HMM)", y = "Probability") +
  theme_minimal()
