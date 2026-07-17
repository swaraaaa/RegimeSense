# RegimeSense — Detecting Financial Market Regimes

**Market Regime Detection · Hidden Markov Model · K-Means · Portfolio Optimization · R**

> Final Project — MS Financial Engineering, Stevens Institute of Technology (Dec 2025)

> Advisor: Prof. Ionut Florescu | Authors: Swara Dave, Swapnil Pant

---

## 📌 Overview

Financial markets transition between periods of stability and stress — yet most traditional portfolios use static allocation rules that ignore these shifts. **RegimeSense** tackles this problem by building a data-driven regime detection framework that:

1. Identifies distinct market environments (Calm, Neutral, Turbulent) from 20 years of market data
2. Models regime persistence and transitions using a Hidden Markov Model (HMM)
3. Dynamically adjusts portfolio allocation based on the inferred regime
4. Evaluates performance against SPY buy-and-hold and a 60/40 benchmark

---

## 📊 Key Results

| Strategy | Annual Return | Annual Volatility | Max Drawdown |
|---|---|---|---|
| SPY Buy & Hold | 12.1% | 19.1% | -55.2% |
| 60/40 Portfolio | 8.5% | 11.0% | -31.6% |
| **RegimeSense** | **8.6%** | **12.1%** | **-34.6%** |

**RegimeSense outperforms SPY on drawdown by over 20 percentage points**, delivering comparable returns to the 60/40 benchmark while offering meaningfully better downside protection.

---

## 🗂️ Data Sources

Daily market data from 2005–2025:

- **SPY** — S&P 500 equity proxy
- **VIX** — CBOE Volatility Index (market uncertainty)
- **TNX** — 10-year U.S. Treasury yield
- **IRX** — 3-month Treasury bill rate
- **IEF** — Intermediate-term U.S. Treasuries (portfolio allocation)
- **GLD** — Gold ETF (portfolio allocation)

---

## ⚙️ Methodology

### Feature Engineering
Five features constructed from raw market data:
- SPY daily log returns
- 21-day realized volatility of SPY
- 21-day rolling VIX average
- Rolling SPY–VIX correlation (risk-off signal)
- Term spread: TNX − IRX (yield curve / recession signal)

### Regime Detection
- **K-Means clustering** used first to identify candidate regime groupings (2-state and 3-state configurations explored)
- **Hidden Markov Model (HMM)** applied to model regime persistence and probabilistic transitions over time
- Final output: 3 regimes — Calm, Neutral, Turbulent — with smooth probabilistic assignments

### Portfolio Allocation Rules

| Market Regime | Economic Interpretation | Portfolio Allocation |
|---|---|---|
| Calm | Low volatility, stable growth | 100% SPY |
| Neutral | Transition, rising uncertainty | 50% SPY / 30% IEF / 20% GLD |
| Turbulent | High volatility, crisis-like | 60% IEF / 40% GLD |

Rebalancing is triggered automatically on regime transitions — no discretionary timing or return forecasting required.

---

## 📁 Repository Structure

```
RegimeSense/
├── Data/
│   ├── spy.xlsx
│   ├── vix.xlsx
│   ├── 10yr.xlsx
│   ├── 3months.xlsx
│   └── Fed.xlsx
├── Code/
│   ├── HMM.R                        # HMM model fitting
│   ├── HMM_PhaseII.R                # Phase II HMM refinement
│   ├── kmeansCleanBetter.R          # K-Means clustering
│   ├── k-means_featureengineering.R # Feature construction
│   ├── 21dayRegimeSegmentation.R    # Rolling regime segmentation
│   └── Final_Presentation_Update.R  # Portfolio backtesting & evaluation
└── PLots/                           # Output visualizations
```

---

## 🚀 How to Run

1. Clone the repo and open `Code/Code.Rproj` in RStudio
2. Install required packages:
```r
install.packages(c("depmixS4", "ggplot2", "dplyr", "xts", "PerformanceAnalytics", "readxl"))
```
3. Run scripts in this order:
   - `k-means_featureengineering.R` — build features
   - `kmeansCleanBetter.R` — initial regime clustering
   - `HMM.R` → `HMM_PhaseII.R` — fit HMM and refine
   - `Final_Presentation_Update.R` — backtest and evaluate portfolio

---

## 📜 References

- Krsteva, I. (2014). Estimation and Optimization of Multi-Factor Models with Regime Switching. Stevens Institute of Technology.
- Guidolin, M., & Timmermann, A. (2007). Asset allocation under multivariate regime switching. *Journal of Economic Dynamics and Control.*
- Baitinger, T., & Hoch, J. (2024). Simplicity vs. complexity: HMM and HSMM for regime-based asset allocation. *Quantitative Finance.*

---

## 👤 Author

**Swara Dave** — MS Financial Engineering, Stevens Institute of Technology
[![LinkedIn](https://img.shields.io/badge/LinkedIn-swara--dave-blue?style=flat&logo=linkedin)](https://linkedin.com/in/swara-dave) [![GitHub](https://img.shields.io/badge/GitHub-swaraaaa-black?style=flat&logo=github)](https://github.com/swaraaaa)
