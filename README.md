# 🚗 Do Mandatory Seat Belt Laws Save Lives? | An Econometric Evaluation

This repository presents a robust applied econometric analysis of the impact of mandatory seat belt laws on traffic fatalities in the U.S., leveraging a panel dataset spanning 1983–1997 across 48 states.

We apply a Two-Stage Least Squares (2SLS) methodology using staggered policy adoptions as instruments to tackle endogeneity in seatbelt usage data. Our analysis compares states under no law, secondary enforcement, and primary enforcement to estimate causal effects on highway fatality rates.

---

## 📌 Motivation

Motor vehicle crashes are a leading cause of preventable deaths. Observational studies suggest that seatbelt usage improves crash survival rates, but estimating population-level impacts is empirically difficult due to risk selection and reverse causality. We overcome this using an econometric identification strategy rooted in policy shifts.

---

## 🧠 Key Questions

- Do secondary and primary seatbelt enforcement laws significantly reduce traffic fatalities?
- How strong is the relationship between policy enforcement type and observed seatbelt use?
- What are the econometric challenges in causal inference and how can they be addressed?

---

## 📊 Dataset

- Source: `USSeatBelts` panel dataset (1983–1997), available via the AER R package
- Unit: State-year level observations
- Key Variables:
  - `fatalities`: deaths per 100 million vehicle-miles
  - `seatbelt`: proportion of seatbelt usage
  - `enforce`: enforcement type (none, secondary, primary)
  - `income`, `alcohol`, `speed65`, `drinkage`: policy/environment controls

---

## ⚙️ Methodology

We use a Two-Stage Least Squares (2SLS) estimation:

**First Stage:**
``log(seatbeltᵢₜ) = α + π₁ Secondaryᵢₜ + π₂ Primaryᵢₜ + γXᵢₜ + δᵢ + λₜ + εᵢₜ``  
Instruments: enforcement law types

**Second Stage:**
``log(fatalitiesᵢₜ) = β * log(seatbelt̂ᵢₜ) + θXᵢₜ + δᵢ + λₜ + ηᵢₜ``  
Controls: income, alcohol law, speed limits, drinking age; state & year fixed effects

---

## 📈 Results Summary

- **Secondary Enforcement:**  
  - 2SLS Estimate: –0.099 (statistically significant at 10%)  
  - A 10% increase in seatbelt use lowers fatalities by ~1%
- **Primary Enforcement:**  
  - 2SLS Estimate: –0.154 (not statistically significant due to small N)
  - Effect directionally supports stronger enforcement

---

## 🧾 Final Output Variables

| Variable Name      | Description |
|--------------------|-------------|
| state              | U.S. State |
| year               | Year of observation |
| fatalities         | Deaths per 100M vehicle miles |
| seatbelt           | Proportion wearing seatbelts |
| enforce            | Type of law: None, Secondary, or Primary |
| income             | Real per-capita income |
| alcohol            | Open container law indicator |
| speed65            | 65mph speed limit indicator |
| drinkage           | Legal drinking age ≥ 21 |

---
## 📚 References

1. **Fatality Analysis Reporting System (FARS) Database**  
   U.S. Department of Transportation, National Highway Traffic Safety Administration (NHTSA)  
   🔗 <https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars>

2. **Cohen, Alma & Einav, Liran** (2003)  
   *The Effects of Mandatory Seat Belt Laws on Driving Behavior and Traffic Fatalities*  
   _The Review of Economics and Statistics_, **85** (4), 828–843  
   🔗 <https://doi.org/10.1162/003465303772815786>

3. **Stock, James H. & Watson, Mark W.** (2007)  
   *Introduction to Econometrics*  
   2nd Edition, Pearson Education • ISBN 9780321278876

---

## 🧪 Requirements

To reproduce the results and figures, install the following packages:

```txt
AER
plm
tidyverse
ggplot2
dplyr
readr
stargazer
fixest
lmtest
sandwich






