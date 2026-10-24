library(dplyr)
library(readr)
library(nlme)
library(ggplot2)

# install.packages("eurostat")
library(eurostat)

## Duomenų paruošimas
ess_raw <- read.csv("Duomenys/ESS10.csv", stringsAsFactors = FALSE)
gdp_raw <- read.csv("Duomenys/GDP.csv", stringsAsFactors = FALSE)

countries <- c(
  "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR",
  "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT",
  "PL", "PT", "RO", "SI", "SK", "FI", "SE", "IS", "NO", "CH"
)

tax_gdp <- get_eurostat(
  id = "gov_10a_taxag",
  filters = list(
    freq = "A",
    unit = "PC_GDP",
    sector = "S13",
    na_item = "D2_D5_D91_D61_M_D995",
    geo = countries,
    TIME_PERIOD = "2021"
  ),
  time_format = "raw",
  type = "code"
)

# --- Mokesčiai (% BVP) ---
tax_clean <- tax_gdp %>%
  select(cntry = geo, tax_pct_gdp = values) %>%   # in older eurostat versions the column is `values`; newer: `values` too
  mutate(
    tax_pct_gdp = as.numeric(tax_pct_gdp),
    cntry = ifelse(cntry == "EL", "GR", cntry)    # same recode as GDP, to match ESS
  ) %>%
  filter(!is.na(cntry) & !is.na(tax_pct_gdp)) %>%
  distinct(cntry, .keep_all = TRUE)

# --- BVP ---
gdp_clean <- gdp_raw %>%
  select(cntry = geo, gdp_pc = OBS_VALUE) %>%
  mutate(
    gdp_pc = as.numeric(gdp_pc),
    gdp_pc_k = gdp_pc / 1000, # BVP tūkstančiais EUR
    cntry = ifelse(cntry == "EL", "GR", cntry)
  ) %>%
  filter(!is.na(cntry) & !is.na(gdp_pc)) %>%
  distinct(cntry, .keep_all = TRUE)

# --- ESS ---
ess_selected <- ess_raw %>%
  select(cntry, stfgov, hinctnta, mbtru, agea, polintr) %>%
  mutate(
    stfgov   = as.numeric(stfgov),
    hinctnta = as.numeric(hinctnta),
    mbtru    = as.numeric(mbtru),
    agea     = as.numeric(agea),
    polintr  = as.numeric(polintr)
  ) %>%
  filter(
    !is.na(cntry) &
      !is.na(stfgov)   & stfgov <= 10 &
      !is.na(hinctnta) & hinctnta <= 10 &
      !is.na(mbtru)    & mbtru <= 3 &
      !is.na(agea)     & agea <= 110 &
      !is.na(polintr)  & polintr <= 4      # drops 7, 8, 9 (refusal / don't know / no answer)
  ) %>%
  mutate(
    union_member = as.numeric(mbtru == 1),
    pol_interst  = factor(
      polintr,
      levels = 1:4,
      labels = c("Very interested", "Quite interested",
                 "Hardly interested", "Not at all interested")
    )
  )


# --- Sujungimas: ESS + BVP + mokesčiai ---
df_final <- ess_selected %>%
  inner_join(gdp_clean, by = "cntry") %>%
  inner_join(tax_clean, by = "cntry")

# Patikrinimas
unique(df_final$cntry)
setdiff(unique(ess_selected$cntry), unique(df_final$cntry))  # ESS countries dropped by the joins
summary(df_final)

cat("Eilučių (respondentų) skaičius:", nrow(df_final), "\n")
cat("Šalių (II lygio aibių) skaičius:", length(unique(df_final$cntry)), "\n\n")

print(head(df_final))

## Random intercept model (GDP explains only beta_0)
m1 <- lme(stfgov ~ gdp_pc_k + hinctnta + union_member + agea,
          random = ~ 1 | cntry,
          data = df_final, method = "REML")

summary(m1)
AIC(m1)

## Full model (GDP explains beta_0, beta_1, beta_2, beta_3)
ctrl <- lmeControl(maxIter = 200, msMaxIter = 200, opt = "optim")

m2 <- lme(stfgov ~ (hinctnta + union_member + agea) * gdp_pc_k,
          random = ~ hinctnta + union_member + agea | cntry,
          data = df_final, control = ctrl)

summary(m2)
AIC(m2)


qqnorm(resid(m3))

m3 <- lme(stfgov ~ (hinctnta + agea) * gdp_pc_k,
          random = ~ hinctnta + agea | cntry,
          data = df_final, control = ctrl)
AIC(m3)


