library(dplyr)
library(readr)
library(nlme)
library(ggplot2)

## Duomenų paruošimas
ess_raw <- read.csv("Duomenys/ESS10.csv", stringsAsFactors = FALSE)
gdp_raw <- read.csv("Duomenys/GDP.csv", stringsAsFactors = FALSE)

gdp_clean <- gdp_raw %>%
  select(cntry = geo, gdp_pc = OBS_VALUE) %>%
  mutate(
    gdp_pc = as.numeric(gdp_pc),
    gdp_pc_k = gdp_pc / 1000, # BVP tūkstančiais EUR
    cntry = ifelse(cntry == "EL", "GR", cntry)
  ) %>%
  filter(!is.na(cntry) & !is.na(gdp_pc))

ess_selected <- ess_raw %>%
  select(cntry, stfgov, hinctnta, mbtru, agea) %>%
  mutate(
    stfgov   = as.numeric(stfgov),
    hinctnta = as.numeric(hinctnta),
    mbtru    = as.numeric(mbtru),
    agea     = as.numeric(agea)
  ) %>%
  filter(
    !is.na(cntry) &
      !is.na(stfgov)   & stfgov <= 10 &
      !is.na(hinctnta) & hinctnta <= 10 &
      !is.na(mbtru)    & mbtru <= 3 &
      !is.na(agea)     & agea <= 110
  ) %>%
  mutate(union_member = as.numeric(mbtru == 1))  # 1 = current member, 0 = otherwise

df_final <- ess_selected %>%
  inner_join(gdp_clean, by = "cntry")

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


