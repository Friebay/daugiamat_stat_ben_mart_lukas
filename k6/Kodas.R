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
  mutate(
    union_member = ifelse(mbtru == 1, 1, 0)
  )


df_final <- ess_selected %>%
  inner_join(gdp_clean, by = "cntry")

cat("Eilučių (respondentų) skaičius:", nrow(df_final), "\n")
cat("Šalių (II lygio aibių) skaičius:", length(unique(df_final$cntry)), "\n\n")

print(head(df_final))


## Aprašomoji statistika --------------
vars_summary <- df_final %>%
  select(stfgov, hinctnta, union_member, agea, gdp_pc_k) %>%
  summary()
print(vars_summary)

## Grafikas:
country_names <- c(
  "BG" = "Bulgarija", "HR" = "Kroatija", "MK" = "Š. Makedonija", "SK" = "Slovakija",
  "SI" = "Slovėnija", "ME" = "Juodkalnija", "GR" = "Graikija", "LT" = "Lietuva",
  "IT" = "Italija", "CZ" = "Čekija", "EE" = "Estija", "FR" = "Prancūzija",
  "BE" = "Belgija", "PT" = "Portugalija", "HU" = "Vengrija", "NL" = "Nyderlandai",
  "IE" = "Airija", "IS" = "Islandija", "NO" = "Norvegija", "FI" = "Suomija",
  "CH" = "Šveicarija"
)
overall_mean <- mean(df_final$stfgov)
plot_data <- df_final %>%
  group_by(cntry) %>%
  summarise(mean_stfgov = mean(stfgov), .groups = "drop") %>%
  mutate(
    country_label = country_names[cntry],
    status = ifelse(mean_stfgov >= overall_mean, "Aukštesnis nei vidurkis", "Žemesnis nei vidurkis")
  )

p_clean <- ggplot(plot_data, aes(x = reorder(country_label, mean_stfgov), y = mean_stfgov, fill = status)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_stfgov)), 
            hjust = -0.15, size = 3.6, fontface = "bold", color = "gray15") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 8.2), breaks = seq(0, 8, by = 2), expand = c(0, 0)) +
  scale_fill_manual(values = c("Aukštesnis nei vidurkis" = "#2b7bba", 
                               "Žemesnis nei vidurkis" = "#d95f02")) +
  labs(
    title = "Pasitenkinimas nacionaline vyriausybe (0–10 balų)",
    subtitle = paste0("Punktyrinė linija žymi bendrą imties vidurkį (", round(overall_mean, 2), ")"),
    x = NULL,
    y = "Vidutinis balas",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30")
  )
p_clean
ggsave("stfgov_presentation.png", plot = p_clean, width = 6.2, height = 6.0, dpi = 300)

## Pirmo lygmens koreliaciju matrica
cor_vars <- df_final %>% select(hinctnta, union_member, agea)
cor_mat <- cor(cor_vars, use = "complete.obs")
print(round(cor_mat, 3))

# P-reikšmės koreliacijoms patikrinti
cat("\nKoreliacijos testas tarp hinctnta ir union_member:\n")
print(cor.test(df_final$hinctnta, df_final$union_member))



## Besąlyginis modelis
m0 <- lme(
  fixed  = stfgov ~ 1,
  random = ~ 1 | cntry,
  data   = df_final,
  method = "ML"
)

summary(m0)

## Dispersijos ir ICC
var_comp_m0 <- VarCorr(m0)
tau00_sq    <- as.numeric(var_comp_m0["(Intercept)", "Variance"])
sigma_sq_m0 <- as.numeric(var_comp_m0["Residual", "Variance"])
icc_val <- tau00_sq / (tau00_sq + sigma_sq_m0)

cat("\n================ DISPERSIJOS IR ICC ================\n")
cat(sprintf("Šalių lygmens dispersija (tau00^2) : %.4f\n", tau00_sq))
cat(sprintf("Individų lygmens dispersija (sigma^2): %.4f\n", sigma_sq_m0))
cat(sprintf("Besąlyginio modelio ICC            : %.4f (arba %.2f%%)\n", icc_val, icc_val * 100))
cat(sprintf("Modelio log-tikėtinumas (logLik)   : %.2f\n", logLik(m0)))
cat(sprintf("Modelio Akaikės indeksas (AIC)     : %.2f\n", AIC(m0)))



## Sudėtinis modelis
m1 <- lme(
  fixed  = stfgov ~ hinctnta + union_member + agea + gdp_pc_k,
  random = ~ 1 | cntry,
  data   = df_final,
  method = "ML"
)

summary(m1)

## Dispersijos ir kokybės rodiklių palyginimas
var_comp_m1 <- VarCorr(m1)
tau00_sq_m1 <- as.numeric(var_comp_m1["(Intercept)", "Variance"])
sigma_sq_m1 <- as.numeric(var_comp_m1["Residual", "Variance"])

delta_aic <- AIC(m1) - AIC(m0)
r1_sq     <- (sigma_sq_m0 - sigma_sq_m1) / sigma_sq_m0
r2_sq     <- (tau00_sq - tau00_sq_m1) / tau00_sq

# AIC pokytis
print(anova(m0, m1))

cat(sprintf("Besąlyginio modelio AIC: %.2f\n", AIC(m0)))
cat(sprintf("Sudėtinio modelio AIC:   %.2f\n", AIC(m1)))
cat(sprintf("AIC sumažėjimas:         %.2f\n", abs(delta_aic)))
cat(sprintf("I lygmens liekamoji dispersija sigma^2: m0 = %.4f -> m1 = %.4f\n", sigma_sq_m0, sigma_sq_m1))
cat(sprintf("I lygmens dispersijos sąlyginis sumažėjimas: %.2f%%\n", r1_sq * 100))
cat(sprintf("II lygmens dispersijos sąlyginis sumažėjimas: %.2f%%\n", r2_sq * 100))

# Lygtis
fix_eff <- fixef(m1)
cat(sprintf("stfgov_hat = %.3f + %.3f*hinctnta + (%.3f)*union_member + %.3f*agea + %.3f*gdp_pc_k\n\n",
            fix_eff["(Intercept)"], fix_eff["hinctnta"], fix_eff["union_member"], fix_eff["agea"], fix_eff["gdp_pc_k"]))

# Q-Q grafikai
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# I lygmens liekanų Q-Q grafikas
qqnorm(residuals(m1, type = "normalized"), 
       main = "I lygmens liekanų Q-Q", 
       pch = 16, col = rgb(0.17, 0.48, 0.73, 0.2), cex = 0.8)
qqline(residuals(m1, type = "normalized"), col = "firebrick", lwd = 2)

# II lygmens atsitiktinių efektų (u_0) Q-Q grafikas
qqnorm(ranef(m1)$'(Intercept)', 
       main = "II lygmens efektų (u_0) Q-Q", 
       pch = 16, col = "#2b7bba", cex = 1.2)
qqline(ranef(m1)$'(Intercept)', col = "firebrick", lwd = 2)

par(mfrow = c(1, 1))
