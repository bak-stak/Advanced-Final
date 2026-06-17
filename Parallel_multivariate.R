mod_completo_int <- glmer(Abstension
                          ~ Institutional_distrust * eta_classe
                          + Stealth_democracy
                          + Macro_region
                          + scol
                          + (1 | eta_classe),
                          # tolto random slope perché τ11 = 0
                          family = binomial,
                          data = Regions_df)



stargazer(mod_completo_int, type = "text")

ggpredict(mod_completo_int,
          terms = c("Institutional_distrust [0:10 by=0.5]",
                    "eta_classe"),
          type = "random") |>
  plot() +
  labs(title = "Predicted probabilities of Abstension",
       x = "Institutional distrust",
       y = "Populist vote",
       color = "Coorte") +
  theme_bw()

tab_model(mod_completo_int,
          show.re.var = TRUE,    # mostra varianza random effects
          show.icc = TRUE,       # ICC: quanto conta la coorte
          show.r2 = TRUE,
          dv.labels = "Astensione")


AIC(mod_completo_int, mod_completo_int)