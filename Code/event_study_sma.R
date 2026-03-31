

# ── La SMA se instaló en 2013 — evento único para todo Chile ──────────────────
# Tratamiento = sectores con alto ENCORE score expuestos al nuevo enforcement
# Control = sectores con bajo ENCORE score (poco afectados por SMA)
# Identificación: within bank-firm, ¿los sectores más contaminantes
# recibieron menos crédito después de 2013?

df_sma <- df |>
  # Incluir TODAS las regiones incluyendo RM — el evento es nacional
  bind_rows(
    cmf_annual |>
      filter(regid == 13, !is.na(score_share)) |>
      mutate(
        lnewcred_w  = Winsorize(lnewcred,
                                quantile(lnewcred, probs = c(0.01, 0.99),
                                         na.rm = TRUE)),
        bankfirm_id = as.integer(interaction(ins_cod, id, drop = TRUE)),
        IDB         = as.integer(factor(ins_cod)),
        IDR         = as.integer(factor(regid)),
        IDS         = as.integer(factor(subrubroeconómico))
      )
  ) |>
  mutate(
    IDR_fixed    = as.integer(factor(regid)),
    # Event time relativo a 2013 (instalación SMA)
    event_time   = as.integer(year - 2013),
    event_time_w = case_when(
      event_time < -2 ~ -2L,   # solo 2011-2012 pre
      event_time >  8 ~  8L,   # hasta 2021 post
      TRUE            ~ event_time
    ),
    event_time_f = relevel(factor(event_time_w), ref = "-1"),
    # Centrar score_share
    score_dm     = score_share - mean(score_share, na.rm = TRUE)
  )

cat("Sample para DiD SMA 2013:\n")
cat("  Obs:     ", nrow(df_sma), "\n")
cat("  Regiones:", n_distinct(df_sma$regid), "(incluye RM)\n")
cat("  Años pre (2011-2012):", sum(df_sma$year < 2013), "\n")
cat("  Años post (2013+):  ", sum(df_sma$year >= 2013), "\n")


# Cambiar referencia a 2012 y ver si 2011 es plano
df_sma <- df_sma |>
  mutate(
    event_time_f = relevel(factor(event_time_w), ref = "-1")  # -1 = 2012
  )


# ── Estimar event study ────────────────────────────────────────────────────────
es_sma <- feols(
  lnewcred_w ~ i(event_time_f, score_dm, ref = "-1") |
    bankfirm_id + IDB^year + IDS^year,
  data    = df_sma,
  cluster = ~IDR_fixed + IDS
)

summary(es_sma)



# Tidy y plot rápido
broom::tidy(es_sma, conf.int = TRUE) |>
  filter(str_detect(term, "event_time_f")) |>
  mutate(
    event_time = as.integer(str_extract(term, "-?\\d+")),
    year       = 2013 + event_time
  ) |>
  bind_rows(
    tibble(event_time = -1L, year = 2012L, estimate = 0,
           conf.low = 0, conf.high = 0, p.value = 1)
  ) |>
  arrange(event_time) |>
  ggplot(aes(event_time, estimate)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, colour = "black",
             linewidth  = 0.5, linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, linewidth = 0.6, colour = "black") +
  geom_point(size = 3, colour = "black", fill = "black", shape = 21) +
  scale_x_continuous(
    breaks = -2:8,
    labels = as.character(-2:8)
  ) +
  labs(
    title    = "Event Study DiD — Log Average Credit Balance",
    subtitle = "ENCORE score × years relative to SMA installation (2013 = t0)",
    x        = "Years relative to SMA installation (t = 0 in 2013)",
    y        = "Coefficient (ENCORE score × event time)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(colour = "grey40", size = 10, hjust = 0),
    axis.line     = element_line(colour = "black", linewidth = 0.4),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(dout("fig_event_sma2013.png"), width = 9, height = 5, dpi = 300)


