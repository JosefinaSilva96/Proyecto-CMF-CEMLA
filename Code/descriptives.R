library(tidyverse)

library(tidyverse)

# ── Graph 1: annual average lnewcred ──────────────────────────────────────────
plot1_data <- cmf_annual %>%
  group_by(year) %>%
  summarise(mean_lnewcred = mean(lnewcred, na.rm = TRUE))

ggplot(plot1_data, aes(x = year, y = mean_lnewcred)) +
  geom_line(color = "#3266ad", linewidth = 1) +
  geom_point(color = "#3266ad", size = 2.5) +
  labs(
    title = "Log new credits — annual average",
    x = NULL, y = "log(new credits)"
  ) +
  scale_x_continuous(breaks = unique(plot1_data$year)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# ── Graph 2: by ENCORE group (d75_score) ──────────────────────────────────────
plot2_data <- cmf_annual %>%
  mutate(encore_group = if_else(d75_score == 1, "High ENCORE", "Low ENCORE")) %>%
  group_by(year, encore_group) %>%
  summarise(mean_lnewcred = mean(lnewcred, na.rm = TRUE), .groups = "drop")

ggplot(plot2_data, aes(x = year, y = mean_lnewcred,
                       color = encore_group, linetype = encore_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("High ENCORE" = "#3266ad", "Low ENCORE" = "#888780")) +
  scale_linetype_manual(values = c("High ENCORE" = "solid", "Low ENCORE" = "dashed")) +
  scale_x_continuous(breaks = unique(plot2_data$year)) +
  labs(
    title = "Log new credits by ENCORE group",
    x = NULL, y = "log(new credits)",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

library(tidyverse)
library(sf)
library(chilemapas)

# ── Asignar corte a cada región ───────────────────────────────────────────────
court_data <- tibble(
  codigo_region = c("05","06","07","13","16",
                    "08","09","10","11","12","14",
                    "01","02","03","04","15"),
  court = c(rep("Santiago (2013)",    5),
            rep("Valdivia (2014)",    6),
            rep("Antofagasta (2017)", 5))
)

# ── Construir mapa regional agregando desde comunas ───────────────────────────
mapa_base <- mapa_comunas %>%
  group_by(codigo_region) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  sf::st_as_sf()                          # <- esto falta

# ── Unir con cortes ───────────────────────────────────────────────────────────
mapa <- mapa_base %>%
  left_join(court_data, by = "codigo_region") %>%
  sf::st_as_sf()                          # <- por si acaso también acá

# ── Paleta ────────────────────────────────────────────────────────────────────
court_colors <- c(
  "Santiago (2013)"    = "#3266ad",
  "Valdivia (2014)"    = "#1D9E75",
  "Antofagasta (2017)" = "#D85A30"
)

# ── Plot ──────────────────────────────────────────────────────────────────────
ggplot(mapa) +
  geom_sf(aes(fill = court), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = court_colors, name = "Environmental Court") +
  labs(
    title   = "Environmental Courts in Chile",
    caption = "Regions assigned by court jurisdiction and entry year"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position   = c(0.25, 0.35),
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    plot.title        = element_text(hjust = 0.5, size = 13),
    plot.caption      = element_text(size = 8, color = "gray50")
  )

#####

library(tidyverse)
library(sf)
library(chilemapas)
library(patchwork)

# ── Construir mapa regional ───────────────────────────────────────────────────
mapa_base <- mapa_comunas %>%
  group_by(codigo_region) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  sf::st_as_sf()

# ── Agregar score_share por región ────────────────────────────────────────────
sma_data <- cmf_annual %>%
  filter(!is.na(regid), regid %in% 1:16) %>%
  mutate(codigo_region = str_pad(regid, width = 2, pad = "0")) %>%
  group_by(codigo_region) %>%
  summarise(sma_ratio = mean(score_share, na.rm = TRUE), .groups = "drop")

# ── Crear categorías ──────────────────────────────────────────────────────────
mapa <- mapa_base %>%
  left_join(sma_data, by = "codigo_region") %>%
  sf::st_as_sf() %>%
  mutate(sma_cat = case_when(
    sma_ratio <= 0.38                    ~ "]38%]",
    sma_ratio > 0.38 & sma_ratio <= 0.42 ~ "]38% to 42%]",
    sma_ratio > 0.42 & sma_ratio <= 0.46 ~ "]42% to 46%]",
    sma_ratio > 0.46 & sma_ratio <= 0.49 ~ "]46% to 49%]",
    sma_ratio > 0.49                     ~ "]more than 49%]",
    TRUE                                 ~ NA_character_
  )) %>%
  mutate(sma_cat = factor(sma_cat, levels = c(
    "]38%]", "]38% to 42%]", "]42% to 46%]", "]46% to 49%]", "]more than 49%]"
  )))

# ── Paleta ────────────────────────────────────────────────────────────────────
sma_colors <- c(
  "]38%]"           = "#C4A882",
  "]38% to 42%]"    = "#8FBC8F",
  "]42% to 46%]"    = "#D4C84A",
  "]46% to 49%]"    = "#E8820A",
  "]more than 49%]" = "#C0150A"
)

# ── Bounding boxes fijas por franja (longitud/latitud WGS84) ─────────────────
# Norte:  regiones 01–07  → lat -30 a -17
# Centro: regiones 08–09, 13, 16 → lat -38 a -30
# Sur:    regiones 10–15  → lat -56 a -38

make_panel <- function(data, xmin, xmax, ymin, ymax, show_legend = FALSE) {
  ggplot(data) +
    geom_sf(aes(fill = sma_cat), color = "white", linewidth = 0.3) +
    scale_fill_manual(
      values   = sma_colors,
      name     = NULL,
      na.value = "gray90",
      drop     = FALSE
    ) +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    theme_void(base_size = 10) +
    theme(
      legend.position = if (show_legend) "right" else "none",
      legend.text     = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      panel.border    = element_rect(color = "gray80", fill = NA, linewidth = 0.3)
    )
}

p_norte  <- make_panel(mapa, xmin = -76, xmax = -66, ymin = -30, ymax = -17)
p_centro <- make_panel(mapa, xmin = -74, xmax = -69, ymin = -38, ymax = -30)
p_sur    <- make_panel(mapa, xmin = -76, xmax = -66, ymin = -56, ymax = -38,
                       show_legend = TRUE)

# ── Combinar ──────────────────────────────────────────────────────────────────
p_norte + p_centro + p_sur +
  plot_layout(ncol = 3) +
  plot_annotation(
    title   = NULL,
    caption = NULL,
    theme   = theme(
      plot.title   = element_text(hjust = 0.5, size = 11, face = "bold"),
      plot.caption = element_text(size = 8, hjust = 0)
    )
  )

ggsave(
  filename = "C:/WBG/GitHub/Proyecto-CMF-CEMLA/Outputs/graphs/fig2_sma_ratio_map.png",
  width  = 10,
  height = 6,
  dpi    = 300,
  bg     = "white"
)
