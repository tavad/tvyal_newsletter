library(sf)
library(tidyverse)
library(ggrepel)
library(scatterpie)

yerevan_boundaries <- st_read("~/R/Gcapatker/2024_09_07_Yerevan_shapefile/data/boundary-polygon.gpkg")

str(yerevan_boundaries)

damage <- read_csv("~/R/newsletter/2024/2024_09_09_electricity/electrical_network_damage.scv")

damage <- 
  damage |> 
  filter(district != "Ընդամենը") |> 
  mutate(
    district = ifelse(district == "Մաշտոց", "Կենտրոն", district)
  ) |> 
  group_by(district) |> 
  summarise(
    total_damaged_cables = sum(total_damaged_cables),
    other_org_damaged_cabels = sum(other_org_damaged_cabels),
    .groups = "drop"
  ) |> 
  janitor::adorn_totals("row", name = "Ընդամենը") |> 
  mutate(
    other_org_damaged_pct = other_org_damaged_cabels / total_damaged_cables,
    hec_damaged_pct = 1 - other_org_damaged_pct
  )


# Prepare data for map fill
yerevan_map <- yerevan_boundaries %>%
  left_join(damage %>% filter(district != "Ընդամենը"), by = c("NAME" = "district"))

# Prepare data for pie charts
yerevan_centroids <- yerevan_map %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2]
  ) %>% 
  select(NAME, x, y, hec_damaged_pct, other_org_damaged_pct, total_damaged_cables) %>%
  filter(!is.na(hec_damaged_pct)) %>%  # Remove rows with NA values
  pivot_longer(cols = c(hec_damaged_pct, other_org_damaged_pct), 
               names_to = "damage_type", 
               values_to = "value") %>%
  st_drop_geometry() |> 
  pivot_wider(names_from = damage_type, values_from = value)

total_row <- 
  damage |> 
  filter(district == "Ընդամենը") |> 
  rename(NAME = district) |> 
  mutate(
    x = 44.4,
    y = 40.1,
  ) |> 
  select(NAME, total_damaged_cables, x, y, hec_damaged_pct, other_org_damaged_pct)

yerevan_centroids <- bind_rows(yerevan_centroids, total_row)



# Create the plot
ggplot() +
  # Add the map layer
  geom_sf(data = yerevan_map) +
  
  # Add the pie charts
  geom_scatterpie(
    data = yerevan_centroids,
    aes(x = x, y = y, group = NAME),
    cols = c("hec_damaged_pct", "other_org_damaged_pct"),
    alpha = 0.6,
    pie_scale = 4
  ) +
  
  geom_text(
    data = yerevan_centroids,
    aes(x = x, y = y, label = paste0(round(hec_damaged_pct * 100), "%")),
    nudge_x =  0.005,
    nudge_y = -0.005,
    size = 2.5
  ) +
  geom_text(
    data = yerevan_centroids,
    aes(x = x, y = y, label = paste0(round(other_org_damaged_pct * 100), "%")),
    nudge_x = -0.005,
    nudge_y =  0.007,
    size = 2.5
  ) +
  
  # Add district names
  geom_text(
    data = yerevan_centroids,
    aes(x = x, y = y, label = paste0(NAME, " (", total_damaged_cables, ")")),
    vjust = -5,
    size = 3
  ) +
  
  geom_text(
    aes(x = 44.4, y = 40.1, label = "փակագծում նշված թիվը վնասված\n մալուխների քանակն է"),
    vjust = 4,
    size = 2.5
  ) +
  
  # Set up separate scales for the map fill and pie chart colors
  scale_fill_manual(
    values = new_palette_colors[c(6,2)],
    labels = c("ՀԷՑ-ի կողմից վնասված մալուխներ",
               "Այլ կազմակերպությունների կողմից վնասված մալուխներ")
  ) +
  
  # Add labels and theme
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Երևանում վնասված մալուխները ըստ համայքների",
    subtitle = "2024թ․ հունվար-օգոստոս ամիսների ընթացքում վնասված մալուխների քանակ",
    caption = caption_f()
  ) +
  theme_tvyal() +
  coord_sf(expand = FALSE) +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  )

