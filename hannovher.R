library(ggplot2)
library(osmdata)
library(ggspatial)

streets <- read.csv("street_names.csv")
pal <- ggthemes::ptol_pal()(6)
gender_col <- c(
  "F" = pal[[4]],
  "W" = pal[[5]],
  "M" = pal[[1]],
  "WM" = pal[[1]],
  "def" = NA
)

add_street_cols <- function(d, def_col = "grey") {
  gender_col[["def"]] <- def_col
  d$osm_lines <- d$osm_lines |>
    dplyr::left_join(streets, by = c("name" = "Street.Name")) |>
    dplyr::mutate(Probable.Gender = ifelse(is.na(Probable.Gender), "def", Probable.Gender),
                  Color = gender_col[Probable.Gender])
  d
}

coord_h <- getbb("Hannover, Germany")
opq_h <- coord_h |> opq()

big_streets <- opq_h |>
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) |>
  osmdata_sf() |>
  add_street_cols("grey60")

med_streets <- opq_h |>
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) |>
  osmdata_sf() |>
  add_street_cols("grey60")

small_streets <- opq_h |>
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "unclassified", "service", "footway")) |>
  osmdata_sf() |>
  add_street_cols("grey45")

river <- opq_h |>
  add_osm_feature(key = "water",
                  value = c("basin", "river", "canal", "lake", "pond")) |>
  osmdata_sf()

railway <- opq_h |>
  add_osm_feature(key = "railway", value = "rail") |>
  osmdata_sf()

ggplot() +
  geom_sf(data = river$osm_polygons,
          inherit.aes = FALSE,
          color = "grey15",
          fill = "grey15",
          size = .8) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "gray30",
          size = .2,
          linetype = "dashed") +
  geom_sf(data = small_streets$osm_lines,
          aes(color = Color),
          inherit.aes = FALSE,
          size = .2) +
  geom_sf(data = med_streets$osm_lines,
          aes(color = Color),
          inherit.aes = FALSE,
          size = .3) +
  geom_sf(data = big_streets$osm_lines,
          aes(color = Color),
          inherit.aes = FALSE,
          size = .5) +
  geom_sf(data = small_streets$osm_lines[small_streets$osm_lines$Probable.Gender == "WM", ],
          color = pal[5],
          inherit.aes = FALSE,
          size = .2,
          linetype = "11") +
  geom_sf(data = med_streets$osm_lines[med_streets$osm_lines$Probable.Gender == "WM", ],
          color = pal[5],
          inherit.aes = FALSE,
          size = .3,
          linetype = "11") +
  geom_sf(data = big_streets$osm_lines[big_streets$osm_lines$Probable.Gender == "WM", ],
          color = pal[5],
          inherit.aes = FALSE,
          size = .5,
          linetype = "11") +
  scale_color_identity() +
  coord_sf(xlim = c(9.68, 9.834), ylim = c(52.315, 52.438), expand = FALSE) +
  labs(title = stringr::str_glue("Hannov<span style='color:{pal[5]}'>*her*</span>"),
       subtitle = stringr::str_glue(
         "Nach <span style='color:{pal[5]}'>**Frauen**</span>, ",
         "<span style='color:{pal[1]}'>**Männern**</span> und ",
         "<span style='color:{pal[4]}'>**Familien**</span> ",
         "benannte Straßen Hannovers."),
       caption = stringr::str_glue(
         "Julien Delarocque (2024).<br>Straßennamen zu großen Teilen ",
         "anhand von Hanke, C. (2014), Hannovers Straßennamen erzählen ",
         "Geschichte (2. Aufl.), Medien-Verlag Schubert, kategorisiert.<br>",
         "Geodaten: (c) OpenStreetMap Mitwirkende (ODbL).")) +
  theme_void() +
  theme(plot.title = ggtext::element_markdown(size = 72,
                                              family = "Open Sans",
                                              hjust = 0,
                                              colour = "grey70",
                                              margin = margin(0, 0, 3, -1.25, "mm")),
        plot.subtitle = ggtext::element_markdown(size = 24,
                                                 family = "Open Sans",
                                                 hjust = 0,
                                                 colour = "grey70",
                                                 margin = margin(0, 0, 5, 0, "mm")),
        plot.caption = ggtext::element_markdown(size = 12,
                                                family = "Open Sans",
                                                hjust = 0.5,
                                                colour = "grey70",
                                                margin = margin(7, 0, 0, 0, "mm"),
                                                lineheight = 1.5),
        plot.background = element_rect(fill = "grey20", linetype = 0),
        plot.margin = margin(2.2, 2.2, 2.2, 2.2, "cm"))

ggsave("hannovher.png", width = 420, height=594, units = "mm")
