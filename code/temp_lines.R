library(tidyverse)
library(scales)
library(glue)

t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na = "***") |>
  select(year = Year, month.abb) |>
  drop_na() |>
  pivot_longer(-year, names_to="month", values_to="t_diff")

last_dec <- t_diff |>
  filter(month == "Dec") |>
  mutate(year = year + 1,
         month = "last_Dec")

next_jan <- t_diff |>
  filter(month == "Jan") |>
  mutate(year = year - 1,
         month = "next_Jan")

t_data <- bind_rows(last_dec, t_diff, next_jan) |>
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) -1,
         this_year = year == 2022) 

t_data

t_data |>
  filter(year == 2022) |>
  slice_max(month_number)

t_data |>
  ggplot(aes(month_number, t_diff, group = year, color = year, size = this_year)) +
  geom_hline(yintercept = 0, color = "white") +
  geom_line() +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb, sec.axis = dup_axis(name = NULL, label = NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2), sec.axis = dup_axis(name = NULL, label = NULL)) +
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.25, 1), guide = "none") +
  scale_color_viridis_c(breaks = seq(1880, 2020, 20), 
                        guide = guide_colorbar(frame.colour = "white", frame.linewidth = 0.5)) +
  coord_cartesian(xlim = c(1,12)) +
  labs(
    x = NULL,
    y = "Temperature change since pre-industrial times [\u00b0C]",
    title = "Global temperature change since 1880 by month"
  ) +
  theme(
    plot.background = element_rect(fill = "#CCCCCC"),
    panel.background = element_rect(fill = "black", color = "white", size = 1),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", size = 13),
    axis.ticks = element_line(color = "white"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "black", size = 13),
    plot.title = element_text(color = "black", hjust = 0.5, size = 15),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "black"),
    legend.key.height = unit(60, "pt")
  ) 

ggsave("visuals/temp_lines.png", width = 8, height = 4.5)



