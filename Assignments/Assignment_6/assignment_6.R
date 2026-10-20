
library(tidyverse)
dat<-read_csv("../../Data/Biolog_Plate_Data.csv")
view(dat)
glimpse(dat)
colSums(is.na(dat))
unique(dat$`Sample ID`)
table(dat$'Sample ID')

dat_long <- dat %>%
  pivot_longer(
    cols = c(Hr_24, Hr_48, Hr_144),
    names_to = "Time",
    values_to = "Absorbance"
  )

View(dat_long)

dat_long <- dat_long %>%
  mutate(Type = if_else(
    `Sample ID` %in% c("Soil_1", "Soil_2"),
    "Soil",
    "Water"
  ))

table(dat_long$Type)


dat_plot <- dat_long %>%
  filter(Dilution == 0.1)

View(dat_plot)

dat_plot <- dat_plot %>%
  mutate(Time = as.numeric(
    str_remove(Time, "Hr_")
  ))

unique(dat_plot$Time)


ggplot(dat_plot, aes(x = Time, y = Absorbance,
                     color = Type)) +
  geom_point() +
  facet_wrap(~ `Sample ID`) +
  labs(
    title = "Absorbance Over Time",
    x = "Time (hours)",
    y = "Absorbance"
  ) +
  theme_bw()
file.exists("../../Data/biolog.png")
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
dat_ita <- dat_long %>%
  filter(Substrate == "Itaconic Acid")
nrow(dat_ita)
dat_avg <- dat_ita %>%
  group_by(`Sample ID`, Type, Dilution, Time) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance),
    .groups = "drop"
  )
View(dat_avg)
dat_avg <- dat_avg %>%
  mutate(Time = as.numeric(str_remove(Time, "Hr_")))
unique(dat_avg$Time)

p <- ggplot(dat_avg,
            aes(x = Time,
                y = Mean_Absorbance,
                color = Type,
                group = interaction(`Sample ID`, Dilution))) +
  geom_line() +
  geom_point(size = 3) +
  facet_wrap(~ `Sample ID`) +
  labs(
    title = "Itaconic Acid - Time: {frame_along} hours",
    x = "Time (hours)",
    y = "Mean Absorbance"
  ) +
  theme_bw() +
  transition_reveal(Time)
animate(p, nframes = 60, fps = 10,
        width = 800, height = 500,
        renderer = gifski_renderer())
anim_save("Assignment_6.gif")
