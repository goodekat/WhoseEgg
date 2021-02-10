
library(ggplot2)
library(gganimate)

nangles <- 10
df <- 
  data.frame(
    x = rep(0, nangles), 
    y = rep(0, nangles),
    label = rep("<*)))><", nangles), 
    angle = seq(1, 360, length.out = nangles)
  ) %>% 
  mutate(id = 1:n())

plot <- 
  df %>%
  ggplot(aes(x = x, y = y, label = label, angle = angle)) + 
  geom_text() + 
  theme_void()

plot

plot + transition_states(states = id)

nangles <- 25
df <- 
  data.frame(
    x = cos(seq(0, 2*pi, length = nangles)), 
    y = sin(seq(0, 2*pi, length = nangles)), 
    label = rep("<*)))><", nangles), 
    angle = seq(1, 360, length.out = nangles)
  ) %>% 
  mutate(id = 1:n())

plot <- 
  df %>%
  ggplot(aes(x = x, y = y, label = label, angle = angle)) + 
  geom_text() + 
  theme_void()

plot

plot + transition_states(states = id)

