library(ggplot2)
library(datos)
library(forcats)
library(dplyr)
library(scales)
library(dichromat)

datos <- encuesta %>% 
  count(estado_civil, raza) %>% 
  group_by(estado_civil) %>% 
  summarise(raza, n, prop = n/sum(n), .groups = "drop")


colors <- list(
  "#2F86A6",
  "#34BE82",
  "#726A95"
)

colors2 <- dichromat(colors, type = "deutan")
colors3 <- dichromat(colors, type = "protan")
colors4 <- dichromat(colors, type = "tritan")

ggplot(data = datos,
       mapping = aes(
         x = prop,
         y = fct_reorder2(estado_civil,prop,raza, .desc = F,
                          .fun = function(x,y) x[y == "Otra"]),
         fill = raza)) +
  geom_col(
    position = position_stack(reverse = T)
  ) +
  geom_text(
    aes(label = percent(prop,0.1)),
    position = position_stack(0.5,reverse = T),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  labs(y = "Estado Civil") +
  scale_fill_manual(values = colors) +
  scale_x_continuous(labels = percent_format())+
  theme_minimal()





