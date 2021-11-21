library(ggplot2)
library(datos)
library(scales)
library(dplyr)


# Plot 1,  flores iris ----------------------------------------------------

ggplot(
  data = flores,
  mapping = aes(
    x = Largo.Sepalo,
    y = Largo.Petalo,
    color = Especies
  )
) +
geom_point()



# Plot 2, estado civil ----------------------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    x = estado_civil
  )
) +
  geom_bar()


# Plot 3, utilizando after_stat  ------------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    x = estado_civil
  )
) +
  geom_bar() +
  geom_text(
    mapping = aes(
      label = after_stat(count)
    ), 
    stat = "count"
  )


# Plot 4, estado civil por raza 01 ----------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    x = estado_civil,
    fill = raza,
  )) +
  geom_bar() +
  geom_text(
    mapping = aes(
      label = after_stat(count) 
    ), 
    stat = "count")


# Plot 4, estado civil por raza 02 ----------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    x = estado_civil,
    fill = raza, 
  )) +
  geom_bar() +
  geom_text(
    mapping = aes(
      label = after_stat(count) 
    ), 
    position = position_stack(
      vjust = 0.5
    ), 
    stat = "count") 


# Plot 5,  position dodge -------------------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    x = estado_civil,
    fill = raza, 
  )) +
  geom_bar(
    position = position_dodge()
  ) +
  geom_text(
    mapping = aes(
      label = after_stat(count) 
    ), 
    position = position_dodge(
      width = 0.9
    ), 
    stat = "count") 


# Plot 6  position fill ---------------------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    x = estado_civil,
    fill = raza, 
  )) +
  geom_bar(
    position = position_fill()
  ) +
  geom_text(
    mapping = aes(
      label = after_stat(count) 
    ), 
    position = position_fill(
      vjust = 0.5
    ), 
    stat = "count") 



# Plot 7,  scale_x --------------------------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    y = estado_civil, fill = raza, 
  )) +
  geom_bar(
    position = position_fill()
  ) +
  geom_text(
    mapping = aes(
      label = after_stat(count) 
    ), 
    position = position_fill(
      vjust = 0.5
    ), stat = "count") +
  scale_x_continuous(
    labels = percent_format() 
  )


# Plot 8,  scale_fill -----------------------------------------------------

ggplot(
  data = encuesta, 
  mapping = aes(
    y = estado_civil, fill = raza, 
  )) +
  geom_bar(
    position = position_fill()
  ) +
  geom_text(
    mapping = aes(
      label = after_stat(count) 
    ), 
    position = position_fill(
      vjust = 0.5
    ), stat = "count") +
  scale_x_continuous(
    labels = percent_format() 
  ) +
  scale_fill_manual(
    values = c("#2F86A6","#FFCE45","#34BE82")
  )


# Plot 9,  geom_col -------------------------------------------------------

datos <- encuesta %>% 
  count(estado_civil, raza) %>% 
  group_by(estado_civil) %>% 
  summarise(raza, n, prop = n/sum(n), .groups = "drop")

ggplot(
  data = datos,
  mapping = aes(
    y = estado_civil,
    x = prop,
    fill = raza
  )
) +
  geom_col() +
  geom_text(mapping = aes(
    label = percent(prop,0.1)
  ),
  position = position_stack(0.5)
  ) +
  scale_x_continuous(
    labels = percent_format()
  )


# Plot 10,  facetas -------------------------------------------------------

ggplot(
  data = encuesta,
  mapping = aes(
    y = cut_width(edad, 10),
  )
) +
  geom_bar() +
  facet_wrap(~religion)


# Plot 11, facetas 2 ------------------------------------------------------

ggplot(
  data = encuesta,
  mapping = aes(
    y = cut_width(edad, 10), 
  )
) +
  geom_bar() +
  facet_wrap(
    facets = ~fct_lump_n(
      religion,
      n = 3,
      other_level = "Otras")
  )
