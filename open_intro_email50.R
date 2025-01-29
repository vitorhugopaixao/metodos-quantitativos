# Introducao para dados
## Linguagem de dados

#library
library(tidyverse)
install.packages("openintro")
library(openintro)

# Informações do db
?email50

# Inspecionar
view(email50)
glimpse(email50)

# Filtrar
email50_big <- email50 |>
  filter(number == "big")

# media 
median_num_char <- median(email50$num_char)

# Condicional
email50_2 <- email50 |> 
  mutate(num_char_cat = if_else(num_char < median_num_char,
        "menor que mediana", "maior que mediana"))

# Contagem
email50_2 |> count(num_char_cat)
