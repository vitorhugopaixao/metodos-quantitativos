# Introducao para dados
## Linguagem de dados

#library
library(tidyverse)
install.packages("openintro")
library(openintro)

#conjunto de dados High School and Beyond
high_school_beyond <- hsb2

#visualizando df
view(high_school_beyond)
glimpse(high_school_beyond)

# Tipos de Variaveis
#var categóricas ordinais
glimpse(high_school_beyond$id)

#var categóricas nominais
glimpse(hsb2$gender)

glimpse(hsb2$race)
unique(hsb2$race)

glimpse(high_school_beyond$ses)

glimpse(high_school_beyond$schtyp)

glimpse(high_school_beyond$prog)
unique(high_school_beyond$prog)

# Var numerica discreta
glimpse(high_school_beyond$read)
summary(high_school_beyond$read)

glimpse(high_school_beyond$write)
summary(high_school_beyond$write)

glimpse(high_school_beyond$math)
summary(high_school_beyond$math)

glimpse(high_school_beyond$science)
summary(high_school_beyond$science)

glimpse(high_school_beyond$socst)
summary(high_school_beyond$socst)

# Entubar x |> f(y) = f(x,y)
7 |> sum(3) 
sum(3,7)

high_school_beyond |> 
  count(schtyp)

high_school_beyond |> filter(schtyp == "public")

high_school_beyond_public <- high_school_beyond |> 
  filter(schtyp == "public")

high_school_beyond_public |> 
  count(schtyp)

# media
mean(high_school_beyond$read)

avg_read <- mean(high_school_beyond$read)

(avg_read2 <- mean(high_school_beyond$read)) #visualiza e armazena.

# condicional (if_else)
high_school_beyond_2 <-  high_school_beyond |> 
  mutate(read_cat = if_else( read < avg_read, "reprovado",
                              "aprovado")
        ) 

