# Bibliotecas
library(tidyverse)
# Database
## Motor Trend Car Road Tests
?mtcars
data(mtcars)

# Inspecionar Data Frame
str(mtcars)
head(mtcars,10)
tail(mtcars,10)

## Variáveis(v)
### V.categoricas nominais: vs e am
class(mtcars$vs)
unique(mtcars$vs)
mtcars$vs <- factor(mtcars$vs, 
                  levels = c(0,1),
                  labels = c('v-shaped','straight'))

class(mtcars$am)
unique(mtcars$am)
mtcars$am <- factor(mtcars$am,
                    levels = c(0,1),
                    labels = c('automatic', 'manual'))

# Tabelas Dados Categóricos
## Frequência Engine = vs
tabela_engine <- data.frame(mtcars$vs) %>%
  rename(engine = mtcars.vs) %>%
  count(engine) %>% 
  rename(count = n) %>%
  add_row(engine = 'TOTAL', count = sum(.$count))

  
## Frequência Transmission = am
tabela_transmission <- data.frame(mtcars$am) %>%
  rename(transmission = mtcars.am) %>%
  count(transmission) %>%
  rename(count = n) %>%
  add_row(transmission = 'TOTAL', count = sum(.$count))

  
## Tabela com duas variaveis categoricas
tabela_vs_am <- data.frame(mtcars$vs,mtcars$am) %>%
  rename(engine = mtcars.vs, transmission = mtcars.am) %>%
  count(engine, transmission) %>%
  rename(count = n)

## Tabela com duas variaveis categoricas e percentual
tbl_vs_am_percent <- tabela_vs_am %>%
  mutate(percent = round(count/sum(count),4)*100) %>%
  add_row(engine = 'TOTAL', count = sum(.$count), percent = floor(sum(.$percent)))

### V.numerica.disc: cyl, hp, gear e carb.
class(mtcars$cyl)
unique(mtcars$cyl)
summary(mtcars$cyl)

class(mtcars$hp)
unique(mtcars$hp)
summary(mtcars$hp)

class(mtcars$gear)
unique(mtcars$gear)
summary(mtcars$gear)

class(mtcars$carb)
unique(mtcars$carb)
summary(mtcars$carb)

summary(mtcars[, c('cyl', 'hp', 'gear', 'carb')])


###v.numerica.cont: mpg, disp, drat, wt e qsec.
class(mtcars$mpg)
summary(mtcars$mpg)

class(mtcars$disp)
summary(mtcars$disp)

class(mtcars$drat)
summary(mtcars$drat)

class(mtcars$wt)
summary(mtcars$wt)

class(mtcars$qsec)
summary(mtcars$qsec)

str(mtcars[, c('mpg', 'disp', 'drat', 'wt', 'qsec')])
summary(mtcars[, c('mpg', 'disp', 'drat', 'wt', 'qsec')])
