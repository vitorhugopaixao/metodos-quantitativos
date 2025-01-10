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
summary(mtcars$vs)

class(mtcars$am)
unique(mtcars$am)
mtcars$am <- factor(mtcars$am,
                    levels = c(0,1),
                    labels = c('automatic', 'manual'))
summary(mtcars$am)

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
  mutate(percent = round(count/sum(count),5)*100) %>%
  add_row(engine = 'TOTAL', count = sum(.$count), percent = floor(sum(.$percent)))

# V.numerica.disc: cyl, hp, gear e carb.
class(mtcars$cyl)
unique(mtcars$cyl)
summary(mtcars$cyl)

## Frequência cylinders = cyl
tabela_cylinders <- data.frame(mtcars$cyl) %>%
  rename(cylinders = mtcars.cyl) %>%
  count(cylinders) %>% 
  rename(count = n) %>%
  mutate(cylinders = as.character(cylinders)) %>%
  add_row(cylinders = 'TOTAL', count = sum(.$count))

## Frequência horsepower = hp
class(mtcars$hp)
unique(mtcars$hp)
summary(mtcars$hp)
# Tabela distribuição de frequência hp
## 1º Rol dados
sort(mtcars$hp) 
## 2º Amplitude total
ampli_total_hp <- max(mtcars$hp) - min(mtcars$hp) 
## 3º nº de classes método Sturges
n.classes_hp <- nclass.Sturges(mtcars$hp) 
### 4º Tamanho classes
t.classes_hp <- ceiling(ampli_total_hp/n.classes_hp) 
#### 5º Limite Intervalo de classes
limit_inter_hp <- seq(min(mtcars$hp), (min(mtcars$hp) + (n.classes_hp * t.classes_hp)), by = t.classes_hp)
limit_inter_hp
## Histograma
### 1º Freq c/ intervalos
freq_hp <- cut(mtcars$hp, breaks = limit_inter_hp, right = FALSE)
table(freq_hp)
### 2º Tabela Histigrama
tbl_histograma_hp <- as.data.frame(table(limit_inter_hp = freq_hp)) %>%
  mutate(freq_rel_hp = round(Freq/sum(Freq),4)) %>%
  add_row(limit_inter_hp = 'TOTAL', Freq = sum(.$Freq), freq_rel_hp = sum(.$freq_rel_hp))

## Frequência forward gears = gear
class(mtcars$gear)
unique(mtcars$gear)
summary(mtcars$gear)

tabela_gear <- data.frame(mtcars$gear) %>%
  count(mtcars.gear) %>%
  rename(n_forward_gears = mtcars.gear, count = n) %>%
  mutate(n_forward_gears = as.character(n_forward_gears)) %>%
  add_row(n_forward_gears = 'TOTAL', count = sum(.$count))

## Frequência carburetors = carb
class(mtcars$carb)
unique(mtcars$carb)
summary(mtcars$carb)

tabela_carb <- data.frame(mtcars$carb) %>%
  count(mtcars.carb) %>%
  rename(num_carb = mtcars.carb, count = n) %>%
  mutate(num_carb = as.character(num_carb)) %>%
  add_row(num_carb = 'TOTAL', count = sum(.$count))

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



