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

### V.numerica.disc: cyl, hp, gear e carb.
class(mtcars$cyl)
unique(mtcars$cyl)
summary(mtcars$cyl)
# Tabela distribuição de frequência cyl
## 1º Rol dados
sort(mtcars$cyl)
## 2º Amplitude total
amplitude_total_cyl <- max(mtcars$cyl) - min(mtcars$cyl)
## 3º nº de classes método Sturges
n.classes_cyl <- nclass.Sturges(mtcars$cyl)
### 4º Tamanho classes
t.classes_cyl <- ceiling(amplitude_total_cyl/n.classes_cyl)
#### 5º Limites Intervalo de classes
limt_classes_cyl <- seq(min(mtcars$cyl), min(mtcars$cyl) + (n.classes_cyl * t.classes_cyl), by = t.classes_cyl)
limt_classes_cyl
## Histograma
### 1º Frequencia c/ intervalos
freq_cyl <- cut(mtcars$cyl, breaks = limt_classes_cyl, right = FALSE) 
### 2º Tabela Histigrama
tabela_histo_cyl <- as.data.frame(table(freq_cyl))

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



