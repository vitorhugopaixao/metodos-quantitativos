# Bibliotecas
library(tidyverse)
library(ggplot2)
# Database
## Motor Trend Car Road Tests
?mtcars
data(mtcars)

# Inspecionar estrutura Data Frame
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

ggplot(mtcars$vs, aes(x = Categoria, y = Valores, fill = Categoria)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Gráfico de Barras com ggplot2",
    x = "Categorias",
    y = "Valores"
  ) +
  theme_minimal()

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

# V.numerica.discretas: cyl, hp, gear e carb.
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
tbl_histograma_hp <- as.data.frame(table(freq_hp)) %>%
  rename(horsepower = freq_hp) %>%
  mutate(freq_rel_hp = round(Freq/sum(Freq),4)) %>%
  add_row(horsepower = 'TOTAL', Freq = sum(.$Freq), freq_rel_hp = sum(.$freq_rel_hp))

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
# Tabela distribuição de frequência mpg
## 1º Rol dados
sort(mtcars$mpg) 
## 2º Amplitude total
ampli_total_mpg <- max(mtcars$mpg) - min(mtcars$mpg) 
## 3º nº de classes método Sturges
n.classes_mpg <- nclass.Sturges(mtcars$mpg) 
### 4º Tamanho classes
t.classes_mpg <- ceiling(ampli_total_mpg/n.classes_mpg) 
#### 5º Limite Intervalo de classes
limit_inter_mpg <- seq(min(mtcars$mpg), (min(mtcars$mpg) + (n.classes_mpg * t.classes_mpg)), by = t.classes_mpg)
limit_inter_mpg
## Histograma
### 1º Freq c/ intervalos
freq_mpg <- cut(mtcars$mpg, breaks = limit_inter_mpg, right = FALSE)
table(freq_mpg)
### 2º Tabela Histigrama
tbl_histograma_mpg <- as.data.frame(table(freq_mpg)) %>%
  rename(miles_gallon = freq_mpg, count_mpg = Freq) %>%
  mutate(freq_rel_mpg = round(count_mpg/sum(count_mpg),5)*100) %>%
  add_row(miles_gallon = 'TOTAL', count_mpg = sum(.$count_mpg), freq_rel_mpg = sum(.$freq_rel_mpg))


# Tabela distribuição de frequência disp
class(mtcars$disp)
unique(mtcars$disp)
summary(mtcars$disp)
## 1º Rol dados
sort(mtcars$disp)
## 2º Amplitude total
ampli_total_disp <- max(mtcars$disp) - min(mtcars$disp)
## 3º nº de classes método Sturges
n.classes_disp <- nclass.Sturges(mtcars$disp)
### 4º Tamanho classes
t.classes_disp <- ceiling(ampli_total_disp/n.classes_disp)
#### 5º Limite Intervalo de classes
limit_inter_disp <- seq(floor(min(mtcars$disp)), min(mtcars$disp) + (n.classes_disp * t.classes_disp), by = t.classes_disp)
limit_inter_disp
## Histograma
### 1º Freq c/ intervalos
freq_disp <- cut(mtcars$disp, breaks =  limit_inter_disp, right = FALSE)
table(freq_disp)
### 2º Tabela Histigrama
tbl_histograma_disp <- as.data.frame(table(freq_disp)) %>%
  rename(displacement = freq_disp, count_disp = Freq) %>%
  mutate(freq_rel_disp = round(count_disp/sum(count_disp),4)*100) %>%
  mutate(displacement = as.character(displacement)) %>%
  add_row(displacement = 'TOTAL', count_disp = sum(.$count_disp), freq_rel_disp = sum(.$freq_rel_disp))


class(mtcars$drat)
unique(mtcars$drat)
summary(mtcars$drat)
## 1º Rol dados
sort(mtcars$drat)
## 2º Amplitude total
ampli_total_drat <- max(mtcars$drat) - min(mtcars$drat)
## 3º nº de classes método Sturges
n.classes_drat <- nclass.Sturges(mtcars$drat)
### 4º Tamanho classes
t.classes_drat <- round(ampli_total_drat / n.classes_drat, 1)
#### 5º Limite Intervalo de classes
limit_inter_drat <- seq(min(mtcars$drat), min(mtcars$drat) + (n.classes_drat * t.classes_drat), by = t.classes_drat)
limit_inter_drat
## Histograma
### 1º Freq c/ intervalos
freq_drat <- cut(mtcars$drat, breaks = limit_inter_drat, right = FALSE)
table(freq_drat)
### 2º Tabela Histograma
tbl_histograma_drat <- as.data.frame(table(freq_drat)) %>%
  rename(rear_axle_ratio = freq_drat, count_drat = Freq) %>%
  mutate(freq_rel_drat = round(count_drat/sum(count_drat),4)*100) %>%
  mutate(rear_axle_ratio = as.character(rear_axle_ratio)) %>%
  add_row(rear_axle_ratio = 'TOTAL', count_drat = sum(.$count_drat), freq_rel_drat = sum(.$freq_rel_drat))

# variavel wt
class(mtcars$wt)
summary(mtcars$wt)
## 1º Rol dados
sort(mtcars$wt)
## 2º Amplitude total
ampli_total_wt <- max(mtcars$wt) - min(mtcars$wt)
## 3º nº de classes método Sturges
n.classes_wt <- nclass.Sturges(mtcars$wt)
## 4º Tamanho classes
t.classes_wt <- round(ampli_total_wt/n.classes_wt,1)
## 5º Limites intervalo de classes
limit_inter_wt <- seq(from = min(mtcars$wt), to = min(mtcars$wt) + (n.classes_wt * t.classes_wt), by = t.classes_wt)
limit_inter_wt
### Histograma
#### 1º Freq c/ intervalos
freq_wt <- cut(mtcars$wt, breaks = limit_inter_wt, right = FALSE)
table(freq_wt)
#### 2º Tabela Histograma
tbl_histograma_wt <- as.data.frame(table(freq_wt)) %>%
  rename(weight = freq_wt, count_wt = Freq) %>%
  mutate(freq_rel_wt = round(count_wt/sum(count_wt),4)*100) %>%
  mutate(weight = as.character(weight)) %>%
  add_row(weight = 'TOTAL', count_wt = sum(.$count_wt), freq_rel_wt = sum(.$freq_rel_wt))
    

# variavel qsec
class(mtcars$qsec)
summary(mtcars$qsec)
## 1º Rol dados
sort(mtcars$qsec)
## 2º Amplitude total
ampli_total_qsec <- max(mtcars$qsec) - min(mtcars$qsec)
## 3º nº de classes método Sturges
n.classes_qsec <- nclass.Sturges(mtcars$qsec)
## 4º Tamanho classes
### Add vlr 0.1 para  incluso do valor max na ultima classe, mantem uniformidade dos intervalos.
t.classes_qsec <- ((ampli_total_qsec/n.classes_qsec) + 0.1)
## 5º Limites intervalo de classes
limit_inter_qsec <- seq(from = min(mtcars$qsec), 
                        to = min(mtcars$qsec) + (n.classes_qsec * t.classes_qsec), 
                        by = t.classes_qsec)
limit_inter_qsec
### Histograma
#### 1º Freq c/ intervalos
freq_qsec <- cut(mtcars$qsec, breaks = limit_inter_qsec, right = FALSE)
#### 2º Tabela Histograma
tbl_histograma_qsec <- as.data.frame((table(freq_qsec))) %>%
  rename(quarter_mile_time = freq_qsec, count_wt = Freq) %>%
  mutate(freq_rel_qsec = round(count_wt/sum(count_wt),5)*100) %>%
  mutate(quarter_mile_time = as.character(quarter_mile_time)) %>%
  add_row(quarter_mile_time = "TOTAL", count_wt = sum(.$count_wt), freq_rel_qsec = sum(.$freq_rel_qsec))


str(mtcars[, c('mpg', 'disp', 'drat', 'wt', 'qsec')])
summary(mtcars[, c('mpg', 'disp', 'drat', 'wt', 'qsec')])



