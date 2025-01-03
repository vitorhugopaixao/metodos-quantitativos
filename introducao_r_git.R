# Comandos Basicos

## Instalar Pacotes:
### install.packages("nome_do_pacote")

## Carregar Pacotes:
### library(nome_do_pacote)
library(tidyverse) 
####tidyverse inclui dplyr e ggplot2

### Carregar Pacotes Indvidualmente
#### library(dplyr)
#### library(ggplot2)

## Carregar database de exemplo do R.
library(gapminder)


## Operadores

### Comparacao
2 == 2
2 != 3
2 < 4
2 <= 5

## matematicos
2 + 1
2 * 2
2 - 3
2 / 4
2 ^ 5

# Raiz Quadrada
sqrt(64)

#Logaritmo
log10(100)
log2(32)
log(25, base = 5)
log(16, base = 4)

## Criar Variaveis
### altura <- c(1.72, 1.75, 1.80, 1.55, 1.62, 1.90, 1.69)
#### altura (nome do objeto)
#### <- (operador de atribuicao)
#### c (Combinar ou concatenar valores recebidos em um vetor)
#### (1.72, 1.75, 1.80, 1.55, 1.62, 1.90, 1.69) valores armazenados no objeto "altura"

# Tipos de Variáveis
## Variaveis numericas
### Variaveis continuas
altura <- c(1.72, 1.75, 1.80, 1.55, 1.62, 1.90, 1.69)
peso <- c(70.01, 75.02, 80.03, 55.04, 65.05, 90.06, 75.07)
imc <- peso/(altura^2)
imc

### Variaveis discretas
idade <- c(22, 33, 44, 55, 66, 77, 88)
n_pets <- c(0,1,2,3,4,5,6)

## Variaveis categoricas
### variaveis nominais
genero <- c("masculino", "masculino", "masculino", "masculino", "feminino", "feminino", "feminino")
cor_olhos <- c("castanho", "castanho", "castanho", "castanho", "verde", "azul", "castanho")
nome <- c("antonio", "arthur", "benedito", "bruno", "diana", "elisa", "victorya")
logradouro <- c("rua 1", "rua a", "rua b", "rua 2", "rua c", "rua d", "rua 3")
#### nomes extensos/unicos não se enquadram em variaveis categoricas

### Variaveis ordinais 
nivel_instru <- c("ensino medio", "graduacao", "especializacao", "graduacao", "doutorado", "doutorado", "mestrado")
cargo <- c("estagiario", "gerente", "diretor", "coordenador", "cientista", "cientista", "coordenador")

#### Factor
?factor
factor_gener <- factor(genero)
factor_posic <- factor(cargo)

##### Nivel variaveis categoricas
?levels
levels(factor_gener)
levels(factor_posic)

### Variaveis logicas
#### Sempre maiúsculas
vivo <- c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
doente <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)

### Atribuir db a uma variavel.
gpm <- gapminder
?gapminder

#visualizar
gpm
view(gpm)

# Criar um data frame com 5 variaveis.
df_pessoal <- data.frame(nome, genero, altura, peso, imc)
df_pessoal

# Inspecionar db
##head(db,n_linhas)
?head
head(df_pessoal, 5)

## Inspecionar variaveis
?str
str(gpm)
str(df_pessoal)

### Inspecionar o tipo da variável
?class
class(gpm$year)
class(gpm$country)

## Acessando variaveis
df_pessoal$imc
gpm$country

## Retornar um valor com duplicados removidos. 
?unique
unique(gpm$country)

#editar
edit(df_pessoal)
df_pessoal 
df_funcionario <- edit(df_pessoal)
df_funcionario

# Ajuda sobre pacote.
?gapminder

# Ajuda sobre função de um pacote
??dplyr::mutate
## Exemplo mutate
vendas <- data.frame(
    produto = c('a', 'b', 'c', 'd', 'e'),
    preco = c(10000, 1000, 100, 10, 1),
    qnt_vend = c(100, 200, 300, 400, 500)
)

vendas <- vendas %>%
  mutate(receita = (preco * qnt_vend))
vendas  

## Sumariza de dados
?summary
summary(vendas)
summary(vendas$produto)
summary(vendas$preco)

# Filtra dados
## %>% (pipe) encadeia funcoes passando o resultado da etapa anterior para a próxima etapa.
gpm %>%
  filter(continent == "Americas")

americas_1982 <- gpm %>%
  filter(continent == "Americas" & year == 1982)
view(americas_1982)

america_asia_1982 <- gpm %>%
  filter(continent == "Americas" | continent == "Asia")
view(america_asia_1982)
summary(america_asia_1982)

## Selecionar antes de filtrar
### A coluna filtrada deve constar no select
gpm %>%
  select(gdpPercap, lifeExp, year) %>%
  filter(year == 2007)

## Filtrar, agrupar e sumarizar
  gpm %>%
    filter(year == 2007) %>%
    group_by(continent) %>%
    summarise(median_LifeExp = median(lifeExp))
    
# Tabela de frequencia
## A função pull() extrai a coluna total como um vetor simples. 
## Transforma o resultado (que originalmente seria um data frame 1 x 1) em um unico valor numerico correspondente à contagem.
gpm_distinct <- gpm %>%
  summarise(total = n_distinct(country)) %>%
  pull(total)
view(gpm_distinct)

gpm_distinct <- gpm %>%
  group_by(continent) %>%
  summarise(total = n_distinct(country))
view(gpm_distinct)
