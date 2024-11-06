# mutate

library(tidyverse)

car_crash <- read.csv('dados rstudio/Brazil Total highway crashes 2010 - 2023.csv')

#Selecione as variáveis data, tipo_de_ocorrencia, automovel, bicicleta, onibus, caminhao, 
#moto, trator, outros e total.

car_crash %>% 
  select(data, tipo_de_ocorrencia, automovel)

#Selecione todas as variáveis que contenham a palavra feridos.

car_crash %>%
  select(contains('feridos'))

#Selecione todas as variáveis numéricas.

car_crash %>%
  select(where(is.numeric))  %>%
  na.omit()
 
#Selecione todas as variáveis lógicas.
car_crash %>%
  select(where(is.logical))  %>%
  na.omit()

#Selecione todas as variáveis que terminem com a letra o.
car_crash %>%
  select(ends_with('o'))

#Selecione todas as variáveis que iniciem com a letra t.
car_crash %>%
  select(stars_with('t'))

#Filtre as observações com pelo menos 5 carros E 3 motos envolvidos no acidente.
car_crash %>%
  filter(automovel >= 5, moto >=3)


#Filtre as observações com pelo menos 5 carros OU 3 motos envolvidos no acidente.
car_crash %>%
  select(automovel, moto) %>%
  filter

#Filtre as observações com vítimas.
car_crash %>%
  select(tipo_de_ocorrencia) %>%
  filter(contains("Acidente"))


##car_crash %>%
#  group_by(tipo_de_ocorrencia) %>%
#  summarise(
#    tipos = n()
#  )

#Filtre as observações com pelo menos 5 carros OU 3 motos envolvidos no acidente
#E que ocorreram em alguma das seguintes operadoras: “Autopista Regis Bittencourt”, 
#“Autopista Litoral Sul”, “Via Sul”.


car_crash %>%
  select(automovel, moto, lugar_acidente) %>%
  filter((automovel >= 5 | moto >= 3) & 
           lugar_acidente %in% c("Autopista Regis Bittencourt", 
                            "Autopista Litoral Sul", 
                            "Via Sul"))




##USANDO STAR WARS
#Qual é o número total de espécies únicas presentes? Qual a frequência de indivíduos por espécie?

starwars
#unicas
starwars %>%
  group_by(species) %>%
  summarise(
    especies = n()
  ) %>%
  filter(especies == 1) %>%
  print(n=38)

#total
starwars %>%
  group_by(species) %>%
  summarise(
    frequencia = n()
  ) %>%
  print(n=40)

#Calcule a altura média de personagens masculinos e femininos.

starwars %>%
  group_by(sex) %>%
  summarise(
    altura_media = mean(height, na.rm = TRUE)
  )
  
#Qual é a média de idade dos personagens de cada espécie para personagens masculinos?
starwars %>%
  group_by(species) %>%
  filter(sex %in% c('male')) %>%
  summarise(
    media = mean(birth_year, na.rm = TRUE)
  ) %>%
  print(n=33)


#Para cada espécie presente na base de dados, 
#identifique o personagem mais velho e sua idade correspondente.

starwars %>%
  select(species, birth_year) %>%
  group_by(species) %>%
  filter(!is.na(birth_year)) %>%
  arrange(birth_year) %>%
  slice_tail(n=1)


#Utilizando o banco de dados car_crash formate a coluna data em uma data (dd-mm-yyyy);
car_crash %>%
  select(data) %>%
  mutate(
    data = dmy(data)
  )

#Utilizando o banco de dados car_crash formate a coluna horario para o horário do acidente (hh:mm:ss)
car_crash %>%
  select(horario) %>%
  mutate(
    horario = hms(horario)
  )

#Qual o mês com maior quantidade de acidentes?
car_crash %>%
  mutate(mes = month(data)) %>%
  group_by(mes) %>%
  summarise(
    quantidade = n()
  )
 data <- ymd(data)
#Qual ano ocorreram mais acidentes? 
car_crash %>%
  mutate(ano = year(data)) %>%
  group_by(ano) %>%
  summarise(
    quantidade = n()
  )

#Qual horário acontecem menos acidentes?

car_crash %>%
  mutate(horario = hms(horario)) %>%
  mutate(hora = hour(horario)) %>%
  group_by(hora) %>%
  summarise(quantidade = n()) %>%
  arrange(quantidade) %>% #coloca na ordem crescente
  print(n = 25)

#Qual a média, desvio padrão, mediana, Q1 e Q3 para a quantidade de indivíduos 
#classificados como levemente feridos por mês/ano?






