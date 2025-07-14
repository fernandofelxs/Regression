library(readr)
library(dplyr)
data <- read.csv2("RAIS_CTPS_CAGED_2024_MOV.csv", stringsAsFactors = FALSE)
View(data)

data <- data[c("continente", "uf", "sexo", "nivel_instrucao", "salario", "faixa_etaria")]

data <- data[data$continente != "NÃO ESPECIFICADO",]
table(data$continente)

data <- data[data$sexo != 9,]
data <- data %>%
  mutate(sexo = recode(sexo,
                         "1" = "Masculino",
                         "3" = "Feminino"))
table(data$sexo)

data <- data %>%
  mutate(nivel_instrucao = recode(nivel_instrucao,
                       "01" = "Sem instrução ou fundamental incompleto",
                       "02" = "Fundamental completo",
                       "03" = "Médio incompleto",
                       "04" = "Médio completo",
                       "05" = "Superior incompleto",
                       "06" = "Superior completo",
                       "07" = "Pós-graduação",))
table(data$nivel_instrucao)

data <- data[data$faixa_etaria != "0NA",]
data <- data %>%
  mutate(faixa_etaria = recode(faixa_etaria,
                                  "01" = "Menos de 18 anos",
                                  "02" = "18 a 24 anos",
                                  "03" = "25 a 29 anos",
                                  "04" = "30 a 39 anos",
                                  "05" = "40 a 49 anos",
                                  "06" = "50 a 64 anos",
                                  "07" = "65 anos ou mais",))
table(data$faixa_etaria)
  
data <- data[data$uf != "99",]
data <- data %>%
  rename(regiao = uf) %>%
  mutate(regiao = recode(regiao,
                         # Norte
                         "11" = "Norte",  # RO
                         "12" = "Norte",  # AC
                         "13" = "Norte",  # AM
                         "14" = "Norte",  # RR
                         "15" = "Norte",  # PA
                         "16" = "Norte",  # AP
                         "17" = "Norte",  # TO
                         
                         # Nordeste
                         "21" = "Nordeste",  # MA
                         "22" = "Nordeste",  # PI
                         "23" = "Nordeste",  # CE
                         "24" = "Nordeste",  # RN
                         "25" = "Nordeste",  # PB
                         "26" = "Nordeste",  # PE
                         "27" = "Nordeste",  # AL
                         "28" = "Nordeste",  # SE
                         "29" = "Nordeste",  # BA
                         
                         # Sudeste
                         "31" = "Sudeste",  # MG
                         "32" = "Sudeste",  # ES
                         "33" = "Sudeste",  # RJ
                         "35" = "Sudeste",  # SP
                         
                         # Sul
                         "41" = "Sul",  # PR
                         "42" = "Sul",  # SC
                         "43" = "Sul",  # RS
                         
                         # Centro-Oeste
                         "50" = "Centro-Oeste",  # MS
                         "51" = "Centro-Oeste",  # MT
                         "52" = "Centro-Oeste",  # GO
                         "53" = "Centro-Oeste"   # DF
  ))
table(data$regiao)

data$sexo <- factor(data$sexo, levels = c("Masculino", "Feminino"))
data$continente <- factor(data$continente, levels = c("AMÉRICA DO SUL", "ÁFRICA", "AMÉRICA CENTRAL E CARIBE", "AMÉRICA DO NORTE", "ÁSIA", "EUROPA", "OCEANIA"))
data$regiao <- factor(data$regiao, levels = c("Sudeste", "Sul", "Centro-Oeste", "Nordeste", "Norte"))
data$nivel_instrucao <- factor(data$nivel_instrucao, levels = c("Sem instrução ou fundamental incompleto", "Fundamental completo", "Médio incompleto", "Médio completo", "Superior incompleto", "Superior completo", "Pós-graduação"))
data$faixa_etaria <- factor(data$faixa_etaria, levels = c("Menos de 18 anos", "18 a 24 anos", "25 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 64 anos", "65 anos ou mais"))

data$salario <- as.numeric(data$salario)
summary(data$salario)

modelo <- lm(salario ~ sexo + continente + regiao + nivel_instrucao + faixa_etaria, data = data)
summary(modelo)