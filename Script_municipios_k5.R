# ============================================================================
# SCRIPT K5: MAPAS MUNICIPAIS DE COVID-19 POR SEMANA EPIDEMIOLÓGICA
# ============================================================================
# Objetivo: Gerar mapas de bolhas (bubble maps) dos municípios brasileiros
# mostrando a distribuição geográfica de casos novos, óbitos novos,
# taxa de incidência e taxa de mortalidade por COVID-19.
#
# Saídas geradas:
#   1. Mapa de casos novos por município (FIG_CASOS_MUN.png)
#   2. Mapa de óbitos novos por município (FIG_ÓBITOS_MUN.png)
#   3. Mapa de taxa de incidência por município (FIG_INCIDENCIA_MUN.png)
#   4. Mapa de taxa de mortalidade por município (FIG_MORTALIDADE_MUN.png)
#   5. Tabelas com ranking de municípios (TABELA_*_MUNICIPIOS.png)
#   6. Dados de incidência/mortalidade em Excel para análises adicionais
#
# Dependências: Dataset DT_Monitora_COVID gerado pelo Notebook 3
# ============================================================================

# --- BIBLIOTECAS ---
# tidyverse    : Manipulação e visualização de dados
# lubridate    : Manipulação de datas e semanas epidemiológicas
# data.table   : Leitura rápida de CSV (fread)
# openxlsx     : Leitura/escrita de Excel
# hablar       : Funções sum_(), min_() que tratam NA como 0
# extrafont    : Fontes tipográficas para gráficos
# hrbrthemes   : Temas para ggplot2 (theme_ipsum)
# gghighlight  : Destaque condicional em gráficos
# udunits2     : Conversão de unidades (dependência geoespacial)
# sf           : Simple Features para dados geoespaciais
# brazilmaps   : Shapefiles do Brasil (estados, municípios, regiões)
# ggsn         : Barra de escala e norte para mapas
# ggflags      : Bandeiras como elementos gráficos
# ggrepel      : Rótulos sem sobreposição
# gridExtra    : Arranjo de múltiplos gráficos e tabelas (tableGrob)
library(tidyverse)
library(lubridate)
library(data.table)
library(openxlsx)
library(hablar)
library(extrafont)
library(hrbrthemes)
library(gghighlight)
library(udunits2)
library(sf)
library(brazilmaps)
library(ggsn)
library(ggflags)
library(ggrepel)
library(gridExtra)

# Permitir sobreposição ilimitada de rótulos nos mapas
options(ggrepel.max.overlaps = Inf)

# --- VARIÁVEL DE CONTROLE: SEMANA EPIDEMIOLÓGICA ---
# ATUALIZAR a cada nova SE com o formato "SE XX"
SE="SE 18"

# ============================================================================
# CONFIGURAÇÃO DE DIRETÓRIOS E IMPORTAÇÃO DE DADOS
# NOTA: Descomente o caminho correspondente ao seu usuário
# ============================================================================

# Pasta para salvar figuras
#LINDINHO
path = paste0("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/2024/", SE, " de 2024/")

#path = paste0("C:/Users/narmada.garcia/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/2023/", SE, " de 2023/")

#GOLDEN BUTT
#path = paste0("C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/", SE, " de 2022/")

#olinio
#path = paste0("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/", SE, " de 2023/")

#path = paste0("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/")

#Criar pasta para salvar arquivos
#LINDINHO
dir.create(paste0("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/2024/", SE, " de 2024"))

#dir.create(paste0("C:/Users/narmada.garcia/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/2023/", SE, " de 2023"))


#GOLDEN BUTT
#dir.create(paste0("C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/", SE, " de 2022"))

#olinio
#dir.create(paste0("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/", SE, " de 2023"))


# ============================================================================
# IMPORTAÇÃO DOS DADOS
# - DT_CONTROL: Dataset consolidado com todos os níveis hierárquicos
# - DT_MUN: Base de municípios com coordenadas LAT/LONG
# - dadostcu2019: Estimativas populacionais TCU 2019
# ATUALIZAR: data do arquivo CSV e caminho do usuário
# ============================================================================

#LINDINHO
DT_CONTROL = fread(file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/ROTINAS/1_Data/Dados_Controle/DT_Monitora_COVID_20240504.csv", encoding = "UTF-8") #ATUALIZAR

DT_MUN = read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/ROTINAS/1_Data/DADOSMUNBR_novo.xlsx", sheet = 1)

dadostcu2019 <- readxl::read_excel("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/ROTINAS/1_Data/estimativa_TCU_2019_20200427.xls", sheet = 4)


#plinio
#DT_CONTROL = fread(file = "C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/ROTINAS/1_Data/Dados_Controle/DT_Monitora_COVID_20231230.csv", encoding = "UTF-8") #ATUALIZAR
#
#DT_MUN = read.xlsx("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/ROTINAS/1_Data/DADOSMUNBR_novo.xlsx", sheet = 1)
#
#dadostcu2019 <- readxl::read_excel("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/ROTINAS/1_Data/estimativa_TCU_2019_20200427.xls", sheet = 4)


# ============================================================================
# PREPARAÇÃO E ENRIQUECIMENTO DOS DADOS
# - Cria código IBGE de 6 e 7 dígitos para municípios
# - Adiciona ano epidemiológico, coordenadas geográficas e população
# - Calcula data mínima por semana epidemiológica
# ============================================================================
dadostcu2019 <- dadostcu2019 %>%
  mutate(CODIGOMUN7 = str_c(codigoUF, codigoMUN)) %>%
  mutate(CODIGOMUN7_2 = parse_number(str_sub(CODIGOMUN7, end = 7))) %>%
  mutate(CODIGOMUN7 = parse_number(str_sub(CODIGOMUN7, end = 6))) %>%
  mutate(CODIGOMUN7 = case_when(is.na(CODIGOMUN7) ~ codigoUF, TRUE ~ CODIGOMUN7)) %>%
  select(CODIGOMUN7, POP_TCU2019, CODIGOMUN7_2)

DT_CONTROL = DT_CONTROL %>%
  mutate(anoEpi = epiyear(data))

DT_MUN = DT_MUN %>% select(CODIGOMUN, LAT, LONG) %>% mutate(CODIGOMUN = parse_number(str_sub(CODIGOMUN, end = 6)))

# Converter strings vazias para NA
DT_CONTROL = sjmisc::set_na(DT_CONTROL, na = "")

# Converter data e vincular coordenadas geográficas
DT_CONTROL = DT_CONTROL %>% mutate(data = ymd(data)) %>% left_join(DT_MUN, by = c("CODIGOLOCAL" = "CODIGOMUN"))

# Vincular população TCU 2019
DT_CONTROL = DT_CONTROL %>% full_join(dadostcu2019, by = c("CODIGOLOCAL" = "CODIGOMUN7"))

DT_CONTROL = DT_CONTROL %>%
  rename(POP_TCU2019 = POP_TCU2019.x)

# Calcular primeiro dia de cada semana epidemiológica
DT_CONTROL = DT_CONTROL %>%
  group_by(anoEpi, semanaEpi) %>%
  mutate(minData = min_(data)) %>%
  ungroup()

# ============================================================================
# AGREGAÇÃO SEMANAL POR MUNICÍPIO
# Soma casos e óbitos novos de cada município dentro de cada SE
# ============================================================================
DT_CONTROL_SE = DT_CONTROL %>%
  group_by(ABRANGENCIA, CODIGOLOCAL, CODIGOMUN7_2, NOMELOCAL, anoEpi, semanaEpi, minData, LAT, LONG) %>%
  summarise(casosNovos=sum(casosNovos), obitosNovos=sum(obitosNovos)) %>%
  ungroup()

# ============================================================================
# CLASSIFICAÇÃO DOS MUNICÍPIOS POR NÚMERO DE CASOS/ÓBITOS NOVOS
# Categorias para CASOS: 1(0), 2(1-200), 3(201-500), 4(501+)
# Categorias para ÓBITOS: 1(0), 2(1-5), 3(6-10), 4(11+)
# Filtra apenas a última SE disponível e municípios válidos
# ============================================================================
tbMapa = DT_CONTROL_SE %>% 
  filter(ABRANGENCIA == 7, minData == max(minData)) %>% 
  select(minData, anoEpi, semanaEpi, LONG, LAT, CODIGOLOCAL, CODIGOMUN7_2, casosNovos, obitosNovos) %>% 
  mutate(catsCasos = case_when(casosNovos <= 0 ~ "1",
                               between(casosNovos, 1, 200) ~ "2",
                               between(casosNovos, 201, 500) ~ "3",
                               casosNovos >= 501 ~ "4", 
                               TRUE ~ NA_character_)) %>% 
  mutate(catsObitos = case_when(obitosNovos <= 0 ~ "1",
                                between(obitosNovos, 1, 5) ~ "2",
                                between(obitosNovos, 6, 10) ~ "3",
                                obitosNovos >= 11 ~ "4", 
                                TRUE ~ NA_character_)) %>% 
  mutate_at(c("LONG", "LAT"), ~as.numeric(.)) %>% 
  rename(lat = "LAT", long = "LONG")

tbMapa <- tbMapa %>% 
  filter(CODIGOMUN7_2 != is.na(CODIGOMUN7_2))


##An?lise para um m?s
#DT_CONTROL_SE<-DT_CONTROL_SE %>% 
#  filter(ABRANGENCIA == 7, minData >= "2022-07-16") %>% 
#  group_by(LONG, LAT, CODIGOLOCAL, CODIGOMUN7_2) %>% 
#  summarise(casosNovos = sum(casosNovos), obitosNovos = sum(obitosNovos)) %>% 
#  filter(LAT!=is.na(LAT))
#
#
#tbMapa = DT_CONTROL_SE %>% 
#  mutate(catsCasos = case_when(casosNovos <= 1 ~ "1",
#                               between(casosNovos, 2, 1200) ~ "2",
#                               between(casosNovos, 1201, 2700) ~ "3",
#                               casosNovos > 2701 ~ "4", 
#                               TRUE ~ NA_character_)) %>% 
#  mutate(catsObitos = case_when(obitosNovos <= 1 ~ "1",
#                                between(obitosNovos, 2, 10) ~ "2",
#                                between(obitosNovos, 11, 35) ~ "3",
#                                obitosNovos >= 35 ~ "4", 
#                                TRUE ~ NA_character_)) %>% 
#  mutate_at(c("LONG", "LAT"), ~as.numeric(.)) %>% 
#  rename(lat = "LAT", long = "LONG")


table(tbMapa$catsCasos, exclude = F)
table(tbMapa$catsObitos, exclude = F)

#LINDINHO
Mun_BR<-read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/Municípios_BR.xlsx", sheet =  "Planilha2")

#Mun_BR<-read.xlsx("C:/Users/narmada.garcia/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

#GOLDEN BUTT
#Mun_BR<-read.xlsx("C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

#pLINIO
#Mun_BR<-read.xlsx("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")


tbMapa<-tbMapa %>% 
  left_join(Mun_BR, by = c("CODIGOMUN7_2" = "CODIBGE"), na_matches= "never")

#Elaborando figura (n?mero de casos)
#gg13B = ggplot(filter(tbMapa, !is.na(catsCasos), !str_detect(CODIGOLOCAL, "0000$"))) + 
#  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
#  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) + 
#  geom_point(aes(x = long, y = lat, size = casosNovos, color = catsCasos), shape = 19, alpha = 0.5)+
#  scale_color_manual(values = c("1" = "white", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
#                     name = paste0("Casos novos de covid-19\n(Total = ", format(sum_(tbMapa$casosNovos), big.mark = ".", decimal.mark = ","), ")"), 
#                     labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 1], na.rm = T)),  " munic?pios)"), 
#                                paste0("1-500 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 2], na.rm = T), big.mark = ".", decimal.mark = ","), " munic?pios)"), 
#                                paste0("501-1000 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 3], na.rm = T), big.mark = ".", decimal.mark = ",")," munic?pios)"), 
#                                paste0("1001-", max_(tbMapa$casosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 4], na.rm = T), big.mark = ".", decimal.mark = ","),  " munic?pios)")))+
#  scale_size_continuous(breaks = c(0, 500, 1000, max_(tbMapa$casosNovos)), range = c(0, 10), 
#                        name = paste0("Casos novos de covid-19\n(Total = ", format(sum_(tbMapa$casosNovos), big.mark = "."), ")"), 
#                        labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 1], na.rm = T))," munic?pios)"), 
#                                   paste0("1-500 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 2], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("501-1000 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 3], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("1001-", max_(tbMapa$casosNovos)," (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == 4], na.rm = T), big.mark = ".")," munic?pios)"))) + 
#  gghighlight(catsCasos >= 3, unhighlighted_params = list(alpha = 0.2, color = "steelblue", size=0.8), use_direct_label = F, keep_scales = T) + 
#  theme_void(base_size = 8) +
#  labs(title = paste0("Casos novos de covid-19 por munic?pio de resid?ncia (", SE, ")", x = "", y = ""),
#       subtitle = "Fonte: Secretarias Estaduais de Sa?de/Painel Coronav?rus Brasil") +
#  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
#        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
#  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size=15)) +
#  theme(legend.text = element_text(size=15))+
#  scalebar(filter(tbMapa, !is.na(catsCasos), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
#           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")


# ============================================================================
# MAPA DE BOLHAS: CASOS NOVOS POR MUNICÍPIO
# - Camada base: limites municipais (cinza claro) e estaduais (preto)
# - Pontos: tamanho proporcional ao nº de casos, cor por categoria
#   Transparente(0), Azul(1-200), Dourado(201-500), Vermelho(501+)
# - gghighlight: destaca municípios com categorias 3-4 (alto nº de casos)
# - Camada adicional: pontos azuis para categoria 2 (1-200 casos)
# - Escala de 500km no canto inferior direito
# ============================================================================
tbMapa$catsCasos2 <- tbMapa$catsCasos


gg13B = ggplot(filter(tbMapa, !is.na(catsCasos), !str_detect(CODIGOLOCAL, "0000$"))) + 
  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) +
  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "black", size = .5) + 
  geom_point(aes(x = long, y = lat, size = casosNovos, color = catsCasos), shape = 19, alpha = 0.5)+
  scale_color_manual(values = c("1" = "transparent", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
                     name = paste0("Casos novos de covid-19\n(Total = ", format(sum_(tbMapa$casosNovos), big.mark = ".", decimal.mark = ","), ")"), 
                     labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "1"], na.rm = T), big.mark = "."),  " municípios)"), 
                                paste0("1-200 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "2"], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                paste0("201-500 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "3"], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                paste0("501-", max_(tbMapa$casosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","),  " municípios)"))) +
  scale_size_continuous(breaks = c(1, 200, 500, max_(tbMapa$casosNovos)), range = c(0, 10), 
                        name = paste0("Casos novos de covid-19\n(Total = ", format(sum_(tbMapa$casosNovos), big.mark = ".", decimal.mark = ","), ")"), 
                        labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "1"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("1-200 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "2"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("201-500 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "3"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("501-", max_(tbMapa$casosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsCasos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"))) + 
  gghighlight(catsCasos >= 3, unhighlighted_params = list(alpha = 0.2, color = "transparent", size=0.1), use_direct_label = F, keep_scales = T) + 
  geom_point(data=filter(tbMapa, catsCasos2=="2"), aes(x = long, y = lat), shape = 19, alpha = 0.5, color="steelblue", size=1.0)+
  theme_void(base_size = 8) +
  labs(title = paste0("Casos novos de covid-19 por município de residência (", SE, ")", x = "", y = ""),
       subtitle = "Fonte: Secretarias Estaduais de Saúde/Painel Coronavírus Brasil") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size=15)) +
  theme(legend.text = element_text(size=15))+
  scalebar(filter(tbMapa, !is.na(catsCasos), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")

#Elaborando figura (n?mero de ?bitos)
#gg13_ob = ggplot(filter(tbMapa, !is.na(catsObitos), !str_detect(CODIGOLOCAL, "0000$"))) + 
#  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
#  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) + 
#  geom_point(aes(x = long, y = lat, size = obitosNovos, color = catsObitos), shape = 19, alpha = 0.5)+
#  scale_color_manual(values = c("1" = "whitesmoke", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
#                     name = paste0("?bitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = ".", decimal.mark = ","), ")"), 
#                     labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T)),  " munic?pios)"), 
#                                paste0("1-5 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".", decimal.mark = ","), " munic?pios)"), 
#                                paste0("6-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".", decimal.mark = ",")," munic?pios)"), 
#                                paste0("11-", max_(tbMapa$obitosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","),  " munic?pios)")))+
#  scale_size_continuous(breaks = c(1, 10, 20, max_(tbMapa$obitosNovos)), range = c(1, 10), 
#                        name = paste0("?bitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = "."), ")"), 
#                        labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T))," munic?pios)"), 
#                                   paste0("1-5 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("6-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("11-", max_(tbMapa$obitosNovos)," (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".")," munic?pios)"))) + 
#  gghighlight(catsObitos >= 3, unhighlighted_params = list(alpha = 0.2, color = "steelblue", size=0.8), use_direct_label = F, keep_scales = T) + 
#  theme_void(base_size = 8) +
#  labs(title = paste0("?bitos novos de covid-19 por munic?pio de resid?ncia (", SE, ")", x = "", y = ""),
#       subtitle = "Fonte: Secretarias Estaduais de Sa?de/Painel Coronav?rus Brasil") +
#  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
#        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
#  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size=15)) +
#  theme(legend.text = element_text(size=15))+
#  scalebar(filter(tbMapa, !is.na(catsObitos), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
#           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")

#gg13_ob = ggplot(filter(tbMapa, catsObitos >= 2, !is.na(catsObitos), !str_detect(CODIGOLOCAL, "0000$"))) + 
#  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
#  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) + 
#  geom_point(aes(x = long, y = lat, size = obitosNovos, color = catsObitos), shape = 19, alpha = 0.5)+
#  scale_color_manual(values = c("1" = "whitesmoke", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
#                     name = paste0("?bitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = ".", decimal.mark = ","), ")"), 
#                     labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T), big.mark = "."),  " munic?pios)"), 
#                                paste0("1-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".", decimal.mark = ","), " munic?pios)"), 
#                                paste0("11-20 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".", decimal.mark = ",")," munic?pios)"), 
#                                paste0("21-", max_(tbMapa$obitosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","),  " munic?pios)")))+
#  scale_size_continuous(breaks = c(0, 10, 20, max_(tbMapa$obitosNovos)), range = c(1, 10), 
#                        name = paste0("?bitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = "."), ")"), 
#                        labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("1-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("11-20 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("21-", max_(tbMapa$obitosNovos)," (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".")," munic?pios)"))) + 
#  gghighlight(catsObitos >= 3, unhighlighted_params = list(alpha = 0.2, color = "steelblue", size=0.8), use_direct_label = F, keep_scales = T) + 
#  theme_void(base_size = 8) +
#  labs(title = paste0("?bitos novos de covid-19 por munic?pio de resid?ncia (", SE, ")", x = "", y = ""),
#       subtitle = "Fonte: Secretarias Estaduais de Sa?de/Painel Coronav?rus Brasil") +
#  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
#        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
#  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size=15)) +
#  theme(legend.text = element_text(size=15))+
#  scalebar(filter(tbMapa, !is.na(catsObitos), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
#           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")

# ============================================================================
# MAPA DE BOLHAS: ÓBITOS NOVOS POR MUNICÍPIO
# Mesma estrutura do mapa de casos, mas para óbitos.
# Categorias: Transparente(0), Azul(1-10), Dourado(11-20), Vermelho(21+)
# ============================================================================
tbMapa$catsObitos2 <- tbMapa$catsObitos


gg13_ob = ggplot(filter(tbMapa, !is.na(catsObitos), !str_detect(CODIGOLOCAL, "0000$"))) + 
  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) +
  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
  geom_point(aes(x = long, y = lat, size = obitosNovos, color = catsObitos), shape = 19, alpha = 0.5)+
  scale_color_manual(values = c("1" = "transparent", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
                     name = paste0("Óbitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = ".", decimal.mark = ","), ")"), 
                     labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T), big.mark = "."),  " municípios)"), 
                                paste0("1-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                paste0("11-20 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                paste0("21-", max_(tbMapa$obitosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","),  " municípios)"))) +
  scale_size_continuous(breaks = c(1, 10, 20, max_(tbMapa$obitosNovos)), range = c(0, 10), 
                        name = paste0("Óbitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = ".", decimal.mark = ","), ")"), 
                        labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("1-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("11-20 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("21-", max_(tbMapa$obitosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"))) + 
  gghighlight(catsObitos >= 3, unhighlighted_params = list(alpha = 0.2, color = "transparent", size=0.1), use_direct_label = F, keep_scales = T) + 
  geom_point(data=filter(tbMapa, catsObitos2=="2"), aes(x = long, y = lat), shape = 19, alpha = 0.5, color="steelblue", size=1.0)+
  theme_void(base_size = 8) +
  labs(title = paste0("Óbitos novos de covid-19 por município de residência (", SE, ")", x = "", y = ""),
       subtitle = "Fonte: Secretarias Estaduais de Saúde/Painel Coronavírus Brasil") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size=15)) +
  theme(legend.text = element_text(size=15))+
  scalebar(filter(tbMapa, !is.na(catsObitos), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")


# Salvando mapas de casos e óbitos como PNG em alta resolução
ggsave(gg13B, width = 300, height = 200, units = "mm", file = paste0(path,"FIG_CASOS_MUN.png"), dpi = "retina")
ggsave(gg13_ob, width = 300, height = 200, units = "mm", file = paste0(path,"FIG_ÓBITOS_MUN.png"), dpi = "retina")

# ============================================================================
# TABELAS DE RANKING: MUNICÍPIOS COM MAIOR Nº DE CASOS (CATEGORIAS 3-4)
# Filtra municípios com 201+ casos novos na SE, ordena por volume
# e exporta como imagem PNG usando tableGrob do gridExtra
# ============================================================================

tbMapa_tab<-tbMapa %>% 
  filter(catsCasos>=3)

#LINDINHO
Mun_BR<-read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/Municípios_BR.xlsx",
                  sheet =  "Planilha2")

#Mun_BR<-read.xlsx("C:/Users/narmada.garcia/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

#GOLDEN BUTT
#Mun_BR<-read.xlsx("C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

#plinio
#Mun_BR<-read.xlsx("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")


tbMapa_tab<-tbMapa_tab %>% 
  arrange(desc(tbMapa_tab$casosNovos)) %>% 
  rename("Casos novos por data de notificação"= "casosNovos") %>% 
  rename("UF" = "nomeUF") %>% 
  rename("Município" = "nomeMUN")

##Para o m?s:
#tbMapa_tab <- tbMapa_tab[, c(9,10,5)]

##Para a SE:
tbMapa_tab <- tbMapa_tab[, c(12,13,8)]


png(paste0(path,"TABELA_CASOS_MUNICIPIOS.png"), height = ,30*nrow(tbMapa_tab), width = 200*ncol(tbMapa_tab))
p<-tableGrob(tbMapa_tab)
grid.arrange(p)
dev.off()


# ============================================================================
# TABELAS DE RANKING: MUNICÍPIOS COM MAIOR Nº DE ÓBITOS (CATEGORIAS 3-4)
# Mesma lógica da tabela de casos, mas para óbitos (6+ óbitos novos)
# ============================================================================

tbMapa_tab<-tbMapa %>%
  filter(catsObitos>=3)

#LINDINHO
Mun_BR<-read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/Municípios_BR.xlsx",
                  sheet =  "Planilha2")
#Mun_BR<-read.xlsx("C:/Users/narmada.garcia/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx",
#                  sheet =  "Planilha2")

#GOLDEN BUTT
#Mun_BR<-read.xlsx("C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

#plinio
#Mun_BR<-read.xlsx("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

tbMapa_tab<-tbMapa_tab %>% 
  arrange(desc(tbMapa_tab$obitosNovos)) %>% 
  rename("Óbitos novos por data de notificação"= "obitosNovos") %>% 
  rename("UF" = "nomeUF") %>% 
  rename("Município" = "nomeMUN")

##Para o m?s:
#tbMapa_tab <- tbMapa_tab[, c(9,10,6)]

##Para a SE:
tbMapa_tab <- tbMapa_tab[, c(12,13,9)]

png(paste0(path, "TABELA_OBITOS_MUNICIPIOS.png"), height = ,50*nrow(tbMapa_tab), width = 200*ncol(tbMapa_tab))
p<-tableGrob(tbMapa_tab)
grid.arrange(p)
dev.off()

# ============================================================================
# MAPA DE TAXA DE INCIDÊNCIA (CASOS POR 100 MIL HABITANTES)
# Vincula população TCU 2019 e calcula: (casosNovos / POP) * 100.000
# Classificação em 5 categorias baseadas em quintis/Jenks
# Cores: Transparente → Verde → Salmão → Laranja → Vermelho
# ============================================================================
tbMapa = tbMapa  %>% full_join(dadostcu2019, by = c("CODIGOLOCAL" = "CODIGOMUN7"))

tbMapa$incidencia<-round((tbMapa$casosNovos/tbMapa$POP_TCU2019)*100000,1)

# Resumo estatístico para definir pontos de corte das categorias
summary(tbMapa$incidencia)

#tbMapa_incidencia = tbMapa %>% 
#  mutate(catsincidencia = case_when(incidencia <= 46.64 ~ "1",
#                               between(incidencia, 46.65, 318.26) ~ "2",
#                               between(incidencia, 318.27, 630.9) ~ "3",
#                               between(incidencia, 631, 917.36) ~ "4",
#                               incidencia > 917.37 ~ "5", 
#                               TRUE ~ NA_character_))

#tbMapa_incidencia = tbMapa %>% 
#  mutate(catsincidencia = case_when(incidencia <= 18.25 ~ "1",
#                               between(incidencia, 18.26, 86.65) ~ "2",
#                               between(incidencia, 86.66, 143.85) ~ "3",
#                               between(incidencia, 143.86, 204.85) ~ "4",
#                               incidencia > 204.86 ~ "5", 
#                               TRUE ~ NA_character_))

tbMapa_incidencia = tbMapa %>% 
  mutate(catsincidencia = case_when(incidencia <= 20.47 ~ "1",
                               between(incidencia, 20.48, 72.85) ~ "2",
                               between(incidencia, 72.86, 124.61) ~ "3",
                               between(incidencia, 124.62, 171.20) ~ "4",
                               incidencia > 171.21 ~ "5", 
                               TRUE ~ NA_character_))

table(tbMapa_incidencia$catsincidencia, exclude = F)

# ============================================================================
# MAPA DE TAXA DE MORTALIDADE (ÓBITOS POR 100 MIL HABITANTES)
# Calcula: (obitosNovos / POP) * 100.000
# Classificação em 5 categorias baseadas em quintis/Jenks
# ============================================================================
tbMapa$mortalidade<-round((tbMapa$obitosNovos/tbMapa$POP_TCU2019)*100000,1)

# Resumo estatístico para definir pontos de corte
summary(tbMapa$mortalidade)

#tbMapa_mortalidade = tbMapa %>% 
#  mutate(catsmortalidade = case_when(mortalidade<= 2.1 ~ "1",
#                                    between(mortalidade, 2.2, 6.72) ~ "2",
#                                    between(mortalidade, 6.73, 13.9) ~ "3",
#                                    between(mortalidade, 14, 30.21) ~ "4",
#                                    mortalidade > 30.22 ~ "5", 
#                                    TRUE ~ NA_character_))

#tbMapa_mortalidade = tbMapa %>% 
#  mutate(catsmortalidade = case_when(mortalidade <= 0.25 ~ "1",
#                                    between(mortalidade, 0.26, 1.25) ~ "2",
#                                    between(mortalidade, 1.26, 3.15) ~ "3",
#                                    between(mortalidade, 3.16, 5.05) ~ "4",
#                                    mortalidade > 5.06 ~ "5", 
#                                    TRUE ~ NA_character_))

tbMapa_mortalidade = tbMapa %>% 
  mutate(catsmortalidade = case_when(mortalidade <= 0.21 ~ "1",
                                    between(mortalidade, 0.22, 0.44) ~ "2",
                                    between(mortalidade, 0.45, 0.72) ~ "3",
                                    between(mortalidade, 0.73, 1.41) ~ "4",
                                    mortalidade > 1.42 ~ "5", 
                                    TRUE ~ NA_character_))

table(tbMapa_mortalidade$catsmortalidade, exclude = F)


# ============================================================================
# GERAÇÃO DOS MAPAS DE INCIDÊNCIA E MORTALIDADE
# Mesma estrutura dos mapas de casos/óbitos, mas usando taxas por 100mil hab.
# ============================================================================

#gg13C = ggplot(filter(tbMapa_incidencia, !is.na(catsincidencia), !str_detect(CODIGOLOCAL, "0000$"))) + 
#  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
#  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) + 
#  geom_point(aes(x = long, y = lat, size = incidencia, color = catsincidencia), shape = 19, alpha = 0.5)+
#  scale_color_manual(values = c("1" = "steelblue", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
#                     name = paste0("Taxa de incid?ncia de covid-19 \n (/100 mil hab.)"), 
#                     labels = c(paste0("<50 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == "1"], na.rm = T), big.mark = ".", decimal.mark = ",")," munic?pios)"), 
#                                paste0("51-500 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 2], na.rm = T), big.mark = ".", decimal.mark = ","), " munic?pios)"), 
#                                paste0("501-1000 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 3], na.rm = T), big.mark = ".", decimal.mark = ",")," munic?pios)"), 
#                                paste0("1001-", max_(tbMapa_incidencia$incidencia), " (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == "4"], na.rm = T), big.mark = ".", decimal.mark = ","),  " munic?pios)")))+
#  scale_size_continuous(breaks = c(1, 1000, 1001, max_(tbMapa_incidencia$incidencia)), range = c(1, 10), 
#                        name = paste0("Taxa de incid?ncia de covid-19 \n (/100 mil hab.)"), 
#                        labels = c(paste0("<50 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == "1"], na.rm = T), big.mark = ".", decimal.mark = ",")," munic?pios)"), 
#                                   paste0("51-500 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == "2"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("501-1000 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == "3"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("1001-", max_(tbMapa_incidencia$incidencia)," (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == "4"], na.rm = T), big.mark = ".")," munic?pios)"))) + 
#  gghighlight(catsincidencia >= 3, unhighlighted_params = list(alpha = 0.2, color = "steelblue", size=0.8), use_direct_label = F, keep_scales = T) + 
#  theme_void(base_size = 8) +
#  labs(title = paste0("Taxa de incid?ncia de casos novos de covid-19 por munic?pio de resid?ncia (", SE, ")", x = "", y = ""),
#       subtitle = "Fonte: Secretarias Estaduais de Sa?de/Painel Coronav?rus Brasil") +
#  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
#        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
#  theme(legend.text = element_text(size=15))+
#  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size = 15)) + 
#  scalebar(filter(tbMapa_incidencia, !is.na(catsincidencia), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
#           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")

tbMapa_incidencia$catsincidencia2 <- tbMapa_incidencia$catsincidencia

gg13C = ggplot(filter(tbMapa_incidencia, !is.na(catsincidencia), !str_detect(CODIGOLOCAL, "0000$"))) + 
  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) + 
  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
  geom_point(aes(x = long, y = lat, size = incidencia, color = catsincidencia), shape = 19, alpha = 0.5)+
  scale_color_manual(values = c("1" = "transparent", "2" = "darkolivegreen1", "3" = "lightsalmon2", "4" = "darkorange", "5" = "darkred"), 
                     name = paste0("Taxa de incidência de covid-19 \n (/100 mil hab.)"), 
                     labels = c(paste0("0-20,47 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 1], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                paste0("20,48-72,85 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 2], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                paste0("72,86-124,62 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 3], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                paste0("124,63-171,20 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 4], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                paste0("171,21-", max_(tbMapa_incidencia$incidencia), " (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 5], na.rm = T), big.mark = "", decimal.mark = ","),  " municípios)")))+
  scale_size_continuous(breaks = c(20.48, 72.85, 124.62, 171.20, max_(tbMapa_incidencia$incidencia)), range = c(1, 10), 
                        name = paste0("Taxa de incidência de covid-19 \n (/100 mil hab.)"), 
                        labels = c(paste0("0-20,47 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 1], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                   paste0("20,48-72,85 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 2], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                   paste0("72,86-124,62 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 3], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                   paste0("124,63-171,20 (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 4], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                   paste0("171,21-", max_(tbMapa_incidencia$incidencia)," (", format(n_distinct(tbMapa_incidencia$CODIGOLOCAL[tbMapa_incidencia$catsincidencia == 5], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"))) + 
  #gghighlight(catsincidencia >= 2, unhighlighted_params = list(alpha = 0.2, color = "steelblue", size=0.8), use_direct_label = F, keep_scales = T) + 
  gghighlight(catsincidencia >= 3, unhighlighted_params = list(alpha = 0.2, color = "transparent", size=0.1), use_direct_label = F, keep_scales = T) + 
  geom_point(data=filter(tbMapa_incidencia, catsincidencia2=="2"), aes(x = long, y = lat), shape = 19, alpha = 0.5, color="darkolivegreen1", size=1.8)+
  theme_void(base_size = 8) +
  labs(title = paste0("Taxa de incidência de covid-19 por município de residência (", SE, ")", x = "", y = ""),
       subtitle = "Fonte: Secretarias Estaduais de Saúde/Painel Coronavírus Brasil") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size = 15)) + 
  scalebar(filter(tbMapa_incidencia, !is.na(catsincidencia), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")


#gg13C_ob = ggplot(filter(tbMapa_mortalidade, !is.na(catsmortalidade), !str_detect(CODIGOLOCAL, "0000$"))) + 
#  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
#  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) + 
#  geom_point(aes(x = long, y = lat, size = mortalidade, color = catsmortalidade), shape = 19, alpha = 0.5)+
#  scale_color_manual(values = c("1" = "steelblue", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
#                     name = paste0("Taxa de mortalidade de covid-19 \n (/100 mil hab.)"), 
#                     labels = c(paste0("0-1 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "1"], na.rm = T)),  " munic?pios)"), 
#                                paste0("2-20 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "2"], na.rm = T), big.mark = ".", decimal.mark = ","), " munic?pios)"), 
#                                paste0("21-30 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "3"], na.rm = T), big.mark = ".", decimal.mark = ",")," munic?pios)"), 
#                                paste0("31-", max_(tbMapa_mortalidade$mortalidade), " (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "4"], na.rm = T), big.mark = ".", decimal.mark = ","),  " munic?pios)")))+
#  scale_size_continuous(breaks = c(1, 20, 30, max_(tbMapa_mortalidade$mortalidade)), range = c(1, 10), 
#                        name = paste0("Taxa de mortalidade de covid-19 \n (/100 mil hab.)"), 
#                        labels = c(paste0("0-1 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "1"], na.rm = T))," munic?pios)"), 
#                                   paste0("2-20 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "2"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("21-30 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "3"], na.rm = T), big.mark = ".")," munic?pios)"), 
#                                   paste0("31-", max_(tbMapa_mortalidade$mortalidade)," (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "4"], na.rm = T), big.mark = ".")," munic?pios)"))) + 
#  gghighlight(catsmortalidade >= 3, unhighlighted_params = list(alpha = 0.2, color = "steelblue", size=0.8), use_direct_label = F, keep_scales = T) + 
#  theme_void(base_size = 8) +
#  labs(title = paste0("Taxa de mortalidade de ?bitos novos de covid-19 por munic?pio de resid?ncia (", SE, ")", x = "", y = ""),
#       subtitle = "Fonte: Secretarias Estaduais de Sa?de/Painel Coronav?rus Brasil") +
#  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
#        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
#  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size = 15)) + 
#  theme(legend.text = element_text(size=15))+
#  scalebar(filter(tbMapa_mortalidade, !is.na(catsmortalidade), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
#           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")

tbMapa_mortalidade$catsmortalidade2 <- tbMapa_mortalidade$catsmortalidade

gg13C_ob = ggplot(filter(tbMapa_mortalidade, !is.na(catsmortalidade), !str_detect(CODIGOLOCAL, "0000$"))) + 
  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) + 
  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
  geom_point(aes(x = long, y = lat, size = mortalidade, color = catsmortalidade), shape = 19, alpha = 0.5)+
  scale_color_manual(values = c("1" = "transparent", "2" = "darkolivegreen1", "3" = "lightsalmon2", "4" = "darkorange", "5" = "darkred"), 
                     name = paste0("Taxa de mortalidade de covid-19 \n (/100 mil hab.)"), 
                     labels = c(paste0("0-0,21 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "1"], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                paste0("0,22-0,44 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == 2], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                paste0("0,45-0,72 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == 3], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                paste0("0,73-1,41 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == 4], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                paste0("1,42-", max_(tbMapa_mortalidade$mortalidade), " (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "5"], na.rm = T), big.mark = "", decimal.mark = ","),  " municípios)")))+
  scale_size_continuous(breaks = c(0.22, 0.44, 0.72, 1.41, max_(tbMapa_mortalidade$mortalidade)), range = c(1, 10), 
                        name = paste0("Taxa de mortalidade de covid-19 \n (/100 mil hab.)"), 
                        labels = c(paste0("0-0,21 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "1"], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                   paste0("0,22-0,44 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == 2], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                   paste0("0,45-0,72 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == 3], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                   paste0("0,73-1,41 (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == 4], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                   paste0("1,42-", max_(tbMapa_mortalidade$mortalidade)," (", format(n_distinct(tbMapa_mortalidade$CODIGOLOCAL[tbMapa_mortalidade$catsmortalidade == "5"], na.rm = T), big.mark = "", decimal.mark = ",")," municípios)"))) + 
  #gghighlight(catsmortalidade >= 2, unhighlighted_params = list(alpha = 0.2, color = "steelblue", size=0.8), use_direct_label = F, keep_scales = T) + 
  gghighlight(catsmortalidade >= 3, unhighlighted_params = list(alpha = 0.2, color = "transparent", size=0.1), use_direct_label = F, keep_scales = T) + 
  geom_point(data=filter(tbMapa_mortalidade, catsmortalidade2=="2"), aes(x = long, y = lat), shape = 19, alpha = 0.5, color="darkolivegreen1", size=1.8)+
  theme_void(base_size = 8) +
  labs(title = paste0("Taxa de mortalidade de covid-19 por município de residência (", SE, ")", x = "", y = ""),
       subtitle = "Fonte: Secretarias Estaduais de Saúde/Painel Coronavírus Brasil") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size = 15)) + 
  theme(legend.text = element_text(size=15))+
  scalebar(filter(tbMapa_mortalidade, !is.na(catsmortalidade), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")


#Salvando imagem
write.xlsx(tbMapa_incidencia, file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/MAPAS KERNEL INC E MORT COVID/2024/SE/SE 18/tbMapa_tab_incid_SE.xlsx", overwrite = T) ###ATUALIZAR!!!
#write.csv2(tbMapa_incidencia, file = "C:/Users/eucilene.santana/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/MAPAS KERNEL INC E MORT COVID/2023/SE/SE 52/tbMapa_tab_incid_SE_2.csv", overwrite = T) ###ATUALIZAR!!!
write.xlsx(tbMapa_mortalidade, file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/MAPAS KERNEL INC E MORT COVID/2024/SE/SE 18/tbMapa_tab_mort_SE.xlsx", overwrite = T) ###ATUALIZAR!!!
#write.csv2(tbMapa_mortalidade, file = "C:/Users/eucilene.santana/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/MAPAS KERNEL INC E MORT COVID/2023/SE/SE 52/tbMapa_tab_mort_SE_2.csv", overwrite = T) ###ATUALIZAR!!!
ggsave(gg13C, width = 300, height = 200, units = "mm", file = paste0(path, "FIG_INCIDENCIA_MUN.png"), dpi = "retina")
ggsave(gg13C_ob, width = 300, height = 200, units = "mm", file = paste0(path,"FIG_MORTALIDADE_MUN.png"), dpi = "retina")

################################################################################

# ============================================================================
# TABELA DE RANKING: MUNICÍPIOS COM MAIOR TAXA DE INCIDÊNCIA (CATEGORIAS 4-5)
# Filtra municípios com incidência acima do 4º quintil
# ============================================================================

tbMapa_tab_incidencia<-tbMapa_incidencia %>% 
  filter(catsincidencia>=4)

#LINDINHO
#Mun_BR<-read.xlsx("C:/Users/narmada.garcia/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx",
#                  sheet =  "Planilha2")
Mun_BR<-read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/Municípios_BR.xlsx",
                  sheet =  "Planilha2")

#GOLDEN BUTT
#Mun_BR<-read.xlsx("C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

#olinio
#Mun_BR<-read.xlsx("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")



tbMapa_tab_incidencia<-tbMapa_tab_incidencia %>% 
  arrange(desc(tbMapa_tab_incidencia$incidencia)) %>% 
  rename("Taxa de incidência por data de notificação"= "incidencia") %>% 
  rename("UF" = "nomeUF") %>% 
  rename("Município" = "nomeMUN")


##Para o m?s:
#tbMapa_tab_incidencia <- tbMapa_tab_incidencia[, c(9,10,12)]

##Para a SE:
tbMapa_tab_incidencia <- tbMapa_tab_incidencia[, c(12,13,19)]

#Verificar o tamanho da figura salva (caso necess?rio modificar height e width)
png(paste0(path, "TABELA_INCIDENCIA_MUNICIPIOS.png"), height = ,4000*nrow(tbMapa_tab), width =400*ncol(tbMapa_tab))
p<-tableGrob(tbMapa_tab_incidencia)
grid.arrange(p)
dev.off()


# ============================================================================
# TABELA DE RANKING: MUNICÍPIOS COM MAIOR TAXA DE MORTALIDADE (CATEGORIAS 4-5)
# ============================================================================

tbMapa_tab_mortalidade<-tbMapa_mortalidade %>% 
  filter(catsmortalidade>=4)

#LINDINHO
#Mun_BR<-read.xlsx("C:/Users/narmada.garcia/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx",
#                  sheet =  "Planilha2")
Mun_BR<-read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/Municípios_BR.xlsx",
                 sheet =  "Planilha2")

#GOLDEN BUTT
#Mun_BR<-read.xlsx("C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")

#olinio
#Mun_BR<-read.xlsx("C:/Users/pbrte/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/NOTIFICA??O DI?RIA/Munic?pios_BR.xlsx", sheet =  "Planilha2")


tbMapa_tab_mortalidade<-tbMapa_tab_mortalidade %>% 
  arrange(desc(tbMapa_tab_mortalidade$mortalidade)) %>% 
  rename("Taxa de mortalidade por data de notificação"= "mortalidade") %>% 
  rename("UF" = "nomeUF") %>% 
  rename("Município" = "nomeMUN")

#Para o m?s:
#tbMapa_tab_mortalidade <- tbMapa_tab_mortalidade[, c(9,10,13)]

##Para a SE:
tbMapa_tab_mortalidade <- tbMapa_tab_mortalidade[, c(12,13,20)]

#Verificar o tamanho da figura salva (caso necess?rio modificar height e width)
png(paste0(path, "TABELA_MORTALIDADE_MUNICIPIOS.png"), height = ,660*nrow(tbMapa_tab), width = 200*ncol(tbMapa_tab))
p<-tableGrob(tbMapa_tab_mortalidade)
grid.arrange(p)
dev.off()

# ============================================================================
# SEÇÃO DESATIVADA: GRÁFICOS DE MÉDIA MÓVEL DIÁRIA
# Blocos comentados abaixo geravam gráficos de barras diárias com linha
# de média móvel de 7 dias para casos e óbitos. Foram substituídos pelos
# gráficos semanais do script K4 (teste_Card_semanal_k4.Rmd)
# ============================================================================

# Figura de casos diários (dados de ontem)

#LINDINHO
#DT_CONTROL = fread(file = "C:/Users/eucilene.santana/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/ROTINAS/1_Data/Dados_Controle/DT_Monitora_COVID_20230212.csv", encoding = "UTF-8") #ATUALIZAR

#GOLDEN BUTT
#DT_CONTROL = fread(file = "C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/ROTINAS/1_Data/Dados_Controle/DT_Monitora_COVID_20220829.csv", encoding = "UTF-8")


#4 semanas epidemiol?gicas anteriores a atual em an?lise

#data_limite<-Sys.Date()-32

#data_limite_label<-format(data_limite,"%d-%b")
#data_limite_label2<-Sys.Date()-1
#data_limite_label2<-format(data_limite_label2,"%d-%b")

#filter_day<-format(Sys.Date()-1, "%a")
#
#tbDia = DT_CONTROL %>%
#  filter(ABRANGENCIA == 1) %>%
#  select(data, casosNovos, obitosNovos) %>%
#  mutate(diaSC = lubridate::wday(data, label = T, abbr = T)) %>%
#  mutate(diaSE = lubridate::epiweek(data)) %>% 
#  mutate(diaSE = case_when(diaSC == "ter" & diaSE == "9" ~ as.character(diaSE),
#                           !diaSC == "ter" & diaSE == "9" ~ "",
#                           diaSC == "dom" ~ as.character(diaSE),
#                           !diaSC == "dom" ~ "")) %>% 
#  mutate(smaCasosNovos = round(zoo::rollmean(casosNovos, k = 7, fill = 0, align = "right"), 0),
#         smaObitosNovos = round(zoo::rollmean(obitosNovos, k = 7, fill = 0, align = "right"),0))
#
#tbDia<-tbDia %>% 
#  filter(data>=data_limite)
#
##Formatar eixo x (data)
#tbDia$data_sel<-format(tbDia$data,"%d-%b (%a)")
#
##Modificar formato do n?mero
#tbDia$smaCasosNovos_label<-format(tbDia$smaCasosNovos, big.mark=".", decimal.mark = ",") 
#
##Calcular diferen?a em porcentagem da m?dia m?vel (sem casas decimais)
#annotate_label<-round((((tbDia$smaCasosNovos[32]/tbDia$smaCasosNovos[25])-1)*100),0)
#
##Verificar se o sinal de % de mudan?a ? positivo, negativo ou se n?o houve mudan?a na m?dia m?vel em 7 dias
#altera??o<- case_when(annotate_label>0 ~"Aumento",
#                      annotate_label<0 ~"Redu??o",
#                      TRUE ~ "Sem altera??o")
#
##checar altera??o
#altera??o
#
#annotate_label<-abs(annotate_label)
##Obs: se a altera??o for igual a zero ("Sem altera??o"), utilizar na linha 506 (linha do gr?fico abaixo com o mesmo framename):
##annotate("text", x = Sys.Date()-5, y = max(tbDia$casosNovos)+6000, label = paste0(altera??o, " \n na m?dia m?vel de casos \n nos ?ltimos 7 dias"), fontface = "bold", size= 10)+
#
#
##Criar limite de eixo y
#limite_y<-max(tbDia$casosNovos)+10000
#
#
##Figura
#gg1AAA = ggplot(tbDia, aes(data)) +
#  geom_bar(aes(y = casosNovos), stat = "identity", fill = "#9ecae1") +
#  geom_line(aes(y = smaCasosNovos, colour = "#141313"), size = 2) +
#  scale_fill_manual(name = "", labels = c("#9ecae1" = "Casos di?rias de covid-19"), values = c("#9ecae1" = "#9ecae1")) +
#  scale_colour_manual(name = "", labels = c("#141313" = "M?dia M?vel Simples dos ?ltimos 7 dias"), values = c("#141313" = "#141313")) +
#  scale_y_continuous(limits = c(0, limite_y), breaks = seq(0, limite_y, 10000), expand = c(0,0), labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
#  scale_x_continuous(labels = tbDia$data_sel, breaks = tbDia$data, minor_breaks = NULL, expand = c(0,1.1),guide = guide_axis(check.overlap = TRUE, angle = 90)) + 
#  geom_label(data=tbDia %>% filter(diaSC == filter_day), aes(label=smaCasosNovos_label, y=smaCasosNovos), size=10)+
#  labs(title = paste0("Casos de covid-19 por data da notifica??o ", "(", data_limite_label, " a ", data_limite_label2, ")"),
#       x = "Data da notifica??o", y = "Casos novos de covid-19") +
#  annotate("text", x = Sys.Date()-5, y = max(tbDia$casosNovos)+6000, label = paste0(altera??o, " de ", annotate_label, "% \n na m?dia m?vel de casos \n nos ?ltimos 7 dias"), fontface = "bold", size= 10)+
#  #annotate("text", x = Sys.Date()-5, y = max(tbDia$casosNovos)+6000, label = paste0(altera??o, " \n na m?dia m?vel de casos \n nos ?ltimos 7 dias"), fontface = "bold", size= 10)+
#  theme_ipsum(grid = F, base_size = 16, axis_text_size = 30) +
#  theme(legend.position = "bottom",
#        panel.grid.major.y = element_line(colour = "#f0f0f0"),
#        axis.text.x = element_text(angle = 0, vjust = .5, hjust = .5),
#        axis.line.x = element_line(colour = "black"),
#        axis.ticks.x = element_blank(),
#        axis.title.x = element_text(size = 30),
#        axis.title.y = element_text(size = 30),
#        plot.title = element_text(size = 30),
#        legend.text = element_text(size = 30))
#
##Checar Figura
#gg1AAA
#
##Salvando imagem
#ggsave(gg1AAA, width = 700, height = 500, units = "mm", file = paste0(path, "FIG_CASOS_MEDIA_MOVEL.png"), dpi = "retina")
#
#################################################################################
#################################################################################
#
##Figura de ?bitos di?rios
#
##Modificar formato do n?mero
#tbDia$smaObitosNovos_label<-format(tbDia$smaObitosNovos, big.mark=".", decimal.mark = ",") 
#
##Calcular diferen?a em porcentagem da m?dia m?vel (sem casas decimais)
#annotate_label_obitos<-round((((tbDia$smaObitosNovos[32]/tbDia$smaObitosNovos[25])-1)*100),0)
#
##Verificar se o sinal de % de mudan?a ? positivo, negativo ou se n?o houve mudan?a na m?dia m?vel em 7 dias
#altera??o_obitos<- case_when(annotate_label_obitos>0 ~"Aumento",
#                             annotate_label_obitos<0 ~"Redu??o",
#                             TRUE ~ "Sem altera??o")
#
##checar altera??o
#altera??o_obitos
#
#annotate_label_obitos<-abs(annotate_label_obitos)
#
##Se houver altre??o de % nos obitos trocar o comando na linha 565 (linha do gr?fico abaixo com o mesmo framename): 
##annotate("text", x = Sys.Date()-5, y = max(tbDia$obitosNovos)++30, label = paste0(altera??o, " de ", annotate_label_obitos, "% \n na m?dia m?vel de ?bitos \n nos ?ltimos 7 dias"), fontface = "bold", size= 10)+
#
#
##Criar limite de eixo y
#limite_y_obitos<-max(tbDia$obitosNovos)+100
#
#
##Figura
#gg1BBB = ggplot(tbDia, aes(data)) +
#  geom_bar(aes(y = obitosNovos), stat = "identity", fill = "#e33230") +
#  geom_line(aes(y = smaObitosNovos, colour = "#141313"), size = 2) +
#  scale_fill_manual(name = "", labels = c("#e33230" = "?bitos di?rias de covid-19"), values = c("#e33230" = "#e33230")) +
#  scale_colour_manual(name = "", labels = c("#141313" = "M?dia M?vel de ?bitos dos ?ltimos 7 dias"), values = c("#141313" = "#141313")) +
#  scale_y_continuous(limits = c(0, limite_y_obitos), breaks = seq(0, limite_y_obitos, 100), expand = c(0,0), labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
#  scale_x_continuous(labels = tbDia$data_sel, breaks = tbDia$data, minor_breaks = NULL, expand = c(0,1.1),guide = guide_axis(check.overlap = TRUE, angle = 90)) + 
#  geom_label(data=tbDia %>% filter(diaSC == filter_day), aes(label=smaObitosNovos_label, y=smaObitosNovos), size=10)+
#  labs(title = paste0("?bitos de covid-19 por data da notifica??o ", "(", data_limite_label, " a ", data_limite_label2, ")"),
#       x = "Data da notifica??o", y = "?bitos novos de covid-19") +
#  #annotate("text", x = Sys.Date()-5, y = max(tbDia$obitosNovos)++30, label = paste0(altera??o_obitos, "\n na m?dia m?vel de ?bitos \n nos ?ltimos 7 dias"), fontface = "bold", size= 10)+
#  annotate("text", x = Sys.Date()-5, y = max(tbDia$obitosNovos)++30, label = paste0(altera??o_obitos, " de ", annotate_label_obitos, "% \n na m?dia m?vel de ?bitos \n nos ?ltimos 7 dias"), fontface = "bold", size= 10)+
#  theme_ipsum(grid = F, base_size = 16, axis_text_size = 30) +
#  theme(legend.position = "bottom",
#        panel.grid.major.y = element_line(colour = "#f0f0f0"),
#        axis.text.x = element_text(angle = 0, vjust = .5, hjust = .5),
#        axis.line.x = element_line(colour = "black"),
#        axis.ticks.x = element_blank(),
#        axis.title.x = element_text(size = 30),
#        axis.title.y = element_text(size = 30),
#        plot.title = element_text(size = 30),
#        legend.text = element_text(size = 30))
#
##Checar Figura
#
#
#
#
#gg1BBB
#
##Salvando imagem
#ggsave(gg1BBB, width = 550, height = 400, units = "mm", file = paste0(path, "FIG_OBITOS_MEDIA_MOVEL.png"), dpi = "retina")
#
##################################################################################