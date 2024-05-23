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
library(BAMMtools)
library(maptools)




options(ggrepel.max.overlaps = Inf)

#ATUALIZAR
SE="Abril"

#############################################

#Pasta para salvar figuras

path = paste0("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/2024/", SE, " de 2024/")

#Criar pasta para salvar arquivos

dir.create(paste0("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/2024/", SE, " de 2024"))


#############################################

#ATUALIZAR (com data do ultimo dia da SE em an?lise)


DT_CONTROL = fread(file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/ROTINAS/1_Data/Dados_Controle/DT_Monitora_COVID_20240504.csv", encoding = "UTF-8") #ATUALIZAR

DT_MUN = read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/ROTINAS/1_Data/DADOSMUNBR_novo.xlsx", sheet = 1)

dadostcu2019 <- readxl::read_excel("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/ROTINAS/1_Data/estimativa_TCU_2019_20200427.xls", sheet = 4)



dadostcu2019 <- dadostcu2019 %>%
  mutate(CODIGOMUN7 = str_c(codigoUF, codigoMUN)) %>% 
  mutate(CODIGOMUN7_2 = parse_number(str_sub(CODIGOMUN7, end = 7))) %>%
  mutate(CODIGOMUN7 = parse_number(str_sub(CODIGOMUN7, end = 6))) %>%
  mutate(CODIGOMUN7 = case_when(is.na(CODIGOMUN7) ~ codigoUF, TRUE ~ CODIGOMUN7)) %>% 
  select(CODIGOMUN7, POP_TCU2019, CODIGOMUN7_2)

DT_CONTROL = DT_CONTROL %>% 
  mutate(anoEpi = epiyear(data)) %>% 
  mutate(mes = month(data))

DT_MUN = DT_MUN %>% select(CODIGOMUN, LAT, LONG) %>% mutate(CODIGOMUN = parse_number(str_sub(CODIGOMUN, end = 6)))

DT_CONTROL = sjmisc::set_na(DT_CONTROL, na = "")

DT_CONTROL = DT_CONTROL %>% mutate(data = ymd(data)) %>% left_join(DT_MUN, by = c("CODIGOLOCAL" = "CODIGOMUN"))

DT_CONTROL = DT_CONTROL %>% full_join(dadostcu2019, by = c("CODIGOLOCAL" = "CODIGOMUN7"))

DT_CONTROL = DT_CONTROL %>% 
  rename(POP_TCU2019 = POP_TCU2019.x)

DT_CONTROL = DT_CONTROL %>% 
  group_by(anoEpi, mes, semanaEpi) %>% 
  mutate(minData = min_(data)) %>%
  ungroup()


#######################################################################################################

#ATUALIZAR FILTRAR APENAS  AS SEMANAS DO MÊS


DT_CONTROL_MES  <- DT_CONTROL %>% 
  filter(data >= as.Date("2024-03-31") & data <= as.Date("2024-05-04"))

######################################################################################################

DT_CONTROL_SE = DT_CONTROL_MES %>% 
  group_by(ABRANGENCIA, CODIGOLOCAL, CODIGOMUN7_2, NOMELOCAL, semanaEpi,anoEpi, mes, minData, LAT, LONG,POP_TCU2019) %>% 
  summarise(casosNovos=sum(casosNovos), obitosNovos=sum(obitosNovos)) %>% 
  ungroup()


#Classificando munic?pios pelo n?mero de casos novos novitifados na SE
tbMapa = DT_CONTROL_SE %>% 
  filter(ABRANGENCIA == 7, minData == max(minData)) %>% 
  select(minData, anoEpi, mes, LONG, LAT, CODIGOLOCAL, CODIGOMUN7_2, casosNovos, obitosNovos,POP_TCU2019) %>% 
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

getJenksBreaks(tbMapa$casosNovos, 4) ###atualizar
getJenksBreaks(tbMapa$obitosNovos, 4) ###atualizar

table(tbMapa$catsCasos)
table(tbMapa$catsObitos)


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


#table(tbMapa$catsCasos, exclude = F)
#table(tbMapa$catsObitos, exclude = F)


Mun_BR<-read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/Municípios_BR.xlsx", sheet =  "Planilha2")


tbMapa<-tbMapa %>% 
  left_join(Mun_BR, by = c("CODIGOMUN7_2" = "CODIBGE"), na_matches= "never")



#Elaborando figura (n?mero de casos)

tbMapa$catsCasos2 <- tbMapa$catsCasos

gg13B = ggplot(filter(tbMapa, !is.na(catsCasos), !str_detect(CODIGOLOCAL, "0000$"))) + 
  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) +
  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
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





gg13B



tbMapa$catsObitos2 <- tbMapa$catsObitos


gg13_ob = ggplot(filter(tbMapa, !is.na(catsObitos), !str_detect(CODIGOLOCAL, "0000$"))) + 
  geom_sf(data = get_brmap("City"), fill = "transparent", colour = "#f0f0f0", size = .3) +
  geom_sf(data = get_brmap("State"), fill = "transparent", colour = "#252525", size = .5) + 
  geom_point(aes(x = long, y = lat, size = obitosNovos, color = catsObitos), shape = 19, alpha = 0.5)+
  scale_color_manual(values = c("1" = "transparent", "2" = "steelblue", "3" = "darkgoldenrod1", "4" = "darkred"), 
                     name = paste0("Óbitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = ".", decimal.mark = ","), ")"), 
                     labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T), big.mark = "."),  " municípios)"), 
                                paste0("1-5 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"), 
                                paste0("6-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".", decimal.mark = ",")," municípios)"), 
                                paste0("11-", max_(tbMapa$obitosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","),  " municípios)"))) +
  scale_size_continuous(breaks = c(1, 5, 10, max_(tbMapa$obitosNovos)), range = c(0, 10), 
                        name = paste0("Óbitos novos de covid-19\n(Total = ", format(sum_(tbMapa$obitosNovos), big.mark = ".", decimal.mark = ","), ")"), 
                        labels = c(paste0("0 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "1"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("1-5 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "2"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("6-10 (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "3"], na.rm = T), big.mark = ".")," municípios)"), 
                                   paste0("11-", max_(tbMapa$obitosNovos), " (", format(n_distinct(tbMapa$CODIGOLOCAL[tbMapa$catsObitos == "4"], na.rm = T), big.mark = ".", decimal.mark = ","), " municípios)"))) + 
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




gg13_ob




tbMapa_tab<-tbMapa_tab %>% 
  arrange(desc(tbMapa_tab$casosNovos)) %>% 
  rename("Casos novos por data de notificação"= "casosNovos") %>% 
  rename("UF" = "nomeUF") %>% 
  rename("Município" = "nomeMUN")


write.xlsx(tbMapa_tab, file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/2024/Março de 2024/tbMapa_tab_casos.xlsx", overwrite = T)


#Criar figura de ?bitos com a tabela dos munic?pios com classifica??o 3 e 4


Mun_BR<-read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/Municípios_BR.xlsx",
                  sheet =  "Planilha2")


tbMapa_tab<-tbMapa_tab %>% 
  arrange(desc(tbMapa_tab$obitosNovos)) %>% 
  rename("Óbitos novos por data de notificação"= "obitosNovos") %>% 
  rename("UF" = "nomeUF") %>% 
  rename("Município" = "nomeMUN")



write.xlsx(tbMapa_tab, file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/NOTIFICAÇÃO DIÁRIA/2024/Março de 2024/tbMapa_tab_obitos.xlsx", overwrite = T)


################################################################################

#Elaborando figura (taxa de incid?ncia)


tbMapa$incidencia<-round((tbMapa$casosNovos/tbMapa$POP_TCU2019)*100000,1)

#Classificando munic?pios pela taxa de incidencia na SE
summary(tbMapa$incidencia) # atualizar


tbMapa_incidencia = tbMapa %>% 
  mutate(catsincidencia = case_when(incidencia <= 20.47 ~ "1",
                                    between(incidencia, 20.48, 72.85) ~ "2",
                                    between(incidencia, 72.86, 124.61) ~ "3",
                                    between(incidencia, 124.62, 171.20) ~ "4",
                                    incidencia > 171.21 ~ "5", 
                                    TRUE ~ NA_character_))

table(tbMapa_incidencia$catsincidencia, exclude = F)

#Elaborando figura (taxa de mortalidade)
tbMapa$mortalidade<-round((tbMapa$obitosNovos/tbMapa$POP_TCU2019)*100000,1)

#Classificando munic?pios pela taxa de mortalidade na SE
summary(tbMapa$mortalidade)


tbMapa_mortalidade = tbMapa %>% 
  mutate(catsmortalidade = case_when(mortalidade <= 0.21 ~ "1",
                                     between(mortalidade, 0.22, 0.44) ~ "2",
                                     between(mortalidade, 0.45, 0.72) ~ "3",
                                     between(mortalidade, 0.73, 1.41) ~ "4",
                                     mortalidade > 1.42 ~ "5", 
                                     TRUE ~ NA_character_))

table(tbMapa_mortalidade$catsmortalidade, exclude = F)


#Elaborando figuras (taxa de incidencia e mortalidade)

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


gg13C

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
       subtitle = "Fonte: Secretarias Estaduais de Sa?de/Painel Coronavírus Brasil") +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))+
  theme(legend.position = c(0.2, 0.2), axis.text = element_blank(), legend.title = element_text(face = "bold", size = 15)) + 
  theme(legend.text = element_text(size=15))+
  scalebar(filter(tbMapa_mortalidade, !is.na(catsmortalidade), !str_detect(CODIGOLOCAL, "0000$")), dist = 500, location = "bottomright", 
           transform = T, dist_unit = "km", st.dist = 0.03, st.size = 3, model = "WGS84")

gg13C_ob 


#Salvando imagem

write.xlsx(tbMapa_incidencia, file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/MAPAS KERNEL INC E MORT COVID/2024/Mês/Março/tbMapa_incid_mês.xlsx", overwrite = T) ###ATUALIZAR!!!
write.xlsx(tbMapa_mortalidade, file = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/MAPAS KERNEL INC E MORT COVID/2024/Mês/Março/tbMapa_mort_mês.xlsx", overwrite = T) ###ATUALIZAR!!!
ggsave(gg13C, width = 300, height = 200, units = "mm", file = paste0(path, "FIG_INCIDENCIA_MUN.png"), dpi = "retina")
ggsave(gg13C_ob, width = 300, height = 200, units = "mm", file = paste0(path,"FIG_MORTALIDADE_MUN.png"), dpi = "retina")

