library(tidyverse)
library(lubridate)

# Lectura reporte lab de redcap
reporte_lab <- REDCapR::redcap_report(redcap_uri = "https://redcap.upch.edu.pe/api/", token = Sys.getenv("token_pisaac_1"), report_id = 159L, raw_or_label = "label", raw_or_label_headers = "label")$data


# Data nueva filtrada (nuevos enrolados) para subir
data_lab_1 <- reporte_lab %>% 
  filter(
    lubridate::ymd(`Fecha de colección de la muestra`) == lubridate::today()
  ) %>%
  # filter(lubridate::hour(`Hora de colección`) > 12) %>%
  # View()
  mutate(
    Proyecto = "PISAAC",
    Visita = "Basal",
    Examenes = "PROTOCOLO",
    `Hora de colección` = format(`Hora de colección`, format = "%H:%M:%S")
    # `Hora de colección` = paste(lubridate::hour(`Hora de colección`), lubridate::minute(`Hora de colección`), lubridate::second(`Hora de colección`), sep = ":")
  ) %>% 
  select(
    `Código del participante:`,
    Proyecto,
    `Código de muestra`,
    Visita,
    `Iniciales del participante`,
    `Tipo de muestra`,
    Examenes,
    Sexo,
    `Fecha de colección de la muestra`,
    `Hora de colección`,
    `Completado por`,
    `Establecimiento de Salud`
  ) %>% 
  anti_join(
    googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1G92S2BquXfDpBHtarxn38e3N1Tev6tLdTruXZdSZM_M/edit#gid=2104432393", sheet = "PISAAC_DATA_LABORATORIO"),
    by = "Código del participante:"
  )
