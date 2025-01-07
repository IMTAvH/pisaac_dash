library(tidyverse)
library(lubridate)

# Lectura reporte lab de redcap
reporte_lab <- REDCapR::redcap_report(redcap_uri = "https://redcap.upch.edu.pe/api/", token = Sys.getenv("token_pisaac_1"), report_id = 159L, raw_or_label = "label", raw_or_label_headers = "label")$data

# Lectura de datos actuales en sheet de resultados
entrega_res_actual <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rS3yanBpJZdL5blHZuRNokaObOSCVdN0rl2pL-QpmwA/edit#gid=1236108235", sheet = "04ABR2022")

# Data nueva filtrada (nuevos enrolados) para subir a resultados
results_lab_1 <- reporte_lab %>% 
  filter(
    lubridate::ymd(`Fecha de colección de la muestra`) == lubridate::today()
  ) %>% 
  # filter(lubridate::hour(`Hora de colección`) > 12) %>%
  mutate(
    Proyecto = "PISAAC",
    # `Edad del participante` = round_2(x = `Edad del participante`),
    Mail = "NO TIENE"
  ) %>% 
  select(
    Proyecto,
    `Fecha de colección de la muestra`,
    `Establecimiento de Salud`,
    `Código del participante:`,
    `Iniciales del participante`,
    Sexo,
    `Edad del participante`,
    `Número que será usado para chatbot`,
    `Número de contacto de emergencia`,
    Mail
  ) %>% 
  anti_join(
    entrega_res_actual,
    by = c("Código del participante:" = "CÓDIGO")
  )