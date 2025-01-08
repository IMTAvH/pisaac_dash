library(tidyverse)

reporte_rsv_6m <- REDCapR::redcap_report(redcap_uri = "https://redcap.upch.edu.pe/api/",token = Sys.getenv("token_pisaac_2"), report_id = 760L)$data

# Actualizar reporte en googlesheet ----
xpert_ped_lab_2 <- reporte_rsv_6m %>% 
  filter(
    !is.na(multiplex_tres_15),
    enrol_99 >= lubridate::ymd("2024-07-15"),
    enrol_163 <= 6
  ) %>%
  select(
    enrol_20, # DNI
    enrol_207, # Acta de nacimiento
    pre_16, # Establecimiento
    enrol_163, # Edad en meses
    enrol_99, # Fecha de llenado de ficha ICE
    enrol_8, # código de participante
    
    lab_res_multiplex_7, # COV2
    lab_res_multiplex_9, # Flu A
    lab_res_multiplex_11, # Flu B
    
    multiplex_tres_15, # RSV
    multiplex_tres_16, # RSV Ct

    xpert_12, # RSV
    xpert_13 # RSV Ct
  ) %>% 
  relocate(enrol_8) %>% 
  mutate(
    enrol_20 = case_when(
      is.na(enrol_20) ~ as.character(enrol_207),
      T ~ enrol_20
    ),

    across(c(multiplex_tres_15, xpert_12, lab_res_multiplex_7, lab_res_multiplex_9, lab_res_multiplex_11), ~ case_when(. == 1 ~ "Positive", . == 2 ~ "Negative"))
  ) %>% 
  select(
    enrol_8, # Codigo
    enrol_20, # DNI
    enrol_163, # Edad en Meses
    enrol_99, # Fecha de enrolamiento
    pre_16, # Establecimiento
    lab_res_multiplex_7, # COV2
    lab_res_multiplex_9, # Flu A
    lab_res_multiplex_11, # Flu B
    multiplex_tres_15, # RSV
    multiplex_tres_16, # RSV Ct
    xpert_12, # RSV
    xpert_13 # RSV Ct
  ) %>% 
  anti_join(
    googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1SLBUKaig0w9B46pBMII1BqUu6Zi1MLnMogRVeYuE1OQ/edit?gid=994036930#gid=994036930", sheet = "Data"),
    by = c("enrol_8"="Codigo")
  )
  