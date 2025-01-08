library(tidyverse)
library(lubridate)

datos_2 <- REDCapR::redcap_report(redcap_uri = "https://redcap.upch.edu.pe/api/",
                                  token = Sys.getenv("token_pisaac_2"),  
                                  report_id = 767, raw_or_label = "label",guess_type = FALSE)$data

ped_sb_lab_2 <- datos_2 %>%
  filter(
    enrol_1 == "Si",
    pre_16 == "HOSPITAL SERGIO BERNALES",
    stringr::str_detect(pre_18, "PEDIATRIA"),
    enrol_2 > as.Date(today())-14
  ) %>%
  # View()
  mutate(
    crf_multiplex = lab_resultado_multiplex_influenza_sarscov2_complete,
    crf_respi = multiplex_influenza_sarscov2_y_vsr_complete,
    crf_xpert = xpert_xpress_flu_vsr_complete,
    enrol_2 = as.Date(enrol_2),
    enrol_13 = as.numeric(enrol_13),
    enrol_163 = as.numeric(enrol_163),
    `Nombre Completo` = paste(enrol_9, enrol_10, enrol_11),
    `Edad años` = as.period(lubridate::interval(lubridate::ymd(enrol_12), lubridate::as_date(enrol_2)))$year,
    `Edad meses` = as.period(lubridate::interval(lubridate::ymd(enrol_12), lubridate::as_date(enrol_2)))$month,
    `Edad días` = as.period(lubridate::interval(lubridate::ymd(enrol_12), lubridate::as_date(enrol_2)))$day,
    Edad = paste(`Edad años`, "años", `Edad meses`,"meses", `Edad días`,"días", sep = " "),
    `Fecha muestra` = as_date(str_sub(col_muestra_4,1,10)),
    `N° Documento` = case_when(
      !is.na(enrol_207) ~ paste("Acta:", (enrol_207)),
      !is.na(enrol_20) & enrol_203 == "DNI PERUANO" ~ paste("DNI:", (enrol_20)),
      !is.na(enrol_20) & enrol_203 == "CARNE DE EXTRANJERIA" ~ paste("CE:", (enrol_20)),
      !is.na(enrol_20) & enrol_203 == "PASAPORTE" ~ paste("PASAPORTE:", (enrol_20)),
      !is.na(enrol_20) & enrol_203 == "CEDULA" ~ paste("CÉDULA:", (enrol_20)),
      !is.na(enrol_20) & enrol_203 == "OTRO" ~ paste("DOC:", (enrol_20)),
      is.na(enrol_20) ~ "No tiene",
    ),
    `Fecha procesamiento` = case_when(
      !is.na(lab_res_multiplex_2) &  crf_multiplex == "Complete" ~  lab_res_multiplex_2,
      !is.na(lab_res_multiplex_2) & (crf_multiplex %in% c("Unverified", "Incomplete")) ~  "Pendiente",
      TRUE ~ "Pendiente"),
    ct_covid = case_when(
      lab_res_multiplex_7 == "Positivo" &  crf_multiplex == "Complete" ~ paste("CT SARS-CoV-2:", lab_res_multiplex_8)),
    ct_fluA = case_when(
      lab_res_multiplex_9 == "Positivo" &  crf_multiplex == "Complete" ~ paste("CT Flu A:", lab_res_multiplex_10)),
    ct_fluB = case_when(
      lab_res_multiplex_11 == "Positivo" &  crf_multiplex == "Complete" ~ paste("CT Flu B:", lab_res_multiplex_12)),
    ct_vsr = case_when(
      xpert_12 == "Positivo" & crf_xpert == "Complete" ~ paste("CT RSV:", xpert_13),
      multiplex_tres_15 == "Positivo" &  crf_multiplex == "Complete" ~ paste("CT RSV:", multiplex_tres_16)
    ),
    lab_res_multiplex_7 = case_when(
      is.na(lab_res_multiplex_7) | 
        (!is.na(lab_res_multiplex_7) & (crf_multiplex %in% c("Unverified", "Incomplete"))) ~ "Pendiente",
      !is.na(lab_res_multiplex_7) &  crf_multiplex == "Complete" ~ lab_res_multiplex_7,
      TRUE ~ "Pendiente"
    ),
    lab_res_multiplex_9 = case_when(
      is.na(lab_res_multiplex_9) | 
        (!is.na(lab_res_multiplex_9) & (crf_multiplex %in% c("Unverified", "Incomplete"))) ~ "Pendiente",
      !is.na(lab_res_multiplex_9) &  crf_multiplex == "Complete" ~ lab_res_multiplex_9,
      TRUE ~ "Pendiente"
    ),
    lab_res_multiplex_11 = case_when(
      is.na(lab_res_multiplex_11) | 
        (!is.na(lab_res_multiplex_11) & (crf_multiplex %in% c("Unverified", "Incomplete"))) ~ "Pendiente",
      !is.na(lab_res_multiplex_11) &  crf_multiplex == "Complete" ~ lab_res_multiplex_11,
      TRUE ~ "Pendiente"
    ),
    `Resultado RSV` = case_when(
      is.na(multiplex_tres_15) |
        (!is.na(multiplex_tres_15) & (crf_respi %in% c("Unverified", "Incomplete"))) ~ "Pendiente",
      enrol_13 >= 1 & crf_respi == "Complete" ~ multiplex_tres_15,
      enrol_163 > 6 & crf_respi == "Complete" ~ multiplex_tres_15,
      enrol_163 <= 6 & multiplex_tres_15 == "Positivo" & crf_respi == "Complete" ~ multiplex_tres_15,
      enrol_163 <= 6 & multiplex_tres_15 == "Negativo" & 
        (is.na(xpert_12) | (!is.na(xpert_12) & (crf_xpert %in% c("Unverified", "Incomplete")))) ~ "Pendiente",
      enrol_163 <= 6 & multiplex_tres_15 == "Negativo" & !is.na(xpert_12) & crf_xpert == "Complete" ~ xpert_12,
      TRUE ~ "Pendiente"
    )) %>% 
  rowwise() %>%
  mutate(
    Observaciones = case_when(
      `Fecha procesamiento` == "Pendiente" ~ "Pendiente",
      !is.na(ct_covid) | !is.na(ct_fluA) | !is.na(ct_fluB) | !is.na(ct_vsr) ~ 
        paste(
          na.omit(c(ct_covid, ct_fluA, ct_fluB, ct_vsr)),
          collapse = "; "
        ),
      TRUE ~ "-")
  ) %>% 
  select(
    `Fecha muestra`,
    Codigo = enrol_8,
    `Nombre Completo`,
    Edad,
    `N° Documento`,
    `Fecha hospitalización` = pre_19,
    `Fecha procesamiento`,
    `Resultado COVID-19` = lab_res_multiplex_7,
    `Resultado Influenza A` = lab_res_multiplex_9,
    `Resultado Influenza B` = lab_res_multiplex_11,
    `Resultado RSV`,
    Observaciones
  ) %>% 
  arrange(Codigo)