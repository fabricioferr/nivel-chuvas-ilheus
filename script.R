################################################################################
################################################################################
############################ Nível das chuvas em Ilhéus ########################
################################################################################
################################################################################


# Importando pacotes ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(forcats)
library(readr)
library(janitor)
library(lubridate)
library(showtext)

font_add_google(
  name = "Roboto Condensed",
  family = "roboto"
  )

showtext_auto()

# Importando dados --------------------------------------------------------

## Importamos os dados no site do Instituto Nacional de Metereologia
## "https://portal.inmet.gov.br/dadoshistoricos"

data_raw <- readr::read_csv2(
  file = "INMET_NE_BA_A410_ILHEUS_01-01-2022_A_31-10-2022.csv",
  locale = locale(date_names = "pt", decimal_mark = ",", grouping_mark = "."),
  show_col_types = FALSE,
  skip = 8,
  col_select = 1:19
  ) 

# Manipulando os dados ----------------------------------------------------

## Fazemos algumas manipulações básicas para extrair o horário das observações

data_processed <- data_raw %>% 
  janitor::clean_names() %>%
  mutate(
    hora_utc = stringr::str_remove_all(string = hora_utc, pattern = " UTC"),
    data_formatada = as.POSIXct(x = hora_utc, tz = "GMT", format = "%H%M"),
    hora_formatada = format(data_formatada, format = "%H:%M")
    ) %>% 
  select(
    data, data_formatada, hora_formatada, precipita_o_total_hor_rio_mm, temperatura_do_ar_bulbo_seco_horaria_c
    )

glimpse(data_processed)

# Fazendo algumas análises ------------------------------------------------


## As chuvas são mais intensas em qual horário?

precip_ilheus <- data_processed %>% 
  group_by(hora_formatada) %>% 
  summarise(precip_media = mean(precipita_o_total_hor_rio_mm, na.rm = TRUE))

plot_precip_ilheus <- precip_ilheus %>% 
  ggplot(aes(x = forcats::fct_reorder(hora_formatada, precip_media), y = precip_media))+
  geom_col(fill = "#B0E0E6")+
  coord_flip()+
  theme_classic()+
  theme(
    text = element_text(family = "roboto", size = 12),
    axis.title = element_text(family = "roboto", colour = "black")
    )+
  geom_text(aes(
    label = scales::comma(
      x = precip_media, suffix = " mm", accuracy = 0.01,
      big.mark = ".", decimal.mark = ","
      )
    ),
    hjust = -0.01, colour = "black")+
  scale_y_continuous(
    labels = scales::number_format(
      suffix = " mm", big.mark = ".", decimal.mark = ","
      )
    )+
  labs(
    x = "Horário",
    y = "Volume em mm",
    title = "Precipitação média na Cidade de Ilhéus em 2022",
    subtitle = "Volume médio de chuvas, em milímetros, ao longo do ano de 2022",
    caption = "Elaboração: @fabr_ferreirac com dados do INMET."
    )

plot(plot_precip_ilheus)

ggsave(
  plot = plot_precip_ilheus,
  filename = "plot_precip_ilheus.png",
  width = 12,
  height = 5,
  dpi = 150,
  units = "in"
  )
