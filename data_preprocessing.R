library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.5
library(tidyr) # Tidy Messy Data, CRAN v1.1.3
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.3
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package, CRAN v1.4.4
library(data.table) # Extension of `data.frame`, CRAN v1.14.0
library(lubridate) # Make Dealing with Dates a Little Easier, CRAN v1.7.10
library(see) # Visualisation Toolbox for 'easystats' and Extra Geoms, Themes and Color Palettes for 'ggplot2', CRAN v0.6.3

# Theme set ---------------------------------------------------------------
theme_set(see::theme_lucid() +
            theme(strip.background = element_blank()))

##### ГИДРОЛОГИЯ #####
# Загрузка данных ---------------------------------------------------------
ts <- read.csv('data/raw/track_2_package/train.csv',
               stringsAsFactors = F)

ts1 <- read.csv('data/raw/2_track_cp1/extra_train.csv',
                stringsAsFactors = F)

ts2 <- read.csv('data/raw/2_track_cp2/extra_train.csv',
                stringsAsFactors = F)

ts3 <- read.csv('data/raw/2_track_cp3/extra_train.csv',
                stringsAsFactors = F)

ts4 <- read.csv('data/raw/2_track_cp4/extra_train.csv',
                stringsAsFactors = F)

# Объединение вместе
train_sample <- do.call("rbind", list(ts, ts1))#, ts2, ts3, ts4

summary(train_sample)

# Предподготовка данных ---------------------------------------------------
# Даты как даты, id как факторы

train_sample$date <- as.Date(strftime(train_sample$date,
                                      format = "%Y-%m%-%d"))

train_sample$station_id <- factor(train_sample$station_id)

save(train_sample, file = 'train_sample_hydro.RData')

# Визуализация трейнингового датасета -------------------------------------

#load('train_sample_hydro.RData')
# ggplot(train_sample,
#        aes(x = date,
#            y = stage_max,
#            col = station_id)) +
#   geom_line() +
#   labs(x = 'Дата',
#        y = 'Уровень воды, см',
#        col = 'Гидропост') +
#   facet_wrap(station_id~.,
#              scales = 'free_y',
#              ncol = 3,
#              strip.position = 'right') +
#   theme_light(base_size = 16) +
#   ggsave(filename = 'train_levels.png',
#          device = 'png',
#          width = 14,
#          height = 18,
#          units = 'in')

# Long to wide ------------------------------------------------------------
to_cast <- list(names(train_sample[,-c(1,2,3,4,6,7,10,12,13,14)]))

hydro_train_df <- data.table::dcast(setDT(train_sample),
                                    date~station_id, 
                                    value.var = to_cast)

save(hydro_train_df, file = 'hydro_train_df.RData')

# Кросскорреляция ---------------------------------------------------------
library(corrr)
library(stringr)

major_stations <- c(3019,
                    3027:3030,
                    3035,
                    3041,
                    3045,
                    3050,
                    3230)

hydro_train_df %>% 
  as_tibble() %>% 
  dplyr::select(contains("stage_max")) %>% 
  dplyr::select(-contains("delta_stage_max")) %>% 
  dplyr::select(contains(as.character(major_stations))) %>% 
  rename_all(~str_remove(., "stage_max_")) %>% 
  corrr::correlate() %>% 
  rearrange()  %>%
  shave() %>% 
  # fashion() %>% 
  rplot(print_cor = T) +
  ggtitle("stage_max") +
  theme_lucid()


##### МЕТЕОРОЛОГИЯ #####
# Загрузка данных ---------------------------------------------------------
ms <- read.csv('data/raw/track_2_package/meteo_3hours.csv',
               stringsAsFactors = F)
ms1 <- read.csv('data/raw/2_track_cp1/extra_meteo_3hours.csv',
                stringsAsFactors = F)
ms2 <- read.csv('data/raw/2_track_cp2/extra_meteo_3hours.csv',
                stringsAsFactors = F)
ms3 <- read.csv('data/raw/2_track_cp3/extra_meteo_3hours.csv',
                stringsAsFactors = F)
ms4 <- read.csv('data/raw/2_track_cp4/extra_meteo_3hours.csv',
                stringsAsFactors = F)

# Объединение вместе ------------------------------------------------------
train_meteo <- do.call("rbind", list(ms, ms1)) # , ms2, ms3, ms4
train_meteo$date <- as.Date(strftime(train_meteo$date,
                                     format = "%Y-%m%-%d"))

# Расчет среднесуточных характеристик -------------------------------------
train_meteo_1d <- train_meteo %>%
  dplyr::group_by(date, station_id) %>%
  dplyr::summarise(temp = mean(air_temperature, # среднесутончая темп
                               na.rm = T),
                   prec = sum(precipitation, # суточные суммы осадков
                              na.rm = T))
# Summary
summary(train_meteo_1d)

# Фильтр осадков -----------------------------------------------------

# train_meteo_1d %>%
#   as_tibble() %>% 
#   filter(station_id == 30471) %>% 
#   filter(year(date) %in% c(1990, 1987)) %>% 
#   ggplot(aes(x = date,
#              y = prec)) +
#   geom_hline(aes(yintercept = 250),
#              color = "#FF3030") +
#   geom_col() +
#   scale_x_date(date_labels = "%b") +
#   labs(subtitle = "Метеостанция 30471",
#        x = NULL,
#        y = "Осадки, мм") +
#   facet_wrap(~year(date),
#              scales = "free") +
#   ggsave("figures/meteo_example.png",
#          dpi = 300,
#          w = 9, h = 4.5)

# фильтруем данные об осадках согласно рекомендациям отсюда http://meteo.ru/data/163-basic-parameters#%D0%BE%D0%BF%D0%B8%D1%81%D0%B0%D0%BD%D0%B8%D0%B5-%D0%BC%D0%B0%D1%81%D1%81%D0%B8%D0%B2%D0%B0-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85
# Таблица 2 - для наших метеостанций не более 250 мм осадков между сроками
train_meteo_1d[train_meteo_1d$prec >= 250,] <- NA
train_meteo_1d <- na.omit(train_meteo_1d)
summary(train_meteo_1d)
save(train_meteo_1d, file = 'train_meteo_1d.RData')

# Long to wide ------------------------------------------------------------
meteo_train_df <- data.table::dcast(setDT(train_meteo_1d), date~station_id, 
                                    value.var = c('temp', 'prec'))
save(meteo_train_df, file = 'meteo_train_df.RData')
#load('meteo_train_df.RData')

##### ГИДРОМЕТЕО #####

# Объединяем гидрологию и метеоданные -------------------------------------
train_df <- merge(hydro_train_df,
                  meteo_train_df,
                  by = 'date')

train_df <- train_df[, -c(grep(pattern = "discharge",
                               colnames(train_df)))]

glimpse(train_df)

# Сохраняем и переходим в Python
save(train_df, file = 'df_input_cp3.RData')

write.csv(train_df, 'df_input_cp3.txt', quote = F, row.names = F)

# load('df_input_cp3.RData')
