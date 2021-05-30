library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(ggsci)
setwd('c:/Users/morey/Google Диск/UH_shared/track_2_package')


# hydrology
ts <- read.csv('c:/Users/morey/Google Диск/UH_shared/track_2_package/train.csv', stringsAsFactors = F)
ts1 <- read.csv('c:/Users/morey/Google Диск/UH_shared/2_track_cp1/extra_train.csv', stringsAsFactors = F)
ts2 <- read.csv('c:/Users/morey/Google Диск/UH_shared/2_track_cp2/extra_train.csv', stringsAsFactors = F)
ts3 <- read.csv('c:/Users/morey/Google Диск/UH_shared/2_track_cp3/extra_train.csv', stringsAsFactors = F)
train_sample <- do.call("rbind", list(ts, ts1))#, ts2, ts3

summary(train_sample)
train_sample$date <- as.Date(strftime(train_sample$date, format = "%Y-%m%-%d"))
train_sample$station_id <- factor(train_sample$station_id)
save(train_sample, file = 'train_sample_hydro.RData')
#load('train_sample_hydro.RData')
# ggplot(train_sample, aes(x=date, y=stage_max, col=station_id)) + geom_line() +
#   labs(x='Дата', y='Уровень воды, см', col='Гидропост') + 
#   facet_wrap(station_id~., scales = 'free_y', ncol = 3, strip.position = 'right') + 
#   theme_light(base_size = 16) + 
#   ggsave(filename = 'train_levels.png', device = 'png', width = 14, 
#          height = 18, units = 'in')
# wide hydrology
to_cast <- list(names(train_sample[,-c(1,2,3,4,6,7,10,12,13,14)]))
hydro_train_df <- data.table::dcast(setDT(train_sample), date~station_id, 
                                    value.var = to_cast)
save(hydro_train_df, file = 'hydro_train_df.RData')

# meteo
ms <- read.csv('c:/Users/morey/Google Диск/UH_shared/track_2_package/meteo_3hours.csv', stringsAsFactors = F)
ms1 <- read.csv('c:/Users/morey/Google Диск/UH_shared/2_track_cp1/extra_meteo_3hours.csv', stringsAsFactors = F)
ms2 <- read.csv('c:/Users/morey/Google Диск/UH_shared/2_track_cp2/extra_meteo_3hours.csv', stringsAsFactors = F)
ms3 <- read.csv('c:/Users/morey/Google Диск/UH_shared/2_track_cp3/extra_meteo_3hours.csv', stringsAsFactors = F)
train_meteo <- do.call("rbind", list(ms, ms1, ms2, ms3))
train_meteo$date <- as.Date(strftime(train_meteo$date, format = "%Y-%m%-%d"))

train_meteo_1d <- train_meteo %>%
  dplyr::group_by(date, station_id) %>%
  dplyr::summarise(temp = mean(air_temperature, na.rm = T),
                   prec = sum(precipitation, na.rm = T))
summary(train_meteo_1d)
# фильтруем данные об осадках согласно рекомендациям отсюда http://meteo.ru/data/163-basic-parameters#%D0%BE%D0%BF%D0%B8%D1%81%D0%B0%D0%BD%D0%B8%D0%B5-%D0%BC%D0%B0%D1%81%D1%81%D0%B8%D0%B2%D0%B0-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85
# Таблица 2 - для наших метеостанций не более 250 мм осадков между сроками
train_meteo_1d[train_meteo_1d$prec >= 250,] <- NA
train_meteo_1d <- na.omit(train_meteo_1d)
summary(train_meteo_1d)
save(train_meteo_1d, file = 'train_meteo_1d.RData')

#wide meteo
meteo_train_df <- data.table::dcast(setDT(train_meteo_1d), date~station_id, 
                                    value.var = c('temp', 'prec'))
save(meteo_train_df, file = 'meteo_train_df.RData')
#load('meteo_train_df.RData')

# merge hydro&meteo
train_df <- merge(hydro_train_df, meteo_train_df, by = 'date')
save(train_df, file = 'df_input_cp3.RData')
write.csv(train_df, 'df_input_cp3.txt', quote = F, row.names = F)
load('df_input_cp3.RData')

train_df <- train_df[, -c(grep(pattern = "discharge", colnames(train_df)))]
train_df
