# Шмакова К.А. Дх-121, вариант 14 - для региона 56 рассчитайте 
#урожайность пшеницы в период с 2007 по 2015 год взяв для рассчета 
#средние суммы активных температур за эти годы, с метеостанций на 
#расстоянии от 50 до 250 км
# проверяем рабочую директорию
rm(list=ls())
getwd()
# устанавливаем пакеты
#install.packages ()
library(tidyverse)
library(rnoaa)
library(lubridate)
# скачиваем список метеостанций
station_data = ghcnd_stations()
write.csv(station_data,"station_data.csv")
station_data=read.csv("station_data.csv")
#После получения списка всех станций, необходимо получить список станций ближайших 
#к столице региона,создав таблицу с именем региона и координатами его столицы
orenburg = data.frame(id = "ORENBURG", latitude = 51.7666,  longitude = 55.1005)
orenburg_around = meteo_nearby_stations(lat_lon_df = orenburg, station_data = station_data,
var = c("PRCP", "TAVG"), year_min = 2007, year_max = 2015)
#получаем индентификатор (id) метеостанции 
orenburg_id=orenburg_around[["ORENBURG"]][["id"]][1]
summary(orenburg_id)
str(orenburg_around)
all_orenburg_data = meteo_tidy_ghcnd(stationid = orenburg_id)
#Получение таблицы по ближайшим метеостанциям
orenburg_table=orenburg_around[[1]]
summary(orenburg_table)
# отфильтруем все станции, на расстоянии от 50 до 250 км
orenburg_table = filter (orenburg_table, ORENBURG.distance > 50 & ORENBURG.distance < 250 )
#нужно убедится, что этот список включает нужные по условию задачи метеостанции
orenburg_stations = orenburg_table 
str(orenburg_stations)
#Таким образом, мы сформировали список необходимых станций
orenburg_stations$id
# Создаем цикл, в который будут скачиваться необходимые данные с метеостанций 
# Промежуточный объект, куда скачиваются данные с кокретной метеостанции
all_i = data.frame()
# Объект, куда скачиваются все данные со всех метеостанций
all_orenburg_meteodata = data.frame()
# Цикл для всех метеостанций
for(i in 1:11)
{
  orenburg_id =  orenburg_around[["ORENBURG"]] [["id"]] [ i]
  data = meteo_tidy_ghcnd(stationid = orenburg_id,
                          var = "TAVG",
                          date_min = "2007-01-01",
                          date_max = "2015-12-31")
  all_orenburg_meteodata =  bind_rows(all_orenburg_meteodata, data)
}
# Записываем полученные данные в файл
write.csv(all_orenburg_meteodata, "all_orenburg_meteodata.csv")
all_orenburg_meteodata
all_orenburg_meteodata = read.csv("all_orenburg_meteodata.csv")
str(all_orenburg_meteodata)
#  Добавим год, месяц,день (получаем результаты за конкретный период времени)
all_orenburg_meteodata = mutate(all_orenburg_meteodata, year = year(date), month = month(date), day = day(date))
str(all_orenburg_meteodata)
# Выведем данные за 2007 - 2015 годы
years_orenburg_meteodata = filter(all_orenburg_meteodata, year %in% c( 2007:2015))
#  Проверим результат
str(years_orenburg_meteodata)
summary(years_orenburg_meteodata)
#Средняя (по годам и метеостанциям) сумма активных температур за месяц
# Приведение средней суммы температур в подходящую форму, при помощи деления на 10
years_orenburg_meteodata[,"tavg"] = years_orenburg_meteodata$tavg/10
# Превратим в нули все NA и где 5<tavg>30
years_orenburg_meteodata[is.na(years_orenburg_meteodata$tavg),"tavg"] = 0
years_orenburg_meteodata[years_orenburg_meteodata$tavg<5, "tavg"] = 0
summary(years_orenburg_meteodata)
#Группируем по метеостанциям, годам и месяцам
alldays = group_by(years_orenburg_meteodata, id, year, month)
#Сумма температуру по этим группам с помощью sum 
sumT_alldays_orenburg = summarize(alldays, tsum = sum(tavg))
summary(sumT_alldays_orenburg)
#Группировка данных по месяцам  
groups_orenburg_months = group_by(sumT_alldays_orenburg, month); groups_orenburg_months
#Расчет среднего по месяцам для всех метеостанций и всех лет
sumT_months=summarize(groups_orenburg_months,St=mean(tsum))
sumT_months
# Расчет урожайности
#Ввод констант для расчета урожайности
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)

#Коэффициент для экспозиции склона (считаем, что все поля идеально ровные)
y = 1.0
#Коэффициент использования ФАР
Kf = 300
#Каллорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25 
#Рассчет Fi по месяцам
sumT_months = mutate(sumT_months, Fi = afi + bfi * y * St)

#Рассчет Yi
sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))
#Расчет урожая, как сумму по месяцам
Yield = sum(sumT_months$Yi)
Yield
# Ответ: 20,3 ц/га 
