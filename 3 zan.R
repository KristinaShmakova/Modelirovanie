#Шмакова К.А. Д-х 121, вариант 14
#задание 3
#создайте модель множественной линейной регрессии дневных потоков углекислого газа
#за весенний период 2013 года по данным измерений методом турбулентной пульсации

rm(list=ls())
getwd()

library("tidyr")
library("tibble")
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")  
#считаем данные и удалим ненужные строчки
eddypro = read.csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
eddypro = eddypro[-1, ]
eddypro = select(eddypro, -(roll))
eddypro = eddypro %>% mutate_if(is.character, factor)

# назовем столбцы допустимыми символами
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "L_")

glimpse(eddypro)
# отберем данные с 13 по 31 мая  и по дневному времени
eddypro = drop_na(eddypro)
eddypro = filter(eddypro, DOY >= 133 & DOY < 152)
eddypro = filter(eddypro, daytime==TRUE)
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

cor_td = cor(eddypro_numeric)
cor_td
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
# создадим обучающую и тестирующую выборки
row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = eddypro_numeric[teach,]
testing_tbl = eddypro_numeric[test,]
#модель 1
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)

summary(mod1)
coef(mod1)
resid(mod1)
confint(mod1)
anova(mod1)
plot(mod1) 

#модель 2
mod2 = lm(co2_flux~ DOY+used_records+Tau+qc_Tau+rand_err_Tau+H+qc_H 
                     +rand_err_H+LE+qc_LE+rand_err_LE + qc_co2_flux+rand_err_co2_flux
                     + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv+ h2o_v.adv+ co2_molar_density  
                     +co2_mole_fraction+ co2_mixing_ratio + h2o_molar_density 
                     + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + 
                       sonic_temperature + air_temperature + air_pressure + air_density + 
                     air_heat_capacity + air_molar_volume + water_vapor_density + e + es + 
                     specific_humidity+RH +Tdew + u_unrot + v_unrot+w_unrot+ u_rot + v_rot
                     + w_rot + max_speed + wind_dir + yaw + pitch+ u.+TKE+ L+X.z.d..L+
                     bowen_ratio+T.+x_peak +x_offset+x_10.+x_30.+x_50.+x_70.+x_90.+un_Tau+
                    Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux +un_h2o_flux 
                    +u_spikes +ts_var +co2_var+w.co2_cov+co2.1+h2o.1, data = teaching_tbl
)          
          
summary(mod2)
coef(mod2)
resid(mod2)
confint(mod2)
anova(mod2)
anova(mod2, mod1)
plot(mod2)

#модель 3

mod3 = lm(co2_flux~DOY+used_records+Tau+qc_Tau+rand_err_Tau+H+qc_H 
          +rand_err_H+LE+qc_LE+rand_err_LE + qc_co2_flux+rand_err_co2_flux
          + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv+ h2o_v.adv+ co2_molar_density  
          +co2_mole_fraction+ co2_mixing_ratio + h2o_molar_density 
          + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + 
            sonic_temperature + air_temperature + air_pressure + air_density + 
            air_heat_capacity + air_molar_volume + water_vapor_density + e + es + 
            specific_humidity+RH +Tdew + u_unrot + v_unrot+w_unrot+ u_rot + v_rot
          + w_rot + max_speed + wind_dir + yaw + pitch+ u.+TKE+ L+X.z.d..L+
            bowen_ratio+T.+x_peak +x_offset+x_10.+x_30.+x_50.+x_70.+x_90.+un_Tau+
            Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux +un_h2o_flux 
         +ts_var +co2_var+co2.1+h2o.1, data = teaching_tbl
)          
summary(mod3)
coef(mod3)
resid(mod3)
confint(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

#корреляционный анализ
cor_teaching_tbl = select(teaching_tbl, co2_flux, DOY, used_records,Tau,qc_Tau,rand_err_Tau,H,qc_H, 
                          rand_err_H,LE,qc_LE,rand_err_LE , qc_co2_flux,rand_err_co2_flux
                          ,h2o_flux ,rand_err_h2o_flux , H_strg ,co2_v.adv, h2o_v.adv, co2_molar_density  
                          ,co2_mole_fraction, co2_mixing_ratio , h2o_molar_density 
                          ,h2o_mole_fraction, h2o_mixing_ratio, h2o_time_lag, 
                            sonic_temperature , air_temperature , air_pressure ,air_density ,
                            air_heat_capacity , air_molar_volume , water_vapor_density , e , es , 
                            specific_humidity,RH ,Tdew , u_unrot , v_unrot,w_unrot, u_rot , v_rot
                          ,w_rot , max_speed , wind_dir , yaw , pitch, u.,TKE, L,X.z.d..L,
                            bowen_ratio,T.,x_peak ,x_offset,x_10.,x_30.,x_50.,x_70.,x_90.,un_Tau,Tau_scf 
                          , un_H ,H_scf ,un_LE ,LE_scf, un_co2_flux,un_h2o_flux 
                          ,u_spikes ,ts_var ,co2_var,w.co2_cov,co2.1,h2o.1)

cor_td = cor(cor_teaching_tbl) %>% as.data.frame

qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod2, teaching_tbl)))
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod2, testing_tbl)))
# примеры графиков
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod2, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod2, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod2, testing_tbl)))
