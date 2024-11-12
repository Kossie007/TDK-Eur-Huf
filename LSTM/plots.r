library(tidyverse)
library(readxl)
library(openxlsx)

# RMSE Boxplots
rmse=read_excel("rmse_results.xlsx")
rmse = rmse[,-1]

rmse_long = rmse %>%
  pivot_longer(cols = rmse_all_variables:rmse_only_mgy, names_to = "name", values_to = "val") %>% 
  mutate(name = substring(name, 6))


ggplot(rmse_long, aes(x = name, y = val))+
  geom_boxplot()+
  theme_bw()

rmse_long_filt = rmse[-c(1:5),] %>%
  pivot_longer(cols = rmse_all_variables:rmse_only_mgy, names_to = "name", values_to = "val") %>% 
  mutate(name = substring(name, 6))


ggplot(rmse_long_filt, aes(x = name, y = val))+
  geom_boxplot()+
  labs(y = 'RMSE',
       title = "LSTM modellek RMSE értékeinek dobozábrái", caption = "Saját számítás és szerkesztés.")+
  scale_x_discrete(labels = c("Teljes modell","Alapmodell","KI mod.","MGY mod.","NM mod.","VM mod."))+
  theme_bw()+
  theme(axis.title.x = element_blank())

ggsave("./plots/rmse_box_filt.jpg")


# optimal lags
opt_lag = read_excel("optimal_lag_mean.xlsx")

ggplot(opt_lag, aes(x = lag, y = rmse))+
  geom_line(size = .7)+
  labs(y = 'Átlagos RMSE', x = "Eredményváltozó időbeli késleltetése",
       title = "Az egyes eredményváltozó késleltetéshez tartozó átlagos RMSE-k",
       subtitle = "50 darabos LSTM modell mintánkon",
       caption = "Saját számítás és szerkesztés.")+
  theme_bw()

ggsave("./plots/optimal_lags.jpg")


#mae
error = read_excel("errors_results.xlsx", sheet = 'no_dummies')[-1]
maes = sapply(error, function(x) mean(abs(x)))

# TIC-----------------------------------------------------------------------------------------------

# y and y_log
y = read_excel("TDK_data.xlsx")
y = y %>%
  mutate(`row.names(eurhuf)` = as.Date(`row.names(eurhuf)`)) %>% 
  filter(`row.names(eurhuf)` >= as.Date("2024-01-31")) %>% 
  rename(y = `EURHUF=X.Close`) %>% 
  .$y

y_log = diff(log(y))

# y_hat and y_log_hat
error = read_excel("errors_results.xlsx", sheet = 'no_dummies')[-1]
y_hat = sapply(error, function(x) y-x)
y_hat = as.data.frame(y_hat)  

y_log_hat = apply(y_hat, 2, function(x) diff(log(x)))
y_log_hat = as.data.frame(y_log_hat)


y_log = y_log[-163]
y_log_hat = y_log_hat[-1,]

plot_df = data.frame(y = y_log, y_hat = y_log_hat$sim_15, t = 1:162)

ggplot(plot_df, aes(x = t))+
  geom_line(aes(y = y), color = "blue", size = 1)+
  geom_line(aes(y = y_hat), color = "red", size = 1)


# TIC = sqrt(sum (y-y_hat)^2) / (sqrt(sum y_hat^2) + sqrt(sum y^2))
calc_tic = function(y, y_hat){
  a = sqrt(sum((y-y_hat)^2))
  b = sqrt(sum(y_hat^2)) 
  c = sqrt(sum(y^2))
  return(a/(b+c))
}


tic = c()
rmse = c()
mae = c()

for (i in 1:500) {
  tic[i] = calc_tic(y_log, y_log_hat[,i])
  rmse[i] = sqrt(mean((y_log-y_log_hat[,i])^2))
  mae[i] = mean(abs(y_log-y_log_hat[,i]))
}

measures = data.frame(mae = mae, rmse = rmse, tic= tic)
summary(measures)

write.xlsx(measures, "LSTM_fit_measures.xlsx")

# ts plot with dots---------------------------------------------------------------------------------
df_full = read_csv("TDK_data2.csv")

df = df_full %>%
  rename(y = `EURHUF.X.Close`,
         date = `row.names.eurhuf.`) %>% 
  mutate(date = as.Date(date),
         kinfo = kinfo_dummy*y,
         vm = vm_dummy*y,
         nm = nm_dummy*y,
         mgy = mgy_dummy*y
         ) %>%
  select(date, y, kinfo, vm, nm, mgy, alapkamat)


kamat = df %>% 
  filter(alapkamat == 1) %>% 
  .$date

df_plot = as.data.frame(sapply(df[-1], function(x) replace(x, x == 0, NA)))
df_plot$date = df$date

alpha = .7
color = "red"

ggplot(df_plot, aes(x = date))+
  geom_line(aes(y = y), color = "blue", size = 1.2)+
  geom_point(aes(y = kinfo), color = color, alpha = alpha)+
  geom_point(aes(y = vm), color = color, alpha = alpha)+
  geom_point(aes(y = nm), color = color, alpha = alpha)+
  geom_point(aes(y = mgy), color = color, alpha = alpha)+
  geom_vline(xintercept = kamat, linetype = 'dashed')+
  labs(y = "EUR-HUF árfolyam",
       title = "Az Euró-Forint árfolyamának alakulása",
       subtitle = "2018.5.31. és 2024.9.14. között, a vizsgált cikkekkel és alapkamat módosításokkal",
       caption = "Forrás: Yahoo Finance, Magyar Hang, MNB. Saját szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y", limits = as.Date(c("2018-05-31", "2024-10-31")))+
  theme_bw()+
  theme(axis.title.x = element_blank())

ggsave("./plots/eurhuf_line.jpg")
