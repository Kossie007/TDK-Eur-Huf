all_data <- read.csv("TDK_data.csv")
                       
all_data[is.na(all_data)] <- 0
Training <- data.frame(Time = as.Date(all_data$row.names.eurhuf., origin = "YY-mm-dd"),
                       eur = all_data$EURHUF.X.Adjusted,
                       usd = all_data$USDHUF.X.Adjusted,
                       kinfo = all_data$kinfo_dummy,
                       vargaMihaly = all_data$vm_dummy,
                       nagyMarton = all_data$nm_dummy,
                       matolcsyGyorgy = all_data$mgy_dummy,
                       alapkamat = as.integer(all_data$alapkamat))

remove(all_data)


# Használt csomagok
library(aTSA)
library(broom)
library(cowplot)
library(dplyr)
library(FinTS)
library(forecast)
library(ggplot2)
library(kableExtra)
library(lmtest)
library(pastecs)
library(rugarch)
library(tidyr)




# Változók ábrázolása
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = eur), color = "blue", size = 1.2) +  
  geom_vline(data = Training[Training$alapkamat == 1, ], aes(xintercept = as.numeric(Time)), 
             color = "orange", linetype = "dashed", size = 1) + 
  geom_point(aes(y = eur), color = "red", size = 0.1) + 
  labs(title = "Az euró árfolyamának alakulása",
       subtitle = "2018. május 31. és 2024. szeptember 14. közötti időszakban",
       x = "Idő",
       y = "EUR árfolyam",
       caption = "Forrás: Yahoo Finance, Magyar Hang, MNB. Saját szerkesztés.")+
  theme_minimal() +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  scale_x_date(breaks = "year", 
               date_labels = "%Y", 
               limits = as.Date(c("2018-05-31", "2024-10-31")))+
  
  ggsave("arfolyam_midnennel_felrakva.png")


 

# Augmented Dickey-Fuller test
adf.test(Training$eur) 
?adf.test
# Stacionerré alakítás
Training$d_eur <- c(NA, diff(log(Training$eur)))
Training <-  Training[-1,]
stat.desc(Training)    
adf.test(Training$d_eur) 
kpss.test(Training$d_eur) 


 # Loghozam
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = d_eur), color = "blue", size = 1.2) +  
  labs(title = "Az euró árfolyam loghozamának alakulása",
       subtitle = "2018. június 03. és 2024. szeptember 14. közötti időszakban",
       y = "Euró-Forint loghozam",
       caption = "Saját számítás és szerkesztés.")+
  theme_minimal() +  # Letisztult téma
  theme(axis.title.x = element_blank()) +
  theme_minimal() +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  scale_x_date(breaks = "year", 
               date_labels = "%Y", 
               limits = as.Date(c("2018-05-31", "2024-10-31")))
  
ggsave("loghozam_alakulasa.png")    
      
      
# Durbin-Watson test      
dwtest(Training$d_eur ~1)      

# Koreelogramok
pacf(Training$d_eur)      
acf(Training$d_eur)      

pacf((Training$d_eur)^2) 
acf((Training$d_eur)^2)

# Breusch-Godfrey test
lmtest::bgtest((Training$d_eur)^2~1, order = 30)      


        
tsTarining <- as.ts(Training$d_eur)

# Az ACF és PACF értékek számítása a differenciált EUR árfolyamra
acf_values <- acf(Training$d_eur, lag.max = 15, plot = FALSE)
pacf_values <- pacf(Training$d_eur, lag.max = 15, plot = FALSE)

# Ljung-Box teszt a Q-statisztika kiszámításához minden lag-re
q_stat <- sapply(1:15, function(lag) Box.test(Training$d_eur, lag = lag, type = "Ljung-Box")$statistic)
p_values <- sapply(1:15, function(lag) Box.test(Training$d_eur, lag = lag, type = "Ljung-Box")$p.value)


korrelogram_data <- data.frame(Lag = 1:15,
                               Autocorrelation = round(acf_values$acf[-1], 3), 
                              "Partial Autocorrelation" = round(pacf_values$acf, 3),
                              "Q-Stat" = round(q_stat, 2),
                              "Prob" = round(p_values, 3))

korrelogram_data %>%
  kbl(digits = 3, align = "c", caption = "Korrelogram az EUR árfolyam differenciált értékeire") %>%
  kable_styling(full_width = FALSE, position = "center")


# https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html
ggplot.corr <- function(data, lag.max = 24, 
                        ci = 0.95, 
                        large.sample.size = TRUE, 
                        horizontal = TRUE, 
                        ylim_min = -1, 
                        ylim_max = 1, 
                        ...) {
  data <- na.omit(data)
    if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}
  
  list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  
  list.pacf <- acf(data, lag.max = lag.max, type = "partial", plot = FALSE)
  df2 <- data.frame(lag = list.pacf$lag, pacf = list.pacf$acf)
  
  plot.acf <- ggplot(data = df1, 
                     aes(x = lag, 
                         y = acf)) +
    geom_hline(yintercept = 0, 
               color = "red", 
               size = 0.7) +
    geom_col(fill = "blue", 
             width = 0.7) +
    geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N), 
               color = "green", 
               linetype = "dashed") +
    geom_hline(yintercept = -qnorm((1 + ci) / 2) / sqrt(N), 
               color = "green", 
               linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(df1$lag), 1)) +
    scale_y_continuous(name = element_blank(), 
                       limits = c(ylim_min, 
                                  ylim_max)) +
    ggtitle("ACF") +
    theme_bw() +
    labs(y="Korreláció mértéke",
        x="Késleltetés",
        caption = "Forrás: Számítások és szerkesztés https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html ."
    )
  
  
  
  plot.pacf <- ggplot(data = df2, 
                      aes(x = lag, 
                          y = pacf)) +
    geom_hline(yintercept = 0, 
               color = "red", 
               size = 0.7) +  
    geom_col(fill = "blue", 
             width = 0.7) +
    geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N), 
               color = "green", 
               linetype = "dashed") +
    geom_hline(yintercept = -qnorm((1 + ci) / 2) / sqrt(N), 
               color = "green", 
               linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(df2$lag, na.rm = TRUE), 1)) +
    scale_y_continuous(name = element_blank(), 
                       limits = c(ylim_min, 
                                  ylim_max)) +
    ggtitle("PACF") +
    theme_bw() +
    labs(y="Korreláció mértéke",
        x="Késleltetés",
        caption = "Forrás: Számítások és szerkesztés https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html ."
    )
  
  cowplot::plot_grid(plot.acf, plot.pacf, nrow = numofrow)
}
 
# caption = "Forrás: Számítások és szerkesztés https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html ."
ggplot.corr(data = Training$d_eur, 
            lag.max = 15, 
            ci = 0.95, 
            large.sample.size = TRUE, 
            horizontal = TRUE, 
            ylim_min = -0.1, 
            ylim_max = 0.1)
ggsave("acf_pacf_vertical.png")
  

# ARMA IC táblák
AIC_table_arima <- matrix(NA, nrow = 7, ncol = 7)
rownames(AIC_table_arima) <- c("AR0", "AR1","AR2","AR3","AR4","AR5","AR6")
colnames(AIC_table_arima) <- c("MA0", "MA1","MA2","MA3","MA4","MA5", "MA6")

BIC_table_arima <- matrix(NA, nrow = 7, ncol = 7)
rownames(BIC_table_arima) <- c("AR0", "AR1","AR2","AR3","AR4","AR5","AR6")
colnames(BIC_table_arima) <- c("MA0", "MA1","MA2","MA3","MA4","MA5", "MA6")

HQ_table_arima <- matrix(NA, nrow = 7, ncol = 7)
rownames(HQ_table_arima) <- c("AR0", "AR1","AR2","AR3","AR4","AR5","AR6")
colnames(HQ_table_arima) <- c("MA0", "MA1","MA2","MA3","MA4","MA5", "MA6")


for (i in 0:6) {
  for (j in 0:6) {
    arima_eur <- forecast::Arima(Training$d_eur, order = c(i, 0, j))
    AIC_table_arima[i +1, j +1] <- AIC(arima_eur)
    BIC_table_arima[i +1, j +1] <- BIC(arima_eur)
    HQ_table_arima[i +1, j +1] <- ICglm::HQIC(arima_eur)
  }
}

# AIC
as.data.frame(AIC_table_arima) %>%
  kbl(digits = 3, align = "c", caption = "Akaike információs kritérium az ARIMA(p,q) modelleknél") %>%
  kable_styling(full_width = FALSE, position = "center")
# BIC
as.data.frame(BIC_table_arima) %>%
  kbl(digits = 3, align = "c", caption = "Bayes-Schawarz információs kritérium az ARIMA(p,q) modelleknél") %>%
  kable_styling(full_width = FALSE, position = "center")
# HQ
as.data.frame(HQ_table_arima) %>%
  kbl(digits = 3, align = "c", caption = "Hannan-Quinn információs kritérium az ARIMA(p,q) modelleknél") %>%
  kable_styling(full_width = FALSE, position = "center")


#ARIMA(2,0,2)
arima_202_eur <- forecast::Arima(Training$d_eur, order = c(2, 0, 2))
coeftest_results <- coeftest(arima_202_eur)

coeftest_df_arima <- data.frame(Tengelymetszet = coeftest_results[, "Estimate"],
                                'Standard hiba' = coeftest_results[, "Std. Error"],
                                `z érték` = coeftest_results[, "z value"],
                                `P(>|z|)` = coeftest_results[, "Pr(>|z|)"])
coeftest_df_arima %>%
  kbl(digits = 4, align = "c", caption = "Az ARIMA(2,0,2) modell együtthatótáblája", 
      format.args = list(scientific = TRUE)) %>%
  kable_styling(full_width = FALSE, position = "center")



#ARIMA(2,1,2) ábra
Training$becsult_arima_d_eur <- Training$d_eur - arima_202_eur$residuals
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = d_eur, color = "Tényleges Euró loghozam"), size = 1.2) +
  geom_line(aes(y = becsult_arima_d_eur, color = "Becsült Euró loghozam"), size = 1.2) +
  labs(title = "Tényleges és ARIMA(2,1,2) becsült Euró-Forint loghozam",
       subtitle = "2018. június 03. és 2024. szeptember 14. közötti időszakban",
       y = "Euró- Forint loghozam",
       caption = "Forrás: Yahoo Finance, Magyar Hang, MNB. Saját szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y", limits = as.Date(c("2018-05-31", "2024-10-31")))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) +
  scale_color_manual(values = c("Tényleges Euró loghozam" = "blue", "Becsült Euró loghozam" = "red"))
ggsave("arima_becsult_loghozam.png")

# ARIMAX
xreg_train_eur <- model.matrix(~ kinfo + vargaMihaly + matolcsyGyorgy + nagyMarton + alapkamat- 1, data = Training)
cor(xreg_train_eur) 
arimax_eur_model <- arima(Training$d_eur, 
                          order = c(2, 0, 2), 
                          xreg = Training[,c('kinfo','vargaMihaly','matolcsyGyorgy','nagyMarton', 'alapkamat')])
summary(arimax_eur_model)

coeftest_results_arimax <- coeftest(arimax_eur_model)
coeftest_df_arimax <- data.frame(Tengelymetszet = coeftest_results_arimax[, "Estimate"],
                                 'Standard hiba' = coeftest_results_arimax[, "Std. Error"],
                                 `z érték` = coeftest_results_arimax[, "z value"],
                                 `P(>|z|)` = coeftest_results_arimax[, "Pr(>|z|)"])
                                
coeftest_df_arimax %>%
  kbl(digits = 4, align = "c", caption = "Az ARIMAX(2,0,2) modell értékei", 
      format.args = list(scientific = TRUE)) %>%
  kable_styling(full_width = FALSE, position = "center")


bgtest(arima_202_eur$residuals ~ 1, order = 15) 
hist(arima_202_eur$residuals)
shapiro.test(arima_202_eur$residuals) 

# GARCG
ArchTest(Training$d_eur, lags=15)


# GARCH (22-11)
modell_spec_22_11 <-  ugarchspec(mean.model = list(armaOrder=(c(2,2))),
                                                   variance.model= list(model="sGARCH",     
                                                                        garchOrder=c(1,1)),
                                                   distribution.model= "sstd")              
modell_fit_22_11 <-  ugarchfit(spec= modell_spec_22_11, data= Training$d_eur)
infocriteria(modell_fit_22_11)

coeftest_df_garch <- as.data.frame(modell_fit_22_11@fit$robust.matcoef)
colnames(coeftest_df_garch) <- c("Tengelymetszet", "Standard hiba", "t-érték","P(>|t|)" )

coeftest_df_garch %>%
  kbl(digits = 4, align = "c", caption = "Az GARCH(1,1) modell értékei", 
      format.args = list(scientific = TRUE)) %>%
  kable_styling(full_width = FALSE, position = "center")

becs_szoras_22_11 <- sigma(modell_fit_22_11)
plot(x=Training$Time, 
     y=becs_szoras_22_11, 
     type="l",
     main="Szórás az idő függvényében 22-11",
     ylab="Szórás")







arma_order <- c(2, 2)
distribution_model_wox <- "sstd"
ic_results <- data.frame(i = integer(), 
                         j = integer(), 
                         AIC = numeric(), 
                         BIC = numeric(), 
                         HQ = numeric())

for (i in 0:2) {
  for (j in 0:2) {
    tryCatch({
      modell_spec <- ugarchspec(mean.model = list(armaOrder = arma_order),
                                variance.model = list(model = "sGARCH", garchOrder = c(i, j)),
                                distribution.model = distribution_model_wox)
      
      modell_fit <- ugarchfit(spec = modell_spec, data = Training$d_eur)
  
      ic <- infocriteria(modell_fit)
      ic_results <- rbind(ic_results, data.frame(i = i, j = j, AIC = ic[1], BIC = ic[2], HQ = ic[4]))
      
      cat("Coefficient matrix for GARCH(", i, ",", j, "):\n")
      print(modell_fit@fit$robust.matcoef)
      print("\n")
      
      becs_szoras <- sigma(modell_fit)
      plot(x = Training$Time,
           y = becs_szoras,
           type = "l",
           main = paste("Szórás az idő függvényében GARCH(", i, ",", j, ")"),
           ylab = "Szórás")
      
    }, error = function(e) {
      cat("Error fitting GARCH(", i, ",", j, "):\n")
      print(e$message)
      
    }, warning = function(w) {
      cat("Warning for GARCH(", i, ",", j, "):\n")
      print(w$message)
    })
  }
}

# AIC
aic_table <- ic_results %>%
  select(i, j, AIC) %>%
  spread(j, AIC)

# BIC
bic_table <- ic_results %>%
  select(i, j, BIC) %>%
  spread(j, BIC)

# HQ
hq_table <- ic_results %>%
  select(i, j, HQ) %>%
  spread(j, HQ)

# AIC
aic_table %>%
  kbl(digits = 3, align = "c", caption = "AIC értékek GARCH(i,j) modellekhez") %>%
  kable_styling(full_width = FALSE, position = "center")

# BIC
bic_table %>%
  kbl(digits = 3, align = "c", caption = "BIC értékek GARCH(i,j) modellekhez") %>%
  kable_styling(full_width = FALSE, position = "center")

# HQ
hq_table %>%
  kbl(digits = 3, align = "c", caption = "HQ értékek GARCH(i,j) modellekhez") %>%
  kable_styling(full_width = FALSE, position = "center")

# Legjobb modell
best_model_aic <- ic_results[which.min(ic_results$AIC), ]
best_model_bic <- ic_results[which.min(ic_results$BIC), ]
best_model_hq <- ic_results[which.min(ic_results$HQ), ]

# Legjobb modellek megjelenítése
cat("\nBest model based on AIC:\n")
print(best_model_aic)

cat("\nBest model based on BIC:\n")
print(best_model_bic)

cat("\nBest model based on HQ:\n")
print(best_model_hq)

# Előrejelzés
start_date <- as.Date("2024-02-02")
end_date <- as.Date("2024-09-14")

filtered_data <- Training %>%
  filter(Time >= start_date & Time <= end_date)


# ARMA-GARCH
distribution_model_wx <- "sstd"
arma_order <- c(2, 2)
garch_order <- c(1,1)

# ARIMAX(2,1,2)-GARCH(1,1)
name <- "ARIMA(2,1,2)-GARCH(1,1) modell"
modell_spec <- ugarchspec(mean.model = list(armaOrder = arma_order), 
                                       variance.model = list(model = "sGARCH", 
                                                             garchOrder = garch_order),
                                       distribution.model = distribution_model_wx)
modell_fit <-  ugarchfit(spec= modell_spec, data= Training$d_eur)
plot(modell_fit, which = 'all')

ic <- infocriteria(modell_fit)

result %>%
  kbl(digits = 4, align = "c", caption = "ARIMA(2,1,2)-GARCH(1,1) modell", 
      format.args = list(scientific = TRUE)) %>%
  kable_styling(full_width = FALSE, position = "center")

becs_szoras <- sigma(modell_fit)
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = becs_szoras), color = "blue", size = 1.2) + 
  labs(y = "Szórás",
       title = "Az Euró-Forint loghozamának szórása",
       subtitle = "ARIMA(2,1,2)-GARCH(1,1) modell",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y", limits = as.Date(c("2018-05-31", "2024-10-31")))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("szoras_arima212_garch11.png")

# Gördülőablakos 
garch_roll_result <- ugarchroll(spec = modell_spec, 
                                data = Training$d_eur,  
                                n.start = 1479,     
                                refit.every = 30,   
                                refit.window = 'moving',
                                calculate.VaR = TRUE, 
                                VaR.alpha = 0.05,
                                solver = 'hybrid')

garch_roll_forecast <- as.data.frame(garch_roll_result@forecast$VaR)
garch_roll_forecast <- cbind(garch_roll_forecast, filtered_data$Time[1:162])



# VAR
ggplot(garch_roll_forecast, aes(x = filtered_data$Time[1:162])) +
  geom_line(aes(y = realized, color = "Tényleges loghozam"), color="blue") +
  geom_line(aes(y = `alpha(5%)`, color = "VaR konfidenciaintervallum"), color="red", size = 1.2) +
  labs(y = "Szórás",
       title = "Gördülőablakos konfidenciaintervallum (5%)",
       subtitle = "ARIMA(2,1,2)-GARCH(1,1) modell",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")

ggsave("var_arima212_garch11.png")

report(garch_roll_result, type = "VaR", VaR.alpha = 0.05)




filtered_data$simple <- fitted(modell_fit)[which(Training$Time >= start_date & Training$Time <= end_date)]

# Becsült és tényleges
ggplot(filtered_data, aes(x = Time)) +
  geom_line(aes(y = d_eur, color = "Tényleges loghozam"), size = 1.2) +  
  geom_line(aes(y = simple, color = "Becsült loghozam"), size = 1.2) + 
  labs(y = "Euró-Forint loghozam",
       title = "Tesztidőszakra becsült és tényleges loghozam",
       subtitle = "ARIMA(2,1,2)-GARCH(1,1) modell",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")+
  scale_color_manual(values = c("Tényleges loghozam" = "blue", "Becsült loghozam" = "red"))
ggsave("forecast_arima212_garch11.png")


garch_modell_results <- data.frame(modell = character(),
                                   AIC = numeric(), 
                                   BIC = numeric(), 
                                   HQ = numeric(),
                                   RMSE = numeric(), 
                                   MAE = numeric(), 
                                   TIC = numeric())


actual_values <- filtered_data$d_eur  
predicted_values <- filtered_data$simple


# RMSE
rmse <- sqrt(mean((actual_values - predicted_values)^2))

# MAE
mae <- mean(abs(actual_values - predicted_values))

# TIC 
tic <- sqrt(sum((actual_values - predicted_values)^2)) /
  (sqrt(sum(predicted_values^2)) + sqrt(sum(actual_values^2)))

garch_modell_results <- rbind(garch_modell_results, 
                              data.frame(modell = name,
                                         AIC = ic[1], 
                                         BIC = ic[2], 
                                         HQ = ic[4],
                                         RMSE = rmse, 
                                         MAE = mae, 
                                         TIC = tic))





# GARCHX modellszelekció
distribution_model_wx <- "sstd"
arma_order <- c(2, 2)
garch_order <- c(1, 1)

# Külső regresszorok és kombinációik
external_vars <- c('alapkamat', 'kinfo', 'vargaMihaly', 'matolcsyGyorgy', 'nagyMarton')
external_combinations <- c(external_vars, unlist(lapply(1:length(external_vars), function(n) {
  combn(external_vars, n, simplify = FALSE)
}), recursive = FALSE))
external_combinations <- external_combinations[6:36]

# Eredménytábla
results <- data.frame(
  type = character(),
  index = integer(),
  regressors = character(),
  AIC = numeric(),
  BIC = numeric(),
  HQ = numeric(),
  stringsAsFactors = FALSE
)

# Kombinációk
i <- 0
for (ext_vars in external_combinations) {
  ext_vars <- unlist(ext_vars)
  regressors_matrix <- as.matrix(Training[, ext_vars, drop = FALSE])

  # Várható érték
  tryCatch({
    modell_spec_mean <- ugarchspec(mean.model = list(armaOrder = arma_order,
                                                     external.regressors = regressors_matrix),
                                   variance.model = list(model = "sGARCH",
                                                         garchOrder = garch_order),
                                   distribution.model = distribution_model_wx)
    modell_fit_mean <- ugarchfit(spec = modell_spec_mean, data = Training$d_eur)
    i <- i + 1
    ic_mean <- infocriteria(modell_fit_mean)
    print(i)

    results <- rbind(results, data.frame(
      index = i,
      type = "mean",
      regressors = paste(ext_vars, collapse = ", "),
      AIC = ic_mean[1],
      BIC = ic_mean[2],
      HQ = ic_mean[4]
    ))
  }, error = function(e) {
    cat("Error in mean model with regressors:", paste(ext_vars, collapse = ", "), "\n")
  })

  # Szórás
  tryCatch({
    modell_spec_var <- ugarchspec(mean.model = list(armaOrder = arma_order),
                                  variance.model = list(model = "sGARCH",
                                                        garchOrder = garch_order,
                                                        external.regressors = regressors_matrix),
                                  distribution.model = distribution_model_wx)
    modell_fit_var <- ugarchfit(spec = modell_spec_var, data = Training$d_eur)
    i <- i + 1
    ic_var <- infocriteria(modell_fit_var)
    print(i)

    results <- rbind(results, data.frame(
      index = i,
      type = "variance",
      regressors = paste(ext_vars, collapse = ", "),
      AIC = ic_var[1],
      BIC = ic_var[2],
      HQ = ic_var[4]
    ))
  }, error = function(e) {
    cat("Error in variance model with regressors:", paste(ext_vars, collapse = ", "), "\n")
  })
}
  # Mix
  for (mean_vars in external_combinations) {
    for (var_vars in external_combinations) {

      mean_vars <- unlist(mean_vars)
      var_vars <- unlist(var_vars)

      mean_regressors_matrix <- as.matrix(Training[, mean_vars, drop = FALSE])
      var_regressors_matrix <- as.matrix(Training[, var_vars, drop = FALSE])

      tryCatch({
        modell_spec_mixed <- ugarchspec(mean.model = list(armaOrder = arma_order,
                                                          external.regressors = mean_regressors_matrix),
                                        variance.model = list(model = "sGARCH",
                                                              garchOrder = garch_order,
                                                              external.regressors = var_regressors_matrix),
                                        distribution.model = distribution_model_wx)
        modell_fit_mixed <- ugarchfit(spec = modell_spec_mixed, data = Training$d_eur)
        i <- i + 1
        ic_mixed <- infocriteria(modell_fit_mixed)
        print(i)
        results <- rbind(results, data.frame(
          index = i,
          type = "both_mixed",
          regressors = paste("Mean:", paste(mean_vars, collapse = ", "),
                             "Variance:", paste(var_vars, collapse = ", ")),
          AIC = ic_mixed[1],
          BIC = ic_mixed[2],
          HQ = ic_mixed[4]
        ))
      }, error = function(e) {
        cat("Error in mixed model with mean regressors:", paste(mean_vars, collapse = ", "),
            "and variance regressors:", paste(var_vars, collapse = ", "), "\n")
      })
    }
  }


# IC-k
results %>%
  kbl(digits = 4, align = "c", caption = "A GARCHx modellek infocrotériumai") %>%
  kable_styling(full_width = FALSE, position = "center")
write.csv(results, file="AICBICHQ.csv")

# Legjobb modell
best_model_aic <- results[which.min(results$AIC), ]
best_model_bic <- results[which.min(results$BIC), ]
best_model_hq <- results[which.min(results$HQ), ]

cat("\nBest model based on AIC:\n")
print(best_model_aic)

cat("\nBest model based on BIC:\n")
print(best_model_bic)

cat("\nBest model based on HQ:\n")
print(best_model_hq)

# Top 10 modell
top_10_aic <- results[order(results$AIC), ][1:10, "index"]
print(top_10_aic)

top_10_bic <- results[order(results$BIC), ][1:10, "index"]
print(top_10_bic)

top_10_hq <- results[order(results$HQ), ][1:10, "index"]
print(top_10_hq)


        
      
# Garch végső modellek
distribution_model_wx <- "sstd"
arma_order <- c(2, 2)
garch_order <- c(1,1)

# ARIMAX(2,1,2)-GARCH(1,1), várhatóértékben AK (alapkamat)
name_mean1 <- "ARIMAX(2,1,2)-GARCH(1,1) modell, várható értékben AK"
mean_vars <- as.matrix(Training[,c('alapkamat')])
modell_spec_mean1 <- ugarchspec(mean.model = list(armaOrder = arma_order, 
                                                  external.regressors = mean_vars), 
                                variance.model = list(model = "sGARCH", 
                                                      garchOrder = garch_order),
                                distribution.model = distribution_model_wx)
modell_fit_mean1 <-  ugarchfit(spec= modell_spec_mean1, data= Training$d_eur)
plot(modell_fit_mean1, which = 'all')

ic_mean1 <- infocriteria(modell_fit_mean1)

result_mean1 <- modell_fit_mean1@fit$robust.matcoef
result_mean1 %>%
  kbl(digits = 4, align = "c", caption = "ARIMAX(2,1,2)-GARCH(1,1) modell, várható értékben AK, modell együtthatói", 
      format.args = list(scientific = TRUE)) %>%
  kable_styling(full_width = FALSE, position = "center")

becs_szoras_mean1 <- sigma(modell_fit_mean1)
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = becs_szoras_mean1), color = "blue", size = 1.2) + 
  labs(y = "Szórás",
       title = "Az Euró-Forint loghozamának szórása",
       subtitle = "ARIMAX(2,1,2)-GARCH(1,1) modell, várható értékben AK",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y", limits = as.Date(c("2018-05-31", "2024-10-31")))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))
  
ggsave("szoras_arima212_garch11_mean1.png")

# Gördülőablakos 
garch_roll_result_mean1 <- ugarchroll(spec = modell_spec_mean1, 
                                      data = Training$d_eur,  
                                      n.start = 1479,     
                                      refit.every = 30,   
                                      refit.window = 'moving',
                                      calculate.VaR = TRUE, 
                                      VaR.alpha = 0.05,
                                      solver = 'hybrid')

garch_roll_forecast_mean1 <- as.data.frame(garch_roll_result_mean1@forecast$VaR)
garch_roll_forecast_mean1 <- cbind(garch_roll_forecast_mean1, filtered_data$Time[1:162])

# VAR
ggplot(garch_roll_forecast_mean1, aes(x = filtered_data$Time[1:162])) +
  geom_line(aes(y = realized, color = "Tényleges loghozam"), color="blue") +
  geom_line(aes(y = `alpha(5%)`, color = "VaR konfidenciaintervallum"), color="red", size = 1.2) +
  labs(y = "Szórás",
       title = "Gördülőablakos konfidenciaintervallum (5%)",
       subtitle = "ARIMAX(2,1,2)-GARCH(1,1) modell, várható értékben AK",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")
ggsave("var_arima212_garch11_mean1.png")
report(garch_roll_result_mean1, type = "VaR", VaR.alpha = 0.05)


# Előrejelzés

filtered_data$mean1 <- fitted(modell_fit_mean1)[which(Training$Time >= start_date & Training$Time <= end_date)]

# Becsült és tényleges
ggplot(filtered_data, aes(x = Time)) +
  geom_line(aes(y = d_eur, color = "Tényleges loghozam"), size = 1.2) +  
  geom_line(aes(y = mean1, color = "Becsült loghozam"), size = 1.2) + 
  labs(y = "Euró-Forint loghozam",
       title = "Tesztidőszakra becsült és tényleges loghozam",
       subtitle = "ARIMAX(2,1,2)-GARCH(1,1) modell, várható értékben AK",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) +
  scale_color_manual(values = c("Tényleges loghozam" = "blue", "Becsült loghozam" = "red"))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")
ggsave("forec_arima212_garch11_mean1.png")

 
predicted_values_mean1 <- filtered_data$mean1 


# RMSE
rmse_mean1 <- sqrt(mean((actual_values - predicted_values_mean1)^2))

# MAE
mae_mean1 <- mean(abs(actual_values - predicted_values_mean1))

# TIC 
tic_mean1 <- sqrt(sum((actual_values - predicted_values_mean1)^2)) /
  (sqrt(sum(predicted_values_mean1^2)) + sqrt(sum(actual_values^2)))


garch_modell_results <- rbind(garch_modell_results, 
                              data.frame(modell = name_mean1,
                                         AIC = ic_mean1[1], 
                                         BIC = ic_mean1[2], 
                                         HQ = ic_mean1[4],
                                         RMSE = rmse_mean1, 
                                         MAE = mae_mean1, 
                                         TIC = tic_mean1))





# ARIMAX(2,1,2)-GARCH(1,1), varainciában AK (alapkamat)
name_var1 <- "ARIMA(2,1,2)-GARCHX(1,1) modell, varianciában AK"

var_vars <- as.matrix(Training[,c('alapkamat')])
modell_spec_var1 <- ugarchspec(mean.model = list(armaOrder = arma_order), 
                                variance.model = list(model = "sGARCH", 
                                                      garchOrder = garch_order, 
                                                      external.regressors = var_vars),
                                distribution.model = distribution_model_wx)
modell_fit_var1 <-  ugarchfit(spec= modell_spec_var1, data= Training$d_eur)
plot(modell_fit_var1, which = 'all')

ic_var1 <- infocriteria(modell_fit_var1)

result_var1 <- modell_fit_var1@fit$robust.matcoef
result_var1 %>%
  kbl(digits = 4, align = "c", caption = "ARIMAX(2,1,2)-GARCH(1,1) modell, varianciában AK, modell együtthatói", 
      format.args = list(scientific = TRUE)) %>%
  kable_styling(full_width = FALSE, position = "center")

becs_szoras_var1 <- sigma(modell_fit_var1)
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = becs_szoras_var1), color = "blue", size = 1.2) + 
  labs(y = "Szórás",
       title = "Az Euró-Forint loghozamának szórása",
       subtitle = "ARIMA(2,1,2)-GARCHX(1,1) modell, varianciában AK",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y", limits = as.Date(c("2018-05-31", "2024-10-31")))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("szoras_arima212_garch11_var1.png")

# Gördülőablakos 
garch_roll_result_var1 <- ugarchroll(spec = modell_spec_var1, 
                                      data = Training$d_eur,  
                                      n.start = 1479,     
                                      refit.every = 30,   
                                      refit.window = 'moving',
                                      calculate.VaR = TRUE, 
                                      VaR.alpha = 0.05,
                                      solver = 'hybrid')

garch_roll_forecast_var1 <- as.data.frame(garch_roll_result_var1@forecast$VaR)
garch_roll_forecast_var1 <- cbind(garch_roll_forecast_var1, filtered_data$Time[1:162])

# VAR
ggplot(garch_roll_forecast_var1, aes(x = filtered_data$Time[1:162])) +
  geom_line(aes(y = realized, color = "Tényleges loghozam"), color="blue") +
  geom_line(aes(y = `alpha(5%)`, color = "VaR konfidenciaintervallum"), color="red", size = 1.2) +
  labs(y = "Szórás",
       title = "Gördülőablakos konfidenciaintervallum (5%)",
       subtitle = "ARIMA(2,1,2)-GARCHX(1,1) modell, varianciában AK",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")

ggsave("var_arima212_garch11_var1.png")

report(garch_roll_result_var1, type = "VaR", VaR.alpha = 0.05)


# Előrejelzés

filtered_data$var1 <- fitted(modell_fit_var1)[which(Training$Time >= start_date & Training$Time <= end_date)]

# Becsült és tényleges
ggplot(filtered_data, aes(x = Time)) +
  geom_line(aes(y = d_eur, color = "Tényleges loghozam"), size = 1.2) +  
  geom_line(aes(y = var1, color = "Becsült loghozam"), size = 1.2) + 
  labs(y = "Euró-Forint loghozam",
       title = "Tesztidőszakra becsült és tényleges loghozam",
       subtitle = "ARIMA(2,1,2)-GARCHX(1,1) modell, varianciában AK",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  scale_color_manual(values = c("Tényleges loghozam" = "blue", "Becsült loghozam" = "red"))
ggsave("forecast_arima212_garch11_var1.png")

predicted_values_var1 <- filtered_data$var1 


# RMSE
rmse_var1 <- sqrt(mean((actual_values - predicted_values_var1)^2))

# MAE
mae_var1 <- mean(abs(actual_values - predicted_values_var1))

# TIC 
tic_var1 <- sqrt(sum((actual_values - predicted_values_var1)^2)) /
  (sqrt(sum(predicted_values_var1^2)) + sqrt(sum(actual_values^2)))


garch_modell_results <- rbind(garch_modell_results, 
                              data.frame(modell = name_var1,
                                         AIC = ic_var1[1], 
                                         BIC = ic_var1[2], 
                                         HQ = ic_var1[4],
                                         RMSE = rmse_var1, 
                                         MAE = mae_var1, 
                                         TIC = tic_var1))





# ARIMAX(2,1,2)-GARCH(1,1), várható értékben AK (alapkamat), varianciában AK (alapkamat) és KI (kormányinfó)
name_mean2 <- "ARIMAX(2,1,2)-GARCHX(1,1), várható értékben VM,"


mean_vars <- as.matrix(Training[,c('vargaMihaly')])
modell_spec_mean2 <- ugarchspec(mean.model = list(armaOrder = arma_order, 
                                                       external.regressors = mean_vars), 
                               variance.model = list(model = "sGARCH", 
                                                     garchOrder = garch_order),
                               distribution.model = distribution_model_wx)
modell_fit_mean2 <-  ugarchfit(spec= modell_spec_mean2, data= Training$d_eur)
plot(modell_fit_mean2, which = 'all')

ic_mean2 <- infocriteria(modell_fit_mean2)

result_mean2 <- modell_fit_mean2@fit$robust.matcoef
result_mean2 %>%
  kbl(digits = 4, align = "c", caption = "ARIMAX(2,1,2)-GARCHX(1,1), várható értékben VM", 
      format.args = list(scientific = FALSE)) %>%
  kable_styling(full_width = FALSE, position = "center")

becs_szoras_mean2 <- sigma(modell_fit_mean2)
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = becs_szoras_mean2), color = "blue", size = 1.2) + 
  labs(y = "Szórás",
       title = "Az Euró-Forint loghozamának szórása",
       subtitle = "ARIMAX(2,1,2)-GARCHX(1,1) modell, várható értékben VM",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y", limits = as.Date(c("2018-05-31", "2024-10-31")))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("szoras_arima212_garch11_mean2.png")

# Gördülőablakos 
garch_roll_result_mean2 <- ugarchroll(spec = modell_spec_mean2, 
                                     data = Training$d_eur,  
                                     n.start = 1479,     
                                     refit.every = 30,   
                                     refit.window = 'moving',
                                     calculate.VaR = TRUE, 
                                     VaR.alpha = 0.05,
                                     solver = 'hybrid')

garch_roll_forecast_mean2 <- as.data.frame(garch_roll_result_mean2@forecast$VaR)
garch_roll_forecast_mean2 <- cbind(garch_roll_forecast_mean2, filtered_data$Time[1:162])

# VAR
ggplot(garch_roll_forecast_mean2, aes(x = filtered_data$Time[1:162])) +
  geom_line(aes(y = realized, color = "Tényleges loghozam"), color="blue") +
  geom_line(aes(y = `alpha(5%)`, color = "VaR konfidenciaintervallum"), color="red", size = 1.2) +
  labs(y = "Szórás",
       title = "Gördülőablakos konfidenciaintervallum (5%)",
       subtitle = "ARIMAX(2,1,2)-GARCHX(1,1) modell, várható értékben VM",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0)) +
scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")

ggsave("var_arima212_garch11_mean2.png")

report(garch_roll_result_mean2, type = "VaR", VaR.alpha = 0.05)



# Előrejelzés

filtered_data$mean2 <- fitted(modell_fit_mean2)[which(Training$Time >= start_date & Training$Time <= end_date)]

# Becsült és tényleges
ggplot(filtered_data, aes(x = Time)) +
  geom_line(aes(y = d_eur, color = "Tényleges loghozam"), size = 1.2) +  
  geom_line(aes(y = mean2, color = "Becsült loghozam"), size = 1.2) + 
  labs(y = "Euró-Forint loghozam",
       title = "Tesztidőszakra becsült és tényleges loghozam",
       subtitle = "ARIMAX(2,1,2)-GARCHX(1,1) modell, várható értékben VM",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  scale_color_manual(values = c("Tényleges loghozam" = "blue", "Becsült loghozam" = "red"))
ggsave("forecast_arima212_garch11_mean2.png")

 
predicted_values_mean2 <- filtered_data$mean2 


# RMSE
rmse_mean2 <- sqrt(mean((actual_values - predicted_values_mean2)^2))

# MAE
mae_mean2 <- mean(abs(actual_values - predicted_values_mean2))

# TIC 
tic_mean2 <- sqrt(sum((actual_values - predicted_values_mean2)^2)) /
  (sqrt(sum(predicted_values_mean2^2)) + sqrt(sum(actual_values^2)))


garch_modell_results <- rbind(garch_modell_results, 
                              data.frame(modell = name_mean2,
                                         AIC = ic_mean2[1], 
                                         BIC = ic_mean2[2], 
                                         HQ = ic_mean2[4],
                                         RMSE = rmse_mean2, 
                                         MAE = mae_mean2, 
                                         TIC = tic_mean2))



# ARIMAX(2,1,2)-GARCH(1,1), várható értékben és a varianciában is AK (alapkamat), KI (kormányinfó), MGy (Matolcsy György) és VM (Varga Mihály)
name_overall <- "ARIMAX(2,1,2)-GARCHX(1,1), várható értékben és a varianciában is AK, KI, MGy és VM"

var_vars <- as.matrix(Training[,c('alapkamat', 'vargaMihaly','kinfo','matolcsyGyorgy')])
mean_vars <- as.matrix(Training[,c('alapkamat', 'vargaMihaly', 'kinfo','matolcsyGyorgy')])
modell_spec_overall <- ugarchspec(mean.model = list(armaOrder = arma_order, 
                                                    external.regressors = mean_vars), 
                                  variance.model = list(model = "sGARCH", 
                                                        garchOrder = garch_order, 
                                                        external.regressors = var_vars),
                                     distribution.model = distribution_model_wx)
modell_fit_overall <-  ugarchfit(spec= modell_spec_overall, data= Training$d_eur)
plot(modell_fit_overall, which = 'all')

ic_overall <- infocriteria(modell_fit_overall)

result_overall <- modell_fit_overall@fit$robust.matcoef
result_overall %>%
  kbl(digits = 4, align = "c", caption = "ARIMAX(2,1,2)-GARCHX(1,1), várható értékben és \n a varianciában is AK, KI, MGy és VM, modell együtthatói", 
      format.args = list(scientific = TRUE)) %>%
  kable_styling(full_width = FALSE, position = "center")

becs_szoras_overall <- sigma(modell_fit_overall)
ggplot(Training, aes(x = Time)) + 
  geom_line(aes(y = becs_szoras_overall), color = "blue", size = 1.2) + 
  labs(y = "Szórás",
       title = "Az Euró-Forint loghozamának szórása",
       subtitle = "ARIMAX(2,1,2)-GARCHX(1,1) modell, várható értékben és \n a varianciában is AK, KI, MGy és VM",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  scale_x_date(breaks = "year", date_labels = "%Y", limits = as.Date(c("2018-05-31", "2024-10-31")))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("szoras_arima212_garch11_overall.png")

# Gördülőablakos 
garch_roll_result_overall <- ugarchroll(spec = modell_spec_overall, 
                                           data = Training$d_eur,  
                                           n.start = 1479,     
                                           refit.every = 30,   
                                           refit.window = 'moving',
                                           calculate.VaR = TRUE, 
                                           VaR.alpha = 0.05,
                                           solver = 'hybrid')

garch_roll_forecast_overall <- as.data.frame(garch_roll_result_overall@forecast$VaR)
garch_roll_forecast_overall <- cbind(garch_roll_forecast_overall, filtered_data$Time[1:162])
# VAR
ggplot(garch_roll_forecast_overall, aes(x = filtered_data$Time[1:162])) +
  geom_line(aes(y = realized, color = "Tényleges loghozam"), color="blue") +
  geom_line(aes(y = `alpha(5%)`, color = "VaR konfidenciaintervallum"), color="red", size = 1.2) +
  labs(y = "Szórás",
       title = "Gördülőablakos konfidenciaintervallum (5%)",
       subtitle = "ARIMAX(2,1,2)-GARCHX(1,1) modell, várható értékben és \n a varianciában is AK, KI, MGy és VM",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")
ggsave("var_arima212_garch11_overall.png")

report(garch_roll_result_overall, type = "VaR", VaR.alpha = 0.05)


# Előrejelzés

filtered_data$overall <- fitted(modell_fit_overall)[which(Training$Time >= start_date & Training$Time <= end_date)]

# Becsült és tényleges
ggplot(filtered_data, aes(x = Time)) +
  geom_line(aes(y = d_eur, color = "Tényleges loghozam"), size = 1.2) +  
  geom_line(aes(y = overall, color = "Becsült loghozam"), size = 1.2) + 
  labs(y = "Euró-Forint loghozam",
       title = "Tesztidőszakra becsült és tényleges loghozam",
       subtitle = "ARIMAX(2,1,2)-GARCHX(1,1) modell, várható értékben és \n a varianciában is AK, KI, MGy és VM",
       caption = "Forrás: Saját számítás és szerkesztés.")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) +
  scale_color_manual(values = c("Tényleges loghozam" = "blue", "Becsült loghozam" = "red"))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")
ggsave("forecast_arima212_garch11_overall.png")

 
predicted_values_overall <- filtered_data$overall 


# RMSE
rmse_overall <- sqrt(mean((actual_values - predicted_values_overall)^2))

# MAE
mae_overall <- mean(abs(actual_values - predicted_values_overall))

# TIC 
tic_overall <- sqrt(sum((actual_values - predicted_values_overall)^2)) /
  (sqrt(sum(predicted_values_overall^2)) + sqrt(sum(actual_values^2)))


garch_modell_results <- rbind(garch_modell_results, 
                              data.frame(modell = name_overall,
                                         AIC = ic_overall[1], 
                                         BIC = ic_overall[2], 
                                         HQ = ic_overall[4],
                                         RMSE = rmse_overall, 
                                         MAE = mae_overall, 
                                         TIC = tic_overall))

garch_modell_results %>%
   kbl(digits = 5, align = "c", caption = "ARIMA(X)(2,1,2)-GARCH(X)(1,1) modellek kiértékelése", 
       format.args = list(scientific = FALSE)) %>%
   kable_styling(full_width = FALSE, position = "center")

