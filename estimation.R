#####Load libraries#####
library("tidyverse")
library("lubridate")
library("ggplot2")
library("RColorBrewer")
library("fpp2")           
library("zoo") 
library("lemon")


#PART I. Read and prepare data

#read Bogota data
bogota_data <- read.csv("Porcentaje-Ocupacion-UCI-COVID-19-IPS.csv")
bogota_data$Fecha <- as.Date(bogota_data$Fecha, format = "%m/%d/%Y")

#group Bogota data (UCI/ICU)
bogota_data <- bogota_data %>% 
                group_by(Fecha) %>% 
                  summarise(ICU = sum(Numerador.Camas.ocupadas.covid..19.confirmados.y.probables, na.rm = TRUE))
colnames(bogota_data)[colnames(bogota_data) == "Fecha"] <- "Date"

#read country data, and select Bogota records of hospitalization
bogota_all_data <- read.csv("2020-07-03.csv")
bogota_all_data$Fecha.Not <- as.Date(bogota_all_data$Fecha.Not, format = "%Y-%m-%d")
colnames(bogota_all_data)[colnames(bogota_all_data) == "Fecha.Not"] <- "Date"

#group Bogota data
bogota_all_data <- bogota_all_data[bogota_all_data$Ciudad_municipio == 11001,] %>%
                    group_by(Date, Ubicacion) %>%
                      summarise(number = n()) %>%
                        arrange(Date)
bogota_all_data <- data.frame(bogota_all_data)

#create row if date does not exist for hospital UCI, initially at a value of 0
for(i in 1:nrow(bogota_data))
{
  if(!bogota_data$Date[i] %in% bogota_all_data$Date[bogota_all_data$Ubicacion == "Hospital UCI"])
  {
    bogota_all_data <- rbind(bogota_all_data, data.frame(Date = bogota_data$Date[i], Ubicacion = "Hospital UCI", number = 0))
  }
}

#create row if date does not exist, for hospital, initially at a value of 0
for(i in 1:nrow(bogota_data))
{
  if(!bogota_data$Date[i] %in% bogota_all_data$Date[bogota_all_data$Ubicacion == "Hospital"])
  {
    bogota_all_data <- rbind(bogota_all_data, data.frame(Date = bogota_data$Date[i], Ubicacion = "Hospital", number = 0))
  }
}

#order by date before further aggregations 
bogota_all_data <- bogota_all_data %>% arrange(Date)

#aggregate cumulatively to count hospitalizations
last_hospital <- 0
last_uci <- 0
for(i in 1:nrow(bogota_all_data))
{
  if(bogota_all_data$Ubicacion[i] == "Hospital")
  {
    bogota_all_data$number[i] <- bogota_all_data$number[i] + last_hospital
    last_hospital <- bogota_all_data$number[i]
  }
}

#replace UCI/ICU with Bogota data (which contains both confirmed and probable cases in ICU)
bogota_all_data <- bogota_all_data[bogota_all_data$Date >= min(bogota_data$Date, na.rm = TRUE),]
bogota_all_data <- bogota_all_data[!is.na(bogota_all_data$Date),]

bogota_all_data <- bogota_all_data %>%
                    rowwise() %>%
                      mutate(number2 = ifelse(nrow(bogota_data[bogota_data$Date == Date,]) > 0, 
                                              bogota_data$ICU[bogota_data$Date == Date], 0))

bogota_all_data$number2[bogota_all_data$Ubicacion == "Hospital UCI" & is.na(bogota_all_data$number2)] <- 0
bogota_all_data$number[bogota_all_data$Ubicacion == "Hospital UCI"] <- bogota_all_data$number2[bogota_all_data$Ubicacion == "Hospital UCI"]

#populate number when 0 with the same value as the day before
for(i in 1:nrow(bogota_all_data))
{
  if((bogota_all_data$number[i] == 0 | is.na(bogota_all_data$number[i])) & bogota_all_data$Ubicacion[i] == "Hospital")
  {
    bogota_all_data$number[i] <- bogota_all_data$number[bogota_all_data$Ubicacion == "Hospital" & bogota_all_data$Date == (bogota_all_data$Date[i] - 1)]
  }
  if((bogota_all_data$number[i] == 0 | is.na(bogota_all_data$number[i])) & bogota_all_data$Ubicacion[i] == "Hospital UCI")
  {
    bogota_all_data$number[i] <- bogota_all_data$number[bogota_all_data$Ubicacion == "Hospital UCI" & bogota_all_data$Date == (bogota_all_data$Date[i] - 1)]
  }
}

#plot hospitalizations and hospitalizations in ICU
ggplot(bogota_all_data[bogota_all_data$Ubicacion %in% c("Hospital", "Hospital UCI"),], 
       aes(x = Date, y = number, fill = Ubicacion)) + 
  geom_bar(stat = "identity", position="stack") + 
  xlab("Fecha") + 
  ylab("Conteo") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 20, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 16)) + 
  scale_x_date(date_labels="%d/%m", date_breaks ="2 days", expand = c(0, 0), 
               limits = c(min(bogota_all_data$Date[bogota_all_data$Ubicacion %in% c("Hospital", "Hospital UCI")]), 
                          max(bogota_all_data$Date[bogota_all_data$Ubicacion %in% c("Hospital", "Hospital UCI")]))) + 
  scale_fill_manual(values = c("navyblue", "orange")) + 
  geom_vline(xintercept = as.Date("2020-04-27"), linetype="dashed", color = "grey", size = 1) + 
  annotate(geom="text", x=as.Date("2020-04-26"), y=2600, label="Apertura parcial", color="black") + 
  geom_vline(xintercept = as.Date("2020-06-01"), linetype="dashed", color = "grey", size = 1) + 
  annotate(geom="text", x=as.Date("2020-05-30"), y=2600, label="Cuarentena Kennedy", color="black") + 
  geom_vline(xintercept = as.Date("2020-06-15"), linetype="dashed", color = "grey", size = 1) + 
  annotate(geom="text", x=as.Date("2020-06-14"), y=2600, label="Cuarentena 5 UPZ", color="black") + 
  geom_vline(xintercept = as.Date("2020-06-30"), linetype="dashed", color = "grey", size = 1) + 
  annotate(geom="text", x=as.Date("2020-06-29"), y=2600, label="Cuarentena otras UPZ", color="black") 



########PART II. Build estimation model##########
#function to build model and generate all results
build_model <- function(bogota_all_data, days_train, days_forecast, exponential_flag)
{
  f_holt <- holt(bogota_all_data$number[bogota_all_data$Ubicacion == "Hospital UCI"][1:days_train], exponential = exponential_flag, h = days_forecast)
  fcast <- forecast(f_holt, h = days_forecast)
  forecast_means <- as.numeric(fcast$mean)
  write.table(summary(fcast), file = paste0("forecast_model_exp", exponential_flag, ".txt"))
  
  #final forecast data
  forecast_data <- data.frame(bogota_all_data[bogota_all_data$Ubicacion %in% c("Hospital UCI"), c("Date", "number")])
  forecast_data$forecast_number <- forecast_data$number
  
  for(i in 1:length(forecast_means))
  {
    if(nrow(forecast_data) >= (days_train + i))
    {
      forecast_data$forecast_number[days_train + i] <- forecast_means[i]
    }
    else
    {
      forecast_data <- rbind(forecast_data, data.frame(Date = max(forecast_data$Date) + 1, 
                                                       number = forecast_means[i], 
                                                       forecast_number = forecast_means[i]))
    }
    
  }
  
  forecast_data <- forecast_data %>% mutate(predicted_actual = ifelse(Date <= "2020-07-06", "actual", "predicted"))
  colors <- c("actual" = "orange", "predicted" = "red")
  
  
  #calculate percentages of ocuppation
  number_beds <- c(1039, 1200, 1400, 1600, 1800, 2000)
  
  occupation_beds_1 <- forecast_data %>% mutate(number_beds = number_beds[1], percentage_occupation = forecast_number / number_beds[1])
  occupation_beds_2 <- forecast_data %>% mutate(number_beds = number_beds[2], percentage_occupation = forecast_number / number_beds[2])
  occupation_beds_3 <- forecast_data %>% mutate(number_beds = number_beds[3], percentage_occupation = forecast_number / number_beds[3])
  occupation_beds_4 <- forecast_data %>% mutate(number_beds = number_beds[4], percentage_occupation = forecast_number / number_beds[4])
  occupation_beds_5 <- forecast_data %>% mutate(number_beds = number_beds[5], percentage_occupation = forecast_number / number_beds[5])
  occupation_beds_6 <- forecast_data %>% mutate(number_beds = number_beds[6], percentage_occupation = forecast_number / number_beds[6])
  
  ocuppation_forecast_final <- rbind(occupation_beds_1, occupation_beds_2, occupation_beds_3, occupation_beds_4, occupation_beds_5, occupation_beds_6)
  ocuppation_forecast_final$number_beds <- paste0(as.character(ocuppation_forecast_final$number_beds), " Camas UCI")
  write.csv(ocuppation_forecast_final, file = paste0("forecast_table_exp", exponential_flag, ".csv"))
  
  #plot
  plot <- ggplot(ocuppation_forecast_final, 
         aes(x = Date, y = percentage_occupation * 100, fill = predicted_actual)) + 
    geom_bar(stat = "identity", position="stack") + 
    facet_rep_wrap(~ number_beds, ncol = 2, , scales='fixed', repeat.tick.labels = TRUE) + 
    geom_hline(yintercept = 100, size = 1) + 
    xlab("Fecha") + 
    ylab("Porcentaje Ocupación UCI (%)") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, size = 16),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.position = "none", 
          strip.text.x = element_text(size = 18)) + 
    scale_x_date(date_labels="%d/%m", date_breaks ="4 days", expand = c(0, 0), 
                 limits = c(min(ocuppation_forecast_final$Date), 
                            max(ocuppation_forecast_final$Date))) + 
    scale_fill_manual(values = c("orange", "red"))
  
  ggsave(plot, file = paste0("forecast_plot_exp", exponential_flag, ".pdf"), width = 28, height = 16, limitsize = FALSE)
}

#train holt method of exponential smoothing
days_train <- 82
days_forecast <- 52
exponential_flag <- FALSE

#forecast_pessimistic
build_model(bogota_all_data, days_train, days_forecast, exponential_flag)

