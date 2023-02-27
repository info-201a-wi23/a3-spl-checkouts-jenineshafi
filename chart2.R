
# Chart 2: Ebook and Audiobook Checkouts over 2022 

library("ggplot2")
library("dplyr")
library("stringr")
library("scales")

spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", 
                     stringsAsFactors = FALSE)

spl_data <- spl_data %>% 
            mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_data$Date <- as.Date(spl_data$Date, format = "%Y-%m-%d")

audio_ebook <- spl_data %>% 
               filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
               filter(CheckoutYear %in% "2022") %>%
               group_by(Date, MaterialType) %>% 
               summarize(Sum = sum(Checkouts))

ggplot(audio_ebook, aes(x = Date, y = Sum)) + 
  geom_col(aes(fill = MaterialType)) +
  labs(title = "Audiobook and Ebook Checkouts during 2022", 
       x = "Month in 2022", 
       y = "Number of Checkouts", 
       fill = "Checkout Type") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = label_number_si())
