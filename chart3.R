
# Chart 3: The House of Broken Angels Checkouts over 2022

library("ggplot2")
library("dplyr")
library("scales")

spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", 
                     stringsAsFactors = FALSE)

spl_data <- spl_data %>% 
            mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_data$Date <- as.Date(spl_data$Date, format = "%Y-%m-%d")

house_broken_angels <- spl_data %>% 
                       filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>% 
                       filter(Creator %in% c("Luis Alberto Urrea", 
                                             "Urrea, Luis Alberto")) %>% 
                       filter(Title %in% 
                                  c("The house of broken angels : a novel / 
                                  Luis Alberto Urrea.", 
                                  "The house of broken angels / 
                                  Luis Alberto Urrea.", 
                                  "The house of broken angels / 
                                  by Luis Alberto Urrea.", 
                                  "The House of Broken Angels (Unabridged)", 
                                  "The House of Broken Angels (unabridged)", 
                                  "The House of Broken Angels")) %>% 
                        filter(CheckoutYear %in% "2022") %>%
                        group_by(Date, MaterialType) %>%
                        summarize(house_sum = sum(Checkouts))

ggplot(data = house_broken_angels, aes(x = Date, y = house_sum)) + 
  geom_col(aes(fill = MaterialType)) +
  labs(title = "The House of Broken Angels Checkouts over 2022", 
       x = "Month in 2022", 
       y = "Number of Checkouts", 
       fill = "Checkout Type") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = label_number_si(), limits = c(0,4000))
