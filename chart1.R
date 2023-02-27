
# Chart 1: Digital and physical book checkouts over time (2017-2022)

library("ggplot2")
library("dplyr")
library("scales")

spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", 
                     stringsAsFactors = FALSE)

book_time <- spl_data %>% 
             filter(MaterialType %in% c("EBOOK", "AUDIOBOOK", "BOOK")) %>%
             filter(CheckoutYear %in% c("2017", "2018", "2019", "2020", "2021", 
                                        "2022")) %>% 
             group_by(MaterialType, CheckoutYear) %>% 
             summarize(year_sum = sum(Checkouts))

ggplot(book_time, aes(x = CheckoutYear, y = year_sum)) + 
    geom_line(aes(color = MaterialType)) +
    labs(title = "Ebook, Audiobook, and Book Checkouts from 2017-2022", 
         x = "Year", 
         y = "Number of Checkouts",
         color = "Checkout Type") +
   scale_y_continuous(labels = label_number_si(), limits = c(150000,1550000))
                      