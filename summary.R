
# Digital Checkouts at the Seattle Public Library 


library("dplyr")

spl_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# What proportion of all checkouts are for digital books?
digital_checkout <- spl_data %>% 
                     filter(UsageClass %in% "Digital") %>% 
                     filter(MaterialType %in% c("EBOOK", "AUDIOBOOK"))  

digital_num <- (nrow(digital_checkout))/(nrow(spl_data))

# 42.7% of checkouts are for digital books

# How has the number of digital book checkouts changed over the years (2017-2022)?
digital_over_time <- spl_data %>% 
  filter(UsageClass %in% "Digital") %>%
  filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
  filter(CheckoutYear %in% c("2017", "2018", "2019", "2020", "2021", "2022")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(year_sum = sum(Checkouts))

# 2022 had the most digital book checkouts, with the number increasing since 2017

ebook_over_time <- spl_data %>% 
  filter(UsageClass %in% "Digital") %>%
  filter(MaterialType %in% "EBOOK") %>%
  filter(CheckoutYear %in% c("2017", "2018", "2019", "2020", "2021", "2022")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(year_sum = sum(Checkouts))

# 2020 had most ebook checkouts and has been decreasing since  

audiobook_over_time <- spl_data %>% 
  filter(UsageClass %in% "Digital") %>%
  filter(MaterialType %in% "AUDIOBOOK") %>%
  filter(CheckoutYear %in% c("2017", "2018", "2019", "2020", "2021", "2022")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(year_sum = sum(Checkouts))

# 2022 also had the most audiobook checkouts, with the number increasing since 2017

# What was the most checked out digital book of 2022?
twenty_two_book <- spl_data %>%
                   filter(UsageClass %in% "Digital") %>%
                   filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
                   filter(CheckoutYear %in% "2022") %>%
                   filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>% 
                   pull(Title)

# The House of Broken Angels 

# What month of 2022 had the most digital book checkouts? 
twenty_two_checkouts <- spl_data %>%
                        filter(UsageClass %in% "Digital") %>% 
                        filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>% 
                        filter(CheckoutYear %in% "2022") %>% 
                        group_by(CheckoutMonth) %>%
                        summarize(month_sum = sum(Checkouts)) %>% 
                        filter(month_sum == max(month_sum, na.rm = TRUE)) %>% 
                        pull(CheckoutMonth)

# The month in 2022 with the most checkouts was December (with 188,201 checkouts) 

# What was the most checked out digital book of each month in 2022?
dec_book <- spl_data %>%
            filter(UsageClass %in% "Digital") %>%
            filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
            filter(CheckoutYear %in% "2022") %>%
            filter(CheckoutMonth %in% "12") %>%
            filter(Checkouts == max(Checkouts, na.rm = TRUE))

# Braiding Sweetgrass

nov_book <- spl_data %>%
            filter(UsageClass %in% "Digital") %>%
            filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
            filter(CheckoutYear %in% "2022") %>%
            filter(CheckoutMonth %in% "11") %>%
            filter(Checkouts == max(Checkouts, na.rm = TRUE))

# A Snake Falls to Earth 

oct_book <- spl_data %>%
            filter(UsageClass %in% "Digital") %>%
            filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
            filter(CheckoutYear %in% "2022") %>%
            filter(CheckoutMonth %in% "10") %>%
            filter(Checkouts == max(Checkouts, na.rm = TRUE))

# The House of Broken Angels 

sept_book <- spl_data %>%
             filter(UsageClass %in% "Digital") %>%
             filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
             filter(CheckoutYear %in% "2022") %>%
             filter(CheckoutMonth %in% "9") %>%
             filter(Checkouts == max(Checkouts, na.rm = TRUE))

# The House of Broken Angels 

august_book <- spl_data %>%
               filter(UsageClass %in% "Digital") %>%
               filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
               filter(CheckoutYear %in% "2022") %>%
               filter(CheckoutMonth %in% "8") %>%
               filter(Checkouts == max(Checkouts, na.rm = TRUE))

# Braiding Sweetgrass 

july_book <- spl_data %>%
             filter(UsageClass %in% "Digital") %>%
             filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
             filter(CheckoutYear %in% "2022") %>%
             filter(CheckoutMonth %in% "7") %>%
             filter(Checkouts == max(Checkouts, na.rm = TRUE))

# The Girl in His Shadow: A Novel 

june_book <- spl_data %>%
              filter(UsageClass %in% "Digital") %>%
              filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
              filter(CheckoutYear %in% "2022") %>%
              filter(CheckoutMonth %in% "6") %>%
              filter(Checkouts == max(Checkouts, na.rm = TRUE)) 

# Braiding Sweetgrass 

may_book <- spl_data %>%
            filter(UsageClass %in% "Digital") %>%
            filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
            filter(CheckoutYear %in% "2022") %>%
            filter(CheckoutMonth %in% "5") %>%
            filter(Checkouts == max(Checkouts, na.rm = TRUE)) 

# Braiding Sweetgrass 

april_book <- spl_data %>%
              filter(UsageClass %in% "Digital") %>%
              filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
              filter(CheckoutYear %in% "2022") %>%
              filter(CheckoutMonth %in% "4") %>%
              filter(Checkouts == max(Checkouts, na.rm = TRUE)) 

# Music Is History 

march_book <- spl_data %>%
              filter(UsageClass %in% "Digital") %>%
              filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
              filter(CheckoutYear %in% "2022") %>%
              filter(CheckoutMonth %in% "3") %>%
              filter(Checkouts == max(Checkouts, na.rm = TRUE)) 

# Braiding Sweetgrass 

feb_book <- spl_data %>%
            filter(UsageClass %in% "Digital") %>%
            filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
            filter(CheckoutYear %in% "2022") %>%
            filter(CheckoutMonth %in% "2") %>%
            filter(Checkouts == max(Checkouts, na.rm = TRUE)) 

# The Five Wounds: A Novel 

jan_book <- spl_data %>%
            filter(UsageClass %in% "Digital") %>%
            filter(MaterialType %in% c("EBOOK", "AUDIOBOOK")) %>%
            filter(CheckoutYear %in% "2022") %>%
            filter(CheckoutMonth %in% "1") %>%
            filter(Checkouts == max(Checkouts, na.rm = TRUE))

# Braiding Sweetgrass 
                     
      

              

  
  


                        


