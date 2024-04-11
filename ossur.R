library(tidyverse)
stations <- c('S1', 'SX', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8.1', 'S8.2',
              'S9', 'S11', 'S12', 'Done')

lines <- readLines("/Users/margretsnorradottir/Downloads/HÍ almennt/Vor 2024/Hermun/Lot_ProcessTracking.csv")

# Remove the first row (header)
lines <- lines[-1]

# Split each line into columns based on semicolon delimiter
data <- lapply(strsplit(lines, ";"), function(x) {
  data.frame(matrix(x, ncol = length(x), byrow = TRUE))
})

# Combine the data frames into a single data frame
dat <- do.call(rbind, data)

# Rename columns
colnames(dat) <- c("LotNumber", "ProdType", "Status", "Timestamp")

# Convert Lot_number to numeric (assuming it represents an ID)
dat$LotNumber <- as.numeric(dat$LotNumber)

dat <- dat %>% mutate(
    Status = str_replace(Status, "S9\\+S10", "S9"),
    LotNumber = as.factor(LotNumber),
    ProdType = as.factor(ProdType),
    OrgStatus = as.factor(Status),
    Timestamp = as.POSIXct(Timestamp, format = "%d.%m.%Y %H:%M"),
    Ready = str_detect(Status, "Ready for"),
    Status = str_replace(Status, "Ready for ", ""),
    Status = factor(Status, levels = stations),
    Scrapped = str_detect(Status, "Scrapped$"),
    NextStation = str_extract(OrgStatus, "(?<=Ready for )(.*)$"),
    Station = if_else(Ready, NA, str_extract(Status, "SX|(S[0-9.+]+)")),
    StationStatus = if_else(Ready | Scrapped | str_detect(Status, "Done"), 'End', 'Start')
  ) %>%
  rename(Prod = ProdType, Lot = LotNumber) %>%
  arrange(Timestamp, Lot, Status)

dat.done <- dat %>% # if there is more then one row with Status = 'Done' then only keep the one with
  # the latest Timestamp
  filter(Status == 'Done') %>%
  group_by(Lot) %>%
  arrange(desc(Timestamp)) %>%
  slice(1) %>%
  ungroup()

dat <- dat %>% # I want to keep all entries that are not 'Done' AND the latest 'Done' entry
  filter(Status != 'Done') %>%
  bind_rows(dat.done) %>%
  arrange(Lot, Timestamp, Status)

dat <- dat %>%
  group_by(Lot) %>%
  arrange(Timestamp, Status) %>%  # Make sure data is sorted by Timestamp and Status within each group
  mutate(
    Station = # if NA then find the previous station
      if_else(is.na(Station), lag(Station), Station),
    Station = factor(Station, levels = stations),
  )


dat %>%
  group_by(OrgStatus, Station) %>%
  tally() %>%
  group_by(OrgStatus) %>%
  mutate(grand_total = sum(n)) %>%
  ggplot() +
  geom_bar(aes(x = reorder(OrgStatus, grand_total), y = n, fill = Station), stat = "identity") +
  geom_text(aes(x = reorder(OrgStatus, grand_total), y = grand_total, label = grand_total), hjust =
    1, color = 'white') +
  labs(title = "Number of records per Status", x = NULL, y = "Count",
       subtitle = paste("Total number of records are ", nrow(dat))) +
  coord_flip() +
  theme_minimal()

mdat <- dat %>%
  filter(!is.na(Timestamp)) %>%
  group_by(Lot, Station) %>%
  summarise(
    start_time = min(Timestamp[StationStatus == 'Start'], na.rm = TRUE),
    end_time = max(Timestamp[StationStatus == 'End'], na.rm = TRUE),
  ) %>%
  filter(!is.infinite(end_time - start_time)) %>%
  mutate(
    duration = end_time - start_time
  )

mdat <- mdat %>%
  filter(as.Date(start_time) == as.Date(end_time))

mdat <- mdat %>%
  mutate(
    duration = as.numeric(end_time - start_time, units = "hours")
  )

###############################################
#                                             #
#            Summary statistics               #
#                                             #
###############################################

library(dplyr)
library(ggplot2)

mdat_filt <- mdat %>%
  filter(duration > 0)

# Histogram
station_histograms <- mdat_filt1 %>%
  ggplot(aes(x = duration)) +
  geom_histogram(binwidth = NULL, fill = "hotpink", color = "white") +
  facet_wrap(~Station, scales = "free") +
  labs(title = "Dreifing tíma á hverri stöð",
       x = "Tími (klukkutímar)", y = "Fjöldi") +
  theme_minimal()

print(station_histograms)

################ PÆLINGAR #################
# Spuring um að fjarlægja outliers :) 
# Athuga S8.2 og öll núllin, líka á fleiri stöðvum
# Endurskrifa S5 sem 3 tíma
# Breyta hvernig kæling er reiknuð því hún fer beint
# HVAÐ ER DONE??????? 
# Fyrir D er Done eftir S7 (?)
# Stundum er Done eftir S5
# Er fræs gert milli daga í batches fyrir 8.2?
# VESEN
# Fastur tími fyrir vélarslípun?
########################################### 

# Dreifingar

remove_outliers <- function(data) {
  data %>%
    group_by(Station) %>%
    mutate(Q1 = quantile(duration, 0.25),
           Q3 = quantile(duration, 0.75),
           IQR = Q3 - Q1,
           lower_bound = Q1 - 1.5 * IQR,
           upper_bound = Q3 + 1.5 * IQR) %>%
    filter(duration >= lower_bound & duration <= upper_bound) %>%
    select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound)
}

# Fjarlægja outliers
mdat_filt1 <- remove_outliers(mdat_filt)

##### Aðeins viðbætur frá okkur #####
mdat_filt1 <- mdat_filt1 %>%
  mutate(duration = if_else(Station == "S5", 3, duration))

# Tölfræði fyrir hverja stöð
station_summary <- mdat_filt1 %>%
  group_by(Station) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE),
            sd_duration = sd(duration, na.rm = TRUE),
            min_duration = min(duration, na.rm = TRUE),
            max_duration = max(duration, na.rm = TRUE))

# Histogram og density plot
station_histograms <- mdat_filt1 %>%
  ggplot(aes(x = duration)) +
  geom_histogram(binwidth = NULL, fill = "white", color = "black") +
  facet_wrap(~Station, scales = "free") +
  labs(title = "Dreifing tíma á hverri stöð",
       x = "Tími (klukkutímar)", y = "Fjöldi") +
  theme_minimal()

for (station in unique(mdat_filt1$Station)) {
  station_summary_subset <- station_summary[station_summary$Station == station, ]
  normal_params <- list(mean = station_summary_subset$mean_duration, sd = station_summary_subset$sd_duration)
  uniform_params <- list(min = station_summary_subset$min_duration, max = station_summary_subset$max_duration)
  exp_params <- list(rate = 1 / station_summary_subset$mean_duration)
  
  station_histograms <- station_histograms +
    stat_function(data = filter(mdat_filt1, Station == station), fun = dnorm, args = normal_params,
                  aes(color = "Normal")) +
    stat_function(data = filter(mdat_filt1, Station == station), fun = dunif, args = uniform_params,
                  aes(color = "Uniform")) +
    stat_function(data = filter(mdat_filt1, Station == station), fun = dexp, args = exp_params,
                  aes(color = "Exponential"))
}

station_histograms <- station_histograms +
  scale_color_manual(values = c("Normal" = "hotpink", "Uniform" = "cornflowerblue", "Exponential" = "lightgreen")) +
  theme(legend.position = "top")

print(station_histograms)

#######

mdat_filt1 <- mdat_filt1 %>%
  group_by(Station) %>%
  mutate(
    mean_duration = mean(duration, na.rm = TRUE),
    sd_duration = sd(duration, na.rm = TRUE),
    min_duration = min(duration, na.rm = TRUE),
    max_duration = max(duration, na.rm = TRUE),
    ecdf = ecdf(duration)(duration),
    diff_normal = ecdf - pnorm(duration, mean = mean_duration, sd = sd_duration),
    diff_uniform = ecdf - punif(duration, min = min_duration, max = max_duration),
    diff_exponential = ecdf - pexp(duration, rate = 1 / mean_duration)
  )

# Create Density Plot with histograms and theoretical CDFs
station_histograms <- mdat_filt1 %>%
  ggplot(aes(x = duration)) +
  geom_histogram(binwidth = NULL, fill = "white", color = "black") +
  facet_wrap(~Station, scales = "free") +
  labs(title = "Density Plot of Duration at Each Station",
       x = "Duration (hours)", y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("Normal" = "red", "Uniform" = "blue", "Exponential" = "green")) +
  geom_line(aes(y = ecdf, color = "Empirical ECDF")) +  # Add ECDF
  geom_line(aes(y = diff_normal, color = "Normal CDF")) +  # Add Normal CDF
  geom_line(aes(y = diff_uniform, color = "Uniform CDF")) +  # Add Uniform CDF
  geom_line(aes(y = diff_exponential, color = "Exponential CDF")) +  # Add Exponential CDF
  labs(color = "Distribution")  # Update legend label

print(station_histograms)



