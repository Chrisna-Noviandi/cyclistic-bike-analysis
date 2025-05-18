install.packages("dplyr")
library(dplyr)

#inpot data csv agar bisa dibaca
install.packages("readr")
library(readr)  # Untuk membaca data CSV

install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
install.packages("here")
library(here)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)

install.packages("scales")
library(scales)

# Mengimpor file CSV dari cloud
trip_data_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
trip_data_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# Melihat beberapa baris pertama data
head(trip_data_2019)
head(trip_data_2020)

# Membersihkan dan mengolah data 2019 dengan mengganti nilai usertype
cleaned_data_2019 <- trip_data_2019 %>%
  select(trip_id, start_time, end_time, usertype) %>%  # Memilih kolom yang diperlukan
  filter(!is.na(start_time) & !is.na(end_time) & !is.na(usertype)) %>%  # Menghapus baris dengan NA
  mutate(
    # Menghitung ride_length sebagai selisih waktu antara start_time dan end_time (dalam detik)
    ride_length = as.numeric(difftime(end_time, start_time, units = "secs")),
    
    # Menambahkan kolom day_of_week berdasarkan start_time (hari dalam minggu)
    day_of_week = weekdays(as.Date(start_time)),
    
    # Mengganti nilai usertype: Subscriber menjadi Member, Customer menjadi Casual
    usertype = recode(usertype, "Subscriber" = "member", "Customer" = "casual")
  )


# Membersihkan dan mengolah data 2020
cleaned_data_2020 <- trip_data_2020 %>%
  # Mengganti nama kolom supaya konsisten dengan data 2019
  rename(
    trip_id = ride_id,
    start_time = started_at,
    end_time = ended_at,
    usertype = member_casual
  ) %>%
  # Memilih kolom yang diperlukan
  select(trip_id, start_time, end_time, usertype) %>%
  # Menghapus baris dengan NA pada kolom yang diperlukan
  filter(!is.na(start_time) & !is.na(end_time) & !is.na(usertype)) %>%
  # Menambahkan kolom ride_length dan day_of_week
  mutate(
    # Menghitung ride_length sebagai selisih waktu antara start_time dan end_time (dalam detik)
    ride_length = as.numeric(difftime(end_time, start_time, units = "secs")),
    
    # Menambahkan kolom day_of_week berdasarkan start_time (hari dalam minggu)
    day_of_week = weekdays(as.Date(start_time))
  )

# Melihat hasil
head(cleaned_data_2019)
head(cleaned_data_2020)

# Menampilkan jumlah baris di data 2019
nrow(cleaned_data_2019)
nrow(cleaned_data_2020)

# Melihat nilai unik di kolom 'usertype' pada dataset 2019
unique(cleaned_data_2019$usertype)
unique(cleaned_data_2020$usertype)

# Mengubah trip_id di kedua data frame menjadi tipe data yang sama (misalnya character)
cleaned_data_2019 <- cleaned_data_2019 %>%
  mutate(trip_id = as.character(trip_id))

cleaned_data_2020 <- cleaned_data_2020 %>%
  mutate(trip_id = as.character(trip_id))

# Menggabungkan cleaned_data_2019 dan cleaned_data_2020
combined_data_2019_2020 <- bind_rows(cleaned_data_2019, cleaned_data_2020) %>%
  mutate(
    # Menambahkan kolom 'year' yang berisi tahun dari 'start_time'
    year = format(start_time, "%Y"),
    
    # Menambahkan kolom 'ride_duration' yang mengonversi 'ride_length' ke format jam:menit:detik
    ride_duration = sprintf("%02d:%02d:%02d", 
                            ride_length %/% 3600, # Jam
                            (ride_length %% 3600) %/% 60, # Menit
                            ride_length %% 60) # Detik
  )

# Menampilkan hasil untuk memastikan kolom baru sudah ditambahkan
head(combined_data_2019_2020)


# Mengelompokkan data berdasarkan 'year' dan 'usertype', kemudian menghitung jumlahnya
count_data <- combined_data_2019_2020 %>%
  group_by(year, usertype) %>%
  summarise(count = n(), .groups = 'drop')

# Membuat diagram batang dengan ggplot2
gambar1 <- ggplot(count_data, aes(x = year, y = count, fill = usertype)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +  # Memisahkan batang dengan 'dodge'
  geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = 0, size = 6) +  # Menempatkan label di tengah batang
  scale_y_continuous(labels = label_comma()) +  # Mengatur format angka pada sumbu y
  labs(title = "Number of Users by User Type per Year",  # Mengubah judul menjadi bahasa Inggris
       x = "Year",  # Mengubah label x-axis
       y = "Number of Users",  # Mengubah label y-axis
       fill = "User Type") +  # Mengubah label legenda
  theme_minimal() +
  theme(legend.position = "top")


# Simpan gambar ke file
ggsave("user_type_per_year_plot.png", plot = gambar1, width = 8, height = 6)

# Pastikan ride_length numerik dan konversi ke menit
combined_data_2019_2020$ride_length <- as.numeric(combined_data_2019_2020$ride_length) / 60

# Urutkan hari dari Senin ke Minggu
combined_data_2019_2020$day_of_week <- factor(
  combined_data_2019_2020$day_of_week,
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

# Buat plot
gambar2 <- ggplot(combined_data_2019_2020, aes(x = day_of_week, y = ride_length, color = usertype)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Ride Duration vs Day of the Week by User Type",
    x = "Day of the Week",
    y = "Ride Duration (minutes)",
    color = "User Type"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x), " min")) +
  theme_minimal() +
  theme(legend.position = "top")

# Simpan gambar
ggsave("user_type_vs_ride_length.png", plot = gambar2, width = 8, height = 6)









