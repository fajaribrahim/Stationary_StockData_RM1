# Step 1: Load library
library(tseries)

# Step 2: Membaca data CSV
data <- read.csv("D:/[02] Collage/SEMESTER 7/Analisis Deret Waktu/Tugas/Tugas R Studio/Penugasan Rstudio Materi 1/Dataset/Saham/Semua/ASII.csv")

# Step 3: Periksa struktur dataset dan preview data
str(data)
head(data)

# Step 4: Membersihkan kolom 'date' untuk menghapus bagian waktu dan mengonversinya ke format Date
data$date <- sub("T.*", "", data$date)  # Menghapus bagian waktu
data$date <- as.Date(data$date, format="%Y-%m-%d")  # Mengonversi ke tipe Date

# Step 5: Menyusun data sebagai deret waktu (gunakan kolom 'close' sebagai data utama)
data_ts <- ts(data$close, start = c(2019, 7), frequency = 252)  # Sesuaikan start dan frequency sesuai data Anda

# Step 6: Plot data deret waktu asli
plot(data_ts, main = "Harga Saham ASII", ylab = "Harga Penutupan", xlab = "Waktu")

# Step 7: Melakukan uji Augmented Dickey-Fuller untuk memeriksa stasioneritas data
adf_result <- adf.test(data_ts)
print(adf_result)

# Step 8: Interpretasi hasil uji ADF
if (adf_result$p.value < 0.05) {
  print("Data bersifat stasioner")
} else {
  print("Data tidak bersifat stasioner. Lakukan differencing untuk membuat data stasioner.")
  
  # Step 9: Lakukan differencing jika data tidak stasioner
  data_diff <- diff(data_ts)
  
  # Step 10: Plot data setelah differencing
  plot(data_diff, main = "Data Saham ASII Setelah Differencing", ylab = "Perubahan Harga", xlab = "Waktu")
  
  # Step 11: Uji ADF setelah differencing
  adf_result_diff <- adf.test(data_diff)
  print(adf_result_diff)
  
  # Step 12: Interpretasi hasil uji ADF setelah differencing
  if (adf_result_diff$p.value < 0.05) {
    print("Data setelah differencing bersifat stasioner")
  } else {
    print("Data masih tidak bersifat stasioner setelah differencing")
  }
}