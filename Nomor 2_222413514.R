# ============================================================
# VAR / VECM - Inflasi, Suku Bunga, dan Nilai Tukar Rupiah/USD
# Data bulanan 2010-2024 (180 observasi)
# ============================================================

# ------------------------------------------------------------
# 1. PACKAGE
# ------------------------------------------------------------
packages <- c("readxl", "tseries", "vars", "urca")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

library(readxl)
library(tseries)
library(vars)
library(urca)

# ------------------------------------------------------------
# 2. IMPORT DATA EXCEL
# ------------------------------------------------------------
# Path file Excel
# Gunakan "/" agar path Windows aman dibaca oleh R.
file_path <- "C:/Users/Lenovo/Documents/Folder Tugas/Tugas Kampus/Tugas Tingkat 2/Semester 4/Analisis Runtun Waktu/UAS/No 2/Master Data 2010_2024.xlsx"

raw <- read_excel(
  file_path,
  sheet = "MasterData",
  col_names = FALSE
)

# ------------------------------------------------------------
# 3. UBAH FORMAT WIDE -> BULANAN (180 OBSERVASI)
# ------------------------------------------------------------
# Struktur MasterData:
# setiap tahun terdiri dari 3 kolom:
# BI RATE | Inflation | Kurs Tengah

year_cols <- which(
  !is.na(suppressWarnings(as.numeric(unlist(raw[1, ]))))
)
year_cols <- year_cols[year_cols > 1]

years <- as.integer(
  sapply(year_cols, function(j) raw[[j]][1])
)

bulan <- 1:12

data_list <- lapply(seq_along(year_cols), function(i) {
  
  j <- year_cols[i]
  
  data.frame(
    Tahun = years[i],
    Bulan = bulan,
    Suku_Bunga = as.numeric(raw[[j]][3:14]),
    Inflasi = as.numeric(raw[[j + 1]][3:14]),
    Kurs = as.numeric(raw[[j + 2]][3:14])
  )
})

data <- do.call(rbind, data_list)

# Cek data
data <- data[order(data$Tahun, data$Bulan), ]
row.names(data) <- NULL

cat("Jumlah observasi =", nrow(data), "\n")
print(head(data))
print(tail(data))

if (nrow(data) != 180) {
  warning("Jumlah observasi bukan 180. Periksa struktur Excel.")
}

if (anyNA(data)) {
  stop("Terdapat NA pada data. Periksa kembali file Excel.")
}

# ------------------------------------------------------------
# 4. TRANSFORMASI DATA
# ------------------------------------------------------------
# Kurs diubah ke log agar skala lebih stabil.
# Inflasi dan suku bunga dibiarkan dalam persen karena dapat
# bernilai negatif/positif dan tidak perlu di-log.

data$ln_Kurs <- log(data$Kurs)

# Deret waktu bulanan
Y <- ts(
  data[, c("Inflasi", "Suku_Bunga", "ln_Kurs")],
  start = c(2010, 1),
  frequency = 12
)

# Cek struktur time series
print(Y)

# ------------------------------------------------------------
# 5. EKSPLORASI / GRAFIK
# ------------------------------------------------------------
plot(
  Y,
  main = "Inflasi, Suku Bunga, dan Nilai Tukar Rupiah/USD"
)

# ------------------------------------------------------------
# 6. UJI STASIONERITAS - ADF
# ------------------------------------------------------------
# H0 ADF : terdapat unit root (tidak stasioner)
# Jika p-value < 0.05 -> tolak H0 -> stasioner

adf_level <- data.frame(
  Variabel = colnames(Y),
  P_Value = sapply(
    as.data.frame(Y),
    function(x) adf.test(x, k = 12)$p.value
  )
)

adf_diff1 <- data.frame(
  Variabel = colnames(Y),
  P_Value = sapply(
    as.data.frame(Y),
    function(x) adf.test(diff(x), k = 12)$p.value
  )
)

cat("\n=== ADF LEVEL ===\n")
print(adf_level)

cat("\n=== ADF FIRST DIFFERENCE ===\n")
print(adf_diff1)

# ------------------------------------------------------------
# 7. UJI STASIONERITAS - KPSS (KONFIRMASI)
# ------------------------------------------------------------
# H0 KPSS : data stasioner
# p-value < 0.05 -> tolak H0 -> tidak stasioner

kpss_level <- data.frame(
  Variabel = colnames(Y),
  P_Value = sapply(
    as.data.frame(Y),
    function(x) kpss.test(x, null = "Level")$p.value
  )
)

kpss_diff1 <- data.frame(
  Variabel = colnames(Y),
  P_Value = sapply(
    as.data.frame(Y),
    function(x) kpss.test(diff(x), null = "Level")$p.value
  )
)

cat("\n=== KPSS LEVEL ===\n")
print(kpss_level)

cat("\n=== KPSS FIRST DIFFERENCE ===\n")
print(kpss_diff1)

