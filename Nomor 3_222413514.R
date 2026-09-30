# =========================================================
# ERROR CORRECTION MODEL (ECM)
# Konsumsi Rumah Tangga dan Produk Domestik Bruto (PDB)
# Data Triwulanan 2010 Q1 - 2026 Q2
# =========================================================

# 1. Paket
library(readxl)
library(tseries)
library(urca)

# 2. Baca data Excel
# Pastikan file Excel berada di working directory yang sama
setwd("C:/Users/Lenovo/Documents/Folder Tugas/Tugas Kampus/Tugas Tingkat 2/Semester 4/Analisis Runtun Waktu/UAS/No 3")
raw <- read_excel(
  "Dataset_PDB_Konsumsi Rumah Tangga_2010-2026.xlsx",
  sheet = "Data 1",
  col_names = FALSE
)

# 3. Ambil data dari tabel
# Baris 4 = Konsumsi Rumah Tangga
# Baris 5 = PDB
Konsumsi <- as.numeric(unlist(raw[4, -1]))
PDB      <- as.numeric(unlist(raw[5, -1]))

# Gabungkan data
data <- data.frame(Konsumsi, PDB)

# Hapus observasi yang belum tersedia (misalnya "-")
data <- data[complete.cases(data), ]

# =========================================================
# 4. Bentuk variabel logaritma
# =========================================================

data$ln_Konsumsi <- log(data$Konsumsi)
data$ln_PDB      <- log(data$PDB)

# =========================================================
# 5. Bentuk objek time series
# =========================================================

ts_data <- ts(
  data[, c("ln_Konsumsi", "ln_PDB")],
  start = c(2010, 1),
  frequency = 4
)

# Lihat data time series
print(ts_data)

# =========================================================
# 6. UJI STASIONERITAS ADF
# =========================================================

cat("\n==============================\n")
cat("UJI ADF - LEVEL\n")
cat("==============================\n")

adf_konsumsi_level <- adf.test(ts_data[, "ln_Konsumsi"])
adf_pdb_level      <- adf.test(ts_data[, "ln_PDB"])

print(adf_konsumsi_level)
print(adf_pdb_level)

cat("\n==============================\n")
cat("UJI ADF - FIRST DIFFERENCE\n")
cat("==============================\n")

adf_konsumsi_diff <- adf.test(diff(ts_data[, "ln_Konsumsi"]))
adf_pdb_diff      <- adf.test(diff(ts_data[, "ln_PDB"]))

print(adf_konsumsi_diff)
print(adf_pdb_diff)

# =========================================================
# 7. MODEL JANGKA PANJANG
# =========================================================

cat("\n==============================\n")
cat("MODEL JANGKA PANJANG\n")
cat("==============================\n")

long_run <- lm(
  ln_Konsumsi ~ ln_PDB,
  data = as.data.frame(ts_data)
)

summary(long_run)

# Error Correction Term (ECT)
ECT <- resid(long_run)

# =========================================================
# 8. UJI KOINTEGRASI ENGLE-GRANGER
#    Uji ADF pada residual persamaan jangka panjang
# =========================================================

cat("\n==============================\n")
cat("UJI KOINTEGRASI (ADF RESIDUAL)\n")
cat("==============================\n")

eg_adf <- ur.df(
  ECT,
  type = "none",
  lags = 4,
  selectlags = "AIC"
)

summary(eg_adf)

# Jika residual stasioner, terdapat kointegrasi
# antara Konsumsi dan PDB.

# =========================================================
# 9. BENTUK DATA ECM
# =========================================================

ecm_data <- data.frame(
  d_ln_Konsumsi = diff(data$ln_Konsumsi),
  d_ln_PDB      = diff(data$ln_PDB),
  ECT_lag1      = ECT[-length(ECT)]
)

# =========================================================
# 10. MODEL ERROR CORRECTION MODEL (ECM)
# =========================================================

cat("\n==============================\n")
cat("MODEL ECM JANGKA PENDEK\n")
cat("==============================\n")

ECM <- lm(
  d_ln_Konsumsi ~ d_ln_PDB + ECT_lag1,
  data = ecm_data
)

summary(ECM)

# =========================================================
# 11. UJI DIAGNOSTIK
# =========================================================

cat("\n==============================\n")
cat("UJI AUTOKORELASI - LJUNG BOX\n")
cat("==============================\n")

Box.test(
  residuals(ECM),
  lag = 4,
  type = "Ljung-Box"
)

cat("\n==============================\n")
cat("UJI NORMALITAS RESIDUAL\n")
cat("==============================\n")

shapiro.test(residuals(ECM))

# =========================================================
# 12. TAMPILKAN KOEFISIEN ECT
# =========================================================

cat("\n==============================\n")
cat("KOEFISIEN ERROR CORRECTION TERM\n")
cat("==============================\n")

ect_coef <- coef(ECM)["ECT_lag1"]
print(ect_coef)

# =========================================================
# 13. KESIMPULAN ECT
# =========================================================

cat("\n==============================\n")
cat("INTERPRETASI ECT\n")
cat("==============================\n")

if (is.na(ect_coef)) {
  cat("Koefisien ECT tidak dapat diperoleh.\n")
} else if (ect_coef < 0 && summary(ECM)$coefficients["ECT_lag1", "Pr(>|t|)"] < 0.05) {
  cat("ECT bernilai negatif dan signifikan.\n")
  cat("Terdapat mekanisme penyesuaian menuju keseimbangan jangka panjang.\n")
  cat("Besarnya koefisien ECT menunjukkan proporsi penyimpangan\n")
  cat("yang dikoreksi pada periode berikutnya.\n")
} else {
  cat("ECT belum menunjukkan koefisien negatif dan signifikan pada taraf 5%.\n")
  cat("Periksa kembali hasil uji kointegrasi dan spesifikasi ECM.\n")
}

# =========================================================
# SELESAI
# =========================================================
