# ============================================================
# ANALISIS VOLATILITAS RETURN HARIAN SAHAM PGAS 2018-2022
# MODEL ARCH/GARCH
# ============================================================

# 1. LIBRARY
library(readxl)
library(xts)
library(tseries)
library(FinTS)
library(rugarch)

# ============================================================
# 2. MEMBACA DATA DAN MEMBENTUK TIME SERIES
# ============================================================

setwd("C:/Users/Lenovo/Documents/Folder Tugas/Tugas Kampus/Tugas Tingkat 2/Semester 4/Analisis Runtun Waktu/UAS/No 1")
data <- read_excel("Dataset_PGAS_2018-2022.xlsx")

# Pastikan tanggal terbaca sebagai Date
data$Tanggal <- as.Date(data$Tanggal)

# Urutkan berdasarkan tanggal
data <- data[order(data$Tanggal), ]

# Bentuk objek time series (xts)
prices <- xts(data$Close, order.by = data$Tanggal)
colnames(prices) <- "Close"

# Cek data
head(prices)
tail(prices)
plot(prices, main = "Harga Penutupan Saham PGAS 2018-2022",
     ylab = "Harga", xlab = "Tanggal")

# ============================================================
# 3. MENGHITUNG RETURN HARIAN
# ============================================================

# Log return:
# r_t = ln(P_t) - ln(P_(t-1))
returns <- na.omit(diff(log(prices)))
colnames(returns) <- "Return"

head(returns)
summary(returns)

plot(returns, main = "Return Harian Saham PGAS",
     ylab = "Log Return", xlab = "Tanggal")

# ============================================================
# 4. UJI STASIONERITAS RETURN (ADF TEST)
# ============================================================

adf_result <- adf.test(as.numeric(returns))

cat("\n=== ADF TEST RETURN ===\n")
print(adf_result)

# H0 : return tidak stasioner
# H1 : return stasioner
# Jika p-value < 0.05 -> tolak H0 -> return stasioner

# ============================================================
# 5. MODEL RATA-RATA RETURN
# ============================================================

# Model rata-rata yang digunakan adalah ARMA(0,0) dengan mean:
# r_t = mu + epsilon_t
# Artinya return dimodelkan memiliki rata-rata konstan.

mean_model <- arima(
  as.numeric(returns),
  order = c(0, 0, 0),
  include.mean = TRUE
)

cat("\n=== MODEL RATA-RATA (ARMA(0,0)) ===\n")
print(mean_model)

cat("\nEstimasi mean (mu) =", mean_model$coef["intercept"], "\n")

# ============================================================
# 6. IDENTIFIKASI EFEK ARCH
# ============================================================

#residual = return - mean(return).

res_mean <- as.numeric(returns) - mean(as.numeric(returns))

# ACF residual kuadrat
par(mfrow = c(1, 2))
acf(res_mean^2, main = "ACF Residual Kuadrat")
pacf(res_mean^2, main = "PACF Residual Kuadrat")
par(mfrow = c(1, 1))

# Uji ARCH-LM
arch_test <- ArchTest(res_mean, lags = 12)

cat("\n=== UJI ARCH-LM ===\n")
print(arch_test)

# H0 : tidak ada efek ARCH
# H1 : terdapat efek ARCH
# Jika p-value < 0.05 -> terdapat heteroskedastisitas bersyarat
# -> pemodelan ARCH/GARCH layak dilakukan

# ============================================================
# 7. ESTIMASI BEBERAPA MODEL ARCH/GARCH
# ============================================================

# Kita bandingkan model sederhana yang paling relevan:
# 1) ARCH(1)
# 2) ARCH(2)
# 3) GARCH(1,1)

# Distribusi Student-t digunakan karena return saham sering
# memiliki ekor distribusi yang lebih tebal daripada normal.

# ---------------------------
# Model ARCH(1)
# ---------------------------
spec_arch1 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 0)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_arch1 <- ugarchfit(
  spec = spec_arch1,
  data = returns
)

# ---------------------------
# Model ARCH(2)
# ---------------------------
spec_arch2 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(2, 0)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_arch2 <- ugarchfit(
  spec = spec_arch2,
  data = returns
)

# ---------------------------
# Model GARCH(1,1)
# ---------------------------
spec_garch11 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

fit_garch11 <- ugarchfit(
  spec = spec_garch11,
  data = returns
)

# ============================================================
# 8. MEMBANDINGKAN MODEL
# ============================================================

cat("\n=== HASIL ARCH(1) ===\n")
show(fit_arch1)

cat("\n=== HASIL ARCH(2) ===\n")
show(fit_arch2)

cat("\n=== HASIL GARCH(1,1) ===\n")
show(fit_garch11)

# Tabel kriteria informasi
info <- rbind(
  ARCH1     = infocriteria(fit_arch1),
  ARCH2     = infocriteria(fit_arch2),
  GARCH11   = infocriteria(fit_garch11)
)

cat("\n=== PERBANDINGAN INFORMATION CRITERIA ===\n")
print(info)

# Model dengan AIC/BIC paling kecil (paling negatif) menjadi kandidat utama.
# Namun pemilihan akhir tetap harus mempertimbangkan uji diagnostik residual.

# ============================================================
# 9. PILIH MODEL TERBAIK
# ============================================================

# Contoh otomatis berdasarkan BIC
bic_values <- c(
  ARCH1 = infocriteria(fit_arch1)["Bayes", 1],
  ARCH2 = infocriteria(fit_arch2)["Bayes", 1],
  GARCH11 = infocriteria(fit_garch11)["Bayes", 1]
)

best_name <- names(which.min(bic_values))

cat("\n=== MODEL TERPILIH BERDASARKAN BIC ===\n")
print(best_name)

if (best_name == "ARCH1") {
  best_fit <- fit_arch1
} else if (best_name == "ARCH2") {
  best_fit <- fit_arch2
} else {
  best_fit <- fit_garch11
}

cat("\n=== RINGKASAN MODEL TERPILIH ===\n")
show(best_fit)

# ============================================================
# 10. EVALUASI MODEL TERPILIH
# ============================================================

std_res <- as.numeric(residuals(best_fit, standardize = TRUE))

jarque.bera.test(std_res)

# a. Ljung-Box residual
lb_res <- Box.test(
  std_res,
  lag = 12,
  type = "Ljung-Box"
)

# b. Ljung-Box residual kuadrat
lb_sq <- Box.test(
  std_res^2,
  lag = 12,
  type = "Ljung-Box"
)

# c. ARCH-LM residual terstandarisasi
arch_remaining <- ArchTest(
  std_res,
  lags = 12
)

cat("\n=== DIAGNOSTIK MODEL TERPILIH ===\n")

cat("\nLjung-Box Residual:\n")
print(lb_res)

cat("\nLjung-Box Residual Kuadrat:\n")
print(lb_sq)

cat("\nARCH-LM Residual:\n")
print(arch_remaining)

# Interpretasi:
# - Ljung-Box residual p-value > 0.05:
#   tidak ada autokorelasi yang tersisa pada residual.
# - Ljung-Box residual kuadrat p-value > 0.05:
#   tidak ada autokorelasi volatilitas yang tersisa.
# - ARCH-LM p-value > 0.05:
#   efek ARCH sudah berhasil ditangkap model.

# ============================================================
# 11. INTERPRETASI PARAMETER VOLATILITAS
# ============================================================

cat("\n=== PARAMETER MODEL TERPILIH ===\n")
print(coef(best_fit))

# Untuk model GARCH(1,1):
# h_t = omega + alpha1 * epsilon_(t-1)^2 + beta1 * h_(t-1)
#
# omega : tingkat dasar varians
# alpha1: respons varians terhadap shock/return masa lalu
# beta1 : persistensi varians masa lalu
#
# alpha1 + beta1 mendekati 1 -> volatilitas sangat persisten
# alpha1 + beta1 jauh lebih kecil dari 1 -> volatilitas lebih cepat kembali
#
# Untuk ARCH(1), hanya komponen shock masa lalu (alpha1) yang
# digunakan dalam persamaan varians.

coef_best <- coef(best_fit)

if ("alpha1" %in% names(coef_best)) {
  alpha1 <- coef_best["alpha1"]
  cat("\nAlpha1 =", alpha1, "\n")
}

if ("beta1" %in% names(coef_best)) {
  beta1 <- coef_best["beta1"]
  persistence <- alpha1 + beta1
  cat("Beta1  =", beta1, "\n")
  cat("Persistence (alpha1 + beta1) =", persistence, "\n")
}

# ============================================================
# 12. VISUALISASI VOLATILITAS BERSYARAT
# ============================================================

sigma_est <- sigma(best_fit)

plot(
  sigma_est,
  main = "Estimasi Volatilitas Bersyarat (Sigma)",
  ylab = "Sigma",
  xlab = "Tanggal"
)

# ============================================================
# 13. FORECAST VOLATILITAS 10 HARI KE DEPAN
# ============================================================

forecast_10 <- ugarchforecast(
  best_fit,
  n.ahead = 10
)

cat("\n=== FORECAST VOLATILITAS 10 HARI KE DEPAN ===\n")
print(forecast_10)

plot(
  forecast_10,
  which = 3,
  main = "Forecast Volatilitas 10 Hari ke Depan"
)

View(prices)