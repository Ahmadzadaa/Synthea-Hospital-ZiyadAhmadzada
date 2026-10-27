# -*- coding: utf-8 -*-
options(encoding="UTF-8")
# ===========================================================================
# SYNTHEA HOSPITAL — İnteraktiv Analiz Paneli
# Akademik müdafiə üçün hazırlanmış R Shiny tətbiqi
# Rəşid Cəfərov | DA101 | 2026
# ===========================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(data.table)
  library(plotly)
  library(shinycssloaders)
  library(DT)
})

# ===========================================================================
# 1. RƏNG PALETİ VƏ KÖMƏKÇI FUNKSİYALAR
# ===========================================================================
C_BLUE  <- "#185FA5"; C_TEAL  <- "#1D9E75"; C_RED   <- "#C0392B"
C_AMBER <- "#EF9F27"; C_PINK  <- "#D4537E"; C_NAVY  <- "#0C2D4D"
C_INK   <- "#1E293B"; C_MUTED <- "#64748B"; C_CARD  <- "#F4F7FA"
C_GRAD  <- c("#A8D5BA","#F4C97A","#F0A868","#E07560","#C0392B")

ly_base <- function(p, xtitle = NULL, ytitle = NULL) {
  p |> layout(
    paper_bgcolor = "white", plot_bgcolor = "white",
    font = list(family = "Arial, DejaVu Sans", color = C_INK, size = 13),
    xaxis = list(title = xtitle, gridcolor = "#E2E8F0", zeroline = FALSE),
    yaxis = list(title = ytitle, gridcolor = "#E2E8F0", zeroline = FALSE),
    hoverlabel = list(bgcolor = C_NAVY, bordercolor = C_NAVY,
                      font = list(color = "white", size = 13)),
    legend = list(bgcolor = "rgba(0,0,0,0)", bordercolor = "rgba(0,0,0,0)")
  )
}
spin <- function(x) withSpinner(x, color = C_BLUE, type = 6, size = 0.6)
az_months <- c("Yan","Fev","Mar","Apr","May","İyun",
               "İyul","Avq","Sen","Okt","Noy","Dek")

# ===========================================================================
# 2. HARDCODEd XÜLASƏ DATA
# ===========================================================================

# Demoqrafiya
gender_hc <- data.frame(GENDER = c("F","M"), N = c(66083, 66524))
marital_hc <- data.frame(
  kat = c("Evli","Subay","Qeyd edilməyib"), N = c(75847, 19162, 37598))
yqr_labels <- c("0-9","10-19","20-29","30-39","40-49",
                "50-59","60-69","70-79","80-89","90+")
yas_hc <- data.frame(qrup = yqr_labels,
                     N = c(12244,13259,15840,15068,16977,22157,18145,11775,6232,910))
olum_yas_hc <- data.frame(qrup = yqr_labels,
                          faiz = c(9.5,6.7,9.3,13.5,25.7,33.0,34.9,45.0,57.6,67.8))

# Ölüm səbəbləri
olum_seb_hc <- data.frame(
  seb = rev(c("Ağciyər xərçəngi","Miokard infarktı","Ağciyər emfizeması",
              "Xroniki obstruktiv bronxit","İnsult","Ürək dayanması",
              "Pnevmoniya","Alzheimer xəstəliyi")),
  faiz = rev(c(9.6,8.6,6.1,6.1,3.7,2.1,1.9,1.2)))

# Müraciət növləri
enc_az <- c("Ambulator müraciət","Simptom müraciəti","Ambulator prosedur",
            "Prenatal ziyarət","Profilaktik müayinə","Xəstə müraciət proseduru",
            "Təcili yardıma qəbul","Problem müraciəti",
            "Asılılıq müalicəsi","Astma izləməsi")
enc_hc <- data.frame(
  az = rev(enc_az),
  N  = rev(c(597116,199518,91515,61816,55945,51027,47176,42125,19028,15851)))

# Trend
yearly_hc <- data.frame(il = 2011:2016,
                        N = c(178098,178666,179991,179004,178677,179612))
monthly_hc <- data.frame(ay = az_months,
                         N = c(90139,82553,92067,87741,91178,87621,91588,91179,88156,91904,88898,91024))
monthly_hc$ay <- factor(monthly_hc$ay, levels = az_months)

# Metabolik risk
bmi_mids   <- seq(11, 59, by = 2)
bmi_counts <- c(339,2063,4441,5213,5127,5984,6441,6862,7288,7810,
                8554,9254,9213,8277,6412,4258,2742,1760,1056,653,366,208,95,43,24)
hba1c_hc <- data.frame(
  kat = c("Prediabet","Diabet riski","Normal"), N = c(26690,3461,2158))

# Anomaliya — asılılıq
recidiv_hc <- data.frame(
  qrup = factor(c("1 dəfə","2-5 dəfə","6-10 dəfə","11-20 dəfə","20+ dəfə"),
                levels = c("1 dəfə","2-5 dəfə","6-10 dəfə","11-20 dəfə","20+ dəfə")),
  N    = c(15,92,149,410,401),
  faiz = c(1.4,8.6,14.0,38.4,37.6))

overdose_hc <- data.frame(
  kat = c("Müalicədən sonra overdose","Overdose olmayıb"),
  N   = c(623, 444))

vaxt_hc <- data.frame(
  muddet = factor(c("6 ay ərzində","6-12 ay","1-2 il","2 il+"),
                  levels = c("6 ay ərzində","6-12 ay","1-2 il","2 il+")),
  N    = c(69,105,166,283),
  faiz = c(11.1,16.9,26.6,45.4))

# ===========================================================================
# 3. MƏLUMAT YÜKLƏNMƏSI (.rds fayllar mövcuddursa)
# ===========================================================================
gender_df  <- gender_hc
marital_df <- marital_hc
yas_df     <- yas_hc
olum_df    <- olum_yas_hc
enc_df     <- enc_hc
yearly_df  <- yearly_hc
monthly_df <- monthly_hc

cat("Məlumat yüklənir...\n")
data_ok <- tryCatch({
  patients  <- data.table(readRDS("patients.rds"))
  encounters <- data.table(readRDS("encounters.rds"))
  
  # Yaş hesablanması (2017-12-31-ə görə)
  patients[, age := as.integer(difftime(
    fifelse(is.na(DEATHDATE), as.IDate("2017-12-31"), DEATHDATE),
    BIRTHDATE, units = "days") / 365.25)]
  
  # Gender
  gender_df <<- patients[, .N, by = GENDER]
  
  # Marital
  m <- patients[, .N, by = MARITAL]
  marital_df <<- data.frame(
    kat = c("Evli","Subay","Qeyd edilməyib"),
    N   = c(m[MARITAL == "M", N], m[MARITAL == "S", N], m[MARITAL == "", N]))
  
  # Yaş qrupu
  patients[, yqr := cut(age,
                        breaks = c(0,10,20,30,40,50,60,70,80,90,Inf), labels = yqr_labels, right = FALSE)]
  yas_df   <<- patients[!is.na(yqr), .N, by = yqr][order(yqr)]
  setnames(yas_df, "yqr", "qrup")
  olum_df  <<- patients[!is.na(yqr),
                        .(faiz = round(100*mean(!is.na(DEATHDATE)), 1)), by = yqr][order(yqr)]
  setnames(olum_df, "yqr", "qrup")
  
  # Müraciət növləri
  enc_raw <- encounters[, .N, by = DESCRIPTION][order(-N)][1:10]
  enc_raw[, az := rev(enc_az)[seq_along(enc_az)][match(DESCRIPTION,
                                                       c("Outpatient Encounter","Encounter for symptom","Outpatient procedure",
                                                         "Prenatal visit","Encounter for 'check-up'","Patient encounter procedure",
                                                         "Emergency room admission","Encounter for problem",
                                                         "Drug addiction therapy","Asthma follow-up"))]]
  enc_raw[is.na(az), az := DESCRIPTION]
  enc_df <<- enc_raw[order(N), .(az, N)]
  
  # Trend
  yearly_df  <<- encounters[year(DATE) >= 2011 & year(DATE) <= 2016,
                            .(N = .N), by = .(il = year(DATE))][order(il)]
  monthly_raw <- encounters[year(DATE) >= 2011 & year(DATE) <= 2016,
                            .(N = .N), by = .(ay_num = month(DATE))][order(ay_num)]
  monthly_df <<- data.frame(
    ay   = factor(az_months[monthly_raw$ay_num], levels = az_months),
    N    = monthly_raw$N)
  
  cat("Məlumat uğurla yükləndi.\n"); TRUE
}, error = function(e) {
  cat("RDS tapılmadı — hardcoded data istifadə edilir.\n"); FALSE
})

# ===========================================================================
# 4. ARIMA DATA (hardcoded — model.rds yoxdur)
# ===========================================================================
ts_vals <- c(14706,13443,15603,14433,14999,14571,15279,14949,14727,15264,14917,
             15207,15271,13682,15133,14620,15055,14457,15372,15180,14653,15461,14751,15031,
             15192,14135,15078,14726,15400,14424,15439,15452,14636,15404,14905,15200,14996,
             13726,15314,14550,15182,14868,14954,15176,14894,15277,14846,15221,14836,13662,
             15629,14592,15145,14672,15046,15217,14655,15317,14687,15219,15138,13905,15310,
             14820,15397,14629,15498,15205,14591,15181,14792,15146)

fore_mean <- c(15152,13874,15149,14657,15270,14617,15332,15228,14746,15311,14893,
               15161,14968,13731,15360,14539,15133,14645,15079,15232,14766,15377,14817,15203)
fore_lo   <- c(14856,13578,14853,14361,14974,14321,15036,14932,14449,15015,14597,
               14865,14665,13428,15057,14236,14829,14341,14775,14929,14462,15074,14514,14900)
fore_up   <- c(15448,14170,15445,14953,15566,14913,15628,15524,15042,15607,15189,
               15457,15272,14035,15664,14843,15436,14948,15383,15536,15069,15681,15121,15507)

t_hist <- 2011 + (0:71)/12
t_fore <- 2017 + (0:23)/12

fore_df <- data.frame(
  t    = t_fore,
  mean = fore_mean,
  lo   = fore_lo,
  up   = fore_up,
  ay   = paste0(rep(c("Yan","Fev","Mar","Apr","May","İyun",
                      "İyul","Avq","Sen","Okt","Noy","Dek"), 2),
                " ", rep(2017:2018, each=12))
)

enc_ts <- ts(ts_vals, start = c(2011,1), frequency = 12)

# STL parçalanması (base R, startup-da bir dəfə)
stl_r  <- stl(enc_ts, s.window = "periodic")
stl_df <- data.frame(
  t         = t_hist,
  trend     = as.numeric(stl_r$time.series[,"trend"]),
  seasonal  = as.numeric(stl_r$time.series[,"seasonal"]),
  remainder = as.numeric(stl_r$time.series[,"remainder"]))

# ACF / PACF (base R, startup-da bir dəfə)
acf_r    <- acf(enc_ts,  lag.max = 36, plot = FALSE)
pacf_r   <- pacf(enc_ts, lag.max = 36, plot = FALSE)
acf_ci   <- 1.96 / sqrt(length(ts_vals))
acf_lags <- as.numeric(acf_r$lag)
acf_vals_v <- as.numeric(acf_r$acf)
pacf_lags  <- as.numeric(pacf_r$lag)
pacf_vals_v <- as.numeric(pacf_r$acf)

# ===========================================================================
# 5. QRAFIK FUNKSİYALARI (hamısı plotly qaytarır)
# ===========================================================================

# --- Pie helper ---
make_pie <- function(labels, values, colors, title) {
  plot_ly(labels = labels, values = values, type = "pie",
          marker = list(colors = colors,
                        line   = list(color = "white", width = 2.5)),
          textinfo     = "label+percent",
          hovertemplate = "%{label}<br>%{value:,} xəstə<extra></extra>",
          textfont = list(size = 14, color = "white")) |>
    layout(title  = list(text = title, font = list(size = 15, color = C_INK)),
           showlegend = TRUE, legend = list(font = list(size = 13)),
           paper_bgcolor = "white", plot_bgcolor = "white",
           hoverlabel = list(bgcolor = C_NAVY, font = list(color = "white", size = 13)))
}

# Cins
plot_cins <- function() {
  n <- if ("GENDER" %in% colnames(gender_df)) gender_df$N else gender_hc$N
  make_pie(c("Qadın","Kişi"), n, c(C_PINK, C_BLUE), "Cins Bölgüsü")
}

# Mədəni vəziyyət
plot_marital <- function() {
  d <- if (exists("marital_df")) marital_df else marital_hc
  make_pie(d$kat, d$N, c(C_TEAL, C_AMBER, "#9E9E9E"), "Mədəni Vəziyyət")
}

# Yaş qrupu
plot_yas <- function() {
  d <- if (is.data.frame(yas_df) && nrow(yas_df) == 10) yas_df else yas_hc
  plot_ly(x = d$qrup, y = d$N, type = "bar",
          marker = list(color = C_BLUE),
          hovertemplate = "<b>%{x}</b><br>%{y:,} xəstə<extra></extra>") |>
    ly_base("Yaş qrupu","Xəstə sayı") |>
    layout(title = list(text = "Yaş Qrupu üzrə Xəstə Sayı",
                        font = list(size = 15, color = C_INK)))
}

# Ölüm — yaş
plot_olum_yas <- function() {
  d <- if (is.data.frame(olum_df) && "faiz" %in% colnames(olum_df)) olum_df else olum_yas_hc
  plot_ly(x = d$qrup, y = d$faiz, type = "bar",
          marker = list(color = C_RED),
          hovertemplate = "<b>%{x}</b><br>%{y:.1f}% ölüm<extra></extra>") |>
    ly_base("Yaş qrupu","Ölüm faizi (%)") |>
    layout(title = list(text = "Yaş Qrupu üzrə Ölüm Faizi",
                        font = list(size = 15, color = C_INK)))
}

# Ölüm səbəbləri
plot_olum_seb <- function() {
  d <- olum_seb_hc
  plot_ly(y = d$seb, x = d$faiz, type = "bar", orientation = "h",
          marker = list(color = C_PINK),
          hovertemplate = "<b>%{y}</b><br>%{x:.1f}%<extra></extra>",
          text = paste0(d$faiz,"%"), textposition = "outside") |>
    ly_base("Vəfat edənlərin payı (%)") |>
    layout(title = list(text = "Ölüm Səbəbləri Reytinqi",
                        font = list(size = 15, color = C_INK)),
           yaxis = list(title = ""))
}

# Müraciət növləri
plot_enc_types <- function() {
  d <- data.frame(
    az = enc_az,
    N  = c(597116,199518,91515,61816,55945,
           51027,47176,42125,19028,15851))
  d <- d[order(d$N), ]
  d$az <- factor(d$az, levels = d$az)
  plot_ly(y = ~az, x = ~N, data = d,
          type = "bar", orientation = "h",
          marker = list(color = C_BLUE),
          hovertemplate = "<b>%{y}</b><br>%{x:,} muraciet<extra></extra>",
          text = format(d$N, big.mark = ","),
          textposition = "outside") |>
    ly_base("Muraciet sayi", "") |>
    layout(
      title = list(text = "En Cox Rast Gelinen Muraciet Novleri",
                   font = list(size = 15, color = C_INK)),
      yaxis = list(title = "", tickfont = list(size = 12)),
      xaxis = list(title = "Muraciet sayi", gridcolor = "#E2E8F0",
                   range = c(0, 700000)))
}

# İllik trend
plot_yearly <- function() {
  d <- if (is.data.frame(yearly_df) && nrow(yearly_df) == 6) yearly_df else yearly_hc
  plot_ly(x = d$il, y = d$N, type = "scatter", mode = "lines+markers",
          line    = list(color = C_BLUE, width = 3),
          marker  = list(color = C_BLUE, size = 10),
          hovertemplate = "<b>%{x}</b><br>%{y:,} müraciət<extra></extra>") |>
    ly_base("İl","Müraciət sayı") |>
    layout(title = list(text = "İllik Müraciət Trendi (2011-2016)",
                        font = list(size = 15, color = C_INK)),
           xaxis = list(tickvals = 2011:2016, tickformat = "d"))
}

# Mövsümi trend
plot_monthly <- function() {
  d <- data.frame(
    ay = factor(az_months, levels = az_months),
    N  = c(90139,82553,92067,87741,91178,87621,
           91588,91179,88156,91904,88898,91024))
  plot_ly(x = ~ay, y = ~N, data = d,
          type = "bar",
          marker = list(color = C_TEAL),
          hovertemplate = "<b>%{x}</b><br>%{y:,} muraciet<extra></extra>") |>
    ly_base("", "Muraciet sayi") |>
    layout(
      title  = list(text = "Aylar uzre Movsumi Trend (2011-2016 cemi)",
                    font = list(size = 15, color = C_INK)),
      yaxis  = list(title = "Muraciet sayi", gridcolor = "#E2E8F0",
                    range = c(75000, 100000)),
      xaxis  = list(title = "", categoryorder = "array",
                    categoryarray = az_months),
      bargap = 0.25)
}

# BMI
plot_bmi <- function() {
  plot_ly(x = bmi_mids, y = bmi_counts, type = "bar",
          marker = list(color = C_TEAL, line = list(color="white", width=0.5)),
          hovertemplate = "BMI ~ %{x}<br>%{y:,} xəstə<extra></extra>") |>
    add_segments(x=25, xend=25, y=0, yend=max(bmi_counts)*1.05,
                 line=list(color=C_AMBER, width=2, dash="dash"),
                 name="Artıq çəki hədd (25)") |>
    add_segments(x=30, xend=30, y=0, yend=max(bmi_counts)*1.05,
                 line=list(color=C_RED, width=2, dash="dash"),
                 name="Piylənmə hədd (30)") |>
    ly_base("BMI","Xəstə sayı") |>
    layout(title = list(text = "BMI Paylanması (Orta: 29.9)",
                        font = list(size = 15, color = C_INK)),
           showlegend = TRUE,
           legend = list(x=0.65, y=0.95, font=list(size=12)))
}

# HbA1c
plot_hba1c <- function() {
  make_pie(hba1c_hc$kat, hba1c_hc$N,
           c(C_AMBER, C_RED, C_TEAL), "HbA1c Risk Bölgüsü")
}

# Asılılıq — recidiv
plot_recidiv <- function() {
  plot_ly(x = recidiv_hc$qrup, y = recidiv_hc$N,
          type = "bar", marker = list(color = C_GRAD),
          text  = paste0(recidiv_hc$N," xəstə\n(",recidiv_hc$faiz,"%)"),
          textposition = "outside",
          hovertemplate = "<b>%{x}</b><br>%{y} xəstə (%{text})<extra></extra>") |>
    ly_base("Müalicəyə qayıdış sayı","Xəstə sayı") |>
    layout(title = list(text = "Asılılıq Müalicəsinə Qayıdış Sayı",
                        font = list(size = 15, color = C_INK)),
           showlegend = FALSE)
}

# Overdose pie
plot_overdose_pie <- function() {
  make_pie(overdose_hc$kat, overdose_hc$N,
           c(C_RED,"#A8D5BA"), "Müalicə Keçənlərdə Overdose")
}

# Overdose vaxt
plot_overdose_vaxt <- function() {
  clrs <- c("#F4C97A","#F0A868","#E07560",C_RED)
  plot_ly(x = vaxt_hc$muddet, y = vaxt_hc$N, type = "bar",
          marker = list(color = clrs),
          text  = paste0(vaxt_hc$N,"\n(",vaxt_hc$faiz,"%)"),
          textposition = "outside",
          hovertemplate = "<b>%{x}</b><br>%{y} xəstə (%{text})<extra></extra>") |>
    ly_base("Müddət","Xəstə sayı") |>
    layout(title = list(text = "Müalicədən Overdose-a Keçən Vaxt",
                        font = list(size = 15, color = C_INK)),
           showlegend = FALSE)
}

# Tarixi seriya
plot_ts_hist <- function() {
  plot_ly(x = t_hist, y = ts_vals, type = "scatter", mode = "lines",
          line = list(color = C_BLUE, width = 1.8),
          hovertemplate = "%{x:.2f}<br>%{y:,} müraciət<extra></extra>") |>
    ly_base("İl","Aylıq müraciət sayı") |>
    layout(title = list(text = "Tarixi Müraciət Seriyası (2011-2016)",
                        font = list(size = 15, color = C_INK)),
           xaxis = list(tickvals = 2011:2017, tickformat = "d"))
}

# STL parçalanması
plot_stl <- function() {
  mk <- function(y_vals, yname, clr) {
    plot_ly(x = stl_df$t, y = y_vals, type = "scatter", mode = "lines",
            line = list(color = clr, width = 2),
            hovertemplate = paste0("%{x:.2f}<br>",yname,": %{y:,.0f}<extra></extra>")) |>
      layout(yaxis = list(title = yname, gridcolor = "#E2E8F0"),
             xaxis = list(gridcolor = "#E2E8F0", tickvals = 2011:2017, tickformat = "d"),
             paper_bgcolor = "white", plot_bgcolor = "white")
  }
  subplot(
    mk(stl_df$trend,    "Trend",      C_BLUE),
    mk(stl_df$seasonal, "Mövsümilik", C_TEAL),
    mk(stl_df$remainder,"Qalıq",      C_MUTED),
    nrows = 3, shareX = TRUE, titleY = TRUE
  ) |> layout(
    title = list(text = "STL Parçalanması — Trend | Mövsümilik | Qalıq",
                 font = list(size = 15, color = C_INK)),
    paper_bgcolor = "white", plot_bgcolor = "white",
    font = list(family = "Arial, DejaVu Sans", color = C_INK),
    showlegend = FALSE
  )
}

# ACF / PACF
plot_acf_pacf <- function() {
  ci_line <- list(color = C_RED, width = 1.5, dash = "dash")
  make_panel <- function(lags, vals, ttl) {
    plot_ly() |>
      add_segments(x = lags, xend = lags, y = 0, yend = vals,
                   line = list(color = C_BLUE, width = 2.5)) |>
      add_markers(x = lags, y = vals, marker = list(color = C_BLUE, size = 6)) |>
      add_lines(x = c(min(lags)-0.5, max(lags)+0.5), y = c(acf_ci, acf_ci),
                line = ci_line, name = "+95% CI") |>
      add_lines(x = c(min(lags)-0.5, max(lags)+0.5), y = c(-acf_ci,-acf_ci),
                line = ci_line, name = "-95% CI", showlegend = FALSE) |>
      add_lines(x = c(min(lags)-0.5, max(lags)+0.5), y = c(0,0),
                line = list(color = C_INK, width = 0.8), showlegend = FALSE) |>
      layout(title = list(text = ttl, font = list(size = 14, color = C_INK)),
             yaxis = list(title = "Korrelyasiya", gridcolor="#E2E8F0", range = c(-0.5,1.05)),
             xaxis = list(title = "Gecikmə (lag)", gridcolor="#E2E8F0"),
             paper_bgcolor = "white", plot_bgcolor = "white",
             font = list(family="Arial, DejaVu Sans", color=C_INK, size=12))
  }
  subplot(
    make_panel(acf_lags,  acf_vals_v,  "ACF — Avtokorrelyasiya funksiyası"),
    make_panel(pacf_lags, pacf_vals_v, "PACF — Qismən avtokorrelyasiya funksiyası"),
    nrows = 1, shareY = FALSE, titleX = TRUE, titleY = TRUE
  ) |> layout(
    title = list(text = "ACF və PACF Analizi — Lag 12-də mövsümi spike aydın görünür",
                 font = list(size = 15, color = C_INK)),
    paper_bgcolor = "white", showlegend = FALSE)
}

# Proqnoz qrafiki (horizon reactive)
plot_forecast <- function(h) {
  fd <- fore_df[1:h, ]
  t_conn  <- c(t_hist[length(t_hist)], fd$t)
  mn_conn <- c(ts_vals[length(ts_vals)], fd$mean)
  
  plot_ly() |>
    add_ribbons(x = fd$t, ymin = fd$lo, ymax = fd$up,
                fillcolor = "rgba(192,57,43,0.15)", line = list(width=0),
                name = "95% Etibarlılıq intervalı") |>
    add_lines(x = t_hist, y = ts_vals,
              line = list(color = C_BLUE, width = 2),
              name = "Tarixi məlumat (2011-2016)",
              hovertemplate = "%{x:.2f}<br>%{y:,} müraciət<extra></extra>") |>
    add_lines(x = t_conn, y = mn_conn,
              line = list(color = C_RED, width = 2.5),
              name = "Proqnoz",
              hovertemplate = "%{x:.2f}<br>Proqnoz: %{y:,}<extra></extra>") |>
    add_segments(x=2017, xend=2017, y=min(fore_lo)*0.97, yend=max(ts_vals)*1.03,
                 line=list(color=C_MUTED, width=1.2, dash="dot"),
                 showlegend=FALSE) |>
    ly_base("İl","Aylıq müraciət sayı") |>
    layout(
      title  = list(text = paste0("Müraciət Sayı Proqnozu — ", h, " Ay"),
                    font = list(size = 16, color = C_INK)),
      xaxis  = list(tickvals = 2011:2019, tickformat="d", gridcolor="#E2E8F0"),
      legend = list(orientation="h", x=0, y=-0.15, font=list(size=13)),
      hovermode = "x unified")
}

# ===========================================================================
# 6. UI
# ===========================================================================
css_custom <- "
  .main-sidebar { background-color: #0C2D4D !important; }
  .brand-link   { background-color: #143C63 !important; border-bottom: 1px solid #185FA5 !important; }
  .brand-link span { color: white !important; font-weight: 700 !important; font-size: 17px; }
  .sidebar-dark-primary .nav-sidebar>.nav-item>.nav-link.active {
    background-color: #185FA5 !important; color: white !important; border-radius: 6px; }
  .sidebar-dark-primary .nav-sidebar>.nav-item>.nav-link { color: #CADCFC !important; }
  .nav-sidebar>.nav-item>.nav-link:hover { background-color: rgba(24,95,165,.25) !important; border-radius:6px; }
  .main-header { border-bottom: 2px solid #185FA5 !important; }
  .small-box { border-radius: 10px !important; }
  .card { border-radius: 10px !important; border: none !important;
          box-shadow: 0 2px 12px rgba(0,0,0,.08) !important; }
  .card-header { border-radius: 10px 10px 0 0 !important; font-weight: 600; }
  .nav-tabs .nav-link.active { color: #185FA5 !important; border-bottom: 3px solid #185FA5 !important; font-weight:600; }
  .sidebar-mini .brand-text, .sidebar-mini .nav-link span { display: block !important; }
  body { background-color: #F8FAFC !important; }
  .info-box { border-radius: 10px !important; box-shadow: 0 2px 12px rgba(0,0,0,.08) !important; }
"

ui <- bs4DashPage(
  header = bs4DashNavbar(skin = "dark", status = "white", border = TRUE),
  sidebar = bs4DashSidebar(
    skin = "dark", status = "primary",
    tags$head(tags$style(HTML(css_custom))),
    sidebarMenu(
      id = "menu",
      menuItem("Ümumi Baxış",      tabName="overview",    icon=icon("home"),
               badgeLabel="Baxış", badgeColor="primary"),
      menuItem("Deskriptiv Analiz",tabName="descriptive", icon=icon("chart-bar"),
               badgeLabel="1",     badgeColor="primary"),
      menuItem("Anomaliya Analizi",tabName="anomaly",     icon=icon("exclamation-triangle"),
               badgeLabel="2",     badgeColor="danger"),
      menuItem("Proqnoz",    tabName="arima",       icon=icon("chart-line"),
               badgeLabel="3",     badgeColor="success"),
      menuItem("Nəticələr",        tabName="results",     icon=icon("check-circle"),
               badgeLabel="4",     badgeColor="info")
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      
      # ================================================================
      # TAB 1: ÜMUMI BAXIŞ
      # ================================================================
      bs4TabItem(tabName = "overview",
                 fluidRow(
                   bs4ValueBox(value=tags$h3("132,607",style="color:white;margin:0;"),
                               subtitle="Xəstə", icon=icon("user"), color="primary",   width=3, elevation=3),
                   bs4ValueBox(value=tags$h3("1,263,669",style="color:white;margin:0;"),
                               subtitle="Müraciət", icon=icon("stethoscope"), color="success", width=3, elevation=3),
                   bs4ValueBox(value=tags$h3("5,383,318",style="color:white;margin:0;"),
                               subtitle="Müşahidə", icon=icon("flask"), color="warning",  width=3, elevation=3),
                   bs4ValueBox(value=tags$h3("2011–2017",style="color:white;margin:0;"),
                               subtitle="Tədqiqat dövrü", icon=icon("calendar"), color="navy",    width=3, elevation=3)
                 ),
                 tags$hr(style="border-color:#E2E8F0; margin:8px 0 18px;"),
                 tags$h5(style="color:#64748B; margin-bottom:14px; font-weight:600;",
                         "⭐ Əsas Tapıntılar"),
                 fluidRow(
                   bs4InfoBox(title="Asılılıq Recidivi", value="98.6%",
                              subtitle="Müalicə keçənlərin müalicəyə qayıdış faizi",
                              icon=icon("redo"), color="danger", width=3, fill=TRUE),
                   bs4InfoBox(title="Prediabet Riski", value="82.6%",
                              subtitle="HbA1c ölçülənlər arasında prediabet faizi",
                              icon=icon("heartbeat"), color="warning", width=3, fill=TRUE),
                   bs4InfoBox(title="ARIMA Dəqiqliyi", value="99.35%",
                              subtitle="MAPE 0.65% — 24 aylıq proqnoz",
                              icon=icon("chart-line"), color="success", width=3, fill=TRUE),
                   bs4InfoBox(title="90+ Yaşda Ölüm", value="67.8%",
                              subtitle="Ən yüksək yaş qrupunda ölüm faizi",
                              icon=icon("user-clock"), color="indigo", width=3, fill=TRUE)
                 ),
                 tags$hr(style="border-color:#E2E8F0; margin:8px 0 18px;"),
                 fluidRow(
                   bs4Card(title="Layihə Haqqında", status="primary", solidHeader=TRUE, width=12, elevation=2,
                           tags$p(style="font-size:15px; line-height:1.8; color:#1E293B;",
                                  tags$b("Synthea Hospital Analizi"), " — 132,607 xəstənin, 1,263,669 müraciətin və ",
                                  "5,383,318 klinik müşahidənin dörd mərhələli analizi. Deskriptiv statistika, ",
                                  "anomaliya aşkarlanması, asılılıq müalicəsi pattern analizi və ARIMA zaman sırası ",
                                  "proqnozu. ARIMA(0,0,0)(2,1,1)[12] modeli ilə 24 aylıq proqnoz; MAPE 0.65%."
                           ),
                           tags$div(style="display:flex; gap:20px; margin-top:10px; flex-wrap:wrap;",
                                    tags$span(style=paste0("background:",C_BLUE,"; color:white; padding:4px 14px; border-radius:20px; font-size:13px;"), "Deskriptiv"),
                                    tags$span(style=paste0("background:",C_RED,";  color:white; padding:4px 14px; border-radius:20px; font-size:13px;"), "Anomaliya"),
                                    tags$span(style=paste0("background:",C_TEAL,"; color:white; padding:4px 14px; border-radius:20px; font-size:13px;"), "ARIMA Proqnoz"),
                                    tags$span(style=paste0("background:",C_NAVY,"; color:white; padding:4px 14px; border-radius:20px; font-size:13px;"), "Nəticələr")
                           )
                   )
                 )
      ),
      
      # ================================================================
      # TAB 2: DESKRİPTİV ANALİZ
      # ================================================================
      bs4TabItem(tabName = "descriptive",
                 tabsetPanel(
                   tabPanel("Demoqrafiya",
                            fluidRow(
                              bs4Card(title="Cins Bölgüsü",      status="primary", width=4, elevation=2, spin(plotlyOutput("p_cins",   height="320px"))),
                              bs4Card(title="Mədəni Vəziyyət",    status="primary", width=4, elevation=2, spin(plotlyOutput("p_marital",height="320px"))),
                              bs4Card(title="Yaş Qrupu üzrə Xəstə Sayı", status="primary", width=4, elevation=2, spin(plotlyOutput("p_yas", height="320px")))
                            )
                   ),
                   tabPanel("Olum Analizi",
                            fluidRow(
                              bs4Card(title="Yaş üzrə Ölüm Faizi", status="danger", solidHeader=TRUE, width=6, elevation=2, spin(plotlyOutput("p_olum_yas", height="370px"))),
                              bs4Card(title="Ölüm Səbəbləri",       status="danger", solidHeader=TRUE, width=6, elevation=2, spin(plotlyOutput("p_olum_seb", height="370px")))
                            )
                   ),
                   tabPanel("Muraciet Analizi",
                            fluidRow(
                              bs4Card(title="Müraciət Növləri (Top 10)", status="primary", width=7, elevation=2, spin(plotlyOutput("p_enc_types", height="380px"))),
                              bs4Card(title="İllik Trend (2011-2016)",    status="primary", width=5, elevation=2, spin(plotlyOutput("p_yearly",    height="380px")))
                            ),
                            fluidRow(
                              bs4Card(title="Mövsümi Trend (Aylar üzrə)", status="info", width=12, elevation=2, spin(plotlyOutput("p_monthly", height="280px")))
                            )
                   ),
                   tabPanel("Metabolik Risk",
                            fluidRow(
                              bs4Card(title="BMI Paylanması", status="success", solidHeader=TRUE, width=7, elevation=2, spin(plotlyOutput("p_bmi",   height="370px"))),
                              bs4Card(title="HbA1c Riski",   status="warning", solidHeader=TRUE, width=5, elevation=2, spin(plotlyOutput("p_hba1c", height="370px")))
                            ),
                            fluidRow(
                              bs4Card(width=12, background="white", elevation=0,
                                      tags$p(style="font-size:14px; color:#64748B; margin:0;",
                                             icon("info-circle"), " Orta BMI 29.9 — populyasiyanın əksəriyyəti artıq çəki həddindədir. ",
                                             "HbA1c ölçülən 32,309 xəstənin 82.6%-i prediabet, 10.7%-i diabet riskindədir."))
                            )
                   )
                 )
      ),
      
      # ================================================================
      # TAB 3: ANOMALİYA ANALİZİ
      # ================================================================
      bs4TabItem(tabName = "anomaly",
                 fluidRow(
                   bs4ValueBox(value=tags$h3("5",  style="color:white;margin:0;"), subtitle="Ghost müraciət",       icon=icon("ghost"),    color="danger",  width=2, elevation=3),
                   bs4ValueBox(value=tags$h3("805",style="color:white;margin:0;"), subtitle="Ölüm günü şübhəli",   icon=icon("skull"),    color="orange",  width=2, elevation=3),
                   bs4ValueBox(value=tags$h3("11", style="color:white;margin:0;"), subtitle="Sertifikat erkən",     icon=icon("file-alt"),color="warning", width=2, elevation=3),
                   bs4ValueBox(value=tags$h3("17", style="color:white;margin:0;"), subtitle="Mənfi HbA1c",          icon=icon("vial"),    color="danger",  width=2, elevation=3),
                   bs4ValueBox(value=tags$h3("9,653",style="color:white;margin:0;"),subtitle="Təkrar SSN",          icon=icon("id-card"), color="maroon",  width=2, elevation=3),
                   bs4ValueBox(value=tags$h3("12",style="color:white;margin:0;"),   subtitle="Təkrar pasport",      icon=icon("passport"),color="maroon",  width=2, elevation=3)
                 ),
                 tabsetPanel(
                   tabPanel("Anomaliya Icmali",
                            fluidRow(
                              bs4Card(title="Data Keyfiyyəti Qeydləri", status="danger", solidHeader=TRUE, width=12, elevation=2,
                                      tags$table(class="table table-hover", style="font-size:14px;",
                                                 tags$thead(tags$tr(tags$th("Anomaliya"), tags$th("Say"), tags$th("Statusu"), tags$th("Hərəkət"))),
                                                 tags$tbody(
                                                   tags$tr(tags$td("Ölümdən sonra müraciət (ghost)"), tags$td(tags$b("5",style="color:#C0392B;")), tags$td("Anomaliya"), tags$td("Proqnozdan silindi")),
                                                   tags$tr(tags$td("Ölüm günü şübhəli müraciət"),     tags$td(tags$b("805",style="color:#EF9F27;")),tags$td("Şübhəli"), tags$td("İşarələndi, saxlanıldı")),
                                                   tags$tr(tags$td("Sertifikat ölümdən əvvəl (max 9.4 il)"),tags$td(tags$b("11",style="color:#EF9F27;")), tags$td("Şübhəli"), tags$td("İşarələndi")),
                                                   tags$tr(tags$td("Mənfi HbA1c (min -0.9%)"),        tags$td(tags$b("17",style="color:#C0392B;")), tags$td("Bioloji mümkünsüz"), tags$td("İşarələndi")),
                                                   tags$tr(tags$td("Eyni SSN — fərqli xəstə"),        tags$td(tags$b("9,653",style="color:#6D2E46;")),tags$td("Sistem xətası"), tags$td("İşarələndi")),
                                                   tags$tr(tags$td("Eyni pasport — fərqli xəstə"),    tags$td(tags$b("12",style="color:#6D2E46;")), tags$td("Sistem xətası"), tags$td("İşarələndi"))
                                                 )
                                      )
                              )
                            )
                   ),
                   tabPanel("Asililiiq Muaricesi",
                            fluidRow(
                              bs4Card(title="Müalicəyə Qayıdış Sayı", status="danger", solidHeader=TRUE, width=7, elevation=2, spin(plotlyOutput("p_recidiv", height="370px"))),
                              bs4Card(title="Overdose Bölgüsü",        status="danger", solidHeader=TRUE, width=5, elevation=2, spin(plotlyOutput("p_overdose_pie", height="370px")))
                            ),
                            fluidRow(
                              bs4Card(title="Müalicədən Overdose-a Keçən Vaxt", status="danger", width=12, elevation=2, spin(plotlyOutput("p_overdose_vaxt", height="270px")))
                            ),
                            fluidRow(
                              bs4Card(width=12, background="white", elevation=0,
                                      tags$p(style="font-size:14px; color:#64748B; margin:0;",
                                             icon("exclamation-triangle", style="color:#C0392B;"),
                                             tags$b(" Əsas tapıntı: "),
                                             "1,067 asılılıq müalicəsi xəstəsinin 98.6%-i 2+ dəfə müalicəyə qayıdıb (orta 17.8 dəfə). ",
                                             "Bu xəstələrin 58.4%-i müalicə keçirməsinə baxmayaraq overdose-a düçar olub."))
                            )
                   )
                 )
      ),
      
      # ================================================================
      # TAB 4: ARIMA PROQNOZ
      # ================================================================
      bs4TabItem(tabName = "arima",
                 tabsetPanel(
                   tabPanel("Tarixi Seriya",
                            bs4Card(title="Aylıq Müraciət Seriyası — 2011-2016 (72 ay)", status="primary", width=12, elevation=2, spin(plotlyOutput("p_ts_hist", height="420px"))),
                            bs4Card(width=12, background="white", elevation=0,
                                    tags$p(style="font-size:14px; color:#64748B; margin:0;",
                                           "Seriya vizual olaraq stasionardır (ADF testi: p = 0.018). Mövsümi dalğalanma aydın görünür — bu, ARIMA-nın seasonal (mövsümi) komponentini əsaslandırır."))
                   ),
                   tabPanel("STL Parcalanmasi",
                            bs4Card(title="STL Parçalanması: Trend | Mövsümilik | Qalıq", status="info", width=12, elevation=2, spin(plotlyOutput("p_stl", height="500px"))),
                            bs4Card(width=12, background="white", elevation=0,
                                    tags$p(style="font-size:14px; color:#64748B; margin:0;",
                                           icon("info-circle"),
                                           " Trend xətti demək olar ki, düzdür — seriya sabitdir (~179K). ",
                                           "Mövsümilik paneli hər ilin 12 aylıq pattern-ini göstərir. Qalıq isə izah edilməyən təsadüfi hissədir."))
                   ),
                   tabPanel("ACF PACF",
                            bs4Card(title="Avtokorrelyasiya Analizi", status="primary", width=12, elevation=2, spin(plotlyOutput("p_acf_pacf", height="450px"))),
                            bs4Card(width=12, background="white", elevation=0,
                                    tags$p(style="font-size:14px; color:#64748B; margin:0;",
                                           icon("info-circle"),
                                           " Lag 12-dəki əhəmiyyətli sürçmə mövsümi ARIMA komponentinin seçilməsini əsaslandırır. ",
                                           "Qırmızı punktir xəttlər 95% etibarlılıq intervalını göstərir."))
                   ),
                   tabPanel("Proqnoz",
                            fluidRow(
                              bs4Card(width=4, elevation=2,
                                      tags$h6(style="color:#64748B; font-weight:600; margin-bottom:10px;", "Proqnoz Horizontu"),
                                      sliderInput("horizon", NULL, min=6, max=24, step=6, value=24,
                                                  ticks=TRUE, width="100%",
                                                  pre="", post=" ay")
                              ),
                              bs4ValueBox(value=tags$h3("0.65%",style="color:white;margin:0;"),
                                          subtitle="MAPE", icon=icon("bullseye"), color="success", width=2, elevation=3),
                              bs4ValueBox(value=tags$h3("133.56",style="color:white;margin:0;"),
                                          subtitle="RMSE", icon=icon("ruler"), color="primary", width=2, elevation=3),
                              bs4ValueBox(value=tags$h3("0.54",style="color:white;margin:0;"),
                                          subtitle="MASE (< 1 yaxşıdır)", icon=icon("check"), color="info", width=2, elevation=3),
                              bs4ValueBox(value=tags$h3("99.35%",style="color:white;margin:0;"),
                                          subtitle="Ümumi dəqiqlik", icon=icon("star"), color="teal", width=2, elevation=3)
                            ),
                            fluidRow(
                              bs4Card(title=NULL, status="primary", width=12, elevation=3,
                                      spin(plotlyOutput("p_forecast", height="420px")))
                            ),
                            fluidRow(
                              bs4Card(title="Model Məlumatı", status="primary", solidHeader=TRUE, width=12, elevation=2,
                                      tags$div(style="display:flex; gap:40px; flex-wrap:wrap; font-size:14px;",
                                               tags$div(tags$b("Model: "), tags$code("ARIMA(0,0,0)(2,1,1)[12]")),
                                               tags$div(tags$b("Sıra uzunluğu: "), "72 ay (2011-2016)"),
                                               tags$div(tags$b("Parametrlər: "), "sar1=-0.12 | sar2=-0.60 | sma1=-0.65"),
                                               tags$div(tags$b("AIC: "), "800.46"),
                                               tags$div(tags$b("Seçim metodu: "), "auto.arima() — AIC optimallaşdırması"),
                                               tags$div(tags$b("ADF testi: "), "p = 0.018 (stasionar)")
                                      )
                              )
                            )
                   ),
                   tabPanel("Proqnoz Cedveli",
                            bs4Card(title="24 Aylıq Proqnoz Cədvəli", status="primary", solidHeader=TRUE, width=12, elevation=2,
                                    downloadButton("dl_forecast", "📥 Excel (.csv) kimi yüklə",
                                                   style=paste0("background-color:",C_TEAL,"; color:white; border:none; border-radius:6px; padding:8px 18px; margin-bottom:14px; font-weight:600;")),
                                    DTOutput("tbl_forecast")
                            )
                   )
                 )
      ),
      
      # ================================================================
      # TAB 5: NƏTİCƏLƏR
      # ================================================================
      bs4TabItem(tabName = "results",
                 fluidRow(
                   bs4Card(title="Nə Etdik və Nə Tapdıq", status="navy", solidHeader=TRUE, width=12, elevation=3,
                           tags$div(style="display:grid; grid-template-columns:1fr 1fr; gap:16px;",
                                    tags$div(style=paste0("border-left:4px solid ",C_BLUE,"; padding:12px 16px; background:#F4F7FA; border-radius:0 8px 8px 0;"),
                                             tags$b(style=paste0("color:",C_BLUE,";"),"Deskriptiv Analiz"),
                                             tags$p(style="margin:6px 0 0; font-size:14px; color:#1E293B;",
                                                    "132K xəstə, 1.26M müraciət. 90+ yaşda ölüm 67.8%-ə çatır. ",
                                                    "Metabolik risk geniş yayılıb: orta BMI 29.9, HbA1c ölçülənlərin 82.6%-i prediabet.")),
                                    tags$div(style=paste0("border-left:4px solid ",C_RED,"; padding:12px 16px; background:#F4F7FA; border-radius:0 8px 8px 0;"),
                                             tags$b(style=paste0("color:",C_RED,";"),"Anomaliya Analizi"),
                                             tags$p(style="margin:6px 0 0; font-size:14px; color:#1E293B;",
                                                    "Data keyfiyyəti yoxlandı: 5 ghost müraciət, mənfi HbA1c, SSN/pasport duplikatı. ",
                                                    "Proqnoz üçün yalnız 5 qeyd silindi.")),
                                    tags$div(style=paste0("border-left:4px solid #6FD0B0; padding:12px 16px; background:#F0FDF7; border-radius:0 8px 8px 0;"),
                                             tags$b(style="color:#0F766E;","⭐ Əsas Tapıntı"),
                                             tags$p(style="margin:6px 0 0; font-size:14px; color:#1E293B;",
                                                    "Asılılıq müalicəsi xəstələrinin 98.6%-i ortalama 17.8 dəfə qayıdıb. ",
                                                    "58.4%-i therapy keçirməsinə baxmayaraq overdose-a düçar olub.")),
                                    tags$div(style=paste0("border-left:4px solid ",C_TEAL,"; padding:12px 16px; background:#F4F7FA; border-radius:0 8px 8px 0;"),
                                             tags$b(style=paste0("color:",C_TEAL,";"),"ARIMA Proqnozu"),
                                             tags$p(style="margin:6px 0 0; font-size:14px; color:#1E293B;",
                                                    "ARIMA(0,0,0)(2,1,1)[12] modeli müraciət həcmini MAPE 0.65% xəta ilə — ",
                                                    "yəni 99.35% dəqiqliklə — 24 ay irəli proqnozlaşdırdı."))
                           )
                   )
                 ),
                 tags$hr(style="border-color:#E2E8F0; margin:8px 0 18px;"),
                 fluidRow(
                   bs4Card(title="Xəstəxana Rəhbərliyi üçün", status="primary", solidHeader=TRUE, width=6, elevation=2,
                           tags$ul(style="font-size:14px; line-height:2; color:#1E293B;",
                                   tags$li("Kadr, büdcə və yatağı bir il qabağa planlaşdırmaq"),
                                   tags$li("Data nəzarəti: SSN/pasport yoxlama sistemini tətbiq etmək"),
                                   tags$li("Asılılıq müalicə proqramını yenidən qiymətləndirmək"),
                                   tags$li("Yüksək riskli qruplara (metabolik, asılılıq) fokus artırmaq"),
                                   tags$li("Mövsümi piklərə uyğun resurs optimallaşdırması")
                           )
                   ),
                   bs4Card(title="Xəstələr üçün", status="success", solidHeader=TRUE, width=6, elevation=2,
                           tags$ul(style="font-size:14px; line-height:2; color:#1E293B;",
                                   tags$li("Mövsümi pikə uyğun planlaşdırma → qısa gözləmə müddəti"),
                                   tags$li("Keyfiyyətli, fasiləsiz qulluq"),
                                   tags$li("Prediabet skrininqinin genişləndirilməsi → erkən müdaxilə"),
                                   tags$li("Asılılıq xəstələri üçün uzunmüddətli izləmə proqramı"),
                                   tags$li("Şəxsiyyət sisteminin möhkəmləndirilməsi → etibarlı qeydlər")
                           )
                   )
                 )
      )
    ) # bs4TabItems sonu
  ),   # bs4DashBody sonu
  footer = bs4DashFooter(
    tags$span(style="color:#64748B; font-size:13px;",
              "Synthea Hospital Analizi | Rəşid Cəfərov | DA101 | 2026")
  )
)

# ===========================================================================
# 7. SERVER
# ===========================================================================
server <- function(input, output, session) {
  
  # --- Deskriptiv ---
  output$p_cins        <- renderPlotly({ plot_cins()        })
  output$p_marital     <- renderPlotly({ plot_marital()     })
  output$p_yas         <- renderPlotly({ plot_yas()         })
  output$p_olum_yas    <- renderPlotly({ plot_olum_yas()    })
  output$p_olum_seb    <- renderPlotly({ plot_olum_seb()    })
  output$p_enc_types   <- renderPlotly({ plot_enc_types()   })
  output$p_yearly      <- renderPlotly({ plot_yearly()      })
  output$p_monthly     <- renderPlotly({ plot_monthly()     })
  output$p_bmi         <- renderPlotly({ plot_bmi()         })
  output$p_hba1c       <- renderPlotly({ plot_hba1c()       })
  
  # --- Anomaliya ---
  output$p_recidiv       <- renderPlotly({ plot_recidiv()       })
  output$p_overdose_pie  <- renderPlotly({ plot_overdose_pie()  })
  output$p_overdose_vaxt <- renderPlotly({ plot_overdose_vaxt() })
  
  # --- ARIMA ---
  output$p_ts_hist  <- renderPlotly({ plot_ts_hist()  })
  output$p_stl      <- renderPlotly({ plot_stl()      })
  output$p_acf_pacf <- renderPlotly({ plot_acf_pacf() })
  
  output$p_forecast <- renderPlotly({
    h <- as.integer(input$horizon)
    plot_forecast(h)
  })
  
  # Proqnoz cədvəli
  output$tbl_forecast <- renderDT({
    d <- data.frame(
      "Ay"           = fore_df$ay,
      "Proqnoz"      = formatC(fore_df$mean, format="d", big.mark=","),
      "Alt_hedd_95" = formatC(fore_df$lo,   format="d", big.mark=","),
      "Ust_hedd_95" = formatC(fore_df$up,   format="d", big.mark=","),
      check.names = FALSE
    )
    datatable(d, rownames=FALSE, options=list(pageLength=12, dom="tip", scrollX=TRUE),
              class="table table-hover table-sm")
  })
  
  output$dl_forecast <- downloadHandler(
    filename = function() paste0("synthea_proqnoz_", Sys.Date(), ".csv"),
    content  = function(file) {
      d <- data.frame(Ay=fore_df$ay, Proqnoz=fore_df$mean,
                      Alt_95=fore_df$lo, Ust_95=fore_df$up)
      write.csv(d, file, row.names=FALSE, fileEncoding="UTF-8")
    }
  )
}

# ===========================================================================
# 8. TƏTBIQ
# ===========================================================================
shinyApp(ui = ui, server = server)


library(rsconnect)
rsconnect::deployApp("C:/Users/Vivobook/Desktop/synthea")
