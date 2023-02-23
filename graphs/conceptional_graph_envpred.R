# Step 1: Generate a time series of sea surface temperature with a high amount of noise over ten years with generally increasing trend. Plot it.

# set seed for reproducibility
set.seed(123)

# generate time series with increasing trend and noise
n <- 360  # 12 months x 10 years
t <- seq(1, n)
trend <- 0.01 * t  # increase by 0.1 degrees per month
noise <- rnorm(n, mean = 0, sd = 0.5)
sst <- trend + noise

# plot time series
plot(sst, type = "l", xlab = "Time", ylab = "Sea Surface Temperature", main = "Sea Surface Temperature with Noise and Trend")

# Step 2: Remove the linear trend from the time series. Plot it.

# remove trend using linear regression
time <- cbind(t)
lm_fit <- lm(sst ~ time)
sst_detrend <- resid(lm_fit)  # extract residuals from linear regression

# plot detrended time series
plot(sst_detrend, type = "l", xlab = "Time", ylab = "Detrended Sea Surface Temperature", main = "Detrended Sea Surface Temperature with Noise")

# Step 3: Model a new seasonal trend with monthly averages and interpolation between the averages. Plot it.

# compute monthly averages
monthly_avg <- tapply(sst_detrend, rep(1:12, each = 30), mean)

# interpolate between monthly averages
interpolated_avg <- approx(1:12, monthly_avg, xout = t)

# plot seasonal trend
plot(interpolated_avg$y, type = "l", xlab = "Time", ylab = "Seasonal Trend", main = "Modeled Seasonal Trend with Monthly Averages and Interpolation")

# Step 4: Subtract the modeled seasonal trend from the time series with the linear trend already removed. Plot it.

sst_resid <- sst_detrend - interpolated_avg$y

# plot residual time series
plot(sst_resid, type = "l", xlab = "Time", ylab = "Residual Sea Surface Temperature", main = "Residual Sea Surface Temperature after Removing Trend and Seasonality")

# Step 5: Decompose the residual time series (i.e., noise) with a Fourier transform. Plot the partial frequencies. 

# decompose residual time series using Fourier transform
fft_resid <- fft(sst_resid)
partial_freq <- Mod(fft_resid)^2/n

# plot partial frequencies
plot(partial_freq, type = "l", xlab = "Frequency", ylab = "Partial Frequencies", main = "Partial Frequencies of Residual Time Series")

# Step 6: Plot the log-transformed power spectral density of the frequencies.

# compute power spectral density
psd <- spectrum(sst_resid, plot = FALSE)
log_psd <- 10*log10(psd$spec)

# plot log-transformed power spectral density
plot(log_psd, type = "l", xlab = "Frequency", ylab = "Log Power Spectral Density", main = "Log Power Spectral Density of Residual Time Series")