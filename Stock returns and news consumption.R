
# Stock returns and news consumption   
# Packages and Libraries
install.packages(c("lmtest", "car", "sandwich", "plm", "stargazer", "ggplot2"))
require(lmtest)
require(car)
require(sandwich)
require(plm)
require(stargazer)
require(ggplot2)

## PART 0
# Load data
svi <- read.csv("C:/Users/asmam/OneDrive/SEMESTER TWO/FIE401/2. Assignment Two/SVI.csv")

# Remove missing values
svi <- na.omit(svi)

# Adjust dates
svi$date <- as.Date(svi$date, format = "%d%B%Y")
# If the line above gives NA dates, use this instead:
# svi$date <- as.Date(svi$date, format = "%d%b%Y")

# Claim the dataset to be panel
svi <- pdata.frame(svi, index = c("ticker", "date"))

## PART 1
# Regression table 1

# Prepare the variables
svi$ln_SVI <- log(1 + svi$SVI)
svi$absRET <- abs(svi$RET)

# Model
fit <- plm(ln_SVI ~ absRET, data = svi, model = "pooling")

# Standard errors
se.1 <- coeftest(fit)[, 2]
se.2 <- coeftest(fit, vcov = vcovHC)[, 2]
se.3 <- coeftest(fit, vcov = vcovHC(fit, cluster = "group", type = "sss"))[, 2]

# Report the table
stargazer(
  fit, fit, fit,
  se = list(se.1, se.2, se.3),
  keep.stat = c("n", "rsq", "adj.rsq"),
  dep.var.labels = "log(SVI + 1)",
  covariate.labels = c("|StockRet|"),
  report = "vc*t",
  type = "text"
)

# Plot 1
# Prediction from the model
svi$part1 <- predict(fit, svi)

# Graph
ggplot(data = svi, aes(x = RET, y = ln_SVI)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = part1, colour = "Prediction"))

## PART 2
# Regression table 2

# Prepare the variables
svi$absvwretd <- abs(svi$vwretd)
svi$ln_SVI_lag <- log(1 + svi$SVI_lag)

# Model 1
fit.1 <- plm(ln_SVI ~ absRET + absvwretd, data = svi, model = "pooling")
se.1 <- coeftest(fit.1, vcov = vcovHC(fit.1, cluster = "group", type = "sss"))[, 2]

# Model 2
fit.2 <- plm(ln_SVI ~ absRET + ln_SVI_lag, data = svi, model = "pooling")
se.2 <- coeftest(fit.2, vcov = vcovHC(fit.2, cluster = "group", type = "sss"))[, 2]

# Model 3
fit.3 <- plm(ln_SVI ~ absRET + absvwretd + ln_SVI_lag, data = svi, model = "pooling")
se.3 <- coeftest(fit.3, vcov = vcovHC(fit.3, cluster = "group", type = "sss"))[, 2]

# Report the table
stargazer(
  fit.1, fit.2, fit.3,
  se = list(se.1, se.2, se.3),
  keep.stat = c("n", "rsq", "adj.rsq"),
  dep.var.labels = "log(SVI + 1)",
  covariate.labels = c("|StockRet|", "|MarketRet|", "log(SVI prev day + 1)"),
  report = "vc*t",
  type = "text"
)

# Plot 2
# Regression 1
fit <- lm(ln_SVI ~ ln_SVI_lag, data = svi)
svi$y_res <- resid(fit)

# Regression 2
fit <- lm(absRET ~ ln_SVI_lag, data = svi)
svi$x_res <- resid(fit)

# Regression 3
fit <- lm(y_res ~ x_res, data = svi)
svi$y_res_hat <- predict(fit, data = svi)

# Graph
ggplot(data = svi, aes(x = x_res, y = y_res)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = y_res_hat, colour = "Prediction"))

## PART 3
# Regression table 3

# Model 1
fit.1 <- plm(
  ln_SVI ~ absRET + absvwretd + ln_SVI_lag,
  data = svi,
  model = "within",
  effect = "individual"
)
se.1 <- coeftest(fit.1, vcov = vcovHC(fit.1, cluster = "group", type = "sss"))[, 2]

# Model 2
fit.2 <- plm(
  ln_SVI ~ absRET + absvwretd + ln_SVI_lag,
  data = svi,
  model = "within",
  effect = "time"
)
se.2 <- coeftest(fit.2, vcov = vcovHC(fit.2, cluster = "group", type = "sss"))[, 2]

# Model 3
fit.3 <- plm(
  ln_SVI ~ absRET + ln_SVI_lag,
  data = svi,
  model = "within",
  effect = "twoway"
)
se.3 <- coeftest(fit.3, vcov = vcovHC(fit.3, cluster = "group", type = "sss"))[, 2]

# If you keep absvwretd in the third model, you may get an error when estimating clustered standard errors.
# Since absvwretd is absorbed by day fixed effects, remove it.

# Report the table
stargazer(
  fit.1, fit.2, fit.3,
  se = list(se.1, se.2, se.3),
  keep.stat = c("n", "rsq", "adj.rsq"),
  report = "vc*t",
  type = "text",
  dep.var.labels = "log(SVI + 1)",
  covariate.labels = c("|StockRet|", "|MarketRet|", "log(SVI prev day + 1)"),
  add.lines = list(
    c("Stock FE", "Yes", "No", "Yes"),
    c("Day FE", "No", "Yes", "Yes")
  )
)
