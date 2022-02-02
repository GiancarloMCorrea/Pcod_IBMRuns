par(mfrow = c(2,3))

# MZL:
lens = seq(from = 0.02, to = 0.2, by = 0.01)
wtgs = 2.63E-6 * lens ^ 2.23

plot(lens, wtgs, xlab = 'length in um', ylab = 'weight in ug', main = 'MZL')

# Cop:
lens = seq(from = 200, to = 1400, by = 10)
wtgs = 2.4E-8 * lens ^ 2.85

plot(lens, wtgs, xlab = 'length in um', ylab = 'weight in ug', main = 'Cop')
points(1000, (10^-7.62)*(1000)^2.85, col = 2, pch = 16, cex = 2)

# NcaO:
lens = seq(from = 200, to = 1400, by = 10)
wtgs = 1E-10 * lens ^ 3.56

plot(lens, wtgs, xlab = 'length in um', ylab = 'weight in ug', main = 'NCaO')
points(1000, ((10^-2.32)*(1)^3.56)*1000, col = 2, pch = 16, cex = 2)

# NcaS:
lens = seq(from = 400, to = 3000, by = 10)
wtgs = 2.75E-12 * lens ^ 4.03

plot(lens, wtgs, xlab = 'length in um', ylab = 'weight in ug', main = 'NCaS')
points(2000, ((10^-11.561)*(2000)^4.034), col = 2, pch = 16, cex = 2)

# EupS:
lens = seq(from = 3, to = 25, by = 1)
wtgs = 7.83E-9 * lens ^ 3.02

plot(lens, wtgs, xlab = 'length in um', ylab = 'weight in ug', main = 'EupS')

# EupO:
lens = seq(from = 3, to = 30, by = 1)
wtgs = 1.38E-8 * lens ^ 2.92

plot(lens, wtgs, xlab = 'length in um', ylab = 'weight in ug', main = 'EupO')
