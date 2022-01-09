####
lag = lagsarlm(as.factor(incidence) ~ num.shared + county1 + year,
               data=all.shared.users, listw = county.w,tol.solve=1.0e-30, zero.policy=TRUE)

summary(lag)
