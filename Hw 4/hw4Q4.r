library(MASS)
data("swiss")
help(swiss)

null <- lm(Fertility ~ 1, data = swiss) # 1 here means the intercept
full <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data = swiss)
# Forward selection
stepAIC(null, scope = list(lower = null, upper = full), data = swiss, direction = "forward")
stepAIC(full, scope = list(lower = null, upper = full), data = swiss, direction = "backward")
stepAIC(null, scope = list(lower = null, upper = full), data = swiss, direction = "both")
