library(MASS)
data("swiss")
help(swiss)

null <- lm(Fertility ~ 1, data = swiss) # 1 here means the intercept
full <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data = swiss)
# Forward selection
stepAIC(null, scope = list(lower = null, upper = full), data = swiss, direction = "forward")
stepAIC(full, scope = list(lower = null, upper = full), data = swiss, direction = "backward")
stepAIC(null, scope = list(lower = null, upper = full), data = swiss, direction = "both")
# Forward got AIC to 189.86
# Had Edu, Cath, Inf, Agri v Fertility
# Backward got AIC 189.86
# Had Agri, Inf, Cath, and Edu v Fertility
# Step got AIC 189.86
# Had Edu, Cath, Inf, and Agri v Fertility
# thus all models agree with similar coefficients

# Education explains our data the best due to it's low AIC score (in comparison to the rest)
# Then Catholic and inf.mort is extremely similar.
# Then Examination has some effects All forward



#
