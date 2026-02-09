# load packages
library(tidyverse)
library(lme4)
library(car)
library(glmmTMB)
library(DHARMa)
library(emmeans)

#install.packages("glmmTMB", repos="https://glmmTMB.github.io/glmmTMB/repos", type="binary")
#devtools::install_github("glmmTMB/glmmTMB/glmmTMB")

# load in the data
DSData <- read.csv("LittorinaAbundanceData_DS2021.csv")
DSDataSummer <- read.csv("SummerData.csv")

test_no_percent_cover <- DSData %>%
  dplyr::select(-notes)%>%
  dplyr::select(-percent_cover_barnacles)%>%
  na.omit() %>%
  mutate(wave_exposure = as.factor(wave_exposure),
         treatment = fct_relevel(treatment, "Control", "Mass_Mortality", "Cleared"))

SummerData <- DSDataSummer %>%
  dplyr::select(-notes)%>%
  dplyr::select(-percent_cover_barnacles)%>%
  na.omit() %>%
  mutate(wave_exposure = as.factor(wave_exposure),
         treatment = fct_relevel(treatment, "Control", "Mass_Mortality", "Cleared"))

# Amelia's attempt(s) at modeling

# poisson distribution with glmer, including possible three-way interaction
# and a random intercept for site

mod1 <- glmer(total ~ treatment*wave_exposure*survey + (1|site), 
              family = poisson(), data = test_no_percent_cover)

mod1summer <- glmer(total ~ treatment*wave_exposure*survey + (1|site), 
              family = poisson(), data = SummerData)

# model behaviour looks reasonable
plot(mod1)
qqnorm(residuals(mod1))

plot(mod1summer)
qqnorm(residuals(mod1summer))

# here, can see model summary
summary(mod1)

summary(mod1summer)

# Type III Anova through the car package
Anova(mod1, type = 3)

Anova(mod1summer, type = 3)


#########
# Now the same model through glmmTMB
mod2 <- glmmTMB(total ~ treatment*wave_exposure*survey + (1|site),
                family = poisson(), data = test_no_percent_cover)

# check model assumptions through DHARMa package
plot(simulateResiduals(mod2))
# residuals looks normal

# Note: I tried using poisson and negative binomial families, but quasipoisson seems like the 
# best option from a residuals perspective. It also has the lowest AIC.
#Carter: Quasipossion used when varience is not equal to the mean. In our case here, that could
#be true. However, says this cannot run Quasipossion???

# model summary here
summary(mod2)

# and Type III ANOVA (the important bit)
Anova(mod2, type = 3)

# emmeans allows you to do posthoc comparisons. 
# Here, you can see that both groups are different from control, with mass mortality causing
# increased snail occupancy, and cleared plots having lower occupany.
emmeans(mod1, trt.vs.ctrl ~ treatment)

emmeans(mod1summer, trt.vs.ctrl ~ treatment)

#test
