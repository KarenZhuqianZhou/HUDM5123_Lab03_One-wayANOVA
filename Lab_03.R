# Open the data file by browing the directory
dat <- read.csv(file = file.choose()) #ahi-cesd.csv
#AHI: Authentic Happiness Index
#CES-D: Center for Epidemiological Studies, Depression Scale

dim(rct)
head(rct)

table(rct$occasion)

# Clean data to create a subset that only 
# contains treatment assignment, ahiTotal,
# cesdTotal, id, and measurement occasion.

rct <- subset(x = dat, 
              select = c("id", "occasion", "intervention",
                         "ahiTotal", "cesdTotal"))
dim(rct)
head(rct)

# Create a factor for the intervention group
# variable. From the codebook:
#  Intervention group to which the participant was 
#  randomly assigned. The variable is integer-valued
#  with allowable values from 1-4 inclusive.
#   1 = "Using Signature Strengths"
#   2 = "Three Good Things"
#   3 = "Gratitude Visit"
#   4 = "Recording early memories" 
#   (used as the control condition)

rct$int_fact <- factor(x = rct$intervention, 
                       levels = 1:4,
                       labels = c("SS", "TGT", "GV", "REM"))

str(rct)

# From the code book:
#  Measurement occasion. The variable is 
#  integer-valued with an allowable range 
#  of 0-5 inclusive. 
#    0 = Pretest, i.e. , at enrolment
#    1 = Posttest, i.e. , 7 days after pretest
#    2 = 1-week follow-up, i.e. , 14 days after pretest (7 days after posttest).
#    3 = 1-month follow-up, i.e. , 38 days after pretest (31 days after posttest)
#    4 = 3-month follow-up, i.e. , 98 days after pretest (91 days after posttest)
#    5 = 6-month follow-up, i.e. , 189 days after pretest (182 days after posttest).

# Examine response occasion sample sizes
table(rct$occasion)

# Research question:
#  Does intervention group have an effect on
#  happiness score or depression score as measured,
#  respectively by the AHI and CESD total scores,
#  when measured at 3 months post-intervention?

# We will study methods for repeated meausures
# eventually but for now we will look at "gain
# scores" by taking post - pre differences for 
# the primary outcomes.

# First, need to put the data into wide format.
# Use the reshape() function.
?reshape

# idvar is the ID for each participant.
# timevar is the variable in long format that
#  differentiations multiple records from the 
#  same participant.
# v.names specifies the names of variables that
#  should be spread out in wide format.
rct_wide <- reshape(data = rct, 
                    idvar = "id",
                    timevar = "occasion",
                    v.names = c("ahiTotal", "cesdTotal"),
                    direction = "wide")

dim(rct_wide)
head(rct_wide)

# Calculate gain scores
rct_wide$ahiGS <- rct_wide$ahiTotal.3 - rct_wide$ahiTotal.0
rct_wide$cesdGS <- rct_wide$cesdTotal.3 - rct_wide$cesdTotal.0

mean(rct_wide$ahiGS, na.rm = TRUE)
hist(rct_wide$ahiGS)
mean(rct_wide$cesdGS, na.rm = TRUE)
hist(rct_wide$cesdGS)

# Eliminate cases with missing data at one-month follow-up
rct_wide_ahiGS <- rct_wide[-which(is.na(rct_wide$ahiGS)),]
rct_wide_cesdGS <- rct_wide[-which(is.na(rct_wide$cesdGS)),]

dim(rct_wide)


# Begin by fitting the full model.
head(rct_wide)
lm1 <- lm(ahiGS ~ int_fact, data = rct_wide_ahiGS)
summary(lm1)
lm2 <- lm(cesdGS ~ int_fact, data = rct_wide_cesdGS)
summary(lm2)

# ANOVA on gain scores for AHI
# Classical approach 
library(car)
Anova(lm1, type = 3)
Anova(lm2, type = 3)

# Regression (model comparison) approach
lmF1 <- lm(ahiGS ~ int_fact, data = rct_wide_ahiGS)
lmR1 <- lm(ahiGS ~ 1, data = rct_wide_ahiGS)
anova(lmR1, lm1)

### Pairwise Comparison
# ahiGS
rct_wide_ahiGS$int_fact2 <- relevel(x = rct_wide_ahiGS$int_fact,
                              ref = "REM") # making Group4REM as the reference group
lmF1_R <- lm(ahiGS ~ int_fact2, data = rct_wide_ahiGS)
summary(lmF1_R)
# cesdGS
rct_wide_cesdGS$int_fact2 <- relevel(x = rct_wide_cesdGS$int_fact,
                                    ref = "REM")
lmR1_R <- lm(cesdGS ~ int_fact2, data = rct_wide_ahiGS)
summary(lmR1_R)

library(emmeans)
emm1 <- emmeans(object = lmF1,
                specs = ~ int_fact)
emm2 <- emmeans(object = lmR1_R, specs = ~ int_fact2)

ci1 <- confint(object = emm1,
               level = .90)
ci1

pairs(emm1, adjust = "none")
pairs(emm2, adjust = "none")
library(xtable)
xtable(pairs(emm1, adjust = "none"))

?plot.emmGrid
plot(x = emm1, alpha = .05,
     level = .95,
     adjust = "Tukey",
     comparisons = TRUE,
     horizontal = FALSE,
     xlab = "Intervention",
     ylab = "Estimated Marginal Means (AHI)")
plot(x = emm2, alpha = .05,
     level = .95,
     adjust = "Tukey",
     comparisons = TRUE,
     horizontal = FALSE,
     xlab = "Intervention",
     ylab = "Estimated Marginal Means (CESD)")

confint(pairs(emm2, adjust = "none"))
