#Load libraries 
library(lme4)
library(ggplot2)
library(dplyr)
library(lmerTest)

############
#Question 1
############

#Load the data 
getwd()
pisadata <- read.csv("cw/pisaCanadaMaths.csv")

#Relevel categorical variables 
pisadata$female <- factor(pisadata$female,
                          levels = c(0, 1),
                          labels = c("Male", "Female"))

pisadata$immig  <- factor(pisadata$immig,
                          levels = c(1, 2, 3),
                          labels = c("Native", "2nd Generation", "1st Generation"))

pisadata$langn  <- factor(pisadata$langn,
                          levels = c(1, 2, 3),
                          labels = c("English", "French", "Other"))

pisadata$schprivate <- factor(pisadata$schprivate,
                              levels = c(0, 1),
                              labels = c("Government", "Private"))

#Summary statistics 
summary(pisadata)

#Number of schools
length(unique(pisadata$schoolid))

#Standard deviation for continuous variables 
sapply(pisadata[, c("zmath", "age", "hisced", "homepos", "stubeha")], sd, na.rm = TRUE)

#Frequencies of categorical variables
cat_vars <- c("female", "immig", "langn", "schprivate")

lapply(cat_vars, function(v) {
  freq <- table(pisadata[[v]])
  pct  <- round(prop.table(freq) * 100, 1)
  data.frame(Variable = v, Category = names(freq), 
             Frequency = as.numeric(freq), Percentage = as.numeric(pct))
}) %>% bind_rows()


#Plots 
ggplot(pisadata, aes(x = zmath)) +
  geom_histogram(bins = 60, fill = "blue", colour = "white", alpha = 0.85) +
  labs(title = "Distribution of Standardised Maths Scores",
       x = "Standardised Maths Score", y = "Frequency") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank()
  )

ggplot(pisadata, aes(x = female, y = zmath, fill = female)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#AED6F1", "#1A5276")) +
  labs(x = "Gender", y = "Maths Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

ggplot(pisadata, aes(x = langn, y = zmath, fill = langn)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#1A5276", "#5DADE2", "#A9CCE3")) +
  labs(x = "Language Spoken at Home", y = "Maths Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

ggplot(pisadata, aes(x = immig, y = zmath, fill = immig)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#1A5276", "#2E86C1", "#85C1E9")) +
  labs(x = "Immigration Background", y = "Maths Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

#School-level boxplot uses school means to avoid within-school noise
school.means <- pisadata %>%
  group_by(schoolid, schprivate) %>%
  summarise(mean_zmath = mean(zmath), .groups = "drop")

ggplot(school.means, aes(x = schprivate, y = mean_zmath, fill = schprivate)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#AED6F1", "#1A5276")) +
  labs(x = "School Type", y = "Mean Maths Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(colour = "black")
  )

ggplot(pisadata, aes(x = hisced, y = zmath)) +
  geom_point(alpha = 0.15, colour = "#2E86C1", size = 0.8) +
  geom_smooth(method = "lm", colour = "#1A5276", fill = "#AED6F1", alpha = 0.3) +
  labs(x = "Highest Parental Education (hisced)",
       y = "Maths score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

ggplot(pisadata, aes(x = homepos, y = zmath)) +
  geom_point(alpha = 0.15, colour = "#2E86C1", size = 0.8) +
  geom_smooth(method = "lm", colour = "#1A5276", fill = "#AED6F1", alpha = 0.3, se = TRUE,) +
  labs(x = "Home Possessions Score (homepos)",
       y = "Maths Score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

ggplot(pisadata, aes(x = age, y = zmath)) +
  geom_point(alpha = 0.15, colour = "#2E86C1", size = 0.8) +
  geom_smooth(method = "lm", colour = "#1A5276", fill = "#AED6F1", alpha = 0.3) +
  labs(x = "Age",
       y = "Maths Score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

#stubeha is school-level so plot against school means
school.data <- pisadata %>%
  group_by(schoolid) %>%
  summarise(mean_zmath = mean(zmath),
            stubeha    = first(stubeha),
            .groups    = "drop")

ggplot(school.data, aes(x = stubeha, y = mean_zmath)) +
  geom_point(alpha = 0.5, colour = "#2E86C1", size = 1.2) +
  geom_smooth(method = "lm", colour = "#1A5276", fill = "#AED6F1", alpha = 0.3) +
  labs(x = "Student Behaviour Score (stubeha)",
       y = "Mean Maths Score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

#Model building

#Step 1: Null model
null.model <- lmer(zmath ~ 1 + (1 | schoolid), data = pisadata, REML = FALSE)
summary(null.model)

vc          <- as.data.frame(VarCorr(null.model))
between.var <- vc$vcov[1]
within.var  <- vc$vcov[2]
icc         <- between.var / (between.var + within.var)

cat("Between variance:", between.var)
cat("Within variance:", within.var)
cat("ICC:", icc)

#LRT comparing null model against OLS 
#Halved p-value as sigma2_u is constrained to be non-negative
ols.model <- lm(zmath ~ 1, data = pisadata)
lr.stat   <- 2 * (logLik(null.model)[1] - logLik(ols.model)[1])
p.val     <- 0.5 * (1 - pchisq(lr.stat, 1))

cat("LRT statistic:", lr.stat)
cat("p-value:", p.val)

LR <- -2 * (logLik(ols.model) - logLik(null.model))
LR <- as.numeric(LR)
LR

pchisq(as.numeric(LR), df = 1, lower.tail = FALSE)

#Centre age and hisced for better interpretation
pisadata$age.c    <- pisadata$age    - mean(pisadata$age)
pisadata$hisced.c <- pisadata$hisced - mean(pisadata$hisced)


#Step 2: Individual-level predictors (backward elimination)
model.2.1 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    (1 | schoolid), data = pisadata, REML = FALSE)
summary(model.2.1)


#Step 3: Test gender interactions with each individual-level predictor

#female*immig
model.3.1 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*immig + (1 | schoolid), data = pisadata, REML = FALSE)
anova(model.2.1, model.3.1)

#female*langn
model.3.2 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*langn + (1 | schoolid), data = pisadata, REML = FALSE)
anova(model.2.1, model.3.2)

#female*hisced.c - significant, retained
model.3.3 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + (1 | schoolid), data = pisadata, REML = FALSE)
anova(model.2.1, model.3.3)

#female*homepos
model.3.4 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*homepos + (1 | schoolid), data = pisadata, REML = FALSE)
anova(model.2.1, model.3.4)

#female*age.c
model.3.5 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*age.c + (1 | schoolid), data = pisadata, REML = FALSE)
anova(model.2.1, model.3.5)


#Conclude step 3 with model.3.3 (female*hisced.c) as the final model


#Step 4: School-level predictors added one at a time
#schprivate
model.4.1 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate +
                    (1 | schoolid), data = pisadata, REML = FALSE)
anova(model.3.3, model.4.1)

#stubeha and schprivate
model.4.2 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 | schoolid), data = pisadata, REML = FALSE)
anova(model.4.1, model.4.2)

#stubeha and schprivate were both significant  
#Conclude step 4 with model.4.2 as the final model

#Step 5: Test random slopes for individual-level predictors one at a time
#Mixture chi-squared used as slope variance is constrained to be non-negative

#Scale stubeha to help with convergence
pisadata$stubeha.z <- scale(pisadata$stubeha, center = TRUE, scale = TRUE)

#female
model.5.1 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 + female | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))

lr.5.1 <- 2 * (logLik(model.5.1)[1] - logLik(model.4.2)[1])
p.5.1  <- 0.5 * (1 - pchisq(lr.5.1, 1)) + 0.5 * (1 - pchisq(lr.5.1, 2))
cat("female:  LR =", round(lr.5.1, 3), " p =", round(p.5.1, 4))

#These failed to converge 

## Random slope for immig
#model.5.2 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
#                    female*hisced.c + schprivate + stubeha +
#                    (1 + immig | schoolid), data = pisadata, REML = FALSE,
#                  control = lmerControl(optimizer = "Nelder_Mead"))
#lr.5.2 <- 2 * (logLik(model.5.2)[1] - logLik(model.4.2)[1])
#p.5.2  <- 0.5 * (1 - pchisq(lr.5.2, 1)) + 0.5 * (1 - pchisq(lr.5.2, 2))
#cat("immig:   LR =", round(lr.5.2, 3), " p =", round(p.5.2, 4))

## Random slope for langn
#model.5.3 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
#                    female*hisced.c + schprivate + stubeha +
#                    (1 + langn | schoolid), data = pisadata, REML = FALSE,
#                  control = lmerControl(optimizer = "Nelder_Mead"))
#lr.5.3 <- 2 * (logLik(model.5.3)[1] - logLik(model.4.2)[1])
#p.5.3  <- 0.5 * (1 - pchisq(lr.5.3, 1)) + 0.5 * (1 - pchisq(lr.5.3, 2))
#cat("langn:   LR =", round(lr.5.3, 3), " p =", round(p.5.3, 4))

#hisced.c
model.5.4 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 + hisced.c | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
lr.5.4 <- 2 * (logLik(model.5.4)[1] - logLik(model.4.2)[1])
p.5.4  <- 0.5 * (1 - pchisq(lr.5.4, 1)) + 0.5 * (1 - pchisq(lr.5.4, 2))
cat("hisced.c:  LR =", round(lr.5.4, 3), " p =", round(p.5.4, 4))

#homepos
model.5.5 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 + homepos | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
lr.5.5 <- 2 * (logLik(model.5.5)[1] - logLik(model.4.2)[1])
p.5.5  <- 0.5 * (1 - pchisq(lr.5.5, 1)) + 0.5 * (1 - pchisq(lr.5.5, 2))
cat("homepos: LR =", round(lr.5.5, 3), " p =", round(p.5.5, 4))

#age.c
model.5.6 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 + age.c | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
lr.5.6 <- 2 * (logLik(model.5.6)[1] - logLik(model.4.2)[1])
p.5.6  <- 0.5 * (1 - pchisq(lr.5.6, 1)) + 0.5 * (1 - pchisq(lr.5.6, 2))
cat("age.c:   LR =", round(lr.5.6, 3), " p =", round(p.5.6, 4))

#female and hisced.c most significant individually - test combinations

#female + hisced.c
model.5.7 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 + female + hisced.c | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
lr.5.7 <- 2 * (logLik(model.5.7)[1] - logLik(model.4.2)[1])
p.5.7  <- 0.5 * (1 - pchisq(lr.5.7, 1)) + 0.5 * (1 - pchisq(lr.5.7, 2))
cat("female + hisced.c: LR =", round(lr.5.7, 3), " p =", round(p.5.7, 4))


#female + homepos
model.5.8 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 + female + homepos | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
lr.5.8 <- 2 * (logLik(model.5.8)[1] - logLik(model.4.2)[1])
p.5.8  <- 0.5 * (1 - pchisq(lr.5.8, 1)) + 0.5 * (1 - pchisq(lr.5.8, 2))
cat("female + homepos: LR =", round(lr.5.8, 3), " p =", round(p.5.8, 4))


#hisced.c + homepos
model.5.9 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha +
                    (1 + hisced.c + homepos | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
lr.5.9 <- 2 * (logLik(model.5.9)[1] - logLik(model.4.2)[1])
p.5.9  <- 0.5 * (1 - pchisq(lr.5.9, 1)) + 0.5 * (1 - pchisq(lr.5.9, 2))
cat("hisced.c + homepos: LR =", round(lr.5.9, 3), " p =", round(p.5.9, 4))

#Conclude step 5 with model.5.7 (female + hisced.c random slopes) as best fitting model


#Step 6: Cross-level interactions - test against model.5.7
#Failed to converge 
#model.6.1 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
#                     female*hisced.c + schprivate + stubeha.z + hisced.c*schprivate +
#                     (1 + female + hisced.c | schoolid), data = pisadata, REML = FALSE,
#                   control = lmerControl(optimizer = "Nelder_Mead"))
#anova(model.5.7, model.6.1)


#hisced.c*stubeha.z
model.6.2 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha.z + hisced.c*stubeha.z +
                    (1 + female + hisced.c | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
anova(model.5.7, model.6.2)


#female*stubeha.z
model.6.3 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha.z + female*stubeha.z +
                    (1 + female + hisced.c | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
anova(model.5.7, model.6.3)


#female*schprivate
model.6.4 <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                    female*hisced.c + schprivate + stubeha.z + female*schprivate +
                    (1 + female + hisced.c | schoolid), data = pisadata, REML = FALSE,
                  control = lmerControl(optimizer = "Nelder_Mead"))
anova(model.5.7, model.6.4)
#None of the cross-level interactions are significant - model.5.7 is the final model




#Final model summary
summary(model.5.7)
as.data.frame(VarCorr(model.5.7))

#Between-school variance as a function of hisced.c
hisced_range <- seq(min(pisadata$hisced.c), max(pisadata$hisced.c), length.out = 200)

sigma2_u0  <- 0.1287
sigma_u01  <- 0.009913
sigma2_u1  <- 0.003005

bsv <- sigma2_u0 + 2 * sigma_u01 * hisced_range + sigma2_u1 * hisced_range^2

plot(hisced_range, bsv,
     type = "l",
     xlab = "Parental education (hisced.c)",
     ylab = "Between-school variance",
     main = "")





# Fit with hisced centred only (what you did)
model_centred <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                        female*hisced.c + schprivate + stubeha +
                        (1 + female + hisced.c | schoolid), data = pisadata, REML = FALSE,
                      control = lmerControl(optimizer = "Nelder_Mead"))

# Create scaled version of hisced
pisadata$hisced.c <- scale(pisadata$hisced, center = TRUE, scale = TRUE)

# Fit with hisced scaled
model_scaled <- lmer(zmath ~ 1 + female + immig + langn + hisced.c + homepos + age.c +
                       female*hisced.c + schprivate + stubeha +
                       (1 + female + hisced.c | schoolid), data = pisadata, REML = FALSE,
                     control = lmerControl(optimizer = "Nelder_Mead"))

# Compare log-likelihoods - should be identical if scaling made no difference
logLik(model_centred)
logLik(model_scaled)

# Check convergence warnings for both
summary(model_centred)
summary(model_scaled)









############
#Question 2
############

attfam <- read.csv("cw/attfamUK.csv")

attfam$country_lbl <- factor(attfam$country,
                             levels = 1:4,
                             labels = c("England", "Wales", "Scotland", "N. Ireland"))

attfam$gender      <- factor(attfam$female,  labels = c("Male", "Female"))
attfam$qual_lbl    <- factor(attfam$qualhi,
                             levels = c(0, 1),
                             labels = c("A-level or lower", "Post A-level"))
attfam$partner_lbl <- factor(attfam$partner, labels = c("No partner", "Has partner"))

attfam$age.c  <- attfam$age - mean(attfam$age)
attfam$age.c2 <- attfam$age.c^2


#frequencies 
obs_cats <- c("partner_lbl")
lapply(obs_cats, function(v) {
  freq <- table(attfam[[v]])
  pct  <- round(prop.table(freq) * 100, 1)
  data.frame(Variable = v, Category = names(freq),
             Frequency = as.numeric(freq), Percentage = as.numeric(pct))
}) %>% bind_rows()

# Individual-level variables (one row per person)
attfam_ind <- attfam[!duplicated(attfam$pid), ]

ind_cats <- c("female", "qualhi", "country")
lapply(ind_cats, function(v) {
  freq <- table(attfam_ind[[v]])
  pct  <- round(prop.table(freq) * 100, 1)
  data.frame(Variable = v, Category = names(freq),
             Frequency = as.numeric(freq), Percentage = as.numeric(pct))
}) %>% bind_rows()


attfam_ind <- attfam[!duplicated(attfam$pid), ]

ind_cats <- c("gender", "qual_lbl", "country_lbl")
lapply(ind_cats, function(v) {
  freq <- table(attfam_ind[[v]])
  pct  <- round(prop.table(freq) * 100, 1)
  data.frame(Variable = v, Category = names(freq),
             Frequency = as.numeric(freq), Percentage = as.numeric(pct))
}) %>% bind_rows()


#Continuous variables
cat("Mean age:", mean(attfam$age))
cat("N individuals:", length(unique(attfam$pid)))
cat("N observations:", nrow(attfam))

summary(attfam[, c("zattfam", "age")])
cat("SD zattfam:", sd(attfam$zattfam))
cat("SD age:",sd(attfam$age))


#Plot for 9 trajectories 
first_9_ids <- unique(attfam$pid)[1:9]
attfam_traj <- attfam %>% filter(pid %in% first_9_ids)

ggplot(attfam_traj, aes(x = age, y = zattfam, group = pid)) +
  geom_line(colour = "#2E86C1") +
  geom_point(colour = "#1A5276", size = 1.5) +
  facet_wrap(~pid, ncol = 3, labeller = label_both) +
  labs(
    title = "Attitude to Family Trajectories for Nine Individuals",
    x = "Age (years)", y = "Standardised Attitude Score (zattfam)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

#Null model 
m0 <- lmer(zattfam ~ 1 + (1 | pid), data = attfam, REML = TRUE)
vc <- as.data.frame(VarCorr(m0))
var_between <- vc$vcov[1]
var_within  <- vc$vcov[2]
vpc <- var_between / (var_between + var_within)

cat("Between-individual variance:", round(var_between, 4))
cat("Within-individual variance:",  round(var_within,  4))
cat("VPC (ICC):", round(vpc, 3))
cat("Intercept:", round(fixef(m0), 4))

#Eda plots
ggplot(attfam, aes(x = partner_lbl, y = zattfam, fill = partner_lbl)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#AED6F1", "#1A5276")) +
  labs(x = "Partnership Status", y = "Standardised Attitude Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

ggplot(attfam, aes(x = age, y = zattfam)) +
  geom_point(alpha = 0.15, colour = "#2E86C1", size = 0.8) +
  geom_smooth(method = "loess", colour = "#1A5276", fill = "#AED6F1", alpha = 0.3) +
  labs(x = "Age (years)", y = "Standardised Attitude Score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

attfam_mean <- attfam %>%
  group_by(pid, gender, qual_lbl, country_lbl) %>%
  summarise(mean_zattfam = mean(zattfam), .groups = "drop")

ggplot(attfam_mean, aes(x = gender, y = mean_zattfam, fill = gender)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#AED6F1", "#1A5276")) +
  labs(x = "Gender", y = "Individual Mean Attitude Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

ggplot(attfam_mean, aes(x = qual_lbl, y = mean_zattfam, fill = qual_lbl)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#AED6F1", "#1A5276")) +
  labs(x = "Highest Qualification", y = "Individual Mean Attitude Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

ggplot(attfam_mean, aes(x = country_lbl, y = mean_zattfam, fill = country_lbl)) +
  geom_boxplot(colour = "black", alpha = 0.85, outlier.size = 0.8,
               outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#1A5276", "#2E86C1", "#5DADE2", "#A9CCE3")) +
  labs(x = "Country", y = "Individual Mean Attitude Score") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  )

#Model building
#Null model fitted earlier 

#m0 <- lmer(zattfam ~ 1 + (1 | pid), data = attfam, REML = TRUE)


#Model 2: Adding age as a fixed and random effect 

# Add age as a fixed effect with random intercept only
model_q2_2.0 <- lmer(zattfam ~ age.c + (1 | pid), data = attfam, REML = FALSE)
summary(model_q2_2.0)

#Test random slope for age 
model_q2_2.1 <- lmer(zattfam ~ age.c + (age.c | pid), data = attfam, REML = FALSE)
summary(model_q2_2.1)

lr.2.1 <- 2 * (logLik(model_q2_2.1)[1] - logLik(model_q2_2.0)[1])
p.2.1  <- 0.5 * (1 - pchisq(lr.2.1, 1)) + 0.5 * (1 - pchisq(lr.2.1, 2))
cat("LR statistic:", lr.2.1)
cat("p-value:     ", p.2.1)

# Test quadratic age term to check for non-linearity 
model_q2_2.2 <- lmer(zattfam ~ age.c + age.c2 + (age.c | pid), data = attfam,
                     REML = FALSE)

anova(model_q2_2.1, model_q2_2.2)

#Conclude step 2 with model_q2_2.1 as the final model 

#Step 3: time varying covariates
#partner
model_q2_3.0 <- lmer(zattfam ~ age.c + partner + (age.c | pid), data = attfam,
                     REML = FALSE)

anova(model_q2_2.1, model_q2_3.0)

#Conclude step 3 with model_q2_3.0 as final model 


#Step 4: time invariant covariates
# Test each level-2 predictor individually against model_q2_3.0

#female 
model_q2_4.1 <- lmer(zattfam ~ age.c + partner + female + (age.c | pid),
                     data = attfam, REML = FALSE)
anova(model_q2_3.0, model_q2_4.1)

#qualhi
model_q2_4.2 <- lmer(zattfam ~ age.c + partner + qualhi + (age.c | pid),
                     data = attfam, REML = FALSE,
                     control = lmerControl(optimizer = "Nelder_Mead"))
anova(model_q2_3.0, model_q2_4.2)

#country_lbl
model_q2_4.3 <- lmer(zattfam ~ age.c + partner + country_lbl + (age.c | pid),
                     data = attfam, REML = FALSE)
anova(model_q2_3.0, model_q2_4.3)
#country_lbl not significant so its dropped


# Both female and qualhi individually significant, so test a combined model
model_q2_4.4 <- lmer(zattfam ~ age.c + partner + female + qualhi + (age.c | pid),
                     data = attfam, REML = FALSE)

# Test whether adding qualhi on top of female helps
anova(model_q2_4.1, model_q2_4.4)

# Test whether adding female on top of qualhi helps
anova(model_q2_4.2, model_q2_4.4)

#Conclude step 4 with model_q2_4.1 as final model 


# Final model summary
summary(model_q2_4.1)
as.data.frame(VarCorr(model_q2_4.1))


#Plot for between variance 
sigma2_u0 <- 0.527253
sigma_u01  <- 0.009363
sigma2_u1  <- 0.003758

age_range <- seq(min(attfam$age.c), max(attfam$age.c), length.out = 200)
bsv       <- sigma2_u0 + 2 * sigma_u01 * age_range + sigma2_u1 * age_range^2

bsv_df <- data.frame(age.c = age_range, bsv = bsv)

ggplot(bsv_df, aes(x = age.c, y = bsv)) +
  geom_line(colour = "#1A5276", linewidth = 0.9) +
  labs(
    x = "Age",
    y = "Between-individual variance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(colour = "black")
  )



#Check how if qualhi varies to note in limitations
vary_count <- 0

for (id in unique(attfam$pid)) {
  individual_data <- attfam[attfam$pid == id, ]
  unique_vals <- unique(individual_data$qualhi)
  if (length(unique_vals) > 1) {
    vary_count <- vary_count + 1
  }
}

cat("Individuals where qualhi varies:", vary_count)
