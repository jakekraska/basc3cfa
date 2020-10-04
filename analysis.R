# CFA Analysis of the BASC-3
# Original code developed by Jake Kraska (Monash University)
# Built on R 4.0.2 with R Studio 1.3.1073
# requires file data with 736 obs of 401 variables
# set the working directory to source file directory manually

#---                                        ---#
################# Load Packages #################
#---                                        ---#

require(plyr) # version 1.8.6
require(ggplot2) # version 3.3.2
require(psych) # version 2.0.7
require(knitr) # version 1.29
require(lavaan) # version 0.6-7
require(dplyr) # version 1.0.1
require(tidyr) # version 1.1.1
require(latticeExtra) # version 0.6-29
require(lubridate) # version 1.7.9

#---                                       ---#
################# Set Options #################
#---                                       ---#

set.seed(2019)
options(max.print = 3000)

#---                                            ---#
################# Load Scale Data #################
#---                                            ---#

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set the wd to source file
items <- read.csv("items.csv")
data <- read.csv("data.csv")

#---                                                 ---#
################# Clean and Recode Data #################
#---                                                ---#

# change all empty cells to NA
data[data == "" | data == " "] <- NA
items[items == "" | items == " "] <- NA

# create gender factor
data$Gender <- addNA(as.factor(recode(data$Gender, 
                                      "1" = "Male", 
                                      "2" = "Female", 
                                      "3" = "Other")))

# create birth date variable
data$BirthDate <- as.Date(data$BirthDate, "%d/%m/%Y")

# create administration date variable
data$AdministrationDate <- as.Date(data$AdministrationDate, "%d/%m/%Y")

# create age at assessment
data$AgeAtAssessment <- as.period(interval(data$BirthDate, data$AdministrationDate), unit = "year")

# create age by year
data$Age <- as.integer(gsub("y.*","",data$AgeAtAssessment))
data$AgeFactor <- factor(data$Age, ordered = TRUE, levels = c(seq(6,20,1)))

# create form factor
data$Form <- addNA(as.factor(recode(data$Form, 
                                    "2" = "TRS-C", 
                                    "5" = "PRS-C")))
# create rater gender factor
data$RaterGender <- addNA(as.factor(recode(data$RaterGender,
                                           "1" = "Male", 
                                           "2" = "Female", 
                                           "3" = "Other")))

#---                                            ---#
################# Create Item Sets #################
#---                                            ---#

prsc.subscale.items <- list()
for (i in na.omit(unique(items$prsc_clin))) {
  prsc.subscale.items[[i]] <- filter(items, prsc_clin == i)[[1]]
}

prsc.composite.items <- list()
for (i in na.omit(unique(items$prsc_comp))) {
  prsc.composite.items[[i]] <- filter(items, prsc_comp == i)[[1]]
}

prsc.bsi.items <- filter(items, prsc_bsi == "y")[[1]]

trsc.subscale.items <- list()
for (i in na.omit(unique(items$trsc_clin))) {
  trsc.subscale.items[[i]] <- filter(items, trsc_clin == i)[[1]]
}

trsc.composite.items <- list()
for (i in na.omit(unique(items$trsc_comp))) {
  trsc.composite.items[[i]] <- filter(items, trsc_comp == i)[[1]]
}

trsc.bsi.items <- filter(items, trsc_bsi == "y")[[1]]

#---                                      ---#
################# Split Data #################
#---                                      ---#

basc3prsc <- filter(data, Form == "PRS-C")
basc3trsc <- filter(data, Form == "TRS-C")

#---                                       ---#
################# Explore Data #################
#---                                        ---#

# Age distribution

ggplot(data = data, aes(x = AgeFactor, fill = Form)) +
  geom_bar(position = position_dodge(width = .7)) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = .7)) +
  scale_y_continuous(name = "Number of Participants", breaks = seq.int(0,105,25), limits = c(0,105)) +
  scale_x_discrete(name = "Age") 
  
# Gender distribution

ggplot(data = data, aes(x = Gender, fill = Form)) +
  geom_bar(position = position_dodge(width = .7)) +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = .7)) +
  scale_y_continuous(name = "Number of Participants", breaks = seq.int(0,255,25), limits = c(0,255)) +
  scale_x_discrete(name = "Gender") 

#---                                          ---#
################# Reverse Items  #################
#---                                          ---#

prsc.reverse.items <- c("001","028","083","127") %>% # reverse attention problems to make negative
  append(., c("069","085","168")) %>% # reverse functional communication to make positive
  append(., c("027","046","064","066")) %>% # reverse activities of daily living to make positive
  append(., c("087","163")) # reverse withdrawal to make negative

basc3prsc <- basc3prsc %>% mutate_at(vars(paste0("basc3_r", prsc.reverse.items)),
                                     funs(recode(., `1` = 4, `2` = 3, `3` = 2, `4` = 1)))

trsc.reverse.items <- c("001","021","064") %>% # reverse Attention Problems to make negative
  append(., c("022","032","139")) %>% # reverse Functional Communication to make positive
  append(., c("020")) %>% # reverse Adaptability to make positive
  append(., c("055")) %>% # reverse Learning Problems to make negative
  append(., c("098","144")) # reverse Withdrawal to make negative

basc3trsc <- basc3trsc %>% mutate_at(vars(paste0("basc3_r", trsc.reverse.items)),
                                     funs(recode(., `1` = 4, `2` = 3, `3` = 2, `4` = 1)))

#---                                         ---#
################# Descriptives  #################
#---                                         ---#

psych::describe(select(basc3prsc, ends_with("_t")), na.rm = TRUE)

psych::describe(select(basc3trsc, ends_with("_t")), na.rm = TRUE)

#---                                        ---#
################# CFA Models  #################
#---                                        ---#

# PRS-C Subscale Models
prsc.subscale.models <- list()
for (i in na.omit(unique(items$prsc_clin))) {
  prsc.subscale.models[[i]] <- paste0(i, " =~ ", paste0(filter(items, prsc_clin == i)[[1]], collapse = "+"))
}

# PRS-C Internalizing Model
prsc.internalizing.model <- paste(
  # subscales
  paste("anx =~ ", paste(filter(items, prsc_clin == "Anxiety")[[1]], collapse = "+"), sep = ""),
  paste("dep =~ ", paste(filter(items, prsc_clin == "Depression")[[1]], collapse = "+"), sep = ""),
  paste("som =~ ", paste(filter(items, prsc_clin == "Somatization")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("int =~ NA*anx + dep + som", sep = ""),
  paste("int ~~ 1*int", sep = ""),
  # separate all by lines
  sep = "\n"
)

# PRS-C Externalizing Model
prsc.externalizing.model <- paste(
  # subscales
  paste("hyp =~ ", paste(filter(items, prsc_clin == "Hyperactivity")[[1]], collapse = "+"), sep = ""),
  paste("agg =~ ", paste(filter(items, prsc_clin == "Aggression")[[1]], collapse = "+"), sep = ""),
  paste("con =~ ", paste(filter(items, prsc_clin == "ConductProblems")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("ext =~ NA*hyp + agg + con", sep = ""),
  paste("ext ~~ 1*ext", sep = ""),
  # separate all by lines
  sep = "\n"
)

# PRS-C Adaptive Skills Model
prsc.adaptive.model <- paste(
  # subscales
  paste("adaptability =~ ", paste(filter(items, prsc_clin == "Adaptability")[[1]], collapse = "+"), sep = ""),
  paste("social =~ ", paste(filter(items, prsc_clin == "SocialSkills")[[1]], collapse = "+"), sep = ""),
  paste("functional =~ ", paste(filter(items, prsc_clin == "FunctionalCommunication")[[1]], collapse = "+"), sep = ""),
  paste("leadership =~ ", paste(filter(items, prsc_clin == "Leadership")[[1]], collapse = "+"), sep = ""),
  paste("activities =~ ", paste(filter(items, prsc_clin == "ActivitiesOfDailyLiving")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("adaptive =~ NA*adaptability + social + functional + leadership + activities", sep = ""),
  paste("adaptive ~~ 1*adaptive", sep = ""),
  # separate all by lines
  sep = "\n"
)

# PRS-C Behaviour Symptoms Index Model
prsc.bsi.model <- paste(
  # subscales
  # subscales
  paste("hyp =~ ", paste(filter(items, prsc_clin == "Hyperactivity")[[1]], collapse = "+"), sep = ""),
  paste("agg =~ ", paste(filter(items, prsc_clin == "Aggression")[[1]], collapse = "+"), sep = ""),
  paste("dep =~ ", paste(filter(items, prsc_clin == "Depression")[[1]], collapse = "+"), sep = ""),
  paste("att =~ ", paste(filter(items, prsc_clin == "AttentionProblems")[[1]], collapse = "+"), sep = ""),
  paste("aty =~ ", paste(filter(items, prsc_clin == "Atypicality")[[1]], collapse = "+"), sep = ""),
  paste("wit =~ ", paste(filter(items, prsc_clin == "Withdrawal")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("bsi =~ NA*hyp + agg + dep + att + aty + wit", sep = ""),
  paste("bsi ~~ 1*bsi", sep = ""),
  # separate all by lines
  sep = "\n"
)

# TRS-C Subscale Models
trsc.subscale.models <- list()
for (i in na.omit(unique(items$trsc_clin))) {
  trsc.subscale.models[[i]] <- paste0(i, " =~ ", paste0(filter(items, trsc_clin == i)[[1]], collapse = "+"))
}

# TRS-C Internalizing Model
trsc.internalizing.model <- paste(
  # subscales
  paste("anx =~ ", paste(filter(items, trsc_clin == "Anxiety")[[1]], collapse = "+"), sep = ""),
  paste("dep =~ ", paste(filter(items, trsc_clin == "Depression")[[1]], collapse = "+"), sep = ""),
  paste("som =~ ", paste(filter(items, trsc_clin == "Somatization")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("int =~ NA*anx + dep + som", sep = ""),
  paste("int ~~ 1*int", sep = ""),
  # separate all by lines
  sep = "\n"
)

# TRS-C Externalizing Model
trsc.externalizing.model <- paste(
  # subscales
  paste("hyp =~ ", paste(filter(items, trsc_clin == "Hyperactivity")[[1]], collapse = "+"), sep = ""),
  paste("agg =~ ", paste(filter(items, trsc_clin == "Aggression")[[1]], collapse = "+"), sep = ""),
  paste("con =~ ", paste(filter(items, trsc_clin == "ConductProblems")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("ext =~ NA*hyp + agg + con", sep = ""),
  paste("ext ~~ 1*ext", sep = ""),
  # separate all by lines
  sep = "\n"
)

# TRS-C Adaptive Skills Model
trsc.adaptive.model <- paste(
  # subscales
  paste("adaptability =~ ", paste(filter(items, trsc_clin == "Adaptability")[[1]], collapse = "+"), sep = ""),
  paste("social =~ ", paste(filter(items, trsc_clin == "SocialSkills")[[1]], collapse = "+"), sep = ""),
  paste("functional =~ ", paste(filter(items, trsc_clin == "FunctionalCommunication")[[1]], collapse = "+"), sep = ""),
  paste("leadership =~ ", paste(filter(items, trsc_clin == "Leadership")[[1]], collapse = "+"), sep = ""),
  paste("study =~ ", paste(filter(items, trsc_clin == "StudySkills")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("adaptive =~ NA*adaptability + social + functional + leadership + study", sep = ""),
  paste("adaptive ~~ 1*adaptive", sep = ""),
  # separate all by lines
  sep = "\n"
)

# TRS-C School Problems Model
trsc.school.probs.model <- paste(
  # subscales
  paste("learning =~ ", paste(filter(items, trsc_clin == "LearningProblems")[[1]], collapse = "+"), sep = ""),
  paste("attention =~ ", paste(filter(items, trsc_clin == "AttentionProblems")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("school =~ NA*learning + attention", sep = ""),
  paste("school ~~ 1*school", sep = ""),
  # separate all by lines
  sep = "\n"
)

# TRS-C School Problems Model Covary with Internalising
trsc.school.probs.int.model <- paste(
  # school problems subscales
  paste("learning =~ ", paste(filter(items, trsc_clin == "LearningProblems")[[1]], collapse = "+"), sep = ""),
  paste("attention =~ ", paste(filter(items, trsc_clin == "AttentionProblems")[[1]], collapse = "+"), sep = ""),
  # school problems composite
  paste("school =~ NA*learning + attention", sep = ""),
  paste("school ~~ 1*school", sep = ""),
  # int subscales
  paste("anx =~ ", paste(filter(items, trsc_clin == "Anxiety")[[1]], collapse = "+"), sep = ""),
  paste("dep =~ ", paste(filter(items, trsc_clin == "Depression")[[1]], collapse = "+"), sep = ""),
  paste("som =~ ", paste(filter(items, trsc_clin == "Somatization")[[1]], collapse = "+"), sep = ""),
  # int composite
  paste("int =~ NA*anx + dep + som", sep = ""),
  paste("int ~~ 1*int", sep = ""),
  # covary int and school problems
  paste("school ~~ int", sep = ""),
  # separate all by lines
  sep = "\n"
)

# TRS-C Behaviour Symptoms Index Model
trsc.bsi.model <- paste(
  # subscales
  paste("hyp =~ ", paste(filter(items, trsc_clin == "Hyperactivity")[[1]], collapse = "+"), sep = ""),
  paste("agg =~ ", paste(filter(items, trsc_clin == "Aggression")[[1]], collapse = "+"), sep = ""),
  paste("dep =~ ", paste(filter(items, trsc_clin == "Depression")[[1]], collapse = "+"), sep = ""),
  paste("att =~ ", paste(filter(items, trsc_clin == "AttentionProblems")[[1]], collapse = "+"), sep = ""),
  paste("aty =~ ", paste(filter(items, trsc_clin == "Atypicality")[[1]], collapse = "+"), sep = ""),
  paste("wit =~ ", paste(filter(items, trsc_clin == "Withdrawal")[[1]], collapse = "+"), sep = ""),
  # composite
  paste("bsi =~ NA*hyp + agg + dep + att + aty + wit", sep = ""),
  paste("bsi ~~ 1*bsi", sep = ""),
  # separate all by lines
  sep = "\n"
)

#---                                                      ---#
################# Reliability BASC-3 PRS-C  #################
#---                                                      ---#

prsc.alpha <- list()
for (i in names(prsc.subscale.items)) {
  prsc.alpha[[i]] <- psych::alpha(basc3prsc[prsc.subscale.items[[i]]], check.keys = TRUE)
}

lapply(prsc.alpha, function(x) { x$total })

#---                                                        ---#
################# CFA BASC-3 PRS-C Subscales  #################
#---                                                        ---#

prsc.cfa <- list()
for (i in names(prsc.subscale.items)) {
  y <- which(names(prsc.subscale.items) == i)
  prsc.cfa[[i]] <- cfa(model = prsc.subscale.models[[i]], 
                       data = basc3prsc[prsc.subscale.items[[i]]],
                       ordered = prsc.subscale.items[[i]])
  rm(y)
}

lapply(prsc.cfa, function(x) {x})

lapply(prsc.cfa, function(x) { 
  fitMeasures(x, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
})

lapply(prsc.cfa, function(x) { 
  parameterEstimates(x, standardized = TRUE) %>%
    filter(op == "=~" | op == "~~") %>%
    dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
    kable(digits = 3, format = "pandoc", caption = "Factor Loadings")
})

#---                                                            ---#
################# CFA BASC-3 PRS-C Internalizing  #################
#---                                                            ---#

prsc.internalizing.cfa <- cfa(model = prsc.internalizing.model, 
                              data = basc3prsc[prsc.composite.items$Internalizing], 
                              ordered = prsc.composite.items$Internalizing)

prsc.internalizing.cfa

fitMeasures(prsc.internalizing.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(prsc.internalizing.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                            ---#
################# CFA BASC-3 PRS-C Externalizing  #################
#---                                                            ---#

prsc.externalizing.cfa <- cfa(model = prsc.externalizing.model, 
                              data = basc3prsc[prsc.composite.items$Externalizing], 
                              ordered = prsc.composite.items$Externalizing)

prsc.externalizing.cfa

fitMeasures(prsc.externalizing.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(prsc.externalizing.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                            ---#
################# CFA BASC-3 PRS-C Adaptive Skills  #################
#---                                                            ---#

prsc.adaptive.cfa <- cfa(model = prsc.adaptive.model, 
                         data = basc3prsc[prsc.composite.items$AdaptiveSkills], 
                         ordered = prsc.composite.items$AdaptiveSkills)

prsc.adaptive.cfa

fitMeasures(prsc.adaptive.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(prsc.adaptive.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                ---#
################# CFA BASC-3 PRS-C BSI  #################
#---                                                ---#

prsc.bsi.cfa <- cfa(model = prsc.bsi.model, 
                    data = basc3prsc[prsc.bsi.items], 
                    ordered = prsc.bsi.items)

prsc.bsi.cfa

fitMeasures(prsc.bsi.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(prsc.bsi.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                      ---#
################# Reliability BASC-3 TRS-C  #################
#---                                                      ---#

trsc.alpha <- list()
for (i in names(trsc.subscale.items)) {
  trsc.alpha[[i]] <- psych::alpha(basc3trsc[trsc.subscale.items[[i]]], check.keys = TRUE)
}

lapply(trsc.alpha, function(x) { x$total })

#---                                                        ---#
################# CFA BASC-3 TRS-C Subscales  #################
#---                                                        ---#

trsc.cfa <- list()
for (i in names(trsc.subscale.items)) {
  y <- which(names(trsc.subscale.items) == i)
  trsc.cfa[[i]] <- cfa(model = trsc.subscale.models[[i]], 
                       data = basc3trsc[trsc.subscale.items[[i]]],
                       ordered = trsc.subscale.items[[i]])
  rm(y)
}

lapply(trsc.cfa, function(x) {x})

lapply(trsc.cfa, function(x) { 
  fitMeasures(x, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
})

lapply(trsc.cfa, function(x) { 
  parameterEstimates(x, standardized = TRUE) %>%
    filter(op == "=~" | op == "~~") %>%
    dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
    kable(digits = 3, format = "pandoc", caption = "Factor Loadings")
})

#---                                                            ---#
################# CFA BASC-3 TRS-C Internalizing  #################
#---                                                            ---#

trsc.internalizing.cfa <- cfa(model = trsc.internalizing.model, 
                              data = basc3trsc[trsc.composite.items$Internalizing], 
                              ordered = trsc.composite.items$Internalizing)

trsc.internalizing.cfa

fitMeasures(trsc.internalizing.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(trsc.internalizing.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                            ---#
################# CFA BASC-3 TRS-C Externalizing  #################
#---                                                            ---#

trsc.externalizing.cfa <- cfa(model = trsc.externalizing.model, 
                              data = basc3trsc[trsc.composite.items$Externalizing], 
                              ordered = trsc.composite.items$Externalizing)

trsc.externalizing.cfa

fitMeasures(trsc.externalizing.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(trsc.externalizing.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                            ---#
################# CFA BASC-3 TRS-C School Problems  #################
#---                                                            ---#

trsc.school.probs.cfa <- cfa(model = trsc.school.probs.model, 
                             data = basc3trsc[trsc.composite.items$SchoolProblems], 
                             ordered = trsc.composite.items$SchoolProblems)

trsc.school.probs.cfa

fitMeasures(trsc.school.probs.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(trsc.school.probs.cfa, standardized = TRUE) %>%
  filter(op == "=~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

trsc.school.probs.int.cfa <- cfa(model = trsc.school.probs.int.model, 
                                 data = basc3trsc[c(trsc.composite.items$SchoolProblems,trsc.composite.items$Internalizing)], 
                                 ordered = c(trsc.composite.items$SchoolProblems,trsc.composite.items$Internalizing))

trsc.school.probs.int.cfa

fitMeasures(trsc.school.probs.int.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(trsc.school.probs.int.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                            ---#
################# CFA BASC-3 TRS-C Adaptive Skills  #################
#---                                                            ---#

trsc.adaptive.cfa <- cfa(model = trsc.adaptive.model, 
                         data = basc3trsc[trsc.composite.items$AdaptiveSkills], 
                         ordered = trsc.composite.items$AdaptiveSkills)

trsc.adaptive.cfa

fitMeasures(trsc.adaptive.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(trsc.adaptive.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")

#---                                                ---#
################# CFA BASC-3 TRS-C BSI  #################
#---                                                ---#

trsc.bsi.cfa <- cfa(model = trsc.bsi.model, 
                    data = basc3trsc[trsc.bsi.items], 
                    ordered = trsc.bsi.items)

trsc.bsi.cfa

fitMeasures(trsc.bsi.cfa, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

parameterEstimates(trsc.bsi.cfa, standardized = TRUE) %>%
  filter(op == "=~" | op == "~~") %>%
  dplyr::select("LHS" = lhs, op, "RHS" = rhs, "SE" = se, "Z" = z, "p-value" = pvalue, "Beta" = est, "Std. Beta" = std.all) %>%
  kable(digits = 3, format = "pandoc", caption = "Factor Loadings")
