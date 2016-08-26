# R script to produce all descriptive and exploratory results of the
# pre-registered paper: "The effects of subtitles on learning from online
# education videos in a second language" by Tim van der Zee et al.
#
# Email: t.van.der.zee@iclon.leidenuniv.nl Twitter: @Research_Tim
#
# Excluding the first section, each section starts with computations, the last
# few lines of code of each section give the output.
#
# Run section by section.



#####
##### DEPENDENCIES
#####

if (!require(BayesFactor)) {
  install.packages("BayesFactor")
}

CSL.l <- read.table(
  "JMS - Complexity and Subtitles - Long.csv",
  header = TRUE,
  colClasses = c(
    'factor',
    'numeric',
    'factor',
    'numeric',
    'numeric',
    'factor',
    'factor',
    'numeric'
  ),
  sep = ","
)

CSL.w <- read.table(
  "JMS - Complexity and Subtitles - Wide.csv",
  header = TRUE,
  colClasses = c(
    'factor',
    'numeric',
    'factor',
    'numeric',
    'numeric',
    'numeric',
    'numeric',
    'numeric',
    'numeric',
    'numeric',
    'numeric',
    'numeric'
  ),
  sep = ","
)

#####
##### DESCRIPTIVE RESULTS
##### These corrospond to Table 3-5 in the paper
#####

table3 <-
  data.frame(
    "Gender" =
      c(
        paste(sum(CSL.w$sex == 1),
              " Male (",
              round(sum(CSL.w$sex == 1) /
                      (
                        sum(CSL.w$sex == 1)
                        + sum(CSL.w$sex == 2)
                      ) * 100, 2), "%)"),
        paste(sum(CSL.w$sex == 2),
              " Female (",
              round(sum(CSL.w$sex == 2) /
                      (
                        sum(CSL.w$sex == 1)
                        + sum(CSL.w$sex == 2)
                      ) * 100, 2), "%)"),
        "",
        "",
        ""
      ),
    "Age" =
      c(
        paste("Min", min(CSL.w$age, na.rm = TRUE), "yr"),
        paste("Max", max(CSL.w$age, na.rm = TRUE), "yr"),
        paste("Mean", round(mean(CSL.w$age, na.rm = TRUE), 2), "yr"),
        paste("Sd", round(sd(CSL.w$age, na.rm = TRUE), 2), "yr"),
        ""
      ),
    "Lang" =
      c(
        paste("1:", sum(CSL.w$lang == 1), "(", round(
          sum(CSL.w$lang == 1) / length(CSL.w$lang) * 100, 2
        ), "%)"),
        paste("2:", sum(CSL.w$lang == 2), "(", round(
          sum(CSL.w$lang == 2) / length(CSL.w$lang) * 100, 2
        ), "%)"),
        paste("3:", sum(CSL.w$lang == 3), "(", round(
          sum(CSL.w$lang == 3) / length(CSL.w$lang) * 100, 2
        ), "%)"),
        paste("4:", sum(CSL.w$lang == 4), "(", round(
          sum(CSL.w$lang == 4) / length(CSL.w$lang) * 100, 2
        ), "%)"),
        paste("5:", sum(CSL.w$lang == 5), "(", round(
          sum(CSL.w$lang == 5) / length(CSL.w$lang) * 100, 2
        ), "%)")
      )
  )



table4 <-
  data.frame(
    "Condition" = c(
      "Complex, Subs",
      "Complex, No Subs",
      "Simple, Subs",
      "Simple, No Subs"
    ),
    "Test Mean" = c(
      mean(CSL.w$cs_score),
      mean(CSL.w$cns_score),
      mean(CSL.w$ss_score),
      mean(CSL.w$sns_score)
    ),
    "Test SD" = c(
      sd(CSL.w$cs_score),
      sd(CSL.w$cns_score),
      sd(CSL.w$ss_score),
      sd(CSL.w$sns_score)
    ),
    "ME Mean" = c(
      mean(CSL.w$cs_me),
      mean(CSL.w$cns_me),
      mean(CSL.w$ss_me),
      mean(CSL.w$sns_me)
    ),
    "ME SD" = c(
      sd(CSL.w$cs_me),
      sd(CSL.w$cns_me),
      sd(CSL.w$ss_me),
      sd(CSL.w$sns_me)
    )
  )

table5 <- data.frame(
  "Measure" = c("Test diff", "Test diff sd",
                "ME diff", "ME diff sd"),
  "CS-CNS" = c(
    mean(CSL.w$cs_score - CSL.w$cns_score),
    sd(CSL.w$cs_score - CSL.w$cns_score),
    mean(CSL.w$cs_me - CSL.w$cns_me),
    sd(CSL.w$cs_me - CSL.w$cns_me)
  ),
  "CS-SS" = c(
    mean(CSL.w$cs_score - CSL.w$ss_score),
    sd(CSL.w$cs_score - CSL.w$ss_score),
    mean(CSL.w$cs_me - CSL.w$ss_me),
    sd(CSL.w$cs_me - CSL.w$ss_me)
  ),
  "CS-SNS" = c(
    mean(CSL.w$cs_score - CSL.w$sns_score),
    sd(CSL.w$cs_score - CSL.w$sns_score),
    mean(CSL.w$cs_me - CSL.w$sns_me),
    sd(CSL.w$cs_me - CSL.w$sns_me)
  ),
  "CNS-SS" = c(
    mean(CSL.w$cns_score - CSL.w$ss_score),
    sd(CSL.w$cns_score - CSL.w$ss_score),
    mean(CSL.w$cns_me - CSL.w$ss_me),
    sd(CSL.w$cns_me - CSL.w$ss_me)
  ),
  "CNS-SNS" = c(
    mean(CSL.w$cns_score - CSL.w$sns_score),
    sd(CSL.w$cns_score - CSL.w$sns_score),
    mean(CSL.w$cns_me - CSL.w$sns_me),
    sd(CSL.w$cns_me - CSL.w$sns_me)
  ),
  "SS-SNS" = c(
    mean(CSL.w$ss_score - CSL.w$sns_score),
    sd(CSL.w$ss_score - CSL.w$sns_score),
    mean(CSL.w$ss_me - CSL.w$sns_me),
    sd(CSL.w$ss_me - CSL.w$sns_me)
  )
)

# Show Table 3 through 5

table3
table4
table5

#####
##### EXPLORATORY ANALYSIS: POSTERIOR DISTRIBUTION OF PARAMATERS.
##### WARNING: Takes some time, set iterations to 1e04 or 5 to speed it up.
##### The reported results in the publication were based on 1e06.
##### NOTE: Parameters are estimated using *all* available data, not only the
##### overall best model (Complexity + Language).
##### NOTE 2: Cauchy priors are set to scaling factor of 1/2
#####

posterior.score <- lmBF(
  score ~ compl + sub + lang + compl:sub + compl:lang + sub:lang
  + compl:sub:lang,
  data = CSL.l,
  rscaleFixed = 1 / 2,
  rscaleCont = 1 / 2,
  posterior = TRUE,
  iterations = 1e06
)

posterior.me <- lmBF(
  me ~ compl + sub + lang + compl:sub + compl:lang + sub:lang + compl:sub:lang,
  data = CSL.l,
  rscaleFixed = 1 / 2,
  rscaleCont = 1 / 2,
  posterior = TRUE,
  iterations = 1e06
)

write.csv(posterior.score, file = "posterior_score.csv", row.names = FALSE)
write.csv(posterior.me, file = "posterior_me.csv", row.names = FALSE)

# Output

summary(posterior.score)
summary(posterior.me)
plot(posterior.score[,5:6])
plot(posterior.me[,5:6])

#####
##### CHAIN DIAGNOSTICS.
#####

plot(posterior.score[1:750, "mu"], type = "l")                 # Good
plot(posterior.score[1:750, "compl-1"], type = "l")              # Good
plot(posterior.score[1:750, "sub-1"], type = "l")                # Good
plot(posterior.score[1:750, "lang-lang"], type = "l")               # Good
#plot(posterior.score[1:750, "compl.&.sub"], type = "l")        # Good, slight wandering
#plot(posterior.score[1:750, "compl.&.lang"], type = "l")       # Good, slight wandering
#plot(posterior.score[1:750, "sub.&.lang"], type = "l")         # Good
#plot(posterior.score[1:750, "compl.&.sub.&.lang"], type = "l") # Good
#plot(posterior.score[1:750, "sig2"], type = "l")               # Good, some wandering
#plot(posterior.score[1:1000, "g_id"], type = "l")               # Mediocre
#plot(posterior.score[1:1000, "g_continuous"], type = "l")       # 1-sided ???

acf(posterior.score[, "mu"])                 # Good
acf(posterior.score[, "compl-1"])              # Good
acf(posterior.score[, "sub-1"])                # Good
acf(posterior.score[, "lang-lang"])               # Good
#acf(posterior.score[, "compl.&.sub"])        # Good,very slight auto-correlation at start
#acf(posterior.score[, "compl.&.lang"])       # Good
#acf(posterior.score[, "sub.&.lang"])         # Good
#acf(posterior.score[, "compl.&.sub.&.lang"]) # Good
#acf(posterior.score[, "sig2"])               # Good, slight auto-correlation at start
#acf(posterior.score[, "g_id"])              # Good, some auto-correlation at start
#acf(posterior.score[, "g_continuous"])      # Good

require(coda)

effectiveSize(chains[, 11])
plot.mcmc(chains) #doesnt work
autocorr.diag(chains)

?rjags
