# R script to produce all exploratory results of the pre-registered paper
# "The effects of subtitles on learning from online education videos in a
# second language" by Tim van der Zee et al.
# Email: t.van.der.zee@iclon.leidenuniv.nl
# Twitter: @Research_Tim
# 
# Excluding the first section, each section starts with computations,
# the last few lines of code of each section are to give the output.



#####
##### DEPENDENCIES
#####

if (!require(BayesFactor)) {
  install.packages("BayesFactor")
}
require(BayesFactor)

CSL.l <- read.table("JMS - Complexity and Subtitles - Long.csv", 
                    header=TRUE,
                    colClasses=c('factor', 'numeric', 'factor', 'numeric',
                                 'numeric', 'factor', 'factor', 'numeric'),
                    sep=",")

CSL.w <- read.table("JMS - Complexity and Subtitles - Wide.csv", 
                    header=TRUE,
                    colClasses=c('factor', 'numeric', 'factor', 'numeric',
                                 'numeric', 'numeric', 'numeric', 'numeric',
                                 'numeric', 'numeric', 'numeric', 'numeric'),
                    sep=",")

#####
##### DESCRIPTIVE RESULTS
##### These corrospond to Table 4 and 5 in the paper
#####

table4 <- data.frame("Condition" = c("Complex, Subs", "Complex, No Subs",
                                     "Simple, Subs", "Simple, No Subs"),
                     "Test Mean" = c(mean(CSL.w$cs_score),
                                     mean(CSL.w$cns_score),
                                     mean(CSL.w$ss_score),
                                     mean(CSL.w$sns_score)),
                     "Test SD" = c(sd(CSL.w$cs_score),
                                   sd(CSL.w$cns_score),
                                   sd(CSL.w$ss_score),
                                   sd(CSL.w$sns_score)),
                     "ME Mean" = c(mean(CSL.w$cs_me),
                                   mean(CSL.w$cns_me),
                                   mean(CSL.w$ss_me),
                                   mean(CSL.w$sns_me)),
                     "ME SD" = c(sd(CSL.w$cs_me),
                                 sd(CSL.w$cns_me),
                                 sd(CSL.w$ss_me),
                                 sd(CSL.w$sns_me)))

table5 <- data.frame("Measure" = c("Test diff", "Test diff sd",
                                   "ME diff", "ME diff sd"),
                     "CS-CNS" = c(mean(CSL.w$cs_score - CSL.w$cns_score),
                                  sd(CSL.w$cs_score - CSL.w$cns_score),
                                  mean(CSL.w$cs_me - CSL.w$cns_me),
                                  sd(CSL.w$cs_me - CSL.w$cns_me)),
                     "CS-SS" = c(mean(CSL.w$cs_score - CSL.w$ss_score),
                                 sd(CSL.w$cs_score - CSL.w$ss_score),
                                 mean(CSL.w$cs_me - CSL.w$ss_me),
                                 sd(CSL.w$cs_me - CSL.w$ss_me)),
                     "CS-SNS" = c(mean(CSL.w$cs_score - CSL.w$sns_score),
                                  sd(CSL.w$cs_score - CSL.w$sns_score),
                                  mean(CSL.w$cs_me - CSL.w$sns_me),
                                  sd(CSL.w$cs_me - CSL.w$sns_me)),
                     "CNS-SS" = c(mean(CSL.w$cns_score - CSL.w$ss_score),
                                  sd(CSL.w$cns_score - CSL.w$ss_score),
                                  mean(CSL.w$cns_me - CSL.w$ss_me),
                                  sd(CSL.w$cns_me - CSL.w$ss_me)),
                     "CNS-SNS" = c(mean(CSL.w$cns_score - CSL.w$sns_score),
                                   sd(CSL.w$cns_score - CSL.w$sns_score),
                                   mean(CSL.w$cns_me - CSL.w$sns_me),
                                   sd(CSL.w$cns_me - CSL.w$sns_me)),
                     "SS-SNS" = c(mean(CSL.w$ss_score - CSL.w$sns_score),
                                  sd(CSL.w$ss_score - CSL.w$sns_score),
                                  mean(CSL.w$ss_me - CSL.w$sns_me),
                                  sd(CSL.w$ss_me - CSL.w$sns_me)))

# Show Table 4 and Table 5

table4
table5

#####
##### EXPLORATORY ANALYSIS: POSTERIOR DISTRIBUTION OF PARAMATERS.
##### WARNING: Takes some time, set iterations to 1e04 or 05 to speed it up.
##### The reported results in the publication were based on 1e06.
##### NOTE: Parameters are estimated using *all* available data, not only the 
##### overall best model (Complexity + Language). 
#####


# How to remove ID from posterior?

posterior.score <- lmBF(score ~ compl + sub + lang
                        + compl:sub
                        + compl:lang
                        + sub:lang
                        + compl:sub:lang, 
                        data = CSL.l, 
                        #whichRandom = "id",
                        posterior = TRUE,
                        iterations = 1e04)
posterior.me <- lmBF(score ~ compl + sub + lang
                     + compl:sub
                     + compl:lang
                     + sub:lang
                     + compl:sub:lang, 
                     data = CSL.l, 
                     #whichRandom = "id",
                     posterior = TRUE,
                     iterations = 1e04)

write.csv(posterior.score, file = "posterioscore.csv", row.names = FALSE)

#####
##### OUTPUT.
##### Note: As the paremeters are estimated by sampling from the conjoint
##### posterior distribution, results may vary slightly every time.
##### 

summary(posterior.score)
summary(posterior.me)
# plot(posterior.score)
# plot(posterior.me)

#####
##### CHAIN DIAGNOSTICS. 
#####

plot(posterior.score[1:1000, "mu"], type = "l")                 # Good
plot(chains[1:10000, "compl"], type = "l")              # Good 
plot(chains[1:10000, "sub"], type = "l")                # Good
plot(chains[1:10000, "lang"], type = "l")               # Good
plot(chains[1:10000, "compl.&.sub"], type = "l")        # Good, slight wandering
plot(chains[1:10000, "compl.&.lang"], type = "l")       # Good, slight wandering
plot(chains[1:10000, "sub.&.lang"], type = "l")         # Good
plot(chains[1:10000, "compl.&.sub.&.lang"], type = "l") # Good
plot(chains[1:10000, "sig2"], type = "l")               # Good, some wandering
#plot(chains[1:1000, "g_id"], type = "l")               # Mediocre
#plot(chains[1:1000, "g_continuous"], type = "l")       # 1-sided ???

acf(chains[, "mu"])                 # Good
acf(chains[, "compl"])              # Good
acf(chains[, "sub"])                # Good
acf(chains[, "lang"])               # Good
acf(chains[, "compl.&.sub"])        # Good,very slight auto-correlation at start
acf(chains[, "compl.&.lang"])       # Good
acf(chains[, "sub.&.lang"])         # Good
acf(chains[, "compl.&.sub.&.lang"]) # Good
acf(chains[, "sig2"])               # Good, slight auto-correlation at start
#acf(chains[, "g_id"])              # Good, some auto-correlation at start
#acf(chains[, "g_continuous"])      # Good

require(coda)

effectiveSize(chains[,11])
plot.mcmc(chains) #doesnt work
autocorr.diag(chains)

?rjags
