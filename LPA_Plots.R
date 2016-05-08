library(ggplot2)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(lattice)
library(grid)

install.packages("ggrepel")

setwd("./Documents/Data/Latent_Profile_Analysis")

setwd("C:/Users/kom741/Documents/Data/Latent_Profile_Analysis")

LPA_full <- read.csv("LPA_FINAL_12182015_VARS_ADDED.csv")
str(LPA_full)

LPA_full.df <- data.frame(LPA_full)
LPA_full.df$PROFILE_FINAL_CORRECT <- as.factor(LPA_full.df$PROFILE_FINAL_CORRECT)
LPA_full.df$PROFILE_2 <- as.factor(LPA_full.df$PROFILE_2)

LPA_full.df_No1 <- subset(LPA_full.df, 
                          PROFILE_FINAL_CORRECT == 2.1 | 
                          PROFILE_FINAL_CORRECT == 2.2 | 
                          PROFILE_FINAL_CORRECT == 2.3 | 
                          PROFILE_FINAL_CORRECT == 3
                          )

LPA_full.df$Profile1_Bin[LPA_full.df$PROFILE_FINAL_CORRECT=="1"] <- "1"
LPA_full.df$Profile1_Bin[LPA_full.df$PROFILE_FINAL_CORRECT!="1"] <- "2"

plot(LPA_full.df$TST_X, LPA_full.df$EFFCY_X, col=LPA_full.df$PROFILE_FINAL_CORRECT)

boxplot(TST_X~PROFILE_FINAL_CORRECT, data=LPA_full.df)
boxplot(EFFCY_X~PROFILE_FINAL_CORRECT, data=LPA_full.df)
boxplot(mvpa_troi_dy_mean~PROFILE_FINAL_CORRECT, data=LPA_full.df)
boxplot(percent_sed_mean~PROFILE_FINAL_CORRECT, data=LPA_full.df)

table(LPA_full.df$PROFILE_FINAL_CORRECT)

###First, a scatter plot showing how we cleaved off the poor sleeping group

g <- ggplot(LPA_full.df, aes(x = EFFCY_X, 
                             y = TST_X, 
                             group = Profile1_Bin, 
                             color = factor(Profile1_Bin, 
                                            labels = c("Fairly Active,\nInadequate Sleepers", 
                                                       "All Other Profiles")
                                            )
                             )
            )

scatter_sleep <- 
  (g +
  geom_point(size = 3.5, alpha = 3/4) +
  scale_color_brewer(palette="Set1") +
  stat_ellipse(aes(y=TST_X, 
                   x=EFFCY_X, 
                   fill=factor(Profile1_Bin)
                   ), 
               geom="polygon", level=0.95, alpha=0.2)
  )

scatter_sleep2 <- 
  (g +
     geom_point(size = 5.5, alpha = 3/4) +
     scale_color_brewer(palette="Set1") +
     labs(y = "Nightly Sleep Time (min)", 
          x = "Sleep Efficiency") +
     theme(legend.title = element_blank(), 
           axis.title.y=element_text(size = 18, margin = margin(0,15,0,0)), 
           axis.text.y=element_text(size = 18), 
           axis.text.x=element_text(size = 18), 
           axis.title.x=element_text(size = 18, margin = margin(15,0,0,0)),
           legend.position="right", 
           legend.title=element_blank(),
           legend.text=element_text(size=10),
           legend.key.size = unit(1.5, "lines")
           )
  )


print(scatter_sleep2)

ggsave(scatter_sleep2, file="/Users/pettyferrari/Google Drive/Papers/LPA Paper/Manuscript/Plots/Fig2_Scatter_Sleep.pdf", dpi=400)
ggsave(scatter_sleep2, file="C:/Users/kom741/Google Drive/Papers/LPA Paper/Manuscript/Plots/Fig2_Scatter_Sleep.pdf", dpi = 400)

table(LPA_full.df$PROFILE_2)


### Let's look at how all the groups except #1 compare across MVPA and SED

g_PA <- ggplot(LPA_full.df_No1, aes(y = mvpa_troi_dy_mean, 
                                  x = percent_sed_mean, 
                                  group = PROFILE_FINAL_CORRECT, 
                                  color = factor(PROFILE_FINAL_CORRECT,
                                                 labels = c("Inactive,\nAverage Sleepers",
                                                            "Fairly Active,\nAverage Sleepers",
                                                            "Active,\nAverage Sleepers",
                                                            "Very Active,\nAverage Sleepers & Low Sitters"
                                                            )
                                                 )
                                    )
               )

scatter_PA <- 
  (g_PA +
     geom_point(size = 5.5, alpha = 0.85) +
     scale_color_brewer(palette="Set1")
  )

scatter_PA2 <- 
  (g_PA +
     geom_point(size = 5.5, alpha = 3/4) +
     scale_color_manual(values=alpha(c("#ff7f00", 
                                       "#984ea3", 
                                       "#4daf4a", 
                                       "#377eb8"
                                       ), 1)) +
     scale_x_continuous(labels=percent) +
     guides(fill = guide_legend(reverse = TRUE)) +
     labs(x = "Percent Wear-Time Sedentary", y = "MVPA per day (min)") +
     theme(axis.title.y=element_text(size = 18, margin = margin(0,15,0,0)), 
           axis.text.y=element_text(size = 18), 
           axis.text.x=element_text(size = 18), 
           axis.title.x=element_text(size = 18, margin = margin(15,0,0,0)),
           legend.position="right", 
           legend.title=element_blank(),
           legend.text=element_text(size=10),
           legend.key.size = unit(2, "lines")
           )
  )
           

print(scatter_PA2)

ggsave(scatter_PA2, file="C:/Users/kom741/Google Drive/Papers/LPA Paper/Manuscript/Plots/Fig3_Scatter_PA.jpg", dpi = 400)
ggsave(scatter_PA2, file="/Users/pettyferrari/Google Drive/Papers/LPA Paper/Manuscript/Plots/Fig3_Scatter_PA.pdf", dpi=400)

### let's re-do that PA/SED scatter but with all profiles. Is it too busy?

g_PAfull <- ggplot(LPA_full.df, aes(y = mvpa_troi_dy_mean, 
                                    x = percent_sed_mean, 
                                    group = PROFILE_FINAL_CORRECT, 
                                    color = factor(PROFILE_FINAL_CORRECT,
                                                   labels = c("Fairly Active,\nInadequate Sleepers",
                                                              "Inactive,\nAverage Sleepers",
                                                              "Fairly Active,\nAverage Sleepers",
                                                              "Active,\nAverage Sleepers",
                                                              "Very Active,\nAverage Sleepers & Low Sitters"
                                                              )
                                                  )
                                    )
              )


scatter_PAfull <- 
  (g_PAfull +
     geom_point(size = 5.5, alpha = 3/4) +
     scale_color_manual(values=alpha(c("#e41a1c",
                                       "#ff7f00", 
                                       "#984ea3", 
                                       "#4daf4a", 
                                       "#999999"
                                        ),
                                     1)
                        ) +
     scale_x_continuous(labels=percent) +
     guides(fill = guide_legend(reverse = TRUE)) +
     labs(x = "Percent Wear-Time Sedentary", y = "MVPA per day (min)") +
     theme(axis.title.y=element_text(size = 18, margin = margin(0,15,0,0)), 
           axis.text.y=element_text(size = 18), 
           axis.text.x=element_text(size = 18), 
           axis.title.x=element_text(size = 18, margin = margin(15,0,0,0)),
           legend.position="right", 
           legend.title=element_blank(),
           legend.text=element_text(size=10),
           legend.key.size = unit(2, "lines")
     )
  )

print(scatter_PAfull)
ggsave(scatter_PAfull, file="C:/Users/kom741/Google Drive/Papers/LPA Paper/Manuscript/Plots/Fig3_Scatter_PA_AllProfiles.pdf", dpi = 400)


###now let's look at how group 2 is split up

group2 <- ggplot(LPA_full.df, aes(x = mvpa_troi_dy_mean, 
                             y = percent_sed_mean, 
                             group = PROFILE_2, 
                             color = PROFILE_2))

scatter_PAgroup2 <- 
  (group2 +
     geom_point(alpha = 3/4) +
     scale_color_brewer(palette="Dark2")
  )

print(scatter_PAgroup2)



### Read in the z-score data

ZScores_Long <- read.csv("ZScores_Long.csv")
ZScores_Long.df <- data.frame(ZScores_Long)


### Create a bar graph of the z-scores for profile 1

ZScores_Long_P1.df <- subset(ZScores_Long.df, Profile == "Inadequate Sleepers")

profile_names <- c('1Inadequate Sleepers' = "Fairly Active, Inadequate Sleepers",
                   '2Inactive Sleepers' = "Inactive, Fairly Active Sleepers",
                   '3Fairly Active Sleepers' = "Fairly Active, Average Sleepers",
                   '4Active Sleepers' = "Active, Average Sleepers",
                   '5Very Active Sleepers & Low Sitters' = "Very Active, Average Sleepers & Low Sitters"
                   )

b1 <- ggplot(ZScores_Long.df, aes(x = Variable, y = Zscore, fill = Variable))

profile_barplot <- (b1 +
                     geom_bar(stat = "identity") + 
                     scale_color_brewer(palette="Dark2") +
                     aes(fill = factor(Variable), position="dodge") +
                     expand_limits(y = c(-2.75, 2.5)) +
                     labs(y = "Z-Score", title = "Behavioral Z-Scores by Profile") +
                     theme(axis.title.y=element_text(size = 18, margin = margin(0,15,0,0)), 
                            axis.text.y=element_text(size = 12), 
                            axis.text.x=element_blank(), 
                            axis.title.x=element_blank(),
                            legend.position="right", 
                            legend.title=element_blank(),
                            legend.text=element_text(size=10),
                            legend.key.size = unit(2, "lines")
                           ) +
                     facet_wrap(~Profile, nrow = 5, labeller = as_labeller(profile_names)) +
                     scale_fill_discrete(breaks = c("1TST",
                                                    "2EFFCY",
                                                    "3MVPA",
                                                    "4Perc_Sed"
                                                    ),
                                          labels = c("Total Sleep Time", 
                                                     "Sleep Efficiency", 
                                                     "MVPA", 
                                                     "Percent Wear-Time\nSedentary"
                                                     )
                                         )
                    )
                    
print(profile_barplot)
ggsave(profile_barplot, file="/Users/pettyferrari/Google Drive/Papers/LPA Paper/Manuscript/Plots/Fig1_ZScores.pdf", width = 6, height = 8, units = "in", dpi=400)

### barcharts of the means

means <- read.csv("Means.csv")
means.df <- data.frame(means)

### TST mean barplot

TST <- ggplot(means.df, aes(x = Profile, y = Nightly.Sleep.Time, fill = Profile))

TST_meanbarplot <- (TST +
                         geom_bar(stat = "identity")
                         )
print(TST_meanbarplot)

### Sleep Eff

EFF <- ggplot(means.df, aes(x = Profile, y = Sleep.Efficiency, fill = Profile))

EFF_meanbarplot <- (EFF +
                    geom_bar(stat = "identity")
                    )

print(EFF_meanbarplot)

### MVPA

MVPA <- ggplot(means.df, aes(x = Profile, y = MVPA, fill = Profile))

MVPA_meanbarplot <- (MVPA +
                      geom_bar(stat = "identity")
)

print(MVPA_meanbarplot)

### SED

SED <- ggplot(means.df, aes(x = Profile, y = Percent.Sedentary, fill = Profile))

SED_meanbarplot <- (SED +
                       geom_bar(stat = "identity")
                    )

print(SED_meanbarplot)





