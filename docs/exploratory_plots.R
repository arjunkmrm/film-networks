#script for exploratory plots of dataset
#remember to set directory to 'data_function scripts' folder

library(tidyverse)

#plot movie plots per year
load("year_plots.final.RData")
nplots <- as.data.frame(year_plots.final)
new.nplots <- data.frame(year = as.numeric(year_plots.final$year), times = as.numeric(year_plots.final$times))
ggplot(new.nplots, aes(x = year, y = times)) +
  geom_point(group = 1, size = 1.2) +
  geom_line(group = 1, size = 0.8) + theme_minimal() + xlab("Decade") + ylab("Number of Plots") + geom_smooth(se = FALSE, method = "lm", alpha = 0.2) + 
  theme(axis.text = element_text(color = "black", size = 14), axis.title = element_text(color = "black", size = 15.5),
        legend.text = element_text(color = "black", size = 14), legend.title = element_text(color = "black", size = 15.5),
        panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3)) 
  ggsave("n_plots.png")
  
#significance statistics
nplots.model <- lm(times ~ year, data = new.nplots)
nplots.model <- aov(times ~ year, data = new.nplots)
summary(nplots.model)

#plot sentences per movie across decades
load("token.all.RData")
#obtain data using for loop from tokens
spm <- data.frame(n_sents <- as.numeric(), year <- as.integer())
for(i in seq(from = 1940, to = 2010, by = 10)){
spm.temp <- data.frame(n_sents = tokens_subset(token.all, decade == i) %>% ndoc(), year = as.integer(i))
spm <- rbind(spm.temp, spm)
}
spm$spm <- spm$n_sents/year_plots.final$times
#plot
ggplot(spm, aes(x = year, y = spm)) +
  geom_point(size = 1.2) +
  geom_line(size = 0.8) + theme_minimal() + geom_smooth(se = FALSE, method = "lm", alpha = 0.2) + 
  coord_cartesian(xlim = c(1940, 2010), ylim = c(10, 50)) + xlab("Decade") + ylab("Sentences per Plot") +
  theme(axis.text = element_text(color = "black", size = 14), axis.title = element_text(color = "black", size = 15.5),
        legend.text = element_text(color = "black", size = 14), legend.title = element_text(color = "black", size = 15.5),
        panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
ggsave("spm.png")
#significance stats
spm.model <- lm(spm ~ year, data = spm)
spm.model <- aov(spm ~ year, data = spm)
summary(spm.model)


#plot words per sentence across decades
wps <- data.frame(n <- as.numeric(), year <- as.integer())
for(i in seq(from = 1940, to = 2010, by = 10)){
  wps.temp <- data.frame(n = sum(tokens_subset(token.all, decade == i) %>% ntoken()), year = as.integer(i))
  wps <- rbind(wps.temp, wps)
}
wps$wps <- wps$n/spm$n_sents
#plot
ggplot(wps, aes(x = year, y = wps)) +
  geom_point(size = 1.2) +
  geom_line(size = 0.8) + theme_minimal() + geom_smooth(se = FALSE, method = "lm", alpha = 0.2) + 
  coord_cartesian(xlim = c(1940, 2010), ylim = c(5, 15)) + xlab("Decade") + ylab("Words per Sentence") +
  theme(axis.text = element_text(color = "black", size = 14), axis.title = element_text(color = "black", size = 15.5),
        legend.text = element_text(color = "black", size = 14), legend.title = element_text(color = "black", size = 15.5),
        panel.grid.major = element_line(colour = "grey50", size = 0.3), panel.grid.minor = element_line(colour = "grey50", size = 0.3))
ggsave("wps.png")
#significance stats
wps.model <- lm(wps ~ year, data = wps)
wps.model <- aov(wps ~ year, data = wps)
summary(wps.model)
