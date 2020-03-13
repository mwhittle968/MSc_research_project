#  Winter swarming of Gymnometriocnemus brunalis and other Chironomidae in North Somerset, UK  #
#  Mathilda Whittle, School of Biological Sciences, University of Reading  #

# Install packages

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("egg")
library(egg)
install.packages("car")
library(car)

# Import data (09/03/2020)

R_data <- read.csv(file.choose(), fileEncoding = 'UTF-8-BOM')
R_data2 <- read.csv(file.choose(), fileEncoding = 'UTF-8-BOM')

R_data[R_data == ""] <- NA
R_data2[R_data2 == ""] <- NA

# Subset data for G. brumalis

g_brumalis <- filter(R_data, Species == "Gymnometriocneumus brumalis")

g_brumalis_2 <- select(R_data2, - E_ephemerae, -M_subnitens) %>%
  filter(G_brumalis != "") %>%
  rename(Swarming = G_brumalis)

# 1. Plots for swarming habitat, size, height of G. brumalis

habitat_plot <- ggplot(aes(x = Habitat), data = g_brumalis)+
  geom_bar(stat = "count", width = 0.28, fill = "black")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_text(family = "serif", size = 18, colour = "black"),
        axis.text.x = element_text(family = "serif", size = 18, colour = "black", angle = 45, vjust = 0.9, hjust = 0.9),
        axis.title.y = element_text(family = "serif", size = 20),
        axis.title.x = element_text(family = "serif", size = 20, margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.line = element_line(),
        aspect.ratio = 1.3)+
  scale_y_continuous("No. of swarms", limits = c(0,16), breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16))+
  scale_x_discrete("Habitat",
                   limits = c("shrub", "long grass"))

size_plot <- ggplot(aes(x = Size), data = g_brumalis)+
  geom_bar(stat = "count", width = 0.41, fill = "black")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "serif", size = 18, colour = "black", angle = 45, vjust = 0.9, hjust = 0.9),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif", size = 20, margin = margin(t = 15.5, r = 0, b = 0, l = 0)),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        aspect.ratio = 1.3)+
  scale_y_continuous("No. of swarms", limits = c(0,16), breaks = c(0, 4, 8, 12, 16))+
  scale_x_discrete("Size", limits = c("small", "medium", "large"))

height <- select(g_brumalis, Height) %>%
  group_by(Height) %>%
  summarise(Count = sum(Height == Height)) %>%
  add_row(Height = "1.5 - 2.0", Count = 0, .before = 4) %>%
  add_row(Height = "2.0 - 2.5", Count = 0, .before = 5)

height_plot <- ggplot(aes(x = Height, y = Count), data = height)+
  geom_bar(stat = "identity", width = 0.79, fill = "black")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "serif", size = 18, colour = "black", angle = 45, vjust = 0.9, hjust = 0.9),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "serif", size = 20, margin = margin(t = 14, r = 0, b = 0, l = 0)),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(),
        aspect.ratio = 1.3)+
  scale_y_continuous("No. of swarms", limits = c(0,16), breaks = c(0, 4, 8, 12, 16))+
  scale_x_discrete("Height (m)")

habit_plots <- ggarrange(habitat_plot, size_plot, height_plot, nrow = 1)
ggsave(filename = "habit_plots.jpeg", plot = habit_plots,
       dpi =1200)

# 2. Does the occurance of G. brumalis swarming change with weather conditions? Binomial logistic regression model

# i) model all variables

gb_model <- glm(Swarming ~ Day+Time+Precipitation+Cloud_cover+Temperature+Wind_speed+Wind_direction,
            family = binomial(link="cloglog"),
            data = g_brumalis_2)
summary(gb_model)

# ii) minimise AIC

step(gb_model, direction = c("both"))

# iii) check vif

vif(glm(Swarming ~ Day+Time+Precipitation+Cloud_cover,
        family = binomial(link = "cloglog"),
        data = g_brumalis_2))

# take out Day, check vif again

vif(glm(Swarming ~ Time+Precipitation+Cloud_cover,
        family = binomial(link = "cloglog"),
        data = g_brumalis_2))

# iV) Final model: Swarming ~ Time, Precipitation, Cloud cover

gb_model <- glm(Swarming ~ Time+Precipitation+Cloud_cover,
            family = binomial(link="cloglog"),
            data = g_brumalis_2)
summary(gb_model)