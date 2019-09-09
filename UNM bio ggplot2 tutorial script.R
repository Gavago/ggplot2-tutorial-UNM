############
# UNM Bio Advanced R: GGPLOT2 tutorial ----
###########

# install packages
install.packages("tidyverse")
install.packages("devtools") # for windows use install.packages("devtools", type = "win.binary")
library(devtools)  # load devtools to install datasets from github, i.e. to use install_github()
install_github("Gavago/SIPI-Bridge-data")
install.packages("gridExtra")

# load packages
library(tidyverse) 
library(magrittr)
library(gridExtra)
library(sipibridge) # datasets: cancer, gun_deaths, city_crime, diabetes

# tweak some data
diabetes %<>%
  mutate(age_category = ifelse(Age > mean(Age), "older", "younger"))
gun_deaths %<>%
  filter(!is.na(intent)) 


#####
# 1) Geoms -----
#####
# --- Histogram ----
View(cancer)

gh <- cancer %>%
  ggplot(., aes(x = avg_death))

gh + geom_histogram() 
gh + geom_histogram(binwidth = 5)
gh + geom_histogram(color = "red", fill = "white", binwidth = 5)
gh + geom_histogram(color = "red", fill = "white", binwidth = 5) +
  geom_freqpoly(binwidth = 5)

# density plot
gd <- g + geom_histogram(aes(y = ..density..), color = "red", fill = "white", binwidth = 5)
gd
gdc <- gd + geom_density() # adds curve
gdc
gdc + geom_vline(aes(xintercept = mean(avg_death)), color = "black", linetype = 2)


histogram <- cancer %>%
  ggplot(., aes(x = avg_death)) + 
  geom_histogram(color = "red", fill = "white", binwidth = 5) +
  labs(x = "avg number deaths per 100k", y = "frequency", 
       title = "Rate of cancer deaths cluster around 160 per 100k people") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0, size = 12))
histogram


# --- Barplot ----
View(gun_deaths)

gb <- gun_deaths %>%
  ggplot(aes(x = intent, fill = sex)) 
# geom_bar() does not need y variable 
# because geom_bar counts observations for each category of intent

gb + geom_bar()
gb + geom_bar(position = "dodge") # unstack by fill

bar_plot <- gun_deaths %>%
  ggplot(aes(x = intent, fill = sex)) + 
  geom_bar() +
  labs(y = "gun deaths per 100k", title = "Gun deaths by intent and sex of perpetrator",
       subtitle = "Male suicides account for the highest proportion of total gun deaths.") +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0, size = 12))
bar_plot

# Use *geom_col* when data are already in summary table form
# takes y argument for bar height
intent_table <- gun_deaths %>% # create summary table
  group_by(intent,sex) %>%
  tally()
intent_table

intent_count %>%
  ggplot(aes(x = intent, y = n, fill = sex)) +
  geom_col()
  

# --- Scatter plot ----
# + regression lines
View(diabetes)

gs <- diabetes %>%
  ggplot(aes(x = Pregnancies, y = SkinThickness))

gs + geom_point()
gs + geom_jitter()
gs + geom_jitter(aes(color = Outcome)) # Outcome should be discrete/factor

gso <- diabetes %>%
  mutate(Outcome = as.factor(Outcome)) %>%
  ggplot(aes(x = Pregnancies, y = SkinThickness))

gso + geom_jitter(aes(color = Outcome))
gso + geom_jitter(aes(color = Outcome, shape = Outcome))
gso + geom_jitter(aes(color = Outcome, shape = Outcome), size = 3)
gso + geom_jitter(aes(color = Outcome, shape = Outcome, size = Insulin)) 
# note: now has 2 legends, see section "legends basics" for legend manipulation
gsoi <- gso + geom_jitter(aes(color = Outcome, shape = Outcome, size = Insulin), alpha = 0.5)
gsoi 

# add regression line(s)
gsoi + geom_smooth(method = "lm", color = "black", size = .5) 
gsoi + geom_smooth(method = "lm", color = "black", size = .5, se = F, linetype = 2) 
gsoi + geom_smooth(method = "loess", color = "black", size = .5, se = F, linetype = 2) 
gsoi + geom_smooth( aes(color = age_category), method = "lm", size = 1.5, se = F, linetype = 2) 
# note: 3 legends now, usually a good sign that too many variables mapped to an aesthetic -
# size (insulin), color (diabetes diagnosis and age category), and point shape (diagnosis)

scatter_plot <- gso +
  geom_jitter(aes(color = Outcome, shape = Outcome), size = 3, alpha = .5) +
  geom_smooth( aes(linetype = age_category), method = "lm", size = 1, se = F) +
  labs(x = "Number of pregnancies", y = "Body fat (Skin thickness mm)",
       title = "Diabetes diagnosis by number of pregnancies and body fat",
       caption = "Risk of diabetes is higher among women with increasing pregnancies
       and body fat, which are more likely to be positively correlated among
       older women.") +
  theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = 0, size = 12))
scatter_plot


# --- Box plot ----

box_plot <- cancer %>%
ggplot(aes(x = region, y = avg_incidence, fill = region)) +
  geom_boxplot(alpha = .5) +
  labs(x = "US region", y = "incidence rate of cancer", title = "Cancer incidence by US region",
       caption = "The highest rates of cancer incidence occur in the South,
       but the Northeast has the highest average rates of incidence.") +
  theme(axis.text.x = element_text(angle = 90, size = 10), plot.title = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0, size = 12))
box_plot

# overlay scatter plot
box_plot + geom_jitter()
box_plot + geom_jitter(aes(color = region), width = .2)
# here I'd recommend adding geom_jitter() before geom_boxplot to overlay boxes on points
cancer %>%
  ggplot(aes(x = region, y = avg_incidence, fill = region)) +
  geom_jitter(aes(color = region), width = .2) +
  geom_boxplot(alpha = .5) 


# --- Other useful geoms ----

+ geom_line() # line plot, good for time series
+ geom_hline() # horizontal line, e.g. indicate baseline or population average
+ ggcorrplot::ggcorrplot() #correlation heatmap
+ geom_violin() # violin plot, uses same arguments as geom_histogram

+ geom_text() # add labels at all given xy coordinates
+ geom_label() # same
+ gglabel::geom_label_repel() # special package to make labels automatically not overlap
# fyi, + annotate( geom = "text) adds text to a *single* pair of xy coordinates
# https://ggplot2.tidyverse.org/reference/geom_text.html

+ geom_segment() # create custom length and positioned lines, e.g. a significance bracket
box_plot + 
  geom_segment(aes(x = "Northeast", y = 500, xend = "West", yend = 500),
                        linetype = 2, color = "black") +
  annotate(geom = "text", x = 3, y = 505, label = "*", size = 12) 
  
#####
# 2) Scales ----
#####
# Scales adjust design of aesthetic (i.e. data-related) elements of plot 
# e.g. size, fill, shape, axis tick marks

# Structure of scale function's name is
# scale_"aesthetic to adjust"_"scale to use"()
# arguments are
# values = , limits = , breaks = , names = , labels = 

# colors
scatter_plot + scale_color_brewer(, palette = 2) # note scale_color for points
scatter_plot + scale_color_manual(values = c("darkmagenta","darkorange"))
# more manual colors: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
bar_plot + scale_fill_manual(values = c("darkmagenta","darkorange")) # note scale_fill
box_plot + scale_fill_brewer(palette = 2) 
# color brewer is designed for discrete data, i.e. fills
# are "palettes" or sets of related colors, with many light ones, so not good for continuous data
# more on color brewer: https://ggplot2.tidyverse.org/reference/scale_brewer.html


# axes tick labels and breaks
scatter_plot + scale_y_continuous(limits = c(1, 75))
scatter_plot + scale_x_continuous(breaks = seq(0, 20, 2))
box_plot + scale_x_discrete(labels = c("NC", "NE", "S", "W"))


#####
# 3) Themes ----
#####
# Themes change design of non-data-related elements of plot
# e.g. background, legend and axis label position, text size

# These themes adjust overall look
# do not take text adjustment arguments
# need to be in layer before basic theme() e.g. theme(plot.title = ...)
scatter_plot + theme_classic()
scatter_plot + theme_minimal()
scatter_plot + theme_dark()

# note that common arguments of basic theme() begin with element to adjust
# e.g. 
# plot.title
# plot.x.axis
# legend.position
# text

#####
# 4) Legend basics -----
#####

# Manipulating legends uses either scale or theme functions
# scales are for data-related legend elements
# themes are for non-data-related general design elements

# change legend position
scatter_plot + theme(legend.position = "bottom", 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 12))

# turn off legends
scatter_plot + theme(legend.position = "none") # turn off all legends
scatter_plot + scale_linetype(guide = FALSE) # turn off legend for particular aesthetic

# legend labels
scatter_plot + 
  scale_color_discrete(labels = c("Healthy", "Diabetic")) +
  scale_shape_discrete(labels = c("Healthy", "Diabetic")) 
  # needs 2 scales bc color & shape aes, if just modify 1 would get 2 legends

# legend titles
scatter_plot +
  labs(color = "Diagnosis", shape = "Diagnosis") # rename variable mapped to each aesthetic


#####
# 5) Funsies, facets, and arranging ----
#####

# change font
+ theme(text = element_text(family = ...)) 
bar_plot + theme(text = element_text(family = "serif", size = 12)) 


# add text to plot at single pair xy coords
+ annotate()  
scatter_plot + annotate("text", x = 0.5, y = 100, label = "A)", size = 8) 


# flip xy coordinates
+ coord_flip() 
box_plot + coord_flip()


# add multiple plots onto a single pane

# 1 - facet: create a plot per level of a categorical variable
+ facet_wrap() 
gun_deaths %>%
  ggplot(aes(x = sex, fill = sex)) + 
  geom_bar() +
  facet_wrap(. ~ intent) 
# syntax is dot "." = aesthetic mappings, by "~", categorical variable

# 2 - arrange:
grid.arrange(histogram, scatter_plot, bar_plot, box_plot, nrow = 2)


#####
# 6) Exporting plots ----
#####

ggsave(plot = , filename = "... .pdf", width = , height = )
# plot argument defaults to last plot displayed


#####
# Resources ----
#####

# general dataviz in R4DS:
  # 1 - basic https://r4ds.had.co.nz/data-visualisation.html
  # 2 - advanced https://r4ds.had.co.nz/graphics-for-communication.html
# cheatsheat for ggplot2: 
  # https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf
# everything legends: 
  # https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# colors:
  # popular palettes (sets of colors): https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
  # manual: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  # brewer: https://ggplot2.tidyverse.org/reference/scale_brewer.html
  # viridis (good for colorblind and bw print): https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  # wes anderson palettes: https://github.com/karthik/wesanderson

