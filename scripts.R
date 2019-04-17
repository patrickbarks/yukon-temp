
# libraries
library(tidyverse)
library(scales)
library(readxl)

# download data appendix from Porter et al. (2019)
# (doi.org/10.1038/s41467-019-09622-y)
tmp <- tempfile()
download.file("https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-019-09622-y/MediaObjects/41467_2019_9622_MOESM2_ESM.xlsx", dest = tmp)


# read temperature reconstruction data and simplify column names
dat <- read_xlsx(tmp, sheet = 6) %>% 
  setNames(c("year", "temp", "low", "upp")) %>% 
  mutate(year = 1950 - year) # years given relative to 1950 baseline

# unlink tempfile
unlink(tmp)

# create df for background temperature heatmap
dfheat <- data.frame(x = rep(0, 8),
                     y = seq(-3.5, 3.5, 1),
                     z = factor(1:8))

# function to create year labels with BC/AD suffixes
year_labs <- function(x) {
  sapply(x, function(y) {
    if (y == 0) {
      return("1 AD")
    } else if (y < 0) {
      return(paste(abs(y), "BC"))
    } else {
      return(paste(y, "AD"))
    }
  })
}

# create plot
p <- ggplot(dat) +
  geom_tile(data = dfheat, aes(x = x, y = y, fill = z, width = Inf), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 0.3, alpha = 0.8, linetype = 2) +
  geom_line(aes(x = year, y = temp), size = 0.8) +
  geom_ribbon(aes(x = year, ymin = low, ymax = upp), alpha = 0.2) +
  scale_x_continuous(breaks = seq(-10000, 2000, 2000),
                     expand = c(0.02, 0), labels = year_labs) +
  scale_y_continuous(breaks = seq(-3, 3, 1), expand = c(0.0, 0)) +
  coord_cartesian(xlim = c(-11800, 2200), ylim = c(-3.01, 3.01)) +
  scale_fill_brewer(palette = "RdBu", direction = -1, guide = FALSE) +
  labs(x = NULL,
       y = expression(paste(Delta, T[holocene], " (", degree, C, ")"))) +
  ggtitle("Summer temperature anomalies in central Yukon since 11,000 BC",
          subtitle = "Estimated based on precipitation isotope ratios (data from Porter et al. 2019, Nat. Comm., https://rdcu.be/bxpxn)") +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16.5),
        plot.subtitle = element_text(size = 10.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.4),
        axis.ticks = element_blank())

# print to device
dev.off()
quartz(height = 4.5, width = 8.5, dpi = 150)
print(p)

# save to file
ggsave("fig_temp.png", p, height = 4.5, width = 8.5, dpi = 300)


