library(qqplotr)
library(hexSticker)
library(dplyr)
library(extrafont)
library(ggpubr)

data(urine, package = "boot")

set.seed(1243)

df <- urine %>%
	sample_n(size = 15)

p1 <- df %>%
	ggplot(aes(sample = scale(ph))) +
	stat_qq_band(bandType = "ts", fill = "#bdd7e7", alpha = 0.4) +
	stat_qq_line(colour = "#2171b5", size = I(.3)) +
	stat_qq_point(colour = "#6baed6", size = I(.2)) +
	theme_void() +
	theme_transparent() +
	coord_equal() +
	xlim(-3, 3)

p2 <- df %>%
	sample_n(size = 15) %>%
	ggplot(aes(sample = scale(ph))) +
	stat_qq_band(detrend = TRUE, bandType = "ts", fill = "#bdd7e7", alpha = 0.4) +
	stat_qq_line(detrend = TRUE, colour = "#2171b5", size = I(.3)) +
	stat_qq_point(detrend = TRUE, colour = "#6baed6", size = I(.2)) +
	theme_void() +
	theme_transparent() +
	coord_equal()

p3 <- ggarrange(p1, p2, ncol = 2)

sticker(p1, package = "qqplotr",
				p_size = 12,
				p_color = "#2171b5",
				s_x = 1, s_y = .975, s_width = 10, s_height = 1.9,
				p_family = "Aller_Lt",
				#p_color = "#FFFFFFDD",
				h_color = "#2171b5",
				h_fill = "#eff3ff")
p3
