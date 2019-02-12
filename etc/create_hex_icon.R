require(ggplot2)
require(hexSticker)

#segment_data <- data.frame(x =    c(0.5, 0.5, 4),
#                           xend = c(5.5, 4,   5.5),
#                           y =    c(2.2,   2.2,   5.2),
#                           yend = c(2.2,   5.2,   2.2))
segment_data <- data.frame(x = c(0.5, 5.5, 4, 0.5),
                           y = c(2.2, 2.2, 5.2, 2.2))
label_data <- data.frame(x = c(0.5, 5.5, 4,   3),
                         y = c(1.9, 1.9, 1.9, 1.9),
                         label = c("a", "b", "c", "x"))
fill_data <- data.frame(x = c(0.8, 3, 3, 0.8),
                        y = c(2.3, 2.3, 4.2, 2.3))

R_blue <- rgb(22, 92, 170, maxColorValue = 255)
R_blue2 <- rgb(39, 109, 195, maxColorValue = 255)
R_grey <- rgb(203, 206, 208, maxColorValue = 255)
R_grey2 <- rgb(132, 131, 139, maxColorValue = 255)

g1 <- ggplot() +
  #geom_segment(aes(x = x, xend = xend, y = y, yend = yend), data = segment_data,
  #             col = R_blue, lwd = 2) +
  geom_polygon(aes(x = x, y = y), data = segment_data, col = R_blue, lwd = 2, fill = "white") +
  xlim(0,6) + ylim(0,6) +
  geom_text(aes(x = x, y = y, label = label), data = label_data, size = 3) +
  geom_polygon(aes(x = x, y = y), data = fill_data, fill = "red") +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(), #element_rect(fill = "transparent"),
        plot.background = element_blank(), #element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())

g2 <- ggplot() +
  geom_hexagon(size = 3, fill = "white", color = R_blue) +
  theme_sticker(3) +
  geom_subview(subview = g1, x = 1, y = 1, width = 1.98, height = 1.98) +
  geom_pkgname("triangle", x = 1, y = 0.4, size = 6, color = "black", family = "sans")
plot(g2)

ggsave(g2, width = 48.9, height = 50.8, filename = file.path("triangle_hex.svg"),
       bg = "transparent", units = "mm")

# adjust the image and lines using inkscape
#  save as /etc/logo.svg
