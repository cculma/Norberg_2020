
p5.1 <- LD.plot(data_5.1)
p5.2 <- LD.plot(data_5.2)
p5.3 <- LD.plot(data_5.3)
p5.4 <- LD.plot(data_5.4)


p5.5 <- LD.plot(data_5.5)
class(p5.5)
ggsave(filename = "~/Documents/Cesar/git/big_files/FD_LD.jpg", plot = p5.5, width = 6, height = 6)

