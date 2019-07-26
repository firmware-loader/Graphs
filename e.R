library(ggplot2)
library(plotly)
library(gapminder)
library(ggridges)
library(rowr)
libary(dplyr)

# Load Data

timing <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/Processed/complete.txt",
                     sep = "" , header = F , na.strings ="", stringsAsFactors= F)
timer <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/Timer/Processed/complete_timer.txt", 
                    sep = "" , header = F , na.strings ="", stringsAsFactors= F)
inline <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/Inline/Processed/comple_inline.txt",
                     sep = "" , header = F , na.strings ="", stringsAsFactors= F)

mhz16 <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/Processed/16Mhz.txt",
                     sep = "" , header = F , na.strings ="", stringsAsFactors= F)

#
D7_7 <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/16Mhz/7_7.txt",
                     sep = "" , header = F , na.strings ="", stringsAsFactors= F)
D5_7 <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/16Mhz/5_7.txt",
                   sep = "" , header = F , na.strings ="", stringsAsFactors= F)
D5_11 <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/16Mhz/5_11.txt",
                   sep = "" , header = F , na.strings ="", stringsAsFactors= F)
D8_7 <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/16Mhz/8_7.txt",
                   sep = "" , header = F , na.strings ="", stringsAsFactors= F)

# Make tries into %

timing$V2 <- timing$V2 * 10
timer$V2 <- timer$V2 * 10
inline$V2 <- inline$V2 * 10

cleaned_timer <- timer %>%
  filter(V2==100)

ggplot(timing[which(timing$V2==100),], aes(x=as.factor(V3), y=V1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ylim(0, 100000) +
  xlab("Frequenz") + 
  ylab("Baud")


ggplot(inline[which(inline$V2==100),], aes(x=as.factor(V3), y=V1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ylim(0, 100000) +
  xlab("Frequenz") + 
  ylab("Baud")

ggplot(cleaned_timer, aes(x=as.factor(V3), y=V1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  ylim(0, 100000) +
  xlab("Frequenz") + 
  ylab("Baud")

ggplot(data = timing[which(timing$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * (V2 / 100))) + 
#  geom_point() + 
  geom_jitter(width = 0.1, size = 1) +
  geom_smooth(method="lm", se = FALSE) + 
  ylim(0, 120000) +
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [bps]") +
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]") +
  scale_x_continuous(breaks = seq(0, 12, 1))
#title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten\nbei Benutzung der autom. Taktberechnung",


ggplot(data = inline[which(inline$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * (V2 / 100))) + 
#  geom_point() +  
  geom_jitter(width = 0.1, size = 1) +
  geom_smooth(method="lm", se = FALSE) + 
  ylim(0, 120000) +
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [bps]") +
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]") +
  scale_x_continuous(breaks = seq(0, 12, 1))
#title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten\nbei Benutzung von Assembler"

ggplot(data = timer[which(timer$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * (V2 / 100))) + 
#  geom_point() +
  geom_jitter(width = 0.1, size = 1) + 
  geom_smooth(method="lm", se = FALSE) + 
  ylim(0, 120000) +
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [bps]") +  
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]") +
  scale_x_continuous(breaks = seq(0, 12, 1))
#title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten\nbei Benutzung eines Timers", 

tmpTiming <- timing[timing[, "V3"]==10,]
tmpTiming[tmpTiming[, "V3"]==10,]$V3 <- "Autom. Timing Berechnung"

tmpInline <- inline[inline[, "V3"]==10,]
tmpInline[tmpInline[, "V3"]==10,]$V3 <- "Inline Assembler"

tmpTimer <- timer[timer[, "V3"]==10,]
tmpTimer[tmpTimer[, "V3"]==10,]$V3 <- "Timer"

compare10MHZ <- rbind(tmpTiming, tmpInline, tmpTimer)

ggplot(compare10MHZ, aes(V2, V1, color = V2)) +
  ylim(0, 120000) +
  geom_point() +
  facet_wrap(vars(V3)) +
  ylab("‹bertragungsgeschwindigkeit [bps]") +
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]") +
  xlab("Erfolgreiche ‹bertragungen [%]") 

ggplot(mhz16, aes(V1, V2)) +
  geom_point(size = .5) + 
  annotate("rect", ymin=0, ymax=Inf, xmin=0, xmax=1097, alpha=0.2, fill="red") +
  annotate("rect", ymin=0, ymax=Inf, xmin=1097, xmax=72074, alpha=0.2, fill="green") +
  annotate("rect", ymin=0, ymax=Inf, xmin=72074, xmax=91673, alpha=0.2, fill="yellow") +
  annotate("rect", ymin=0, ymax=Inf, xmin=91673, xmax=Inf, alpha=0.2, fill="red") + 
  xlab("‹bertragungsgeschwindigkeit [bps]") + 
  ylab("Erfolgreiche ‹bertragungen [%]")


D5_7$name <- rep("7 Sync., 7 Trans",nrow(D5_7))
D7_7$name <- rep("5 Sync., 7 Trans",nrow(D7_7))
D8_7$name <- rep("8 Sync., 7 Trans",nrow(D8_7))
D5_11$name <- rep("5 Sync., 11 Trans",nrow(D5_11))

compareCycles <- rbind(D5_7, D7_7, D8_7, D5_11)

ggplot(compareCycles, aes(V1, V2, color = name)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  facet_wrap(~name) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  ylim(c(0,100)) +
  xlab("‹bertragungsgeschwindigkeit [bps]") + 
  ylab("Erfolgreiche ‹bertragungen [%]") +
  labs(color = "Taktzyklen\nSynchronisation /\nDatentransfer")

cpb_timing <- character(length(12))
cpb_timer <- character(length(12))
cpb_inline <- character(length(12))

for (i in 1:12) {
  z <- as.vector(floor(timing[which(timing$V3==i),][['V2']] / 100))
  run <- rle(z)
  # r is drunk / fubar / glitched
  #print(r[1:5]) #[which(max(r$lengths) == r$lengths), ]
  cpb_timing[i] <- (i*1000000) / max(timing[which(timing$V2==100 & timing$V3==i),])
  cpb_timer[i] <- (i*1000000) / max(timer[which(timer$V2==100 & timer$V3==i),])
  cpb_inline[i] <- (i*1000000) / max(inline[which(inline$V2==100 & inline$V3==i),])
}

#cpb_timing[16] <- 16000000 / 88031
#cpb_timer[16] <- 16000000 / 66034
#cpb_inline[16] <- 16000000 / 73699

print(cpb_timing)

max_data <- data.frame(cpb_timing, 1:12, "Autom. Timing")
colnames(max_data) <- c("data", "mhz", "name")

max_data2 <- data.frame(cpb_timer, 1:12, "Timer")
colnames(max_data2) <- c("data", "mhz", "name")

max_data3 <- data.frame(cpb_inline, 1:12, "Assembler")
colnames(max_data3) <- c("data", "mhz", "name")

max_data44 <- rbind(max_data, max_data2, max_data3)

ggplot(max_data44, aes(x = as.numeric(mhz), y = as.numeric(as.character(data)), color = name)) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  xlab("Taktfrequenz [Mhz]") + 
  ylab("Taktzyklen pro ‹bertragenem Bit")


bootsize <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/Bootloader/sizes.txt",
                   sep = "" , header = F , na.strings ="", stringsAsFactors= F)
ggplot(bootsize, aes(x = V3, y = V1, fill = V2)) +
  scale_y_continuous(breaks = seq(0, 600, 50)) +
  geom_bar(stat='identity') +
  ylab("Flashgrˆﬂe [Byte]") +
  labs(fill = "Bereich") +
  xlab("Methode") 