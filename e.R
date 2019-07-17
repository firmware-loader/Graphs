library(ggplot2)
library(plotly)
library(gapminder)
library(ggridges)
library(rowr)

timing <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/Processed/complete.txt",
                     sep = "" , header = F , na.strings ="", stringsAsFactors= F)
timer <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/Timer/Processed/complete_timer.txt", 
                    sep = "" , header = F , na.strings ="", stringsAsFactors= F)
inline <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/Inline/Processed/comple_inline.txt",
                     sep = "" , header = F , na.strings ="", stringsAsFactors= F)

mhz16 <- read.table("E:/Dokumente/seafile/Seafile/Seafile/Main/Dokumente/FH/Bachelor/values/TimingBased/Processed/16Mhz.txt",
                     sep = "" , header = F , na.strings ="", stringsAsFactors= F)

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
  geom_jitter(width = 0.1) +
  geom_smooth(method="lm", se = FALSE) + 
  ylim(0, 120000) +
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [Baud]") +
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]")
#title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten\nbei Benutzung der autom. Taktberechnung",


ggplot(data = inline[which(inline$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * (V2 / 100))) + 
#  geom_point() + 
  geom_jitter(width = 0.1) +
  geom_smooth(method="lm", se = FALSE) + 
  ylim(0, 120000) +
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [Baud]") +
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]")
#title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten\nbei Benutzung von Assembler"

ggplot(data = timer[which(timer$V2>1),], aes(x = V3, y = V1, color = V2, weight = V1 * (V2 / 100))) + 
#  geom_point() +
  geom_jitter(width = 0.1) + 
  geom_smooth(method="lm", se = FALSE) + 
  ylim(0, 120000) +
  xlab("Taktrate des Mikrocontrollers [Mhz]") + 
  ylab("‹bertragungsgeschwindigkeit [Baud]") +  
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]")
#title = "Erfolreiche ‹bertragungen in Abh‰nigkeit von ‹bertragunsgeschwindigkeiten und Taktraten\nbei Benutzung eines Timers", 

tmpTiming <- timing[timing[, "V3"]==10,]
tmpTiming[tmpTiming[, "V3"]==10,]$V3 <- "Autom. Timing Berechnung"

tmpInline <- inline[inline[, "V3"]==10,]
tmpInline[tmpInline[, "V3"]==10,]$V3 <- "Inline Assembler"

tmpTimer <- timer[timer[, "V3"]==10,]
tmpTimer[tmpTimer[, "V3"]==10,]$V3 <- "Timer"

compare10MHZ <- rbind(tmpTiming, tmpInline, tmpTimer)

dat_annot <- data.frame(
  lowerBound = c(600, 50000, 2000),
  upperBound = c(1200, 100000, 4000),
  V3   = c("Autom. Timing Berechnung", "Inline Assembler", "Timer")
)

#  geom_hline(data = dat_annot, aes(yintercept = dat_annot$val, colour = 1), dat_annot$val) +
#  annotate("rect", xmin=0, xmax=Inf, ymin=dat_annot$lowerBound, ymax=dat_annot$upperBound, alpha=0.2, fill="red") +
#  geom_hline(aes(yintercept = V2, colour = V2), 1000) +

ggplot(compare10MHZ, aes(V2, V1, color = V2)) +
  ylim(0, 120000) +
  geom_point() +
  facet_wrap(vars(V3)) +
  ylab("‹bertragungsgeschwindigkeit [Baud]") +
  labs(color = "Erfolg-\nreiche\n‹ber-\ntragungen\n[%]") +
  xlab("Erfolgreiche ‹bertragungen [%]") 

ggplot(mhz16, aes(V1, V2)) +
  geom_point() + 
  annotate("rect", ymin=0, ymax=Inf, xmin=0, xmax=1097, alpha=0.2, fill="red") +
  annotate("rect", ymin=0, ymax=Inf, xmin=1097, xmax=72074, alpha=0.2, fill="green") +
  annotate("rect", ymin=0, ymax=Inf, xmin=72074, xmax=91673, alpha=0.2, fill="yellow") +
  annotate("rect", ymin=0, ymax=Inf, xmin=91673, xmax=Inf, alpha=0.2, fill="red") + 
  xlab("‹bertragungsgeschwindigkeit [Baud]") + 
  ylab("Erfolgreiche ‹bertragungen [%]")

#ignore "random match" (extl. 1 match, because it could be coincidence)

#attach(timing)
#attach(timer)
