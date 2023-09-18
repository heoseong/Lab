rm(list = ls())

library(readr)
library(tidyverse)
library(dplyr)
library(rgl)
library(schemr)
library(ggplot2)

cutflower <- read.csv("lecture_2.csv", header = TRUE)
cutflower <- cutflower |> na.omit()
glimpse(cutflower)
head(cutflower)

cut <- cutflower %>% 
  select(cultivar, concentration, 21:35)
View(cut)

cut_AG <- cut %>% filter(cultivar == "AG")
View(cut_AG)

cut_edit <- cut |> group_by(cultivar, concentration) %>%  
  summarise(LL_1 = mean(L_1),
            aa_1 = mean(a_1),
            bb_1 = mean(b_1),
            LL_6 = mean(L_6),
            aa_6 = mean(a_6),
            bb_6 = mean(b_6),
            LL_9 = mean(L_9),
            aa_9 = mean(a_9),
            bb_9 = mean(b_9),
            LL_11 = mean(L_11),
            aa_11 = mean(a_11),
            bb_11 = mean(b_11),
            LL_14 = mean(L_14),
            aa_14 = mean(a_14),
            bb_14 = mean(b_14)
  )

head(cut_edit)



lab_rgb <- cut_edit |> mutate(lab1 = lab_to_rgb(data.frame(LL_1, aa_1, bb_1)),
                                  lab6 = lab_to_rgb(data.frame(LL_6, aa_6, bb_6)),
                                  lab9 = lab_to_rgb(data.frame(LL_9, aa_9, bb_9)),
                                  lab11 = lab_to_rgb(data.frame(LL_11, aa_11, bb_11)),
                                  lab14 = lab_to_rgb(data.frame(LL_14, aa_14, bb_14))
)


View(lab_rgb)


cut_AG %>% ggplot(aes(x=a_1, y=b_1, color=concentration )) +
  geom_point() + geom_smooth(method="lm")
cut_AG %>% ggplot(aes(x=b_1, y=L_1, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=L_1, y=a_1, color=concentration )) +
  geom_point()

cut_AG %>% ggplot(aes(x=a_6, y=b_6, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=b_6, y=L_6, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=L_6, y=a_6, color=concentration )) +
  geom_point()

cut_AG %>% ggplot(aes(x=a_9, y=b_9, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=b_9, y=L_9, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=L_9, y=a_9, color=concentration )) +
  geom_point()

cut_AG %>% ggplot(aes(x=a_11, y=b_11, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=b_11, y=L_11, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=L_11, y=a_11, color=concentration )) +
  geom_point()

cut_AG %>% ggplot(aes(x=a_14, y=b_14, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=b_14, y=L_14, color=concentration )) +
  geom_point()
cut_AG %>% ggplot(aes(x=L_14, y=a_14, color=concentration )) +
  geom_point()



##########################AG
l1 <- cut_AG$L_1
a1 <- cut_AG$a_1
b1 <- cut_AG$b_1


open3d()
plot3d(a1, b1, l1, col="#B5C28E", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a1,b1,l1)), 
                    centre = c(mean(a1), mean(b1), mean(l1)), level = 0.95)
plot3d(ellips, col = "#FF0000", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("AG_1.png")



l6 <- cut_AG$L_6
a6 <- cut_AG$a_6
b6 <- cut_AG$b_6


open3d()
plot3d(a6, b6, l6, col="#C0CA91", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a6,b6,l6)), 
                    centre = c(mean(a6), mean(b6), mean(l6)), level = 0.95)
plot3d(ellips, col = "#FFBB00", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("AG_6.png")


l9 <- cut_AG$L_9
a9 <- cut_AG$a_9
b9 <- cut_AG$b_9


open3d()
plot3d(a9, b9, l9, col="#BEC991", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a9,b9,l9)), 
                    centre = c(mean(a9), mean(b9), mean(l9)), level = 0.95)
plot3d(ellips, col = "#008000", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("AG_9.png")


l11 <- cut_AG$L_11
a11 <- cut_AG$a_11
b11 <- cut_AG$b_11


open3d()
plot3d(a11, b11, l11, col="#C1C790", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a11,b11,l11)), 
                    centre = c(mean(a11), mean(b11), mean(l11)), level = 0.95)
plot3d(ellips, col = "#800080", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("AG_11.png")


l14 <- cut_AG$L_14
a14 <- cut_AG$a_14
b14 <- cut_AG$b_14


open3d()
plot3d(a14, b14, l14, col="#BDC48B", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a14,b14,l14)), 
                    centre = c(mean(a14), mean(b14), mean(l14)), level = 0.95)
plot3d(ellips, col = "#0100FF", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("AG_14.png")


#########################################BP

cut_BP <- cut %>% filter(cultivar == "BP")
head(cut_BP)

l1 <- cut_BP$L_1
a1 <- cut_BP$a_1
b1 <- cut_BP$b_1


open3d()
plot3d(a1, b1, l1, col="#CBC5C7", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a1,b1,l1)), 
                    centre = c(mean(a1), mean(b1), mean(l1)), level = 0.95)
plot3d(ellips, col = "#FF0000", alpha = 0.3, add = TRUE, type = "wire")
aspect3d(1,1,1)
rgl.snapshot("BP_1.png")



l6 <- cut_BP$L_6
a6 <- cut_BP$a_6
b6 <- cut_BP$b_6


open3d()
plot3d(a6, b6, l6, col="#D1CACB", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a6,b6,l6)), 
                    centre = c(mean(a6), mean(b6), mean(l6)), level = 0.95)
plot3d(ellips, col = "#FFBB00", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("BP_6.png")


l9 <- cut_BP$L_9
a9 <- cut_BP$a_9
b9 <- cut_BP$b_9


open3d()
plot3d(a9, b9, l9, col="#DECFC9", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a9,b9,l9)), 
                    centre = c(mean(a9), mean(b9), mean(l9)), level = 0.95)
plot3d(ellips, col = "#008000", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("BP_9.png")


l11 <- cut_BP$L_11
a11 <- cut_BP$a_11
b11 <- cut_BP$b_11


open3d()
plot3d(a11, b11, l11, col="#B5A0AC", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-40, 40), xlim = c(-40, 40))
ellips <- ellipse3d(cov(cbind(a11,b11,l11)), 
                    centre = c(mean(a11), mean(b11), mean(l11)), level = 0.95)
plot3d(ellips, col = "#800080", alpha = 0.3, add = TRUE, type = "wire")
aspect3d(1,1,1)
rgl.snapshot("BP_11.png")


l14 <- cut_BP$L_14
a14 <- cut_BP$a_14
b14 <- cut_BP$b_14


open3d()
plot3d(a14, b14, l14, col="#C6AFB2", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a14,b14,l14)), 
                    centre = c(mean(a14), mean(b14), mean(l14)), level = 0.95)
plot3d(ellips, col = "#0100FF", alpha = 0.3, add = TRUE, type = "wire")
aspect3d(1,1,1)
rgl.snapshot("BP_14.png")



##############################################KP
cut_KP <- cut %>% filter(cultivar == "KP")
head(cut_KP)


l1 <- cut_KP$L_1
a1 <- cut_KP$a_1
b1 <- cut_KP$b_1


open3d()
plot3d(a1, b1, l1, col="#C8B7B5", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a1,b1,l1)), 
                    centre = c(mean(a1), mean(b1), mean(l1)), level = 0.95)
plot3d(ellips, col = "#FF0000", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KP_1.png")



l6 <- cut_KP$L_6
a6 <- cut_KP$a_6
b6 <- cut_KP$b_6


open3d()
plot3d(a6, b6, l6, col="#DCC6C1", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a6,b6,l6)), 
                    centre = c(mean(a6), mean(b6), mean(l6)), level = 0.95)
plot3d(ellips, col = "#FFBB00", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KP_6.png")


l9 <- cut_KP$L_9
a9 <- cut_KP$a_9
b9 <- cut_KP$b_9


open3d()
plot3d(a9, b9, l9, col="#D0C8A7", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a9,b9,l9)), 
                    centre = c(mean(a9), mean(b9), mean(l9)), level = 0.95)
plot3d(ellips, col = "#008000", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KP_9.png")


l11 <- cut_KP$L_11
a11 <- cut_KP$a_11
b11 <- cut_KP$b_11


open3d()
plot3d(a11, b11, l11, col="#D4BEB1", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a11,b11,l11)), 
                    centre = c(mean(a11), mean(b11), mean(l11)), level = 0.95)
plot3d(ellips, col = "#800080", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KP_11.png")


l14 <- cut_KP$L_14
a14 <- cut_KP$a_14
b14 <- cut_KP$b_14


open3d()
plot3d(a14, b14, l14, col="#D0C6A1", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a14,b14,l14)), 
                    centre = c(mean(a14), mean(b14), mean(l14)), level = 0.95)
plot3d(ellips, col = "#0100FF", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KP_14.png")

####################################KW
cut_KW <- cut %>% filter(cultivar == "KW")
head(cut_KW)


l1 <- cut_KW$L_1
a1 <- cut_KW$a_1
b1 <- cut_KW$b_1


open3d()
plot3d(a1, b1, l1, col="#E0DFDA", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a1,b1,l1)), 
                    centre = c(mean(a1), mean(b1), mean(l1)), level = 0.95)
plot3d(ellips, col = "#FF0000", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KW_1.png")



l6 <- cut_KW$L_6
a6 <- cut_KW$a_6
b6 <- cut_KW$b_6


open3d()
plot3d(a6, b6, l6, col="#E8E4DA", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a6,b6,l6)), 
                    centre = c(mean(a6), mean(b6), mean(l6)), level = 0.95)
plot3d(ellips, col = "#FFBB00", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KW_6.png")


l9 <- cut_KW$L_9
a9 <- cut_KW$a_9
b9 <- cut_KW$b_9


open3d()
plot3d(a9, b9, l9, col="#CDC3C9", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a9,b9,l9)), 
                    centre = c(mean(a9), mean(b9), mean(l9)), level = 0.95)
plot3d(ellips, col = "#008000", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KW_9.png")


l11 <- cut_KW$L_11
a11 <- cut_KW$a_11
b11 <- cut_KW$b_11


open3d()
plot3d(a11, b11, l11, col="#E1D9C8", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a11,b11,l11)), 
                    centre = c(mean(a11), mean(b11), mean(l11)), level = 0.95)
plot3d(ellips, col = "#800080", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KW_11.png")


l14 <- cut_KW$L_14
a14 <- cut_KW$a_14
b14 <- cut_KW$b_14


open3d()
plot3d(a14, b14, l14, col="#D5CBC5", box = TRUE, size=6,
       type ="p", radius = 0.15, zlim = c(40, 120), ylim = c(-30, 30), xlim = c(-30, 30))
ellips <- ellipse3d(cov(cbind(a14,b14,l14)), 
                    centre = c(mean(a14), mean(b14), mean(l14)), level = 0.95)
plot3d(ellips, col = "#0100FF", alpha = 0.3, add = TRUE, type = "wire")
rgl.snapshot("KW_14.png")
