df = readxl::read_excel("C:/Users/alien/Documents/InVest/Beschichten.xlsx", sheet=2)
x = df$Wert


#Chi-quadrat
nortest::pearson.test(x)#, n.classes = ceiling(2 * (n^(2/5))), adjust = TRUE)


#Shapiro
stats::shapiro.test(x)


##Paar andere Tests
nortest::ad.test(x)
nortest::cvm.test(x)


#The Lilliefors (Kolomorov-Smirnov) test is the most famous EDF omnibus test for normality. 
nortest::lillie.test(x)
