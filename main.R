# a) jak budowac model SUR to my wiemy ze jezel pracujemy na CAPM to zmienna objasniana jako stopad nadwyzkowa z WIGU jest wszedzie taka saam, wiec mozemy estymowac model rowanien po rownaiu, ale zeby model byl spelniony to musza alf (wyrazy wolne) byc rowne 0, ale to wcale nie oznacza ze dla calosic one beda rowne zero0, bo to jest test restrykcji. Gdyby to nie byl model SUR to istotnosc parametrow mowilaby nam wszystko, ale ta korelacja miedzy resztami moze sie zdarzyc tak ze cos nie ten teges, alew calosci macierz ok. Testrestrykcji dla calego modelu
# w R sa zaimplementowane testyrestrykcji, ale sa ogolne, ten test jest szczegolny. Statystyka GRs jest przeksztalcona do 

banki<- read.csv("wig_banki_m.csv")
budownictwo<- read.csv("wig_budow_m.csv")
chemia<- read.csv("wig_chemia_m.csv")
energia<- read.csv("wig_energ_m.csv")
info<- read.csv("wig_info_m.csv")
nruchom<- read.csv("wig_nrchom_m.csv")
media<-  read.csv("wig_media_m.csv")
paliwa<-  read.csv("wig_paliwa_m.csv")
spoz<- read.csv("wig_spozyw_m.csv")
wig<- read.csv("wig_m.csv")

wigi<- data.frame(banki$Zamkniecie,budownictwo$Zamkniecie,chemia$Zamkniecie,
                  energia$Zamkniecie,info$Zamkniecie,nruchom$Zamkniecie,media$Zamkniecie,
                  paliwa$Zamkniecie, spoz$Zamkniecie,wig$Zamkniecie)
#obliczenie stop zwrotu
sBanki<- diff(banki$Zamkniecie)/lag(banki$Zamkniecie[1:152])
sBudownictwo<- diff(budownictwo$Zamkniecie)/lag(budownictwo$Zamkniecie[1:152])
sChemia<- diff(chemia$Zamkniecie)/lag(chemia$Zamkniecie[1:152])
sEnergia<- diff(energia$Zamkniecie)/lag(energia$Zamkniecie[1:152])
sInfo<-diff(info$Zamkniecie)/lag(info$Zamkniecie[1:152])
sNruchom<- diff(nruchom$Zamkniecie)/lag(nruchom$Zamkniecie[1:152])
sMedia<-  diff(media$Zamkniecie)/lag(media$Zamkniecie[1:152])
sPaliwa<-  diff(paliwa$Zamkniecie)/lag(paliwa$Zamkniecie[1:152])
sSpoz<- diff(spoz$Zamkniecie)/lag(spoz$Zamkniecie[1:152])
sWig<- diff(wig$Zamkniecie)/lag(wig$Zamkniecie[1:152])

#stopy zwrotu
stopy<- data.frame(sBanki, sBudownictwo, sChemia, sEnergia, sInfo, sNruchom, sMedia, sPaliwa,sSpoz, sWig)
#stopy wolne od ryzyka- od stopy zwrotu odejmuje sie stope wolna od ryzyka
stopy_bez_ryzyka<- stopy-(1/1200)

summary(stopy_bez_ryzyka)
summary(stopy)

#estymacja niezaleznych rownan, zmienna objasniana to stopa zwrotu danego indeksu, zmienna objasniajaca to WIG glowny
mBanki<- lm(stopy_bez_ryzyka$sBanki~stopy_bez_ryzyka$sWig) 
mBudownictwo<- lm(stopy_bez_ryzyka$sBudownictwo~stopy_bez_ryzyka$sWig) 
mChemia<- lm(stopy_bez_ryzyka$sChemia~stopy_bez_ryzyka$sWig) 
mEnergia<- lm(stopy_bez_ryzyka$sEnergia~stopy_bez_ryzyka$sWig) 
mInfo<- lm(stopy_bez_ryzyka$sInfo~stopy_bez_ryzyka$sWig) 
mNruchom<- lm(stopy_bez_ryzyka$sNruchom~stopy_bez_ryzyka$sWig) 
mMedia<-  lm(stopy_bez_ryzyka$sMedia~stopy_bez_ryzyka$sWig) 
mPaliwa<-  lm(stopy_bez_ryzyka$sPaliwa~stopy_bez_ryzyka$sWig) 
mSpoz<- lm(stopy_bez_ryzyka$sSpoz~stopy_bez_ryzyka$sWig) 

# wyodrebnienie alf i bet z wyestymowanych modeli 

alfy <- c(mBanki$coefficients[1],mBudownictwo$coefficients[1],mChemia$coefficients[1],mEnergia$coefficients[1 ],mInfo$coefficients[1],mNruchom$coefficients[1],mMedia$coefficients[1],mPaliwa$coefficients[1],mSpoz$coefficients[1]) 

bety <- c(mBanki$coefficients[2],mBudownictwo$coefficients[2],mChemia$coefficients[2],mEnergia$coefficients[2],mInfo$coefficients[2],mNruchom$coefficients[2],mMedia$coefficients[2],mPaliwa$coefficients[2],mSpoz$coefficients[2]) 

#reszty modelu
reszty<- data.frame(mBanki[2],mBudownictwo[2],mChemia[2],mEnergia[2],mInfo[2],mNruchom[2],mMedia[2],mPaliwa[2],mSpoz[2])

#obliczenie wariancji wig
wariancjaWIG<- sd(stopy$sWig)^2

#obliczenie sredniej WIG
sredniaWIG<- mean(stopy$sWig)

macierzKowariancji<- cov(reszty)

alfyTransponowane<- t(c(mBanki$coefficients[1],mBudownictwo$coefficients[1],mChemia$coefficients[1],mEnergia$coefficients[1],mInfo$coefficients[1],mNruchom$coefficients[1],mMedia$coefficients[1],mPaliwa$coefficients[1],mSpoz$coefficients[1]))

iloscOkresow<-152
iloscModeli<-9
iloscCzynnikow<-1

uvu <- (sredniaWIG^2)/wariancjaWIG 

pierwszyNawias <- iloscOkresow/iloscModeli
drugiNawias <- (iloscOkresow-iloscModeli-iloscCzynnikow)/(iloscOkresow-iloscCzynnikow-1) 
trzeciNawias <- (alfyTransponowane %*% macierzKowariancji %*% alfy)/(1+uvu)
GRS <- pierwszyNawias*drugiNawias*trzeciNawias

qf(0.95, iloscModeli, iloscOkresow-iloscModeli-iloscCzynnikow) 

