# Copyright 2020 Georg Hosoya
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#Data######################################################
# Einlesen der lageso-Daten
dat<-read.csv("https://www.berlin.de/lageso/gesundheit/\
infektionsepidemiologie-infektionsschutz/corona/\
tabelle-bezirke-gesamtuebersicht/index.php/index/all.csv?q=", sep = ";")
################Globals
names(dat)

#Quelle: https://de.statista.com/statistik/daten/studie/1109841/umfrage/einwohnerzahl-bezirke-berlin/
Einwohnerzahlen=list(Berlin=3769000,
                     mitte=385748,
                     friedrichshain_kreuzberg=290386,
                     pankow=409335,
                     charlottenburg_wilmersdorf=343592,
                     spandau=245197,
                     steglitz_zehlendorf=310071,
                     tempelhof_schoeneberg=350984,
                     neukoelln=329917,
                     treptow_koepenick=273689,
                     marzahn_hellersdorf=269967,
                     lichtenberg=294201,
                     reinickendorf=266408)

# Formatierung des Datums
dat$datum <- as.Date(dat$datum, format=c("%Y-%m-%d"))

# Berechnung der Gesamtzahl der gemeldeten Fälle 
# für Berlin
dat$Berlin<-rowSums(dat[3:14])

####################### Functions ######################################
falldynamik<-function(dat, location, days, Einwohnerzahlen)
{ 

input_data<-dat[,which(names(dat)==location)]

window_size<-days-1 # Tage-1

max_data<-length(input_data)
gm<-NULL

for(offset in 1:(max_data-window_size))
{
  faelle_window<-input_data[offset:(offset+window_size)]
  gm[offset]<-mean(faelle_window)
}

date_axis1<-dat$datum[round(window_size/2):(offset-1+round(window_size/2))]

##############################
# Berechnung von Regressionen
# in einem x-Tages-Intervall 
cumsum_gm<-cumsum(gm)
input_data

max_data<-length(cumsum_gm)
faelle_pro_tag<-NULL

for(offset in 1:(max_data-window_size))
{
  faelle_window<-cumsum_gm[offset:(offset+window_size)]
  x<-seq(from=0, to=window_size, by=1)
  m1<-lm(faelle_window~x)
  faelle_pro_tag[offset]<-coefficients(m1)[2]
}

# Darstellung der Steigung der Regressionsgeraden
date_axis2<-date_axis1[round(window_size/2):(offset-1+round(window_size/2))]

#############################
# Berechnung und der Steigungsdifferenzen
# (Beschleunigung oder Bremsung) der neu gemeldeten Fälle
R3<-diff(faelle_pro_tag)
end_date2<-(length(date_axis2))
start_date2<-end_date2-(length(R3))+1

date_axis3<-date_axis2[start_date2:end_date2]

# Experimentell! ##############################
# Überprüfung der Vorhersage des nächsten ######
# x-Tages Mittelwertes auf Basis der
# vorherigen Fallzahl und der
# Beschleunigung
# predicted<-gm[(days+1):length(gm)-1]+R3
# actual<-gm[(days+1):length(gm)]
# length(predicted)
# length(actual)
# m3<-lm(predicted~actual)
# summary(m3)
# plot(predicted~actual)
fallzahl_morgen <- gm[length(gm)]+R3[length(R3)]
fallzahl_morgen # Vorhergesagter 7-Tages-Inzidenz
###############################################

Einwohner<-Einwohnerzahlen[[location]]
inzidenz_xd<-((gm/Einwohner)*(100000*days))
R3_std<-((R3/Einwohner)*(100000*days))
kritischer_wert<-(50*Einwohner/700000)
## DEBUG:
#title<-paste(days, "- Tages-Inzidenz")
#plot(date_axis1, inzidenz_xd, ylab=title, xlab="Datum", ylim=c(0,150))
#abline(h=50)
kritischer_wert
result<-list( datum=dat$datum, # Datum im Originaldatensatz  # auch benötigt für die x-Tages-Inzidenz
              cum_sum=cumsum(input_data), # kumulierte Fallzahl roh
              input_data=input_data ,# neu gemeldete Fälle pro Tag im Orignialdatesatz
              date_axis1=date_axis1, # Datums-Achse für den 7-Tages Mittelwert
              gm = gm, # 7-Tages-Mittelwert
              cumsum_gm = cumsum_gm, # kummulierter 7-Tages-Mittelwert
              date_axis2=date_axis2, # Datums-Achse für die regressionsgeglättete Geschwindigkeit
              faelle_pro_tag=faelle_pro_tag, # Regressionsgeglättete Geschwindigkeit
              date_axis3=date_axis3, # Datums-Achse für die Beschleunigung
              R3=R3, # Beschleunigung der Fälle pro Tag
              R3_std=R3_std, # Standardisierte Beschleunigung
              inzidenz_xd=inzidenz_xd, # x-Tages Inzidenz
              days=days, # Fenstergröße
              location=location, # location
              fallzahl_morgen = fallzahl_morgen # Experimentell: geschätzte Fallzahl
                                                #                morgen
            )
return(result)
}

# Kumulierte Fallzahl
plot_cumsum_raw<-function(result)
{
  plot(result$datum, result$cum_sum, type="l", 
       ylab="kumulierte Fallzahl", xlab="Datum", main=x$location)
}

# Neue Fälle pro Tag
plot_new_cases<-function(result)
{
  plot(result$datum, result$input_data, type="l", ylab="neu gemeldete Fälle pro Tag", 
     xlab="Datum",  main=x$location)
}

# x-Tages Mittelwert der neuen Fälle
plot_x_day_mean_cases<-function(result)
{
  title<-paste(result$days, "-Tages Mittelwert der gemeldeten Fälle")
  plot(result$date_axis1, result$gm, ylab=title, xlab="Datum", type="l", main=x$location)
}

# Regressionsgeglättete x-Tages Mittelwerte
plot_x_day_smoothed_cases<-function(result)
{
  plot(x$date_axis2, x$faelle_pro_tag, ylab="geglättete neue Fälle pro Tag", 
       xlab="Datum", type="l", main=x$location)
}

# Beschleunigung der Fälle pro Tag auf Basis
# der regressionsgeglätteten x-Tages Mittelwerte
plot_acc<-function(result)
{
  
  plot(x$date_axis3, x$R3, ylab="Beschleunigung der neuen Fälle", 
       xlab="Datum", type="l",  main=x$location)
  abline(h=0)
}
# Experimentell: standardisierte Beschleunigung
plot_acc_std<-function(result, ylim)
{
  
  plot(x$date_axis3, x$R3_std, ylab="standardisierte Beschleunigung", 
       xlab="Datum", type="l",  main=x$location, ylim=ylim)
  abline(h=0)
}

# x-Tages-Inzidenz
plot_x_day_incidence<-function(x, max_ylim)
{
  title<-paste(days, "- Tages-Inzidenz")
  plot(x$date_axis1, x$inzidenz_xd, ylab=title, xlab="Datum", ylim=c(0,max_ylim), type="l",  main=x$location)
  abline(h=50)
}

# Anwendung ######################################################
# Wahl des interessierenden Bezirks, bzw. Berlin

names(dat)
location="neukoelln"
# Wahl der Fenstergröße (experimentell) (min=3)
days=7
# Übergabe an die Hauptfunktion
par(mfrow=c(1,1))
x<-falldynamik(dat, location, days, Einwohnerzahlen)
# Plots
# Kumulierte Fallzahlen
plot_cumsum_raw(x)
# Neue Fälle
plot_new_cases(x)
# x-Tages-Mittelwert
plot_x_day_mean_cases(x)
# Regressionsgeglättete-x-Tages-Inzidenz
plot_x_day_smoothed_cases(x)
# Beschleunigung der Fälle pro Tag auf Basis
# der regressionsgeglätteten x-Tages Mittelwerte
plot_acc(x)
x$R3 # Auslesen der Beschleunigung aus dem Objekt
# Abbildung der x-Tages-Inzidenz
plot_x_day_incidence(x,400)
x$inzidenz_xd
# Experimentell: Abbildung der standardisierten 
# Beschleunigung
plot_acc_std(x, ylim=c(-30,30))

##############################################
# Charts Beispiel 
incidence<-NULL;
days=7
location_names<-names(dat)[3:15]
par(mfrow=c(3,4))
for(i in 1:(length(location_names)-1))
{
  current_loc<-location_names[i]
  x<-falldynamik(dat, current_loc, days, Einwohnerzahlen)
  plot_x_day_incidence(x,400)
  # plot_acc_std(x, ylim=c(-20,20)) # Experimentell
  #  plot_acc(x)
  # incidence<-cbind(incidence, x$inzidenz_xd)
}


