rm(list=ls()) # pulisco l'ambiente di lavoro

library(rstudioapi) # carico le librerie che mi serviranno
library(stats)

current_path <- getActiveDocumentContext()$path # prendo il percorso della cartella di lavoro
setwd(dirname(current_path )) # setto l'ambiente di lavoro alla cartella
print(getwd()) # stampo per esserne sicuro

data <- read.csv("Prova UN_Data.csv", header = TRUE)

## Metto i nomi dei paesi come nomi delle righe
row.names(data) = data[,1]
View(data)

## Vedo il tipo delle variabili
str(data)

## Trasformo in numeric i character e gli int
data_num <- as.data.frame(apply(data, 2, as.numeric))
sapply(data_num, class)

## Rimetto i nomi dei paesi alle righe
row.names(data_num) = data[,1]
View(data_num)

## Creo dataset finale
def_data = data_num[,-c(1,2)]
View(def_data)
colnames(def_data)[c(1:8)] <- c("GDPc", "Infant mortality rate", "Health GDP", "Physicians rate",
                                "Education GDP", "Threatned species", "CO2em", "Quality of water")
row.names(def_data) = data[,1]


### Missing values
library(dplyr)
library(tidyr)
library(zoo)
library(cluster)


### Cluster Analysis
d3 <- dist(def_data, method="canberra")  # canberra è più robusta nei confronti di outliers
summary(d3)


dev.new()
dendro_ave <- hclust(d3, method='ward.D2') # rappresento graficamente l'analisi dei cluster e noto 4 tagli principali
plot(dendro_ave,cex=0.5)
gruppi=rect.hclust(dendro_ave, k=4)


gr4 = cutree(dendro_ave, k=4) # Estraggo i 4 gruppi 


gruppo1=def_data[which(gr4==1),1:8] 
View(gruppo1)
summary(gruppo1)

gruppo2=def_data[which(gr4==2),1:8] 
View(gruppo2)
summary(gruppo2)

gruppo3=def_data[which(gr4==3),1:8] 
View(gruppo3)
summary(gruppo3)

gruppo4=def_data[which(gr4==4),1:8] 
View(gruppo4)
summary(gruppo4)

plot = boxplot(gruppo1$`Physicians rate`, gruppo2$`Physicians rate`,gruppo3$`Physicians rate`, gruppo4$`Physicians rate`)
dev.new()
plot2 = boxplot(gruppo1$GDPc, gruppo2$GDPc, gruppo3$GDPc, gruppo4$GDPc)

def_data$gruppi <- c(gr4) ### creato una colonna in cui si vedono i gruppi come colonna
View(def_data)



### ANOVA sui gruppi per ogni variabile

fit1 <- aov(GDPc ~ gruppi, def_data)
fit2 <- aov(Infant_mort_rate ~ gruppi, def_data)
fit3 <- aov(Health_expGDP ~ gruppi, def_data)
fit4 <- aov(Physicians_rate ~ gruppi, def_data)
fit5 <- aov(def_data$`Education GDP` ~ gruppi, def_data)
fit6 <- aov(def_data$`Threatned species` ~ gruppi, def_data)
fit7 <- aov(CO2_em ~ gruppi, def_data)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
summary(fit7)




### Validazione numero di cluster (svolta prima dell'inserimento dei valori medi e mediani)
install.packages("NbClust")
library(NbClust)
clust = NbClust(data = def_data, method = "ward.D2")

### Inserisco i valori mediani in base ai gruppi

# GDPc
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(GDPc = ifelse(is.na(GDPc), median(GDPc, na.rm =TRUE), GDPc))

# Infant mortality rate
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(`Infant mortality rate` = ifelse(is.na(`Infant mortality rate`), median(`Infant mortality rate`, na.rm =TRUE),
                                          `Infant mortality rate`))
# Health GDP
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(`Health GDP` = ifelse(is.na(`Health GDP`), median(`Health GDP`, na.rm =TRUE),
                                          `Health GDP`))
# Physician rate
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(`Physicians rate` = ifelse(is.na(`Physicians rate`), median(`Physicians rate`, na.rm =TRUE),
                               `Physicians rate`))
# Education GDP
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(`Education GDP` = ifelse(is.na(`Education GDP`), median(`Education GDP`, na.rm =TRUE),
                               `Education GDP`))
# Threatned species
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(`Threatned species` = ifelse(is.na(`Threatned species`), median(`Threatned species`, na.rm =TRUE),
                               `Threatned species`))
# CO2em
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(CO2em = ifelse(is.na(CO2em), median(CO2em, na.rm =TRUE),
                               CO2em))
# Quality of water
def_data = def_data %>% 
  group_by(gruppi) %>% 
  mutate(`Quality of water` = ifelse(is.na(`Quality of water`), median(`Quality of water`, na.rm =TRUE),
                               `Quality of water`))

def_data = def_data[,-c(8)] ## abbiamo rimosso quality of water


### Regressione lineare
## Estraggo le variabili
library(MASS)

GDPc = def_data$GDPc
boxplot(GDPc) 
hist(GDPc)

Infant_mort_rate = def_data$`Infant mortality rate`
summary(Infant_mort_rate)
boxplot(Infant_mort_rate) 
hist(Infant_mort_rate)

Health_expGDP = def_data$`Health GDP`
boxplot(Health_expGDP) 
hist(Health_expGDP)

Physicians_rate = def_data$`Physicians rate`
boxplot(Physicians_rate) 
hist(Physicians_rate)

CO2_em = def_data$CO2em
boxplot(CO2_em) 
hist(CO2_em)

ln1 <- lm(Infant_mort_rate ~ GDPc + Health_expGDP + Physicians_rate + CO2_em)
summary(ln1)

########################################
########################################

### Verifica delle ipotesi della regressione lineare
install.packages("lmtest")
library(lmtest)

## In primo luogo verifichiamo che la media degli errori non sia significativamente diversa da zero attuando il
## test t di Student:
residui <- residuals(ln1) ## vettore dei resuidi
t.test(residui)

## Successivamente verifichiamo la normalità della distribuzione degli errori con il test di Shapiro-Wilk:
shapiro <- shapiro.test(residui)
shapiro

## Graficamente si può usare anche il QQ Plot:
qqnorm(residui)
qqline(residui)

## Proseguiamo con il verificare l’omoschedasticità dei residui utilizzando il test di Breusch-Pagan:
model = formula(ln1) # memorizziamo la formula del modello per sempliciità
test_bp = bptest(model, data = def_data)
test_bp

## Verifichiamo l’assenza di correlazione seriale tramite il test di Durbin-Watson:
dw = dwtest(model, data = def_data)
dw

## Traformazione Box Cox
library(MASS)
bc <- boxcox(ln1, lambda = seq(-3,3))
lambda <- bc$x[which.max(bc$y)] ## prendere il valore migliore per fare la trasformazione
Infant_mort_rate_bc = (Infant_mort_rate^lambda-1)/lambda

## Prova con il nuovo modello
ln2 <- lm(Infant_mort_rate_bc ~ GDPc + Health_expGDP + Physicians_rate + CO2_em)
summary(ln2)


## Riverifichiamo che la media degli errori non sia significativamente diversa da zero attuando il
## test t di Student:
residui2 <- residuals(ln2)
t.test(residui2)

## Riverifichiamo la normalità della distribuzione degli errori con il test di Shapiro-Wilk:
## Rappresentiamolo graficamente:
install.packages("olsrr")
library(olsrr) ## libreria per test sulla regressione lineare
normtest <- shapiro.test(residui2)
normtest
qqnorm(residui2)
qqline(residui2)


ols_test_normality(ln2) # conduce 4 test sulla normalità
ols_plot_resid_hist(ln2) # istogramma dei residui

## Proseguiamo con il verificare l’omoschedasticità dei residui utilizzando il test di Goldfeld-Quandt:
model2 <- formula(ln2)
gqtest(ln2) 


## Verifichiamo l’assenza di correlazione seriale tramite il test di Durbin-Watson:
dwtest(model2)


## Verifichiamo la collinerarità tramite la funzione VIF
library(car)
vif(ln2)

### Rappresentazione 3D modello lineare

install.packages("plotly")
library(plotly)

colors <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733") ## scelto 4 colorazioni per i nostri 4 gruppi
colors <- colors[as.numeric(def_data$gruppi)] ## assegnazione dei colori ai 4 gruppi

fig <- plot_ly(def_data, x = ~GDPc, y = ~Infant_mort_rate_bc, z = ~Physicians_rate,
               marker = list(color = ~colors, showscale = FALSE))


fig <- fig %>% layout(scene = list(xaxis = list(title = 'PIL pro capite'),
                                   yaxis = list(title = 'Tasso mortalità infantile'),
                                   zaxis = list(title = 'Medici per mille abitanti')))
                   

fig


### Analisi componenti principali
library(stats)
install.packages("factoextra")
library(factoextra)

### Dataset per PCA tenendo conto di tutte le variabili
data_active = def_data[c(1,2,3,4,5,6,7)]

### Dataset per PCA tenendo conto delle variabili significative della regressione + la Y
data_active2 = data_active[c(1,2,4)]

row.names(data_active) = data[,1] ## Inserimento dei nomi dei paesi
row.names(data_active2) = data[,1] ## Inserimento dei nomi dei paesi

### PCA dataset completo
pca <- prcomp(data_active, scale. = TRUE)
summary(pca)
### PCA tenendo conto delle variabili significative della regressione + la Y
pca2 <- prcomp(data_active2, scale. = TRUE)
summary(pca2)

### Scree plot
fviz_eig(pca)
fviz_eig(pca2)


### Plot PCA
fviz_pca_ind(pca,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    # Evita la sovrapposizione di testo 
)

fviz_pca_ind(pca2,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Evita la sovrapposizione di testo
)



fviz_pca_var(pca,
             col.var = "contrib", # Colore in base al contributo alla PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Evita la sovrapposizione di testo
)

fviz_pca_var(pca2,
             col.var = "contrib", # Colore in base al contributo alla PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Evita la sovrapposizione di testo
)


fviz_pca_biplot(pca, repel = TRUE, habillage=def_data$gruppi, # Suddivisione in gruppi
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_bw()
)

fviz_pca_biplot(pca2, repel = TRUE, habillage=def_data$gruppi, # Suddivisione in gruppi
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_bw()
)



### Contributo delle variabili sulle PCA
res.var <- get_pca_var(pca)
res.var$contrib        

res.var2 <- get_pca_var(pca2)
res.var2$contrib

