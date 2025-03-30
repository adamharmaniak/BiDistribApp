library(ggplot2)
library(plotly)
library(copula)
library(patchwork)
library(dplyr)
library(tibble)
library(MASS)
library(randomForest)
library(mgcv)
library(ks)
library(tidyr)
library(evd)
library(mvtnorm)
library(quantreg)
library(splines)
library(nnet)
library(ggridges)
library(shiny)
library(shinythemes)
library(rlang)


# Dokumentacia

## Co dosadit za parametre do *model_joint_distribution_density()*
#- **data** - dataset
#- **selected_variables** - premenne z datasetu (minimalne 2), c("...", "...", ...)
#- **model_type**
#  - *normalne ["normal"]*
#  - *Studentovo t-rozdelenie ["t"]*
#  - *jadrovo vyhladene ["kernel"]* - mozne zadat aj parameter **bw** (bandwidth), nezadany => automatizovane vyhladzovanie
#- **use_copula** - TRUE/FALSE - pouzit rozklad na marginaly a kopulu alebo nie
#- **copula_type**:
#  *Normalne rozdelenie ("normal")*
#  - Clayton: param > 0 (cim vacsie, tym silnejsia zavislost v spodnom chvoste)
#- Gumbel: param >= 1 (cim vacsie, tym silnejsia zavislost v hornom chvoste)
#- Frank: param != 0 (kladne = pozitivna zavislost, zaporne = negativna zavislost)
#*Studentovo t-rozdelenie "t"*
#  - Clayton
#- Gumbel
#- Joe
#*Jadrove vyhladzovanie "kernel"*
#  - empirical
#- **marginal_densities**:
#  - tvar: c("", "")
#- *"log_dnorm"*
#  - *"dnorm"*
#  
#  ## Co dosadit za parametre do *combine_conditional_models()*
#  - **data** - dataset
#- **selected_variables**
#  - c("Odozva", "Prediktor")
#- "Prediktor" musi byt spojita premenna
#- **mean_method**: 
#  - metody pre odhad podmienenej strednej hodnoty E[Y|X]
#a) Neparametricke modely
#1. *"loess"* (Lokalna regresia, prisposobuje modely (linearne, kvadraticke) podla okolia (75% bodov))
#2. *"gam"* (Generalizacny aditivny model, scitava hladke funkcie)
#3. *"spline"* (podobne principu MKP)
#b) Parametricke modely
#1. *"linear"* (linearny regresny model)
#2. *"poly"* (polynomialny regresny model, zadava sa aj s parametrom poly_mean_degree = 1,2,3,..., pre *poly_mean_degree = 1* dava          *"linear"*)
#3. *"exp"* (exponencialny regresny model)
#- **quantile_method**:
#  - metody pre odhad kvantilovych funkcii
#a)Neparametricke modely
#1. *"spline"* (podobne principu MKP)
#b) Parametricke modely
#1. *"linear"* (linearny regresny model)
#2. *"poly"* (polynomialny regresny model, zadava sa aj s parametrom poly_mean_degree = 1,2,3,..., pre *poly_mean_degree = 1* dava          *"linear"*)
#- **poly_mean_degree** - stupen polynomialneho odhadu E[Y|X] (pouziva sa iba pri metode *"poly"*)
#- **poly_quant_degree** - stupen polynomialneho odhadu Q[Y|X]_Q? (pouziva sa iba pri metode *"poly"*)
#- **quantiles** - kvantily, pre ktore sa vypocitaju Q[Y|X]_Q?, c(Q1, Q2, ...)
#- **specific_x** - konkretna hodnota prediktora X, pre ktoru sa vypocitaju body na Q[Y|X]_Q? a E[Y|X]
#
### Co dosadit za parametre do *classification_model()*
#- **data** - dataset
#- **response_name** - nazov premennej (odozvy), napr. "carb" (DISKRETNA)
#- **predictor_names** - nazvy premennych prediktorov, napr. c("hp", "cyl", ...) alebo iba jeden "hp"
#- **method**:
#  - *"logistic"* - spravi logisticku regresiu alebo multinomicku logisticku regresiu (podla poctu prediktorov)
#- *"lda"* - linearna diskriminacna analyza
#- *"qda"* - kvadraticka diskriminacna analyza
#- *"knn"* - metoda k-najblizsich susedov
#- **k** - kolko susedov bude brat *"knn"* metoda do uvahy
#
### Co dosadit za parametre do *plot_conditional_continuous_densities()*
#- **data** - dataset
#- **selected_variables** -> na vstup sa posielaju vzdy dve premenne vo forme c("Odozva", "Prediktor")
#- *"Odozva"* musi byt spojita premenna
#- **mean_curve** = TRUE/FALSE -> vykreslit strednu hodnotu alebo nie
#- **quantiles** -> pre ktore kvantily pocitat kvantilove funkcie 
#- **mean_poly_degree** -> stupen polynomialnej funkcie strednej hodnoty
#- **quantile_poly_degree** -> stupen polynomialnych kvantilovych funkcii
#- **n_breaks** -> pocet rozdeleni "okna", v resp. podmienenych hustot
#- **density_scaling** -> skalovanie hustot pre lepsi fitting do grafu
#- **normal_density** -> TRUE/FALSE, ne/vykreslit podmienenu hustotu norm. rozdelenia (dnorm())
#- **empirical_density** -> TRUE/FALSE, ne/vykreslit podmienenu empiricku hustotu (density(), beru sa do uvahy rezidualne hodnoty)
#
### Co dosadit za parametre do *plot_conditional_discrete_densities()*
#- **data** - dataset
#- **selected_variables** -> na vstup sa posielaju vzdy dve premenne vo forme c("Odozva", "Prediktor")
#- *"Odozva"* musi byt diskretna premenna
#- **n_breaks** -> pocet rozdeleni "okna", v resp. podmienenych pravdepodobnostnych funkcii
#- **density_scaling** -> skalovanie P-funkcii pre lepsi fitting do grafu
#- **normal_density** -> TRUE/FALSE, ne/vykreslit podmienenu hustotu norm. rozdelenia (dnorm())
#- **empirical_density** -> TRUE/FALSE, ne/vykreslit podmienenu empiricku hustotu (density(), beru sa do uvahy rezidualne

### Co dosadit za parametre do *plot_conditional_densities()*
#- **data** - dataset
#- **selected_variables** -> na vstup sa posielaju vzdy dve premenne vo forme c("Odozva", "Prediktor")
#- **mean_curve** = TRUE/FALSE -> vykreslit strednu hodnotu alebo nie
#- **quantiles** -> pre ktore kvantily pocitat kvantilove funkcie 
#- **mean_poly_degree** -> stupen polynomialnej funkcie strednej hodnoty
#- **quantile_poly_degree** -> stupen polynomialnych kvantilovych funkcii
#- **n_breaks** -> pocet rozdeleni "okna", v resp. podmienenych hustot
#- **density_scaling** -> skalovanie hustot pre lepsi fitting do grafu
#- **normal_density** -> TRUE/FALSE, ne/vykreslit podmienenu hustotu norm. rozdelenia (dnorm())
#- **empirical_density** -> TRUE/FALSE, ne/vykreslit podmienenu empiricku hustotu (density(), beru sa do uvahy rezidualne hodnoty)
#- ordinal -> TRUE/FALSE - spoji koncove pravdepodobnostne body v pripade diskretnej odozvy 
