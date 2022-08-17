# Comparaci-n-de-la-actividad-de-la-colinesterasa-en-lombrices-en-distintas-pr-cticas-agr-colas-en-el
In agricultural activities, the use of pesticides is useful for controlling them, the  organophosphates and carbamates are among the most used chemical compounds.  Earthworms have receptors on their bodies that cause sensitivity to these compounds.  Therefore, in the study of the impact of pesticides on the soil, the cholinesterase activity  biomarker in Lumbricus Terrestris earthworms is very useful in determining the severity of  soil damage. For this reason, the present investigation seeks to compare the magnitude of  cholinesterase activity between each type of land use practice given the climatic season.

  ## Librerias
 
```{r}
library(car)
library(ggplot2)
```
 
 
  ## Datos

```{r}
load("colinesterasa.Rdata")
str(colinesterasa)
```

  
  ## Acerca del experimento
  
  Tomar en cuenta que el diseño que se esta utilizando es factorial y es desbalanceado. En este caso el factor de diseño es el tipo de práctica en el suelo [ bosque (control), BPA, convencional y organico ], y queremos ver si hay un efecto del tipo de práctica en el suelo sobre la actividad colinestarasa en las lombrices. Seguidamente se quiere verificar si el efecto del tipo de práctica en el suelo sobre la actividad colinesterasa de las lombrices es el mismo independientemente de la estacion en la que se este (verano e invierno).

  ## Análisis de supuestos
  
  `1. Supuesto de independencia`
  
  En este caso, como los datos no se estan midiendo en el tiempo, no hay un efecto de aprendizaje o fatiga a lo largo del tiempo, por lo que se puede asumir independencia en las observaciones, ya que las respuestas de las unidades experimentales no se encuentran correlacionadas en un mismo tratamiento. 
  
  `2. Supuesto de homocedasticidad`
  
  - Grafico para visualizar la variabilidad entre cada tratamiento incluyendo ambos factores(tipo de práctica en el suelo y estación).
  
```{r}
media = mean(colinesterasa$ChE)
boxplot(ChE ~ practica + estacion, data = colinesterasa, ylab = "Actividad colinesterasa", xlab = "Tratamientos", names = c("Bosq-Inv,", "BPA-Inv", "Conv-Inv", "Org-Inv","Bosq-Ver,", "BPA-Ver", "Conv-Ver", "Org-Ver"))
abline(h = media, col=2)
```
  
  Se observa que la variabilidad para el tratamiento bosque-invierno, es considerablemente mayor a los demas tratamientos, por lo que esto puede ser una posible evidencia de heterocedasticidad.
  
  
  - Prueba de hipótesis de homocedasticidad
  
```{r}
mod = lm(ChE ~ practica + estacion + practica*estacion, colinesterasa)
bartlett.test(mod$residuals ~ interaction(colinesterasa$practica, colinesterasa$estacion))
```
  
  Con una significancia del 5%, hay suficiente evidencia estadistica para rechazar la H0 de homocedasticidad. Es decir, no podemos asumir varianzas iguales entre los tratamientos.
  
  Solucion: *Minimos cuadrados ponderados, esto por que hay normalidad.*
  
  `3. Supuesto de normalidad`
  
  Ajuste del modelo (modelo lineal: tratamiento de referencia) y qqPlot
  
```{r}
qqPlot(mod$res, ylab = "Residuales", xlab = "Cuantiles de la distribución normal")
```
  
  Al comparar los residuales con los cuantiles teóricos de una distribución normal, todos ellos se colocan muy cerca de una línea recta, esto indica que hay una proporcionalidad entre ellos, con lo cual se puede asumir que provengan de una distribución normal.


  + Minimos cuadrados ponderados
  
  Las observaciones en los tratamientos con varianzas pequeñas dan informacion mas confiable sobre los efectos existentes en la respuesta que aquellas con varianzas grandes. Por lo que este metodo da un peso mayor a aquellas observaciones que provienen de tratamientos con menor varianza.
  
  Las estimaciones de los promedios no difieren entre los minimos cuadrados ponderados y no ponderados.
  
  Al utilizar este metodo no se logra homocedasticidad sino que se trabaja bajo condiciones de heterocedasticidad pero dando peso a las observaciones segun la variabilidad observada en cada tratamiento.
  
  *Proceso de estimacion*
  
  `1. Se calcula la varianza para cada tratamiento.`
  `2. Se usan como ponderaciones los inversos de las varianzas de los tratamientos.`
  
```{r}
v = tapply(colinesterasa$ChE, list(colinesterasa$practica, colinesterasa$estacion), var)
w = 1/v

# Se introduce el vector de pesos a la base de datos, para seguidamente usarlo en el modelo.

vecw = c(rep(w[2,2], 13), rep(w[4,2], 13), rep(w[1,2], 5), rep(w[3,2], 5),rep(w[2,1], 13), rep(w[4,1], 8), rep(w[1,1], 11), rep(w[3,1], 11))
colinesterasa$vec = vecw
```
  
  
  `3. Se ajusta el modelo usando estos pesos con el argumento weights = vecw. A partir de aca las estimaciones se haran con el el modelo lineal: suma nula, esto para efectos de interpretaciones.`
  
```{r}
options(contrasts=c("contr.sum","contr.poly"))
modw = lm(ChE ~ practica*estacion, colinesterasa, weights = vecw)
```

  
  ## Inicio del análisis
  
  *Estimacion de la media general de actividad colinesterasa en lombrices*
  
```{r}
media
```
  
  La media general del nivel de actividad colinesterasa en lombrices es de 209.6706.
  
  *Estimacion de la varianza para cada tratamiento*
  
```{r}
v
```
  
  - Grafico para ver la variabilidad entre cada estacion
  
```{r}
boxplot(ChE ~ estacion, colinesterasa)
abline(h = media, col=2)
```
  
  
  
  *Analisis de interaccion entre el tipo de practica en el suelo y la estacion*

  - Grafico
    
```{r}
ggplot(colinesterasa, aes(x = estacion, y = ChE, group = practica)) +
  stat_summary(fun = "mean", geom = "line", aes(linetype = practica, color = practica)) +
  labs(x = "Estación", y = "Actividad colinesterasa") + 
  theme_classic() 
```
  
   Se observa que la distancia entre la actividad colinesterasa cuando el tipo de practica en el suelo es BPA con respecto a la media cuando el tipo practica en el suelo es convencional en invierno es menor a la distancia que hay entre la media de las mismas practicas de uso de suelo antes mencionadas pero para verano. Esto es evidencia de una posible interaccion entre el tipo de practica en el suelo y la estacion.
  
  - Efectos simples y efectos de interaccion
  
  $$\alpha_i=practica$$
  
  $$\beta_j=estacion$$
  
```{r}
# A pie
m1 = tapply(colinesterasa$ChE, colinesterasa$practica, mean)
alpha1 = m1[1] - media 
alpha2 = m1[2] - media 
alpha3 = m1[3] - media 
alpha4 = m1[4] - media 

alpha = cbind(alpha1, alpha2, alpha3, alpha4)
colnames(alpha) = c("Bosque", "BPA", "Convencional", "Organico")
round(alpha, 3)

m2 = tapply(colinesterasa$ChE, colinesterasa$estacion, mean)
beta1 = m2[1] - media
beta2 = m2[2] - media


beta = cbind(beta1, beta2)
colnames(beta) = c("Verano", "Invierno")
round(beta, 3)

mCI = tapply(colinesterasa$ChE, list(colinesterasa$practica, colinesterasa$estacion), mean)

y11 = media + alpha[1] + beta[1] # bosque-verano
y21 = media + alpha[2] + beta[1] # bpa-verano
y31 = media + alpha[3] + beta[1] # convenciona-verano
y41 = media + alpha[4] + beta[1] # organico-verano
y12 = media + alpha[1] + beta[2] # bosque-invierno
y22 = media + alpha[2] + beta[2] # bpa-invierno
y32 = media + alpha[3] + beta[2] # convencional-invierno
y42 = media + alpha[4] + beta[2] # organico-invierno 

mSI = matrix(c(y11, y21, y31, y41, y12, y22, y32, y42), nrow = 4,
                   dimnames = list(c("Bosque", "BPA", "Convencional", "Organico"),
                                   c("invierno","verano")))

promedios = cbind(as.vector(mCI), as.vector(mSI))
colnames(promedios) = c("Observados", "Estimados")
rownames(promedios) = c("bosque-verano", "BPA-verano", "convencional-verano", "organico-verano", "bosque-invierno", "BPA-invierno", "convencional-invierno", "organico-invierno")
promedios

efectint = mCI - mSI
efectint
```

  
  
  - Prueba de hipotesis de no interaccion
  
  $$H0:(\hat{\alpha\beta})_{ij}=0$$
  
```{r}
anova(modw)
```
  
  Con una significancia del 5%, hay suficiente evidencia estadistica para rechazar la H0 de no interaccion. Es decir, no hay evidencia para asumir que el tipo de practica de uso en el suelo es independiente de la estacion por lo que la estimacion de los intervalos de confianza debe ser por el metodo de intervalos simultaneos de Bonferroni.
  
  
  *Prueba de hipotesis de igualdad de medias*
  
H0: Los promedio entre los tipos de practica de uso en el suelo son iguales.
H1: Al menos un promedio entre los tipos de practica de uso en el suelo es diferente.
  
```{r}
anova(modw)
```
  
  Con una significancia del 5%, hay suficiente evidencia estadistica para rechazar la H0 de que el nivel de actividad colinesterasa es el mismo para cada tipo de practica de uso en el suelo, es decir, hay evidencia de que hay diferencias en la actividad colinesterasa para cada tipo de practica de uso en el suelo. Lo mismo sucede con las estacion. 
  
  *Comparaciones bajo el modelo con interaccion con varianzas diferentes.*
  
  Cuando se rechaza la hipotesis de igualdad de medias es importante construir intervalos de confianza.
  
  Se pueden construir intervalos de confianza pero en el calculo del error estandar no se debe usar el CMRes sino que *se debe respetar que las varianzas no se asumen iguales.*

  
  El modelo es: 
  
  $$\mu_{ij}=\mu+\alpha_i+\beta_j+(\alpha\beta)_{ij}$$
  
  Como el factor de interes es el tipo de practica de uso del suelo, se fija la estacion y se compara diferentes tipos de practica de uso del suelo dada cada estacion. Para tener estimaciones positivas de las diferencias de promedios, se crean los contrastes de tal forma que el contraste estimado sea positivo, es decir, se pone primero aquella media cuya estimacion sea mayor.
  
```{r}
modw$coef
mCI
```
  
  `1. Ortogonalidad`
  
  Contrastes para invierno
  
  1. $\mu_{21}-\mu_{11}$ 
  2. $\mu_{21}-\mu_{31}$ 
  3. $\mu_{21}-\mu_{41}$ 
  4. $\mu_{11}-\mu_{31}$
  5. $\mu_{11}-\mu_{41}$
  6. $\mu_{41}-\mu_{31}$
  
  Tomando el vector de promedios
  
  $$(\mu_{11},\mu_{21},\mu_{31}, \mu_{41},\mu_{12},\mu_{22},\mu_{32},\mu_{42})$$
  
  - Los vectores para obtener el primer grupo de hipotesis (invierno) son:
  
```{r}
v1 = c(-1, 1, 0, 0, 0, 0, 0, 0)
v2 = c(0, 1, -1, 0, 0, 0, 0, 0)
v3 = c(0, 1, 0, -1, 0, 0, 0, 0)
v4 = c(1, 0, -1, 0, 0, 0, 0, 0)
v5 = c(1, 0, 0, -1, 0, 0, 0, 0)
v6 = c(0, 0, -1, 1, 0, 0, 0, 0)
c(v1%*%v2, v1%*%v3, v1%*%v4, v1%*%v5, v1%*%v6, v2%*%v3, v2%*%v4, v2%*%v5, v2%*%v6, v3%*%v4, v3%*%v5, v3%*%v6, v4%*%v5, v4%*%v6, v5%*%v6)
```
  
  Estos 6 vectores no son ortogonales para el bloque de invierno.
  
  
  Contrastes para verano
  
  7. $\mu_{12}-\mu_{22}$
  8. $\mu_{12}-\mu_{32}$
  9. $\mu_{12}-\mu_{42}$
  10. $\mu_{22}-\mu_{32}$
  11. $\mu_{22}-\mu_{42}$
  12. $\mu_{42}-\mu_{32}$  
  
Tomando el vector de promedios
  
  $$(\mu_{11},\mu_{21},\mu_{31}, \mu_{41},\mu_{12},\mu_{22},\mu_{32},\mu_{42})$$
  
  - Los vectores para obtener el segundo grupo de hipotesis es el siguiente:
  
```{r}
v7 = c(0, 0, 0, 0, 1, -1, 0, 0)
v8 = c(0, 0, 0, 0, 1, 0, -1, 0)
v9 = c(0, 0, 0, 0, 1, 0, 0, -1)
v10 = c(0, 0, 0, 0, 0, 1, -1, 0)
v11 = c(0, 0, 0, 0, 0, 1, 0, -1)
v12 = c(0, 0, 0, 0, 0, 0, -1, 1)

c(v7%*%v8, v7%*%v9, v7%*%v10, v7%*%v11, v7%*%v12, v8%*%v9, v8%*%v10, v8%*%v11, v8%*%v12, v9%*%v10, v9%*%v11,v9%*%v12, v10%*%v11, v10%*%v12, v11%*%v12)
```
  
  Estos 6 vectores no son ortogonales para el bloque de verano.
  
  
  - Ahora comparamos los dos bloques entre si.
  
```{r}
c(v1%*%v7, v1%*%v8, v1%*%v9, v1%*%v10, v1%*%v11, v1%*%v12, v2%*%v7, v2%*%v8, v2%*%v9, v2%*%v10, v2%*%v11, v2%*%v12, v3%*%v7, v3%*%v8, v3%*%v9, v3%*%v10, v3%*%v11, v3%*%v12, v4%*%v7, v4%*%v8, v4%*%v9, v4%*%v10, v4%*%v11, v4%*%v12, v5%*%v7, v5%*%v8, v5%*%v9, v5%*%v10, v5%*%v11, v5%*%v12, v6%*%v7, v6%*%v8, v6%*%v9, v6%*%v10, v6%*%v11, v6%*%v12)
```
  
  El primer grupo de vectores si es ortogonal con respecto al segundo grupo de vectores. Por lo que dentro de cada grupo de hipotesis debe hacerce la correccion de bonferroni, pero cada grupo de hipotesis se puede probar de forma independiente al otro grupo. Por lo tanto, se debe usar correccion de bonferroni tomando en cuenta que en cada caso son 6 hipotesis.
  
  `2. Matriz de contrastes`
  
  - Vectores para obtener los promedios de los tratamientos.
  
```{r}
contrasts(colinesterasa$practica)
modw$coef
```
  
  Bosque-Invierno: [1, 1, 0, 0, 1, 1, 0, 0]
  
  BPA-Invierno: [1, 0, 1, 0, 1, 0, 1, 0]
  
  Convencional-Invierno: [1, 0, 0, 1, 1, 0, 0, 1]
  
  Organico-Invierno: [1, -1, -1, -1, 1, -1, -1, -1]
  
  Bosque-Verano: [1, 1, 0, 0, -1, -1, 0, 0]
  
  BPA-Verano: [1, 0, 1, 0, -1, 0, -1, 0]
  
  Convencional-Verano: [1, 0, 0, 1, -1, 0, 0, -1]
  
  Organico-Verano: [1, -1, -1, -1, -1, 1, 1, 1]
  
```{r}
bosq.in = c(1, 1, 0, 0, 1, 1, 0, 0)
bpa.in = c(1, 0, 1, 0, 1, 0, 1, 0)
con.in = c(1, 0, 0, 1, 1, 0, 0, 1)
org.in = c(1, -1, -1, -1, 1, -1, -1, -1)
bosq.ver = c(1, 1, 0, 0, -1, -1, 0, 0)
bpa.ver = c(1, 0, 1, 0, -1, 0, -1, 0)
con.ver = c(1, 0, 0, 1, -1, 0, 0, -1)
org.ver = c(1, -1, -1, -1, -1, 1, 1, 1)

h = cbind(bosq.in, bpa.in, con.in, org.in, bosq.ver, bpa.ver, con.ver, org.ver)

# Aqui verifico que los vectores esten bien construidos.
mCI
t(h)%*%modw$coefficients 
```
  
  
  - Para hacer el calculo de los contrastes, se deben restar los dos vectores que producen cada una de las medias del contraste.
  

```{r}
bpabosq.in = bpa.in - bosq.in
bpacon.in = bpa.in - con.in
bpaorg.in = bpa.in - org.in
bosqcon.in = bosq.in - con.in
bosqorg.in = bosq.in - org.in
orgcon.in = org.in - con.in
bosqbpa.ver = bosq.ver - bpa.ver
bosqcon.ver = bosq.ver - con.ver
bosqorg.ver = bosq.ver - org.ver
bpacon.ver = bpa.ver - con.ver
bpaorg.ver = bpa.ver - org.ver
orgcon.ver = org.ver - con.ver

h1 = cbind(bpabosq.in, bpacon.in, bpaorg.in, bosqcon.in, bosqorg.in, orgcon.in, bosqbpa.ver, bosqcon.ver, bosqorg.ver, bpacon.ver, bpaorg.ver, orgcon.ver)
```
  
  
  - Estimacion del contraste y varianza.
  
```{r}
L = t(h1)%*%modw$coefficients
L # Aqui verifico que todos esten dando positivos

var = diag(t(h1)%*%vcov(modw)%*%h1)
var
```
  
  Todas las varianzas son desiguales por que se esta trabajando con heterocedasticidad, y además el diseño es desbalanceado.
  
  
  - Prueba simultanea de hipotesis.
  
  Las probabilidades deben compararse conta $\alpha/d$ donde $d=6$ y $alpha=0.05$ es el numero de intervalos de confianza que se desean construir.
  
```{r}
options(scipen = 999)
ee = sqrt(var)
t = L/ee
row.names(t) = row.names(L)
anova(modw)[4,1]
p = pt(t, 71, lower.tail = F)
row.names(p) = row.names(L)

# Aqui observamos que hipotesis de diferencias de medias se estan rechanzado ajustando el nivel de significancia

p < 0.05/6
```
  
  Se concluye que en invierno el nivel de actividad colinesterasa en lombrices es diferente cuando el tipo de practica de uso del suelo es BPA y convencional, BPA y organico, y bosque y convencional, mientras que cuando es verano, se concluye que el nivel de actividad colinesterasa es diferente cuando el tipo de practico de uso del suelo es bosque y convencional, BPA y convencional, y organico y convencional.
  
  
  - Calculo de cotas inferiores
  
```{r}
tc = qt(1-0.05/3, 71) # El 3 es el numero de intervalos que se van a construir para invierno y verano

ICI = cbind(L[2:4]-tc*ee[2:4], L[2:4]+tc*ee[2:4])
colnames(ICI) = c("ICII", "ICSI")
ICIV = c(L[8]-tc*ee[8], L[10]-tc*ee[10], L[12]-tc*ee[12])
ICSV = c(L[8]+tc*ee[8], L[10]+tc*ee[10], L[12]+tc*ee[12])
ICV = cbind(ICIV, ICSV)
round(ICI, 2)
round(ICV, 2)
```
  
  *Interpretaciones para invierno*
  
  Se concluye, con 95% de confianza, que el nivel de actividad colinesterasa en lombrices en invierno es al menos 153.96 unidades mayor cuando el tipo de practica de uso del suelo es BPA que cuando es convencional.
  
  Se concluye, con 95% de confianza, que el nivel de actividad colinesterasa en lombrices en invierno es al menos 36.32 unidades mayor cuando el tipo de practica de uso del suelo en BPA que cuando es organico.
  
  Se concluye, con 95% de confianza, que el nivel de actividad colinesterasa en lombrices en invierno es al menos 46.60 unidades mayor cuando el tipo de practica de uso del suelo es bosque que cuando es convencional.
  
  
  *Interpretaciones para verano*
  
  Se concluye, con 95% de confianza, que el nivel de actividad colinesterasa en lombrices en verano es al menos 87.53 unidades mayor cuando el tipo de practica de uso del suelo es bosque que cuando es convencional.
  
  Se concluye, con 95% de confianza, que el nivel de actividad colinesterasa en lombrices en verano es al menos 74.71 unidades mayor cuando el tipo de practica de uso en el suelo es BPA que cuando es convencional.
  
  Se concluye, con 95% de confianza, que el nivel de actividad colinesterasa en lombrices en verano es al menos 57.03 unidades mayor cuando el tipo de practica de uso del suelo es organico que cuando es convencional.


  + Tabla de los intervalos de confianza para cada tipo de practica de uso en el suelo en invierno y verano
  
```{r}
dif = data.frame(practica = as.factor(c("*BPA-Convencional", "*BPA-Convencional", "*Bosque-Convencional", "*Bosque-Convencional", "BPA-Organico", "Organico-Convencional")), Estacion = c("Invierno", "Verano", "Invierno", "Verano",  "Invierno", "Verano"), Li = c(153.96, 74.71, 46.60, 87.53, 36.32, 57.03), Ls = c(301.68, 167.70, 250.20, 242.11, 249.82, 172.10))
dif
```
  

  ## Potencia de la prueba 


  1) Planteamiento de la simulacion
 
```{r}
r = c(11, 5, 13, 13, 8, 13, 11, 5)

mu = c(90, 120, 130, 140, 150, 160, 170, 190)
mus = c(rep(mu[1], 11), rep(mu[2], 5), rep(mu[3], 13), rep(mu[4], 13), rep(mu[5], 8), rep(mu[6], 13), rep(mu[7], 11), rep(mu[8], 5))
var = c(4186.39, 1034.751,  10110.51, 3276.529, 13134.77, 6446.170, 20017.39, 5307.274)
vars = c(rep(var[1], 11), rep(var[2], 5), rep(var[3], 13), rep(var[4], 13), rep(var[5], 8), rep(var[6], 13), rep(var[7], 11), rep(var[8], 5))
n = sum(r)

y = rnorm(n, mus, sqrt(vars))

A = factor(c(rep('convencional', r[1]), rep('convencional',r[2]), rep('BPA', r[3]), rep('BPA', r[4]), rep('organico', r[5]), rep('organico', r[6]), rep('bosque', r[7]), rep('bosque', r[8])))

B = factor(c(rep('invierno', r[1]), rep('verano',r[2]), rep('invierno', r[3]), rep('verano', r[4]), rep('invierno', r[5]), rep('verano', r[6]), rep('invierno', r[7]), rep('verano', r[8])))

```
 
 
 ## Funcion 
  
```{r}
med = function(r, mus, v = 1) {
  
  n = sum(r)
  
  y = rnorm(n, mus, sqrt(vars))
  
A = factor(c(rep('convencional', r[1]), rep('convencional',r[2]), rep('BPA', r[3]), rep('BPA', r[4]), rep('organico', r[5]), rep('organico', r[6]), rep('bosque', r[7]), rep('bosque', r[8])))
  
B = factor(c(rep('invierno', r[1]), rep('verano',r[2]), rep('invierno', r[3]), rep('verano', r[4]), rep('invierno', r[5]), rep('verano', r[6]), rep('invierno', r[7]), rep('verano', r[8])))
  
  mod = lm(y ~ A*B, weights = w)
  p = anova(mod)[3, 5]
  return(p)
}
```
  
  # Simulacion de la potencia
  
```{r}

r = c(11, 5, 13, 13, 8, 13, 11, 5)

n = sum(r)


A = factor(c(rep('convencional', r[1]), rep('convencional',r[2]), rep('BPA', r[3]), rep('BPA', r[4]), rep('organico', r[5]), rep('organico', r[6]), rep('bosque', r[7]), rep('bosque', r[8])))
  
B = factor(c(rep('invierno', r[1]), rep('verano',r[2]), rep('invierno', r[3]), rep('verano', r[4]), rep('invierno', r[5]), rep('verano', r[6]), rep('invierno', r[7]), rep('verano', r[8])))


mu = c(70, 120, 170, 140, 270, 160, 370, 190)

mus = c(rep(mu[1], 11), rep(mu[2], 5), rep(mu[3], 13), rep(mu[4], 13), rep(mu[5], 8), rep(mu[6], 13), rep(mu[7], 11), rep(mu[8], 5))

var = c(4186.39, 1034.751,  10110.51, 3276.529, 13134.77, 6446.170, 20017.39, 5307.274)


vars = c(rep(var[1], 11), rep(var[2], 5), rep(var[3], 13), rep(var[4], 13), rep(var[5], 8), rep(var[6], 13), rep(var[7], 11), rep(var[8], 5))
  
w = 1/vars

#datos <- data.frame(A,B,mus,vars, w)
#------------------------------------------------------------------------------

#---- función

med = function(r, mus, v = 1) {
  
  y = rnorm(n, mus, sqrt(vars))
  
  mod = lm(y ~ A*B, weights = w)
  
  return(mod)
}


bosq.in = c(1, 1, 0, 0, 1, 1, 0, 0)
bpa.in = c(1, 0, 1, 0, 1, 0, 1, 0)
con.in = c(1, 0, 0, 1, 1, 0, 0, 1)
org.in = c(1, -1, -1, -1, 1, -1, -1, -1)
bosq.ver = c(1, 1, 0, 0, -1, -1, 0, 0)
bpa.ver = c(1, 0, 1, 0, -1, 0, -1, 0)
con.ver = c(1, 0, 0, 1, -1, 0, 0, -1)
org.ver = c(1, -1, -1, -1, -1, 1, 1, 1)

bosqcon.in = bosq.in - con.in
bosqbpa.in = bosq.in - bpa.in
bosqorg.in = bosq.in - org.in
orgbpa.in = org.in - bpa.in
orgcon.in = org.in - con.in
bpacon.in = bpa.in - con.in

#verano

bosqcon.ver = bosq.ver - con.ver
bosqbpa.ver = bosq.ver - bpa.ver
bosqorg.ver = bosq.ver - org.ver
orgbpa.ver = org.ver - bpa.ver
orgcon.ver = org.ver - con.ver
bpacon.ver = bpa.ver - con.ver


h1 = cbind(bosqcon.in,bosqbpa.in,bosqorg.in,orgbpa.in,orgcon.in,bpacon.in, bosqcon.ver,bosqbpa.ver,bosqorg.ver,orgbpa.ver,orgcon.ver, bpacon.ver)


tc = qt(1-0.05/3, 71) 

#cantidad que repite
rows <- c('bosqcon.in','orgcon.in','bpacon.in')

M <- 10000
ICI <- NULL


for (j in 1:M) {

  mod = med(r, mus, vars)
  
  L = t(h1)%*%mod$coefficients
  
  vari = diag(t(h1)%*%vcov(mod)%*%h1)
  
  ee = sqrt(vari)

  ICI[j]<- abs(c(L[c(1,5,6)] -tc *ee[c(1,5,6)]))
  
}

ICI = data.frame(ICI)

ICI$trat = rep(rows, length.out = 1000)

ICIrel = ICI %>% filter(ICI >= 100) 
                    

nrow(ICIrel)/10000

```
  
