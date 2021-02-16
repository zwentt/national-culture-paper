#### STEP 1: Null Model

```r
lmm.fit1=lmer(Y ~(1|l2id),data=exdata,REML=F)
summary(lmm.fit1)
```


#### STEP 2: Random Intercept and Fixed Slope Model

```r
lmm.fit2=lmer(Y ~(1|l2id)+Xc+I(Wj-mean(Wj) ),data=exdata,REML=F)
summary(lmm.fit2)
```


#### STEP 3: Random Intercept and Random Slope model

```r
lmm.fit3=lmer(Y ~Xc+(Xc|l2id)+I(Wj-mean(Wj) ),data=exdata,REML=F)
summary(lmm.fit3)
```

#### STEP 4: Cross-Level Interaction Model

```r
lmm.fit4=lmer(Y ~(Xc|l2id)+Xc*I(Wj-mean(Wj) ),data=exdata,REML=F)
summary(lmm.fit4)
```
