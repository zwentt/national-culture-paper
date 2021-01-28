#install.packages(c("StanHeaders", "rstan"), type = "source")
library(rstan)
library(rstantools)
library(brms)
library(haven)
library(StanHeaders)

CultureData <- read_dta("https://github.com/zwentt/national-culture-paper/raw/main/Data_Analysis/CultureData.dta")


guaiv.cross.brm <- brm(outcome ~ agility2cj + guaiv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + guaiv_c*agility2cj +  (guaiv_c | countryx), data = CultureData)

gfuov.cross.brm <- brm(outcome ~ agility2cj + gfuov_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gfuov_c*agility2cj +  (gfuov_c | countryx), data = CultureData)

gpdiv.cross.brm <- brm(outcome ~ agility2cj + gpdiv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gpdiv_c*agility2cj +  (gpdiv_c | countryx), data = CultureData)

ginscolv.cross.brm <- brm(outcome ~ agility2cj + ginscolv_c + firmsizestd + network2xcj + gmci2016c + ginscolv_c*agility2cj +  (ginscolv_c | countryx), data = CultureData)

ghumv.cross.brm <- brm(outcome ~ agility2cj + ghumv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + ghumv_c*agility2cj +  (ghumv_c | countryx), data = CultureData)

gperv.cross.brm <- brm(outcome ~ agility2cj + gperv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gperv_c*agility2cj +  (gperv_c | countryx), data = CultureData)

gigrcolv.cross.brm <- brm(outcome ~ agility2cj + gigrcolv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gigrcolv_c*agility2cj +  (gigrcolv_c | countryx), data = CultureData)

ggndv.cross.brm <- brm(outcome ~ agility2cj + ggndv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + ggndv_c*agility2cj +  (ggndv_c | countryx), data = CultureData)

agility.ginscolv.cross.brm <- brm(sensing2 ~ ginscolv_c + firmsizestd + network2xcj + gmci2016c + ginscolv_c*network2xcj +  (ginscolv_c | countryx), data = CultureData)



# Assertiveness using domestic firms only
gassv.cross.brm <- brm(outcome ~ agility2cj + gassv_c + firmsizestd + network2xcj + gmci2016c + I(firmsizestd*firmsizestd) + gassv_c*agility2cj +  (gassv_c | countryx), data = subset(CultureData, network2x == 0))


# Song et al., 2018 IJPE
# "To specify prior distributions for the parameters, we used diffuse priors similar to the ones used in Stegmueller (2013). To estimate the posterior distributiosn of interests, we adopted MCMC methods. 

# Stegmuller, 2013, American Journal of Political Science 
# "Results presented used inverse gamma priors with small values for shape and scale, which showed to yeild reasonable results in my simulations, despite their technical short-comings (Gelman, 2006). The difference between prior choices is illustrated in Figure 7, which shows interval coverage for country-level covariate effects gamma_1 in a hierarchical linear model under different prior specifications. The slight difference between inverse-gamma and uniform on the standard deviation priors vanish as more information becomes available. This pattern is similar for other coefficient estimates in both linear and probit models. When inverse Wishart priors are used as priors for variance-covariance matrices, I find similar results: differences in prior values are only relevant when little country-level information is available. This suggests that applied researchers who have to work with small samples should test the robustness 




