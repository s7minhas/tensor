Sample and DVs
===

* First a few updates, models based on what we discussed last week are finally running up on an ec2 instance. Our sample is comprised of 161 countries over the period of March 2001 to December 2014 (including info for N. Korea, N. Zealand and Papua New Guinea, it has just gotten).

* The two dependent variables I’m feeding into the MLTR are: Log(Exports) and a standardized version of the material conflict variable. I also include the direct (_i), reciprocal (_ji) and transitive (_ijk) 1 month lags of both of these variables as IVs.

The exogenous IVs are, each lagged by one month
===

* Dyadic Covariates:
     + Presence of a Preferential Trade Agreement (PTA) between i and j (this is an undirected monthly level variable). I include both the direct and transitive version of this variable.
     + Presence of an Alliance between i and j (also undirected, yearly level). Direct and transitive versions.
     + Centroid distance between i and j,  (directed). Direct version.

* Monadic Covariates:

     + Polity, monthly level variable

     + Log(GDP), yearly level variable but imputed at the monthly level in the CRISP dataset

     + Log(Population), yearly level variable but imputed at the monthly level in the CRISP dataset
     + Log(Total Exports to any country), monthly level variable — this is to control for countries that simply do not trade much

Full model specification for set of autoregressive equations:
===

Trade =
	Trade_t-1,ij + Trade_t-1,ji + Trade_t-1,ijk +
	Matl.Conf._t-1,ij + Matl.Conf._t-1,ji + Matl.Conf._t-1,ijk +
	PTA_t-1,ij + PTA_t-1,ijk +
	Ally_t-1,ij + Ally_t-1,ijk +
	Distance_t-1,ij +
	Polipty_t-1,i +
	Log(GPD)_t-1,i +
	Log(Pop)_t-1,i +
	Log(Total Exports)_t-1,i

Matl.Conf =
	Trade_t-1,ij + Trade_t-1,ji + Trade_t-1,ijk +
	Matl.Conf._t-1,ij + Matl.Conf._t-1,ji + Matl.Conf._t-1,ijk +
	PTA_t-1,ij + PTA_t-1,ijk +
	Ally_t-1,ij + Ally_t-1,ijk +
	Distance_t-1,ij +
	Polity_t-1,i +
	Log(GPD)_t-1,i +
	Log(Pop)_t-1,i +
	Log(Total Exports)_t-1,i

Necessary Data Structure
===

* Array
	- n = 161 countries, t = 166 months
	- DVs = 2, Trade and Matl. Conf.
	- Add reciprocal and transitive versions of both variables as endogenous predictors
	- Create another slice in the tensor to hold exogenous exogenous equations, specifically:
		+ PTA
		+ Distance
		+ Ally
		+ GDP
		+ Population
		+ Total Exports
