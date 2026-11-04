"""81b_togo_exclusion.py - Pre-registration 38, B12: pooled OR for both physical and sexual violence without the most extreme country (Togo).
Output: outputs/tables/P38_review/P38_B12_togo_exclusion.csv"""
import sys; sys.path.insert(0, "analysis")
import numpy as np, pandas as pd, pool_numpyro as pn
CE = pd.read_csv("outputs/tables/P33_youth_v3/A2_country_estimates.csv"); rows = []
for o in ["sought_any", "told"]:
    x = CE[(CE.source == "DHS") & (CE.model == "main") & (CE.outcome == o) & (CE.term == "vtypeboth")]; tg = x[x.country == "TG"]; y = x[x.country != "TG"]
    r = pn.pool(y.est.values, y.se.values, list(y.region.values))
    rows.append(dict(outcome=o, togo_OR=float(np.exp(tg.est.iloc[0])), OR_without_togo=r["OR"], lo=r["OR_lo"], hi=r["OR_hi"], countries=len(y)))
pd.DataFrame(rows).to_csv("outputs/tables/P38_review/P38_B12_togo_exclusion.csv", index=False); print(pd.DataFrame(rows).round(2))
