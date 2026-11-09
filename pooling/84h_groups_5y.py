"""84h_groups_5y.py - shares of young survivors in four mutually exclusive disclosure groups, every five years 2005-2025, with 95% CrIs (figure 4, top row).
From the population-weighted curve draws of 84f (P39pw_draws_5y.csv.gz): told no one = 100 - told; told someone but sought no help = told - sought any;
sought informal help only = sought any - formal; sought formal help = formal. The three outcomes were fitted separately, so draws are paired by index
(an approximation for the two difference groups). Output: outputs/tables/P38_review/P39pw_groups_5y.csv
"""
import pandas as pd
T = "outputs/tables/P38_review/"
d = pd.read_csv(T + "P39pw_draws_5y.csv.gz").pivot_table(index=["scope", "year", "draw"], columns="outcome", values="pct").reset_index()
g = pd.DataFrame({"scope": d.scope, "year": d.year, "Told no one": 100 - d.told, "Told someone, no help": d.told - d.sought_any,
                  "Informal help only": d.sought_any - d.formal, "Formal help": d.formal})
L = g.melt(id_vars=["scope", "year"], var_name="group", value_name="pct")
Q = L.groupby(["scope", "year", "group"]).pct.quantile([.5, .025, .975]).unstack().reset_index(); Q.columns = ["scope", "year", "group", "pct", "lo", "hi"]
Q["share_draws_below_0"] = L.groupby(["scope", "year", "group"]).pct.apply(lambda x: (x < 0).mean()).values
Q.to_csv(T + "P39pw_groups_5y.csv", index=False); print(Q[(Q.scope == "All") & Q.year.isin([2005, 2025])].round(1).to_string(index=False))
