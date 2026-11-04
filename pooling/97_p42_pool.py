"""97_p42_pool.py - Pre-registration 42, B/C: pool the country estimates of script 96 with the primary hierarchical model (countries within regions).
Holm for the between-region Q test within analysis x outcome. Output: outputs/tables/P42/P42_pooled.csv"""
import os, sys
os.environ.setdefault("XLA_FLAGS", "--xla_force_host_platform_device_count=4"); sys.path.insert(0, "analysis")
import pandas as pd, pool_numpyro as pn
E = pd.read_csv("outputs/tables/P42/P42_country_estimates.csv")
P = pn.pool_table(E, ["analysis", "model", "outcome", "term"], holm_by=("analysis", "model", "outcome"))
P.to_csv("outputs/tables/P42/P42_pooled.csv", index=False)
print(f"pooled {len(P)} | max R-hat {P.rhat_max.max():.3f} | min ESS {P.ess_min.min():.0f} | divergences {int(P.divergences.sum())}")
