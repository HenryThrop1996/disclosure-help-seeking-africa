"""84b_trend_predictive.py - additions to Figure 3 (Pre-reg 39 model, refitted unchanged with the same seed as 84_trend_model.py).
A. 95% prediction band: the true percentage in a new survey of a new country, adding between-country intercept and slope variation
   and the survey-level residual to the mean curve (sampling error excluded). All countries: the new country also comes from a new region.
B. Partially pooled trajectory a_c + b_c t of every country, with 95% CrIs, and each country's OR per decade and Pr(decline).
Outputs (outputs/tables/P38_review/): P39_pred_band.csv, P39_country_curves.csv, P39_country_slopes.csv, P39_refit_check.csv"""
import os, sys
os.environ.setdefault("XLA_FLAGS", "--xla_force_host_platform_device_count=4"); sys.path.insert(0, "analysis")
import numpy as np, pandas as pd, jax, jax.numpy as jnp, numpyro, numpyro.distributions as dist
from numpyro.infer import MCMC, NUTS
from scipy.special import expit, logit
numpyro.set_host_device_count(4)
src = open("analysis/84_trend_model.py").read(); exec(src[src.index("def model"):src.index("curves, slopes = [], []")])   # identical model
T = "outputs/tables/P38_review/"; REG = ["West", "Central", "East", "Southern"]
E = pd.read_csv(T + "P39_survey_estimates.csv"); S0 = pd.read_csv(T + "P39_slopes.csv")
q = lambda v, ax=0: np.quantile(v, [0.5, 0.025, 0.975], axis=ax)
band, ccur, cslo, chk = [], [], [], []
rng = np.random.default_rng(20261104)
for o in ["told", "sought_any", "formal"]:
    d = E[E.outcome == o].reset_index(drop=True)
    cs = sorted(d.country.unique()); ci = np.array([cs.index(c) for c in d.country])
    creg = d.groupby("country").region.first().reindex(cs); cr = np.array([REG.index(r) for r in creg])
    a0 = float(logit(0.03 if o == "formal" else 0.3))
    mc = MCMC(NUTS(model, target_accept_prob=0.995, max_tree_depth=12), num_warmup=2000, num_samples=2000, num_chains=4, chain_method="parallel", progress_bar=False)
    mc.run(jax.random.PRNGKey(20261023), y=jnp.array(d.logit.values), se=jnp.array(d.se.values), t=jnp.array(d.t.values), ci=jnp.array(ci), cr=jnp.array(cr), n_c=len(cs), n_r=len(REG), a0=a0)
    s = {k: np.asarray(v) for k, v in mc.get_samples().items()}; S = len(s["A"])
    m, lo, hi = q(np.exp(s["B"])); r0 = S0[(S0.outcome == o) & (S0.scope == "All")].iloc[0]
    chk.append(dict(outcome=o, OR_refit=m, lo_refit=lo, hi_refit=hi, OR_84=r0.OR_decade, lo_84=r0.OR_lo, hi_84=r0.OR_hi))
    # A. prediction band
    za, zb, e = rng.standard_normal(S), rng.standard_normal(S), rng.standard_normal(S); ua, ub = rng.standard_normal(S), rng.standard_normal(S)
    for scope in ["All"] + REG:
        if scope == "All":
            a_new = s["A"] + s["omega_a"] * ua + s["sigma_a"] * za; b_new = s["B"] + s["omega_b"] * ub + s["sigma_b"] * zb
            grid = np.linspace((2004 - 2015) / 10, (2025.6 - 2015) / 10, 60)
        else:
            dr = d[d.region == scope]
            if dr.empty: continue
            j = REG.index(scope); a_new = s["A_r"][:, j] + s["sigma_a"] * za; b_new = s["B_r"][:, j] + s["sigma_b"] * zb
            grid = np.linspace(dr.t.min(), dr.t.max(), 40) if dr.t.max() > dr.t.min() else np.array([dr.t.min()])
        for g in grid:
            m, lo, hi = q(expit(a_new + b_new * g + s["sigma_e"] * e))
            band.append(dict(outcome=o, scope=scope, year=2015 + 10 * g, pred_med=100 * m, pred_lo=100 * lo, pred_hi=100 * hi))
    # B. country trajectories
    a_c = s["A_r"][:, cr] + s["sigma_a"][:, None] * s["za"]; b_c = s["B_r"][:, cr] + s["sigma_b"][:, None] * s["zb"]
    for k, c in enumerate(cs):
        dc = d[d.country == c]; t0, t1 = dc.t.min(), dc.t.max()
        for g in (np.linspace(t0, t1, 25) if t1 > t0 else np.array([t0])):
            m, lo, hi = q(expit(a_c[:, k] + b_c[:, k] * g)); ccur.append(dict(outcome=o, country=c, region=creg[c], year=2015 + 10 * g, pct=100 * m, lo=100 * lo, hi=100 * hi))
        m, lo, hi = q(np.exp(b_c[:, k]))
        cslo.append(dict(outcome=o, country=c, region=creg[c], surveys=len(dc), first=dc.year.min(), last=dc.year.max(), OR_decade=m, OR_lo=lo, OR_hi=hi,
                         P_decline=float((b_c[:, k] < 0).mean()), raw_change_pp=100 * (dc.sort_values("year").p.iloc[-1] - dc.sort_values("year").p.iloc[0])))
    print(o, "refit OR/decade %.2f (%.2f-%.2f) vs 84: %.2f (%.2f-%.2f)" % (chk[-1]["OR_refit"], chk[-1]["lo_refit"], chk[-1]["hi_refit"], r0.OR_decade, r0.OR_lo, r0.OR_hi), flush=True)
pd.DataFrame(band).to_csv(T + "P39_pred_band.csv", index=False); pd.DataFrame(ccur).to_csv(T + "P39_country_curves.csv", index=False)
C = pd.DataFrame(cslo); C.to_csv(T + "P39_country_slopes.csv", index=False); pd.DataFrame(chk).to_csv(T + "P39_refit_check.csv", index=False)
M = C[C.surveys >= 2]
for o, g in M.groupby("outcome"):
    print(f"{o}: {len(g)} countries with >=2 surveys; Pr(decline)>0.95: {(g.P_decline > .95).sum()}; >0.5: {(g.P_decline > .5).sum()}; Pr(rise)>0.95: {(g.P_decline < .05).sum()}; raw decline: {(g.raw_change_pp < 0).sum()}")
