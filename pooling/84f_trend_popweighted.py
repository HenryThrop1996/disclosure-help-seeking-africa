"""84f_trend_popweighted.py - Correction 5, post-hoc addition 5: population-weighted trend curves (headline trend figure).
The P39 hierarchical meta-regression (model, priors, sampler settings and seed copied unchanged from 84_trend_model.py). Each posterior draw
gives every country's curve a_c + b_c t; the headline curve is their population-weighted average, sum_c w_c(year) expit(a_c + b_c t), with
w_c(year) = women aged 15-24 in country c and that year (UN WPP 2024), for all countries and for each region. It describes young women in the
countries studied (not a typical or new country; that curve stays in 84_trend_model.py).
Outputs (outputs/tables/P38_review/): P39pw_curves.csv, P39pw_summary.csv
"""
import os
os.environ.setdefault("XLA_FLAGS", "--xla_force_host_platform_device_count=4")
import numpy as np, pandas as pd, jax, jax.numpy as jnp, numpyro, numpyro.distributions as dist
from numpyro.infer import MCMC, NUTS
from numpyro.diagnostics import summary
from scipy.special import expit, logit
numpyro.set_host_device_count(4)
T = "outputs/tables/P38_review/"; REG = ["West", "Central", "East", "Southern"]; YEARS = np.arange(2004, 2026)
E = pd.read_csv(T + "P39_survey_estimates.csv").merge(pd.read_csv(T + "P38_B9_population_by_survey.csv")[["survey", "iso3"]], on="survey")
ISO = E.groupby("country").iso3.first()
WPP = pd.read_csv(os.path.expanduser("~/Documents/08 Data/other_datasets/WPP2024/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"),
                  usecols=["ISO3_code", "Time", "AgeGrp", "PopFemale"])
WPP = WPP[WPP.ISO3_code.isin(ISO.values) & WPP.Time.isin(YEARS) & WPP.AgeGrp.isin(["15-19", "20-24"])]
POP = WPP.groupby(["ISO3_code", "Time"]).PopFemale.sum().unstack("Time")             # thousands; rows iso3, columns year
assert POP.notna().all().all() and len(POP) == ISO.nunique()

def model(y, se, t, ci, cr, n_c, n_r, a0):                          # identical to 84_trend_model.py (a_c, b_c kept as deterministic sites)
    A = numpyro.sample("A", dist.Normal(a0, 1.5)); B = numpyro.sample("B", dist.Normal(0, 1))
    om_a = numpyro.sample("omega_a", dist.HalfNormal(1.0)); om_b = numpyro.sample("omega_b", dist.HalfNormal(0.5))
    sg_a = numpyro.sample("sigma_a", dist.HalfNormal(1.0)); sg_b = numpyro.sample("sigma_b", dist.HalfNormal(0.5))
    with numpyro.plate("regions", n_r):
        A_r = numpyro.deterministic("A_r", A + om_a * numpyro.sample("ua", dist.Normal(0, 1)))
        B_r = numpyro.deterministic("B_r", B + om_b * numpyro.sample("ub", dist.Normal(0, 1)))
    with numpyro.plate("countries", n_c):
        a_c = numpyro.deterministic("a_c", A_r[cr] + sg_a * numpyro.sample("za", dist.Normal(0, 1)))
        b_c = numpyro.deterministic("b_c", B_r[cr] + sg_b * numpyro.sample("zb", dist.Normal(0, 1)))
    sg_e = numpyro.sample("sigma_e", dist.HalfNormal(0.5))
    numpyro.sample("y", dist.Normal(a_c[ci] + b_c[ci] * t, jnp.sqrt(se**2 + sg_e**2)), obs=y)

curves, summ = [], []
q = lambda v: np.quantile(v, [0.5, 0.025, 0.975])
for o in ["told", "sought_any", "formal"]:
    d = E[E.outcome == o].reset_index(drop=True)
    cs = sorted(d.country.unique()); ci = np.array([cs.index(c) for c in d.country])
    creg = d.groupby("country").region.first().reindex(cs); cr = np.array([REG.index(r) for r in creg])
    mc = MCMC(NUTS(model, target_accept_prob=0.995, max_tree_depth=12), num_warmup=2000, num_samples=2000, num_chains=4, chain_method="parallel", progress_bar=False)
    mc.run(jax.random.PRNGKey(20261023), y=jnp.array(d.logit.values), se=jnp.array(d.se.values), t=jnp.array(d.t.values), ci=jnp.array(ci), cr=jnp.array(cr),
           n_c=len(cs), n_r=len(REG), a0=float(logit(0.03 if o == "formal" else 0.3)))
    s = {k: np.asarray(v) for k, v in mc.get_samples().items()}; sg = mc.get_samples(group_by_chain=True)
    dg = summary({k: sg[k] for k in ["A", "B", "sigma_a", "sigma_b", "sigma_e", "a_c", "b_c"]}, group_by_chain=True)
    rhat = max(float(np.max(dg[k]["r_hat"])) for k in dg); ess = min(float(np.min(dg[k]["n_eff"])) for k in dg); div = int(mc.get_extra_fields()["diverging"].sum())
    P = POP.loc[[ISO[c] for c in cs]].to_numpy()                                     # countries x years
    for scope in ["All"] + REG:
        m = np.ones(len(cs), bool) if scope == "All" else (creg.values == scope)
        p_draw = {}
        for j, yr in enumerate(YEARS):
            w = P[m, j] / P[m, j].sum()
            p_draw[yr] = 100 * (expit(s["a_c"][:, m] + s["b_c"][:, m] * (yr - 2015) / 10) * w).sum(1)
            md, lo, hi = q(p_draw[yr]); lo50, hi50 = np.quantile(p_draw[yr], [0.25, 0.75])                     # 50% interval added 2026-11-09 (figure 4)
            curves.append(dict(outcome=o, scope=scope, year=yr, pct=md, lo=lo, hi=hi, lo50=lo50, hi50=hi50))
        ch = p_draw[2025] - p_draw[2005]
        dm = d[d.region == scope] if scope != "All" else d
        summ.append(dict(outcome=o, scope=scope, countries=int(m.sum()), surveys=len(dm), multi_round=int((dm.country.value_counts() >= 2).sum()),
                         pct_2005=q(p_draw[2005])[0], pct_2025=q(p_draw[2025])[0], change_pp=q(ch)[0], change_lo=q(ch)[1], change_hi=q(ch)[2],
                         P_decline=float((ch < 0).mean()), trend_estimable=scope != "Southern", rhat_max=rhat, ess_min=ess, divergences=div))
    a = summ[-5]
    print(f"{o:10s} 2005 {a['pct_2005']:.1f}% -> 2025 {a['pct_2025']:.1f}% | change {a['change_pp']:.1f} pp ({a['change_lo']:.1f} to {a['change_hi']:.1f}) P(decline) {a['P_decline']:.3f} | Rhat {rhat:.3f} ESS {ess:.0f} div {div}")
    for x in summ[-4:]: print(f"    {x['scope']:9s} {x['pct_2005']:.1f} -> {x['pct_2025']:.1f} | {x['change_pp']:.1f} ({x['change_lo']:.1f} to {x['change_hi']:.1f}) P(decline) {x['P_decline']:.3f}")
pd.DataFrame(curves).to_csv(T + "P39pw_curves.csv", index=False); pd.DataFrame(summ).to_csv(T + "P39pw_summary.csv", index=False)
