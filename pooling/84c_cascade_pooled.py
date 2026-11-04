"""84c_cascade_pooled.py - D: Bayesian random-effects pooling of the country cascade percentages (all rounds of a country pooled, as in Figure 2).
logit(p_c) ~ N(theta_c, se_c^2); theta_c = mu_r + tau z_c; mu_r = mu + omega u_r; mu ~ N(logit a0, 1.5); tau ~ HalfNormal(1); omega ~ HalfNormal(1)
(priors as for the intercepts of the trend model). Countries weigh equally ("typical country"), unlike the survey-weighted pooled percentage.
Reported: typical-country % (95% CrI) overall and by region, and the 95% prediction interval for the percentage in a new country.
Input: A8_5_dhs_cascade_ci.csv (country rows, design-based logit CIs). Output: outputs/tables/P33_youth_v3/A8_5_cascade_bayes.csv"""
import os
os.environ.setdefault("XLA_FLAGS", "--xla_force_host_platform_device_count=4")
import numpy as np, pandas as pd, jax, jax.numpy as jnp, numpyro, numpyro.distributions as dist
from numpyro.infer import MCMC, NUTS
from numpyro.diagnostics import summary
from scipy.special import expit, logit
numpyro.set_host_device_count(4)
T = "outputs/tables/P33_youth_v3/"; REG = ["West", "Central", "East", "Southern"]
CI = pd.read_csv(T + "A8_5_dhs_cascade_ci.csv"); C = CI[CI.level == "country"].copy()
def model(y, se, cr, n_r, a0):
    mu = numpyro.sample("mu", dist.Normal(a0, 1.5)); om = numpyro.sample("omega", dist.HalfNormal(1.0)); tau = numpyro.sample("tau", dist.HalfNormal(1.0))
    with numpyro.plate("r", n_r): mu_r = numpyro.deterministic("mu_r", mu + om * numpyro.sample("u", dist.Normal(0, 1)))
    with numpyro.plate("c", len(y)): numpyro.sample("y", dist.Normal(mu_r[cr] + tau * numpyro.sample("z", dist.Normal(0, 1)), se), obs=y)
lg = lambda p: logit(np.clip(p / 100, 1e-4, 1 - 1e-4)); q = lambda v: np.quantile(v, [0.5, 0.025, 0.975]); rows = []; rng = np.random.default_rng(20261104)
for o in ["told", "sought_any", "formal"]:
    d = C[C.outcome == o].reset_index(drop=True); y = lg(d.pct.values); se = (lg(d.hi.values) - lg(d.lo.values)) / (2 * 1.959964)
    cr = np.array([REG.index(r) for r in d.region]); a0 = float(logit(0.03 if o == "formal" else 0.3))
    mc = MCMC(NUTS(model, target_accept_prob=0.995, max_tree_depth=12), num_warmup=2000, num_samples=2000, num_chains=4, chain_method="parallel", progress_bar=False)
    mc.run(jax.random.PRNGKey(20261104), y=jnp.array(y), se=jnp.array(se), cr=jnp.array(cr), n_r=4, a0=a0)
    s = {k: np.asarray(v) for k, v in mc.get_samples().items()}; sg = mc.get_samples(group_by_chain=True)
    dg = summary({k: sg[k] for k in ["mu", "omega", "tau", "mu_r"]}, group_by_chain=True); rhat = max(float(np.max(dg[k]["r_hat"])) for k in dg); div = int(mc.get_extra_fields()["diverging"].sum())
    S = len(s["mu"]); z, u = rng.standard_normal(S), rng.standard_normal(S)
    def add(scope, kind, v): m, lo, hi = q(100 * expit(v)); rows.append(dict(outcome=o, scope=scope, kind=kind, pct=m, lo=lo, hi=hi, countries=len(d) if scope == "All" else int((d.region == scope).sum()), rhat=rhat, divergences=div))
    add("All", "typical country (95% CrI)", s["mu"]); add("All", "new country (95% prediction interval)", s["mu"] + s["omega"] * u + s["tau"] * z)
    for j, r in enumerate(REG):
        add(r, "typical country (95% CrI)", s["mu_r"][:, j]); add(r, "new country (95% prediction interval)", s["mu_r"][:, j] + s["tau"] * z)
    print(o, f"Rhat {rhat:.3f} div {div} tau {np.median(s['tau']):.2f} omega {np.median(s['omega']):.2f}", flush=True)
R = pd.DataFrame(rows); R.to_csv(T + "A8_5_cascade_bayes.csv", index=False); print(R[R.scope == "All"].round(1).to_string(index=False))
