"""41e_perpetrator_type.py - Pre-registration 42: four perpetrator x type indicators for every woman in the disclosure files, from the raw IR files.
PP partner physical (d105a-g,j ever); PS partner sexual (d105h,i,k ever, or d125 with d127 = husband/partner/boyfriend);
NP non-partner physical (d115y == 0, or any d115* == 1); NS non-partner sexual (d124 == 1, or d125 with d127 not a partner or not asked; Correction 4 rule).
Safety check: the indicators must reproduce the existing vtype and perp columns exactly. Output: data/derived/women_perp_type.csv.gz
"""
import zipfile, io, re, numpy as np, pandas as pd
from pathlib import Path
R = __import__("pathlib").Path(__import__("os").environ["RR"]); D = R / "data/derived"
RAW = [R / "data/raw/dhs_ir", Path.home() / "Documents/08 Data/raw_surveys", Path.home() / "Documents/Data/raw_surveys"]
pairs = pd.read_csv(D / "ir_ge_pairs.csv"); pairs = pairs[pairs.usable]
pairs["survey"] = pairs.country + "_" + pairs.years.astype(str).str[:4] + "_" + pairs.ir.str[4:6]
PHYS = [f"d105{c}" for c in "abcdefgj"]; SEX = ["d105h", "d105i", "d105k"]
W = pd.concat([pd.read_csv(D / f, usecols=["survey", "caseid", "vtype", "perp"], dtype={"caseid": str}) for f in ["women_disclosure.csv.gz", "women_disclosure_heldout.csv.gz"]])
W["caseid"] = W.caseid.astype(str).str.strip()
rows = []
for sv in sorted(W.survey.unique()):
    r = pairs[pairs.survey == sv].iloc[0]; f = next(p / r.ir for p in RAW if (p / r.ir).exists())
    z = zipfile.ZipFile(f); raw = z.read([n for n in z.namelist() if n.lower().endswith(".dta")][0])
    with pd.io.stata.StataReader(io.BytesIO(raw)) as rd:
        lab = rd.variable_labels(); vl = rd.value_labels(); ll = dict(zip(rd._varlist, rd._lbllist))
    d115 = [v for v in lab if re.match(r"^d115[a-z]+$", v) and v != "d115y" and not (lab[v] or "").lower().startswith("na -")]
    cols = [c for c in ["caseid", "v044", "d005", "d115y", "d124", "d125", "d127"] + PHYS + SEX + d115 if c in lab]
    d = pd.read_stata(io.BytesIO(raw), columns=list(dict.fromkeys(cols)), convert_categoricals=False)
    for c in d.columns:
        if c != "caseid": d[c] = pd.to_numeric(d[c], errors="coerce").astype("float64")
    d = d[(d.get("v044", 1) == 1) & d.d005.notna() & (d.d005 > 0)].copy(); d["caseid"] = d.caseid.astype(str).str.strip()
    F = pd.Series(False, index=d.index)
    P = [c for c in PHYS if c in d]; S = [c for c in SEX if c in d]
    pp = d[P].isin([1, 2, 3, 4]).any(axis=1); ps_d105 = d[S].isin([1, 2, 3, 4]).any(axis=1) if S else F
    npp = (d["d115y"] == 0) if "d115y" in d else (d[d115].eq(1).any(axis=1) if d115 else F)
    np12 = d["d124"].eq(1) if "d124" in d else F
    sex_ever = d["d125"].eq(1) if "d125" in d else F
    if "d127" in lab and not (lab["d127"] or "").lower().startswith("na"):
        codes = {k: str(x).lower() for k, x in vl.get(ll.get("d127"), {}).items()}
        pc = [k for k, x in codes.items() if re.search(r"husband|partner|boyfriend", x)]
        by_p = sex_ever & d["d127"].isin(pc); by_o = sex_ever & ~d["d127"].isin(pc); resolved = 1
    else: by_p = F; by_o = sex_ever; resolved = 0
    out = pd.DataFrame({"survey": sv, "caseid": d.caseid, "PP": pp.astype(int), "PS": (ps_d105 | by_p).astype(int), "NP": npp.astype(int),
                        "NS": (np12 | by_o).astype(int), "d127_resolved": resolved})
    rows.append(out)
T = pd.concat(rows)
M = W.merge(T, on=["survey", "caseid"], how="left", indicator=True)
assert (M._merge == "both").all(), "unmatched women"
phys = (M.PP | M.NP).astype(bool); sex = (M.PS | M.NS).astype(bool); part = (M.PP | M.PS).astype(bool); nonp = (M.NP | M.NS).astype(bool)
vt = np.select([phys & sex, phys, sex], ["both", "physical", "sexual"], default="none")
pe = np.select([part & nonp, part, nonp], ["both", "partner", "nonpartner"], default="none")
print("vtype reproduced:", (vt == M.vtype).mean(), "| perp reproduced:", (pe == M.perp).mean())
assert (vt == M.vtype).all() and (pe == M.perp).all(), "safety check failed"
M[["survey", "caseid", "PP", "PS", "NP", "NS", "d127_resolved"]].to_csv(D / "women_perp_type.csv.gz", index=False)
v = M[M.vtype != "none"]
print(f"victims {len(v):,} | PP {v.PP.mean():.3f} PS {v.PS.mean():.3f} NP {v.NP.mean():.3f} NS {v.NS.mean():.3f} | d127 resolved surveys {T.groupby('survey').d127_resolved.first().sum()}")
