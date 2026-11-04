"""99_exploratory.py - Correction 5, exploratory checks on Run B (not pre-specified). Output: B/outputs/tables/exploratory/*.csv"""
import os,re,zipfile,io,numpy as np,pandas as pd
os.chdir(os.environ['RR']); OUT='outputs/tables/exploratory/'; os.makedirs(OUT,exist_ok=True)
E=pd.read_csv('outputs/tables/P38_review/P39_survey_estimates.csv'); E['ph']=E.survey.str.split('_').str[2].str[0].astype(int)
def wls(d,cols):
    X=pd.concat([pd.get_dummies(d.country,dtype=float)]+[d[c] for c in cols],axis=1).values; y=d.logit.values; W=1/(d.se.values**2+0.15**2); XtW=X.T*W; B=np.linalg.pinv(XtW@X); b=B@XtW@y; e=y-X@b
    g=d.country.values; M=sum(np.outer(XtW[:,g==c]@e[g==c],XtW[:,g==c]@e[g==c]) for c in np.unique(g)); V=B@M@B; k=len(cols); out={}
    for i,c in enumerate(cols): j=-k+i; out[c]=(float(np.exp(b[j])),float(np.exp(b[j]-1.96*V[j,j]**.5)),float(np.exp(b[j]+1.96*V[j,j]**.5)))
    return out
R=[]
for o in ['sought_any','told','formal']:
    d=E[E.outcome==o].copy(); d=d[d.groupby('country').survey.transform('nunique')>1]; d['tt']=d.t
    def add(lab,dd,cols):
        dd=dd[dd.groupby('country').survey.transform('nunique')>1]
        for c,v in wls(dd,cols).items(): R.append(dict(outcome=o,spec=lab,term=c,countries=dd.country.nunique(),surveys=len(dd),OR=v[0],lo=v[1],hi=v[2]))
    add('all years, linear',d,['tt'])
    add('surveys before 2016',d[d.year<2016],['tt']); add('surveys 2013 onwards',d[d.year>=2013],['tt']); add('surveys before 2013',d[d.year<2013],['tt']); add('surveys 2016 onwards',d[d.year>=2016],['tt'])
    d['kn']=np.maximum(d.year-2014,0)/10; d['k0']=(np.minimum(d.year,2014)-2014)/10; add('piecewise (knot 2014)',d,['k0','kn'])
    d['p7']=(d.ph>=7)*1.0; d['p8']=(d.ph>=8)*1.0; add('year + phase >= 7',d,['tt','p7']); add('year + phase >= 8',d,['tt','p8'])
pd.DataFrame(R).round(3).to_csv(OUT+'trend_shape.csv',index=False); print(pd.DataFrame(R).round(2).to_string(index=False))
# latest-round cascade
Lt=E.sort_values('year').groupby(['outcome','country']).tail(1); Lr=Lt.groupby('outcome').apply(lambda g:pd.Series(dict(surveys=len(g),median_year=g.year.median(),simple_mean=100*g.p.mean(),n_weighted=100*np.average(g.p,weights=g.n))),include_groups=False).round(1); print(Lr); Lr.to_csv(OUT+'latest_round_cascade.csv')
for lo in [2018]:
    q=E[E.year>=lo].groupby('outcome').apply(lambda g:pd.Series(dict(surveys=len(g),mean=100*g.p.mean(),nw=100*np.average(g.p,weights=g.n))),include_groups=False).round(1); print('since',lo);print(q); q.to_csv(OUT+'since2018_cascade.csv')
# sources as % of all survivors
W=pd.concat([pd.read_csv(f'data/derived/{f}',low_memory=False,dtype={'caseid':str}) for f in ['women_disclosure.csv.gz','women_disclosure_heldout.csv.gz']]); W=W[W.sought_any.notna()&(W.vtype!='none')&(W.survey!='SN_2019_8B')&(W.v012<=24)]
W['w']=W.d005/1e6; src=[c for c in W.columns if c.startswith('src_')]; rows=[]
for c in src:
    d=W[W[c].notna()]; a=np.average(d[c],weights=d.w); s=d[d.sought_any==1]; rows.append(dict(source=c,pct_all_survivors=100*a,pct_help_seekers=100*np.average(s[c],weights=s.w),n=len(d)))
pd.DataFrame(rows).round(2).to_csv(OUT+'sources_denominators.csv',index=False); print(pd.DataFrame(rows).round(2).to_string(index=False))
# who is behind non-partner physical violence
pairs=pd.read_csv('data/derived/ir_ge_pairs.csv'); pairs=pairs[pairs.usable]; rows=[]; agg={}; labs_un=set()
for _,r in pairs.iterrows():
    f='data/raw/dhs_ir/'+r.ir
    if not os.path.exists(f): continue
    sv=f"{r.country}_{str(r.years)[:4]}_{r.ir[4:6]}"
    if sv not in set(W.survey): continue
    z=zipfile.ZipFile(f); raw=z.read([n for n in z.namelist() if n.lower().endswith('.dta')][0])
    with pd.io.stata.StataReader(io.BytesIO(raw)) as rd: lab=rd.variable_labels()
    if 'd115y' not in lab: continue
    ds=[v for v in lab if re.match(r'^d115[a-z]+$',v) and v!='d115y' and not (lab[v] or '').lower().startswith('na -')]
    d=pd.read_stata(io.BytesIO(raw),columns=['v012','d005','d115y','d119y']+ds,convert_categoricals=False); d=d[(d.v012<=24)&(d.d115y==0)&d.d119y.notna()&(d.d005>0)]
    if not len(d): continue
    w=d.d005/1e6
    def cat(s):
        s=s.lower().replace('person who has ever physically hurt respondent','').replace('physically hurt by','').replace('hit by','')
        s=re.sub(r'\bperson\b.*','',s) if ':' in s else s
        if re.search(r'husband|partner|boyfriend',s): return 'partner, former partner or boyfriend'
        if re.search(r'mother|father|sister|brother|daughter|\bson\b|relative|in-law|step|family|parent|child',s): return 'family'
        if re.search(r'teacher|employer|police|soldier|friend|acquaint|stranger|neighbo|other|colleague|boss|priest|religious|doctor',s): return 'teacher, employer, others'
        labs_un.add(lab[v] if False else s); return 'unclassified'
    g={}
    for v in ds: g.setdefault(cat(lab[v] or ''),[]).append(v)
    for c,vs in g.items(): agg.setdefault(c,[0,0]); agg[c][0]+=(d[vs].eq(1).any(axis=1)*w).sum(); agg[c][1]+=w.sum()
    rows.append(sv)
o=pd.DataFrame([dict(group=k,pct=100*v[0]/v[1]) for k,v in agg.items()]).round(1); o.to_csv(OUT+'nonpartner_physical_perpetrators.csv',index=False); print(len(rows),'surveys'); print(o.to_string(index=False))
