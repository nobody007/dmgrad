import numpy as np, pandas as pd, xgboost as xgb, optuna, itertools, time, json
from sklearn.model_selection import train_test_split, StratifiedKFold
from sklearn.metrics import roc_auc_score
optuna.logging.set_verbosity(optuna.logging.WARNING)
d=pd.read_csv('bank.csv',sep=';').drop(columns=['duration'])
y=(d.pop('y')=='yes').astype(int).values
X=pd.get_dummies(d,drop_first=False).astype(float)
Xtr,Xte,ytr,yte=train_test_split(X,y,test_size=0.2,stratify=y,random_state=2026)
dtr=xgb.DMatrix(Xtr,label=ytr); dte=xgb.DMatrix(Xte,label=yte)
folds=list(StratifiedKFold(5,shuffle=True,random_state=1).split(Xtr,ytr))
def cv_auc(p):
    params=dict(objective='binary:logistic',eval_metric='auc',eta=0.05,nthread=2,
                max_depth=int(p['max_depth']),min_child_weight=p['min_child_weight'],
                subsample=p['subsample'],colsample_bytree=p['colsample_bytree'],reg_lambda=p['lambda'])
    r=xgb.cv(params,dtr,num_boost_round=2000,folds=folds,early_stopping_rounds=50,verbose_eval=False)
    return float(r['test-auc-mean'].iloc[-1]), len(r)
log={}
# GRID 3^5
grid=dict(max_depth=[2,4,6],min_child_weight=[1,7,50],subsample=[0.5,0.75,1.0],colsample_bytree=[0.5,0.75,1.0],**{'lambda':[0.01,1,100]})
t=time.time(); g=[]
for vals in itertools.product(*grid.values()):
    p=dict(zip(grid.keys(),vals)); a,n=cv_auc(p); g.append(dict(p,auc=a,nrounds=n))
log['grid']=dict(res=g,time=time.time()-t); print('grid',max(x['auc'] for x in g),time.time()-t,flush=True)
def obj(trial):
    p=dict(max_depth=trial.suggest_int('max_depth',2,8),min_child_weight=trial.suggest_float('min_child_weight',1,50,log=True),
           subsample=trial.suggest_float('subsample',0.5,1.0),colsample_bytree=trial.suggest_float('colsample_bytree',0.5,1.0),
           **{'lambda':trial.suggest_float('lambda',1e-2,100,log=True)})
    a,n=cv_auc(p); trial.set_user_attr('nrounds',n); return a
for name in ['random','tpe']:
    log[name]=[]
    for seed in [0,1,2]:
        samp=optuna.samplers.RandomSampler(seed=seed) if name=='random' else optuna.samplers.TPESampler(seed=seed,n_startup_trials=10)
        t=time.time(); st=optuna.create_study(direction='maximize',sampler=samp); st.optimize(obj,n_trials=60)
        vals=[tr.value for tr in st.trials]
        log[name].append(dict(vals=vals,best=st.best_value,params=st.best_params,nrounds=st.best_trial.user_attrs['nrounds'],time=time.time()-t))
        print(name,seed,round(st.best_value,4),round(time.time()-t),st.best_params,flush=True)
# test AUC: default vs grid best vs tpe best (seed0)
def test_auc(p,n):
    params=dict(objective='binary:logistic',eval_metric='auc',eta=0.05,nthread=2,max_depth=int(p['max_depth']),min_child_weight=p['min_child_weight'],subsample=p['subsample'],colsample_bytree=p['colsample_bytree'],reg_lambda=p['lambda'])
    b=xgb.train(params,dtr,num_boost_round=n); return float(roc_auc_score(yte,b.predict(dte)))
gb=max(g,key=lambda x:x['auc']); tb=log['tpe'][0]
log['test']=dict(default=float(roc_auc_score(yte,xgb.XGBClassifier(n_jobs=2).fit(Xtr,ytr).predict_proba(Xte)[:,1])),
                 grid=test_auc(gb,gb['nrounds']),tpe=test_auc(tb['params'],tb['nrounds']))
print(log['test'])
json.dump(log,open('py_log.json','w'))
