import pandas as pd
import os
from sklearn.ensemble import RandomForestClassifier
from sklearn import svm, grid_search, cross_validation
import pickle
import xboost as xgb
 
print('--- Read training labels')
os.chdir('/home/fedor/projects/kaggle/truly_native2')
 
train_labels = pd.read_csv('data/train_v2.csv')
train_keys = dict([a[1] for a in train_labels.iterrows()])
test_files = set(pd.read_csv('data/sampleSubmission_v2.csv').file.values)
 
 
meta_data = pd.read_pickle('features_dump/meta_len__script_len__keywords.pkl')
char_tag = pd.read_pickle('features_dump/char_tag_enc_cols.pkl')
keywords_counts = pd.read_pickle('features_dump/keywords_counts_lowercase.pkl')
boris_counts = pd.read_pickle('features_dump/boris_features.pkl')
new_tag_features = pd.read_pickle('features_dump/tag_features_v2.pkl')
my_new_features = pd.read_pickle('features_dump/some_new_keywords.pkl')
#vw_out_of_sample = pd.read_csv('features_dump_csv/vw_oos_probs.csv')
#vw_out_of_sample.columns = ['file', 'rf_oos']
 
datasets = [keywords_counts, boris_counts, new_tag_features, my_new_features]
#initialization
cols_to_use = char_tag.columns - meta_data.columns
df_full = pd.concat([meta_data, char_tag[cols_to_use]], axis = 1)
for ind, dataset in enumerate(datasets):
    cols_to_use = dataset.columns - df_full.columns
    df_full = pd.concat([df_full, dataset[cols_to_use]], axis = 1)
 
#rf = RandomForestClassifier(n_estimators=1000, n_jobs=4, random_state=0, verbose = 3, oob_score=True)
#clf_ntrees = grid_search.GridSearchCV(rf, parameters, verbose = 3)
train = df_full[df_full.sponsored.notnull()].fillna(0)
test = df_full[df_full.sponsored.isnull() & df_full.file.isin(test_files)].fillna(0)
 
#random forest cross validation
#scores = cross_validation.cross_val_score(rf, train.drop(['file', 'sponsored'], 1), train.sponsored, scoring = 'roc_auc')
#print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
 
param = {'max_depth':16, 'eta':0.05, 'objective':'binary:logistic', 'eval_metric':'auc', 'nthread':4}
params = {}
params["objective"] = "binary:logistic"
params["eta"] = 0.09
#params["min_child_weight"] = 4
params["subsample"] = 0.8
params["scale_pos_weight"] = 1
#params["silent"] = 1
params["max_depth"] = 200
params["eval_metric"] = "auc"
#params['nthread'] = 4
#gbm = xgb.XGBClassifier(max_depth=3, n_estimators=300, learning_rate=0.05)
xgb_train = xgb.DMatrix(train.drop(['file', 'sponsored'], 1), label = train.sponsored)
xgb_test = xgb.DMatrix(test.drop(['file', 'sponsored'], 1))
#cv = xgb.cv(params, xgb_train, 800, nfold=5)
bst = xgb.train( param, xgb_train, 800 )
 
#rf.fit(train.drop(['file', 'sponsored'], 1), train.sponsored)
#pd.DataFrame({'file':train.file, 'sponsored':rf.oob_decision_function_[:,1]}).to_pickle('out_of_sample_preds/rf_oob_more_features.pkl')
xgb_pred = bst.predict(xgb_test)
 
print('--- Create predictions and submission')
submission = test[['file']].reset_index(drop=True)
submission['sponsored'] = xgb_pred
submission.to_csv('xgboost_on_all_features_AUC_962.csv', index=False)
