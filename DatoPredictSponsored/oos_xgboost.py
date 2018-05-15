import pandas as pd
import os
from sklearn.ensemble import RandomForestClassifier
from sklearn import svm, grid_search, cross_validation
import pickle
import xgboost as xgb
from sklearn.cross_validation import StratifiedKFold
import numpy as np
 
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
 
datasets = [keywords_counts, boris_counts, new_tag_features, my_new_features]
#initialization
cols_to_use = char_tag.columns - meta_data.columns
df_full = pd.concat([meta_data, char_tag[cols_to_use]], axis = 1)
for ind, dataset in enumerate(datasets):
    cols_to_use = dataset.columns - df_full.columns
    df_full = pd.concat([df_full, dataset[cols_to_use]], axis = 1)
 
dtrain = df_full[df_full.sponsored.notnull()].fillna(0)
dtest = df_full[df_full.sponsored.isnull() & df_full.file.isin(test_files)].fillna(0)
 
param = {'max_depth':16, 'eta':0.05, 'objective':'binary:logistic', 'eval_metric':'auc', 'nthread':4}
xgb_train = xgb.DMatrix(dtrain.drop(['file', 'sponsored'], 1), label = dtrain.sponsored)
xgb_test = xgb.DMatrix(dtest.drop(['file', 'sponsored'], 1))
 
skf = list(StratifiedKFold(dtrain.sponsored, 5))
 
np_train = np.array( dtrain.drop(['file', 'sponsored'], 1))
label_train = np.array(dtrain.sponsored)
 
dataset_blend_train = np.zeros(len(dtrain))
for i, (train, test) in enumerate(skf):
    print "Fold", i
    X_train = np_train[train]
    X_test = np_train[test]
    Y_train = label_train[train]
    Y_test = label_train[test]
    xgb_train_cur = xgb.DMatrix(X_train, label = Y_train)
    xgb_test_cur = xgb.DMatrix(X_test)
    bst = xgb.train( param, xgb_train_cur, 800 )
    y_submission = bst.predict(xgb_test_cur)
    dataset_blend_train[test] = y_submission
 
xgb_oos = pd.DataFrame({'file':dtrain.file, 'sponsored':dataset_blend_train})
xgb_oos.to_csv('created_data/xgb_oos.csv', index = False)
bst = xgb.train( param, xgb_train, 800 )
xgb_pred = bst.predict(xgb_test)
 
#print('--- Create predictions and submission')
#submission = test[['file']].reset_index(drop=True)
#submission['sponsored'] = xgb_pred
#submission.to_csv('xgboost_on_all_features_AUC_962.csv', index=False)
