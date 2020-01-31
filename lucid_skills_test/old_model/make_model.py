import json
import joblib as jl
import pandas as pd
import xgboost as xgb

# Read in data source and split label from features
train_df = pd.read_csv('train.csv')
y = train_df[['Survived']]
x = train_df.drop(['Survived', 'Name', 'PassengerId'], axis=1)

# Clean features
gender_dict = {'male': 0, 'female': 1}
x['Sex'] = x['Sex'].map(gender_dict)
age_avg = x['Age'].mean()
x['Age'] = x['Age'].fillna(age_avg)
x['Cabin'] = x['Cabin'].fillna('Unknown')
x = pd.get_dummies(x, prefix=['Ticket', 'Cabin', 'Embarked'], columns=['Ticket', 'Cabin', 'Embarked'])

# Build model
model = xgb.XGBClassifier()
model.fit(x.to_numpy(), y.values.ravel())

# Save model
model_file = 'model.pkl'
jl.dump(model, model_file)
column_names = list(x.columns)
model_variables = {'column_names': column_names, 'age': age_avg}
with open('model_variables.json', 'w') as f:
    json.dump(model_variables, f)