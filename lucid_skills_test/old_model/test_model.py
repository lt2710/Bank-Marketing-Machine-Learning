import json
import joblib as jl
import pandas as pd

# Read in data source
test_df = pd.read_csv('test.csv')
x = test_df.drop(['Name', 'PassengerId'], axis=1)

# Read in variables
with open('model_variables.json', 'r') as f:
    model_variables = json.load(f)

age = model_variables['age']
column_names = model_variables['column_names']

# Clean features
gender_dict = {'male': 0, 'female': 1}
x['Sex'] = x['Sex'].map(gender_dict)
x['Age'] = x['Age'].fillna(age)
x['Cabin'] = x['Cabin'].fillna('Unknown')
x = pd.get_dummies(x, 
                   prefix=['Ticket', 'Cabin', 'Embarked'], 
                   columns=['Ticket', 'Cabin', 'Embarked']
                  )
curr_column_names = list(x.columns)

# Fix column names and order
for col in column_names:
    if not (col in curr_column_names):
        x[col] = 0

drop_list = []
for curr_col in curr_column_names:
    if not (curr_col in column_names):
        drop_list.append(curr_col)
x = x.drop(drop_list, axis=1)
x = x[column_names]

# Predict results
model = jl.load('model.pkl')
y = model.predict(x.to_numpy())
print(y)