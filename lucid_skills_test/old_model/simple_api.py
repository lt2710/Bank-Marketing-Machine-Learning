import json
import xgboost
import urlparse
import SocketServer
import pandas as pd
import joblib as jl
from BaseHTTPServer import BaseHTTPRequestHandler

def convert_query(query):
    # Parse response
    columns = [col.split('=')[0] for col in query.split('&')]
    values = [val.split('=')[1] for val in query.split('&')]
    input_df = pd.DataFrame([values], columns=columns)
    return input_df

def run_model(test_df):
    passenger_id = test_df[['PassengerId']].values[0][0]
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
    if y == 0:
        return 'Passenger {}: predicted to be dead.'.format(passenger_id)
    else:
        return 'Passenger {}: predicted to have survived.'.format(passenger_id)

class MyHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        path = urlparse.urlparse(self.path)
        input_df = convert_query(path.query)
        result = run_model(input_df)
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()
        self.wfile.write(str(result))

httpd = SocketServer.TCPServer(("", 8080), MyHandler)
httpd.serve_forever()