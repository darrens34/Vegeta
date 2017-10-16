import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objs as go
import pandas as pd

app = dash.Dash()

df = pd.read_csv("cleanData.csv", parse_dates=[1,2])
df['hour'] = df['TIMESTAMP_END'].apply(lambda x : x.hour)
df['test'] = 1
df = df.set_index('TIMESTAMP_END').groupby('hour')[['SAP_FLOW','test']].mean()


app.layout = html.Div([
    dcc.Graph(
        id='life-exp-vs-gdp',
        figure={
            'data': [
                go.Scatter(
                    x=df[df['test'] == i].index,
                    y=df[df['test'] == i]['SAP_FLOW'],
                    text="Hour / SAP_FLOW",
                    mode='markers',
                    opacity=0.7,
                    marker={
                        'size': 15,
                        'line': {'width': 0.5, 'color': 'white'}
                    },
                    name=i
                ) for i in df.test.unique()
            ],
            'layout': go.Layout(
                xaxis={'title': 'HOUR'},
                yaxis={'title': 'SAP_FLOW'},
                margin={'l': 40, 'b': 40, 't': 10, 'r': 10},
                legend={'x': 0, 'y': 1},
                hovermode='closest'
            )
        }
    )
])

if __name__ == '__main__':
    app.run_server(debug=True)