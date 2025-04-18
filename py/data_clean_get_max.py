import pandas as pd

df = pd.read_csv('nagari.csv', header=None, names=[
    "DTC",
    "DTD",
    "Time",
    "Val",
    "Unit"
])

df_cleaned = df[df['Val'] != 0]

df_cleaned['Time'] = pd.to_datetime(df_cleaned['Time'])
df_cleaned['Year'] = df_cleaned['Time'].dt.year
annual_max = df_cleaned.groupby('Year')['Val'].max()

df_cleaned.to_csv('nagari_cleaned.csv', index=False)
annual_max.to_csv('nagari_ams.csv')