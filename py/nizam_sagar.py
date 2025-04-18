from scipy.stats import gumbel_l
import pandas

df = pandas.read_excel('data/nizam_sagar_inflow.xlsx', index_col="l No", parse_dates=["date"])

df_clean = df[df['Inflow (Cusecs)'].notna()]

df_clean['Year'] = df_clean['date'].dt.year
annual_max = df_clean.groupby('Year')['Inflow (Cusecs)'].max()

print(annual_max)

params_gumbel = gumbel_l.fit(annual_max.values)
print(params_gumbel)