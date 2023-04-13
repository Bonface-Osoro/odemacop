import pandas as pd
from tqdm import tqdm

data_path = '/Users/osoro/Github/odemacop/data/'
results_path = '/Users/osoro/Github/odemacop/results/'

df = pd.read_excel(data_path + "image_processing_data.xlsx")

df = pd.melt(df, id_vars = ["elevation", "flight", "iteration"], 
    value_vars = ["RGB-LWIR_IP", "RGB_IP", "LWIR_IP", "RGB-LWIR", "RGB", "LWIR"],
    var_name ="band", value_name ="accuracy")

df["image_processing"] = "" 

for i in tqdm(df.index, desc = "Processing data"):
    if df["band"].loc[i] == "RGB-LWIR_IP":
        df["image_processing"].loc[i] = "Yes"
        df["band"].loc[i] = "RGB-LWIR"
    elif df["band"].loc[i] == "RGB_IP":
        df["image_processing"].loc[i] = "Yes"
        df["band"].loc[i] = "RGB"
    elif df["band"].loc[i] == "LWIR_IP":
        df["image_processing"].loc[i] = "Yes"
        df["band"].loc[i] = "LWIR"
    else:
        df["image_processing"].loc[i] = "No"
df.head()

df.to_csv(results_path + "mission_emission_results.csv")