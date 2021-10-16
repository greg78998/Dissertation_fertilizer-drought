#!/usr/bin/env python
# coding: utf-8

# # Récupérer les données météorologiques CHIRPS
# 
# ## Objet de mémoire : 
# 
# Mesurer l'impact des sécheresses sur l'usage des engrais (adoption et utilisation) des agriculteurs ougandais entre 2009 et 2020. 
# 
# ## Méthodologie : 
# 
# ### 2 bases de données météo : 
# - Standardised Precipitation Evapotranspiration Index (SPEI)
# - CHIRPS 
# 
# ### Description de la base CHIRPS : 
# 
# L'avantage de ces cartes est d'avoir un point au plus proche des agriculteurs pour au mieux décrire leur situation. Ces données permettent une très grande précision (possibilité de descendre à un niveau de 4 km2).
# 
# ### Description du programme : 
# 
# Ce programme vise à récupérer les informations contenus dans des photos (base : CHIRPS). Il y a une carte par jours entre janvier 1980 et mai 2020. Ce programme va créer des fichiers csv pour stocker l'ensemble de l'information. Un fichier pour un point qui observe le point à toute les périodes (en jours entre 1980 et 2020). 
# 
# Par la suite, un programmme R va ensuite récupérer l'ensemble de ces fichiers pour former une base. 

# In[1]:


import geopandas as gpd 
import os 
import rasterio
import scipy.sparse as sparse 
import pandas as pd
import numpy as np


table = pd.DataFrame(index = np.arange(0,1))

names = "Projection_final"

stations = pd.read_csv("/Users/gregoirehaniquaut/Desktop/Dissertation/Recensement/baseArrangée26_12/"+names+".csv")

stations.rename(columns = {"Long" : 'lon' , "Lat" : "lat"}, inplace = True)


stations['station'] = round(stations["lon"],3).astype(str) +"_" + round(stations["lat"],3).astype(str)
stations.head()

# read the points shapefile using Geopandas

# iterate 
Matrix = pd.DataFrame()

for files in os.listdir("./Desktop/Dissertation/CHIRPS_base/aa") :
    if files[-4:] == ".tif" : 
        dataset = rasterio.open("./Desktop/Dissertation/CHIRPS_base/aa/"+files)
        data_array = dataset.read(1)
        data_array_sparse = sparse.coo_matrix(data_array, shape = (120,160))
        data = files[:-4]
        Matrix[data] = data_array_sparse.toarray().tolist()
        print('Processing is done for the raster: '+ files[:-4])
        
print("First step")

for index, row in stations.iterrows():
    try : 
        station_name = str(row['station'])
        lon = float(row['lon'])
        lat = float(row['lat'])
        x,y = (lon, lat)
        row, col = dataset.index(x, y)
        print('Processing: '+ station_name)


        # Pick the rainfall value from each stored raster array and record it into the previously created 'table'
        for records_date in Matrix.columns.tolist():
            try : 
                a = Matrix[records_date]
                rf_value = a.loc[int(row)][int(col)]
                table[records_date] = rf_value
                transpose_mat = table.T
                transpose_mat.rename(columns = {0: 'station'}, inplace = True)
            except : 
                pass
    
        transpose_mat.to_csv("./Desktop/Dissertation/CHIRPS_ProjetFinal/"+station_name+".csv") 
    except : 
        pass


# In[38]:


import pandas as pd 
stations = pd.read_csv("/Users/gregoirehaniquaut/Desktop/Dissertation/Recensement/baseArrangée26_12/"+names+".csv")
stations.shape 


# In[72]:


ls = []

for i in os.listdir("./Desktop/Dissertation/CHIRPS_ProjetFinal") :
    ls.append(i[:-4])

nom = pd.DataFrame({"stationFichier" : ls,"nb" : True})
stations['station'] = round(stations["Long"],3).astype(str) +"_" + round(stations["Lat"],3).astype(str)

