import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from mpl_toolkits.basemap import Basemap
from matplotlib import rc

rc("font", **{"family":"sans-serif", "sans-serif":["Fira Sans"]})

def draw_us_map():
    
    bakkendat = pd.read_csv(r"data/US_OG_BAKKEN Production Headers.CSV")
    
    surflat = bakkendat['Surface Latitude (WGS84)']
    surflon = bakkendat['Surface Longitude (WGS84)'] 
    
    # Set the lower left and upper right limits of the bounding box:
    lllon = -104
    urlon = -102
    lllat = 47.2
    urlat = 48.2
    
    #lllon = np.floor(np.min(surflon))
    #urlon = np.ceil(np.max(surflon))
    #lllat = np.floor(np.min(surflat))
    #urlat = np.ceil(np.max(surflat))
    
    # and calculate a centerpoint, needed for the projection:
    centerlon = float(lllon + urlon) / 2.0
    centerlat = float(lllat + urlat) / 2.0

    m = Basemap(resolution='i',  # crude, low, intermediate, high, full
                llcrnrlon = lllon, urcrnrlon = urlon,
                lon_0 = centerlon,
                llcrnrlat = lllat, urcrnrlat = urlat,
                lat_0 = centerlat,
                projection='tmerc', ax=ax)
    
    for lat, lon in zip(surflat, surflon):
        x, y = m(lon, lat)
        m.plot(x, y, 'kx', mfc='none')
        
    ax.text(100190, 46343, 'Dunn County, ND', fontsize=16)
    ax.text(9604, 45631, 'McKenzie County, ND', fontsize=16)
    ax.text(114656, 79000, 'Mountrail County, ND', fontsize=16)
    ax.text(11738, 106877, 'Williams County, ND', fontsize=16)
    # Read state boundaries.
    #shp_info = m.readshapefile('basemap//st99_d00', 'states',
                               #drawbounds=True, color='lightgrey')

    # Read county boundaries
    shp_info = m.readshapefile(r'\basemap\cb_2017_us_county_5m', 'counties', drawbounds=True)

if __name__ == "__main__":
    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(16, 9)
    
    ax.axis('off')

    draw_us_map()
        
    # Get rid of some of the extraneous whitespace matplotlib loves to use.
    plt.tight_layout(pad=0, w_pad=0, h_pad=0)
    plt.savefig("1-3.svg", dpi=600, transparent=True, format="svg")
    plt.show()
    
    print('A')