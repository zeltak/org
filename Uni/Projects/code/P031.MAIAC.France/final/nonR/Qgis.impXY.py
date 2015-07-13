#---1 Reference library
from PyQt4.QtGui import *
from PyQt4.QtCore import *
from qgis.core import *
from qgis.utils import iface

#---  2 Turn of the CRS dialog box
#s = QSettings()
#oldValidation = s.value( "/Projections/defaultBehaviour")
#s.setValue( "/Projections/defaultBehaviour", "useGlobal" )


#--- 3 Set csv file name here:  (inputfilename-'InFlnm')
InFlnm='met.lu.WK.rint.csv'

#--- 4  Set csv location pathname here (inputdirpath-'InDrPth')
InDrPth='home/zeltak/ZH_tmp/Rtmp/'

#--- 5 Build file name an path for uri (inputfilepath-'InFlPth')
InFlPth="file:///"+InDrPth+InFlnm

#---  6 Set import Sting here note only need to set x and y and CRS (end of uri string)
uri = InFlPth+"?delimiter=%s&xField=%s&yField=%s&crs=EPSG:4326" % (",","x","y")

#--- 7 Load point layer
implayer = QgsVectorLayer(uri, InFlnm, "delimitedtext")

#--- 8 Confirm something is loaded and valid
implayer.isValid()

#--- 10 Display the layer into QGIS 
QgsMapLayerRegistry.instance().addMapLayer(implayer)

#set style
implayer.loadNamedStyle('/home/zeltak/.qgis2/styles/france.fakeraster.qml')
#refresh 
implayer.triggerRepaint()











