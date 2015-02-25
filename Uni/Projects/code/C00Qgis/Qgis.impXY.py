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
InFlnm='qtrans.csv'

#--- 4  Set csv location pathname here (inputdirpath-'InDrPth')
InDrPth='/home/zeltak/ZH_tmp/R.tmp/'

#--- 5 Build file name an path for uri (inputfilepath-'InFlPth')
InFlPth="file:///"+InDrPth+InFlnm

#---  6 Set import Sting here note only need to set x and y other come for free
uri = InFlPth+"?delimiter=%s&xField=%s&yField=%s" % (",","x","y")

#--- 7 Load point layer
implayer = QgsVectorLayer(uri, InFlnm, "delimitedtext")

#--- 8 Confirm something is loaded and valid
implayer.isValid()

#--- 9 Set CRS (currently set to ITM (2039 in qgis)
implayer.setCrs( QgsCoordinateReferenceSystem(2039, QgsCoordinateReferenceSystem.EpsgCrsId) )


#--- 10 Display the layer into QGIS (but it asks for CRS before displaying_
QgsMapLayerRegistry.instance().addMapLayer(implayer)

#--- 11 turn CRS dialog box back on again
#s.setValue( "/Projections/defaultBehaviour", oldValidation )

#set style
implayer.loadNamedStyle('/home/zeltak/.qgis/fakeraster.qml')











