#!/usr/bin/env python

#-------------------------------------------------------------------------------------------
# Name:  generateMetGrid_Soil.py
#-------------------------------------------------------------------------------------------
# Written by Loretta Clancy
# Written in Python 3.6
#-------------------------------------------------------------------------------------------
# Usage:
# python generateMetGrid_Soil.py -s 11 -f 20

import argparse
import os
import sys
import pandas as pd
import math
from haversine import haversine

workingDir = "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC"
metSourceDir = workingDir + "/met"
simOutputDir = workingDir + "/sim"
baseApsimXML = workingDir + "/prep/BaseWithCultivars.apsimx"
metGridCsv = workingDir + "/gridMet.csv"
soilGridCsv = workingDir + "/gridSoil.csv"
outputCsv = workingDir + "/gridMet_Soil"

soilSourceDir = "/OSM/CBR/AG/WHEATTEMP/work/ApsimNG-test/APSIM_prep/SoilXML"


def getMetGrid():
    # Get a list of met stations
    if os.path.isfile(metGridCsv):
        metGrid = pd.read_csv(metGridCsv)
    else:
        metList = sorted(os.listdir(metSourceDir))
        metGrid = pd.DataFrame(metList, columns=['filename'])

        #extract the long and lat
        #weather file names:  silo_11410-2720.met
        metGrid['long'] = metGrid.filename.apply(lambda x: int(x[5:10])/100)
        metGrid['lat'] = -(metGrid.filename.apply(lambda x: int(x[11:15])/100))

        metGrid['soilLong'] = ""
        metGrid['soilLat'] = ""
        metGrid.head()
        metGrid.to_csv(metGridCsv, sep=',', encoding='utf-8', index=False)

    return metGrid


def getSoilGrid():
    if os.path.isfile(soilGridCsv):
        soilGrid = pd.read_csv(soilGridCsv)
    else:
        #Get a list of the soils
        soilList = sorted(os.listdir(soilSourceDir))
        #convert this into a dataframe of long and lat coordiantes
        soilGrid = pd.DataFrame(soilList, columns=['filename'])

        #extract the long and lat
        #soil file names: 11410-2721.soil
        soilGrid['long'] = soilGrid.filename.apply(lambda x: int(x[0:5])/100)
        soilGrid['lat'] = -(soilGrid.filename.apply(lambda x: int(x[6:10])/100))
        soilGrid.head()
        soilGrid.to_csv(soilGridCsv, sep=',', encoding='utf-8', index=False)

    return soilGrid


def getClosestSoil(long, lat, soilGrid):
    #soil files may/may not match the met file location exactly, so need to determine the closes match to the met file and get that soil file
    #long = 149.50
    #lat =  -30.25

    soilGrid['dist'] = soilGrid.apply(lambda row: haversine((long, lat), (row['long'], row['lat'])), axis=1)

    temp = soilGrid[soilGrid['dist'] == min(soilGrid['dist'])]
    return temp



def processMetGrid(startRow, endRow):


    #filter the metGrid based on the row numbers
    metGrid = getMetGrid()
    print("metGrid.rows: " + str(metGrid.shape[0]))
    soilGrid = getSoilGrid()
    print("soilGrid.rows: " + str(soilGrid.shape[0]))

    gridRows = metGrid.shape[0]
    if (endRow > gridRows):
        endRow = gridRows

    print("startRow:endRow " + str(startRow) + ":" + str(endRow))

    gridData = metGrid[int(startRow)-1:int(endRow)]
    print("gridData.rows: " + str(gridData.shape[0]))

    #get the closed soil location to our met station
    #soilFilename = GenFuncs.getClosestSoil(long, lat, soilGrid) + ".soil"
    #dist = getClosestSoil(metGrid2['long'][0], metGrid2['lat'][0], soilGrid)
    #dist = getClosestSoil(114.0, -27.25, soilGrid)

    for index, eachRow in gridData.iterrows():
        print("long: " + str(eachRow['long']) + " - lat: " + str(eachRow['lat']))
        dist = getClosestSoil(eachRow[1], eachRow[2], soilGrid)
        #print(dist.shape)
        #print(dist)
        ix = dist.index[0]

        if dist is not None:
            gridData.at[index, 'soilLong'] = dist['long'][ix]
            gridData.at[index, 'soilLat'] = dist['lat'][ix]

    # Write the file out again
    newOutputCsv = outputCsv + "_" + str(startRow) + ".csv"
    print(newOutputCsv)
    gridData.to_csv(newOutputCsv, sep=',', encoding='utf-8', index=False)


def main(args):
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--startNo", type=int, help="The starting row number in the file.")
    parser.add_argument("-f", "--finalNo", type=int, help="The ending row number in the file.")

    args = parser.parse_args()
    startingNo = args.startNo
    endingNo = args.finalNo

    #print("start: " + str(startingNo) + " - end: " + str(endingNo))
    processMetGrid(startingNo, endingNo)


if __name__ == '__main__':
    main(sys.argv[1:])


