#!/usr/bin/env python

#-------------------------------------------------------------------------------------------
# Name:  getMetData_netCDF4.py
#-------------------------------------------------------------------------------------------
# Written by Loretta Clancy, adapted from script provided by Eric Zurcher
# Written in Python 3.6
#-------------------------------------------------------------------------------------------
# Purpose:
#   Retrieves data from a netcdf file that contains Silo weather data for the 5km grid of 
#   weather stations in Australia
#   The data is then collated into all data for a single location (latitude, longitude),
#   and ouptuts the data in APSIM format saved as a text file with the extention '.met'.
#-------------------------------------------------------------------------------------------
# Usage:
#   Version 1:  silo2apsim(['--lat', int, '--long', int, '--filename', string])
#               silo2apsim(['--lat', -28.30, '--long', 114.95, '--filename', "\\ag-osm-02-cdc.it.csiro.au\OSM_CBR_AG_WHEATTEMP_work\ApsimNG\LocationList.csv"])
#   Version 2:  silo2apsim(['--id', int])
#               silo2apsim(['--id', 4])
#-------------------------------------------------------------------------------------------

import argparse
import sys, getopt
from netCDF4 import Dataset, num2date, date2index, date2num
from datetime import datetime, timedelta
import os
import pandas as pd
import math

workDir = "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG/"
outputDir = workDir + "met/"
netcdfFile = workDir + "Preparation/silo-apsim-all-1889-2017.nc"
logFilename = workDir + "metLog.txt"

def silo2apsim(idNo, infile):
    latitude = 0
    longitude = 0

    df = pd.read_csv(workDir + infile)

    latitude = df.loc[int(idNo)-1, 'lat']
    longitude = df.loc[int(idNo)-1, 'long']
    filename = outputDir + 'Silo_' + '{0:.2f}'.format(longitude) + "_" +  '{0:.2f}'.format(latitude) + '.met'

    #--------------------------------------------------------------------------
    if (latitude < 0):
        latitude = -latitude

    nearestLat = int( (latitude * 100 + 2.5) / 5) * 5
    nearestLong = int( (longitude * 100 + 2.5) / 5) * 5
    latIdx = (nearestLat - 1000) // 5
    longIdx = (nearestLong - 11200) // 5

    met = Dataset(netcdfFile, "r")
    maxt = met['temp_max_day']
    mint = met['temp_min_day']
    evap = met['evap_span_day']
    radn = met['solar_exposure_day']
    rain = met['rain_day']
    vp = met['vp_day']
    time = met['time']
    lat = met['latitude']
    long = met['longitude']

    startDate = datetime(1950, 1, 1)
    startIdx = date2index(startDate, time)

    endDate = datetime(2017, 12, 31)
    endIdx = date2index(endDate, time)

    maxtData = maxt[startIdx:endIdx+1, latIdx, longIdx]
    mintData = mint[startIdx:endIdx+1, latIdx, longIdx]
    evapData = evap[startIdx:endIdx+1, latIdx, longIdx]
    radnData = radn[startIdx:endIdx+1, latIdx, longIdx]
    rainData = rain[startIdx:endIdx+1, latIdx, longIdx]
    vpData = vp[startIdx:endIdx+1, latIdx, longIdx]

    oneDay = timedelta(days=1)

    #-------------------------------------------------------------------------------------------------------
    # First we need to calculate tav/amp. SILO does this starting from 1 Jan 1957, and going to the present
    #-------------------------------------------------------------------------------------------------------
    tavStart = datetime(1957, 1, 1)
    tavDate = tavStart
    nYears = 2017 - 1957 + 1
    monthVal = [[0 for x in range(12)] for y in range(nYears)]
    monthCount = [[0 for x in range(12)] for y in range(nYears)]
    while tavDate <= endDate:
        delta = tavDate - startDate
        idx = delta.days
        aveTemp = (maxtData[idx] + mintData[idx]) / 2.0
        monthVal[tavDate.year - 1957][tavDate.month - 1] += aveTemp
        monthCount[tavDate.year - 1957][tavDate.month - 1] += 1
        tavDate += oneDay

    aveTempYear = [0 for x in range(nYears)]
    ampYear = [0 for x in range(nYears)]
    for year in range(nYears):
        maxMonth = -100
        minMonth = 100
        for month in range(12):
            monthVal[year][month] = monthVal[year][month] / monthCount[year][month]
            minMonth = min(minMonth, monthVal[year][month])
            maxMonth = max(maxMonth, monthVal[year][month])
        aveTempYear[year] = sum(monthVal[year]) / 12
        ampYear[year] = maxMonth - minMonth

    tav = sum(aveTempYear) / nYears
    amp = sum(ampYear) / nYears

    if  math.isnan(tav) == True:
        logfile = open(logFilename, "a")
        logfile.write('\nError process file: ' + filename)
        logfile.close() 
    else:
        outfile = open(filename, "w")
        outfile.write('[weather.met.weather]')
        outfile.write('\n')
        outfile.write('\nlatitude = %6.2f  (DECIMAL DEGREES)' % (-latitude))
        outfile.write('\nlongitude = %6.2f  (DECIMAL DEGREES)' % (longitude))
        outfile.write('\n')
        outfile.write('\ntav = %5.2f (oC) ! Annual average ambient temperature. Based on 1 Jan 1957 to 2017.' % (tav))
        outfile.write('\namp = %5.2f (oC) ! Annual amplitude in mean monthly temperature. Based on 1 Jan 1957 to 2017.' % (amp))
        outfile.write('\n')
        outfile.write('\nyear   day      radn   maxt    mint   rain   evap      vp     code')
        outfile.write('\n  ()    ()  (MJ/m^2)   (oC)    (oC)   (mm)   (mm)   (hPa)       ()')

        date = startDate
        idx = 0

        reportStartDate = datetime(1957, 1, 1)
        while date <= endDate:
            if date >= reportStartDate:
                outfile.write('\n%4d   %3d    %6.1f  %5.1f   %5.1f  %5.1f  %5.1f   %5.1f   999999' % (date.year, date.timetuple().tm_yday, 
radnData[idx], maxtData[idx], mintData[idx], rainData[idx], evapData[idx], vpData[idx]))
            date += oneDay
            idx += 1
       
        outfile.close()

            
            
def main(args):
    parser = argparse.ArgumentParser(description="Generate APSIM met files from netCDF files")
    parser.add_argument("-i", "--idNo", help="The row number in the file to access for long & lat details.")
    parser.add_argument("-f", "--filename", help="The name of the txt file witht he Locations listed.")

    args = parser.parse_args()
    idNo = args.idNo
    infile = args.filename
    silo2apsim(idNo, infile)


if __name__ == '__main__':
    main(sys.argv[1:])

#--------------------------------------------------------------------------
# VERSION 2 testing:  original args which includes multiple parameters
#--------------------------------------------------------------------------
#if __name__ == "__main__":
#    silo2apsim(['--id', 4])
	
#--------------------------------------------------------------------------
# VERSION 1 testing:  original args which includes multiple parameters
#--------------------------------------------------------------------------
#if __name__ == '__main__':
#    #this will clear the logfile
#    logfile = open(logFilename, "w")
#    logfile.write('Processing files')
#    logfile.close() 
#    #now open the file containing the list of locations
#    df = pd.read_csv(locationListfile)
#    for index, row in df.iterrows():
#        latitude = row['lat']
#        longitude = row['long']
#        nearestLat = int( (-latitude * 100 + 2.5) / 5) * 5;
#        nearestLong = int( (longitude * 100 + 2.5) / 5) * 5;
#        strLat = str(nearestLat).zfill(4)
#        strLong = str(nearestLong).zfill(5)
#        filename = outputDir + 'Silo_' + '{0:.2f}'.format(longitude) + "_" +  '{0:.2f}'.format(latitude) + '.met'
#        if not os.path.exists(filename):
#            lat = str(round(-nearestLat/100.0, 2))
#            long = str(round(nearestLong/100.0, 2))
#            silo2apsim(['--lat', lat, '--long', long, '--filename', filename])
#            print('Data written for latitude ' + strLat + ' longitude ' + strLong)



