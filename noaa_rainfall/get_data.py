import location_id_constants
import requests
newline_char=ord("\n")
city_url_format = "https://www.ncdc.noaa.gov/cag/city/time-series/%s-pcp-%s-%s-1895-2019.csv"
county_url_format = "https://www.ncdc.noaa.gov/cag/county/time-series/CA-%s-pcp-%s-%s-1895-2019.csv"
#helper method to get rid of first three lines of string for header
def remove_first_3_lines(string):
	for i in range(1,4) : 
		newline_idx	= string.find(newline_char) 
		string		= string[ newline_idx+1 : ]
	return string
"""
method to get the data for a city
city_name	: the name of the requested city
month_range : can either be num months up till '12', or 'all' for all months
start_month : the month to start the range at
output_file	: the file to output the csv data to
"""
def get_city_data( city_name, month_range='all', start_month='1' ):
	#get csv data
	city_id = location_id_constants.city_ids[ city_name ]
	response = requests.get(city_url_format % (city_id, month_range, start_month )).content
	trimmed_response = remove_first_3_lines( response )
	#write csv to file	
	output_file = open("%s_precip_data.csv" % city_name, "wb")
	output_file.write( trimmed_response )
	output_file.close()
"""
method to get the data for a county
city_name	: the name of the requested city
month_range : can either be num months up till '12', or 'all' for all months
start_month : the month to start the range at
output_file	: the file to output the csv data to
"""
def get_city_data( county_name, month_range='all', start_month='1' ):
	#get csv data
	county_idx 	= str(location_id_constants.list_of_CA_counties.index( county_name )+1).zfill(3)
	response = requests.get(county_url_format % (county_idx, month_range, start_month )).content
	trimmed_response = remove_first_3_lines( response )
	#write csv to file	
	output_file = open("%s_county_precip_data.csv" % county_name, "wb")
	output_file.write( trimmed_response )
	output_file.close()
" method to get all the coast counties data "
def get_coastal_counties ():
	for county in location_id_constants.list_of_CA_counties_on_coast:
		get_city_data( county )	
get_coastal_counties()
