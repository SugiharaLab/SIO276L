import location_id_constants
import requests
newline_char=ord("\n")
city_url_format = "https://www.ncdc.noaa.gov/cag/city/time-series/%s-pcp-%s-%s-1895-2019.csv"
county_url_format = "https://www.ncdc.noaa.gov/cag/county/time-series/CA-%s-pcp-%s-%s-1895-2019.csv"
state_url_format = "https://www.ncdc.noaa.gov/cag/statewide/time-series/%s-pcp-%s-%s-1895-2019.csv"
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
	output_file = open("city_data/%s_city_precip_data.csv" % city_name, "wb")
	output_file.write( trimmed_response )
	output_file.close()
"""
method to get the data for a county
city_name	: the name of the requested city
month_range : can either be num months up till '12', or 'all' for all months
start_month : the month to start the range at
output_file	: the file to output the csv data to
"""
def get_county_data( county_name, month_range='all', start_month='1' ):
	#get csv data
	county_id 	= location_id_constants.coastal_counties_ids[county_name]
	response  	= requests.get(county_url_format % (county_id, month_range, start_month )).content
	trimmed_response = remove_first_3_lines( response )
	#write csv to file
	output_file = open("coastal_data/%s_county_precip_data.csv" % county_name, "wb")
	output_file.write( trimmed_response )
	output_file.close()
"""
method to get the data for a state
state_name	: the name of the requested city
month_range : can either be num months up till '12', or 'all' for all months
start_month : the month to start the range at
output_file	: the file to output the csv data to
"""
def get_state_data( state_name, month_range='all', start_month='1' ):
	#get csv data
	state_id 	= location_id_constants.state_ids[state_name]
	response  	= requests.get(state_url_format % (state_id, month_range, start_month )).content
	trimmed_response = remove_first_3_lines( response )
	#write csv to file
	output_file = open("state_data/%s_state_precip_data.csv" % state_name, "wb")
	output_file.write( trimmed_response )
	output_file.close()


def get_all_city_data ():
	for city in location_id_constants.city_ids:
		get_city_data( city )	
def get_all_counties_data ():
	for county in location_id_constants.coastal_counties_ids:
		get_county_data( county )	
def get_all_state_data ():
	for state in location_id_constants.state_ids:
		get_state_data( state )	

#get_all_city_data()
#get_all_counties_data()
get_all_state_data()
