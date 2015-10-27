#!/usr/bin/perl -w

# Get from Weather Underground data about all selected weather stations by latitude and longitude as JSON packets, then save
# the packet for each latitude and longitude into seperate text files.
# We do it by latitude and longitude because it's the easiest and reliable way to cover the entire region - creating a list
# of all possible latitude and longitude of a region is easy.
#
# Then extract the IDs of airport and pws stations and save them to seperate text files.
#
# Since we don't get the exact latitude/longitude of pws stations from the latitude/longitude query, we
# build lists of airport and pws stations, then requery on members of those lists to get exact
# data about each.
#
# Finally decode the json packets of the station data requests, and write station data to two text files 
# (one for airport stations and one for pws stations) as csv. 
#
# Format for airport stations is:
#	station,city,state,country,latitude,longitude
#
# Format for pws stations is:
#	station,city,state,country,latitude,longitude
#
# Copyright 2012, Paul V Carter
#

use strict;

# Perl module for issuing http requests and getting responses
use LWP::Simple;

# Perl module that processes JSON data
use JSON;

##### User configuration variables
### Begin TEST

my $region = "Italy";
my $latitude_increment = 0.3;
my $longitude_increment = 0.4;
my $stations_dir = "stationsItaly";

### End Test
##### End user configuration variables

my $logfile = "$stations_dir/get-stations-logfile.txt";
my $airport_stations_list_file = "$stations_dir/airport-stations-list.csv";
my $pws_stations_list_file = "$stations_dir/pws-stations-list.csv";
my $airport_stations_file = "$stations_dir/airport-stations-data.csv";
my $pws_stations_file = "$stations_dir/pws-stations-data.csv";

my $wukeyfile = "WUkey.txt";

#Hash of region names to bounding latitude/longitude rectangle
#North latitude is positive, South is negative
#East longitude is positive, West is negative
my %region_latitude_longitude = (

	"Italy" => {
		"longitude_min" => 7.0, 
		"longitude_max" => 20.0, 
		"latitude_min" => 36.0, 
		"latitude_max" => 46.0}

	);


my %airport_stations_raw = ();
my @airport_stations = ();
my %pws_stations_raw = ();
my @pws_stations = ();


# Dr. Kloog's Weather Underground API key
if (! open WUDATA, $wukeyfile )
{
	die "Can't open $wukeyfile/n";
}

my $wukey = <WUDATA>;
chomp($wukey);

# Create the stations directory if it doesn't already exist
if (! -e $stations_dir)
{
	if (! mkdir($stations_dir) )
	{
		die "Can't create directory $stations_dir\n";
	}
}
		

# Logfile where we note results of (mainly) web operations that may fail
if (! open LOGFILE, ">", $logfile)
{
	die "Can't open $logfile/n";
}

# Our two output files for station id lists
if (! open AIRPORTLISTFILE, ">", $airport_stations_list_file)
{
	die "Can't open $airport_stations_list_file/n";
}

if (! open PWSLISTFILE, ">", $pws_stations_list_file)
{
	die "Can't open $pws_stations_list_file/n";
}

# Our two output files for station data
if (! open AIRPORTFILE, ">", $airport_stations_file)
{
	die "Can't open $airport_stations_file/n";
}

if (! open PWSFILE, ">", $pws_stations_file)
{
	die "Can't open $pws_stations_file/n";
}

# Iterate through latitude/longitude grid, getting stations data for each point
my $latitude;
for ($latitude = $region_latitude_longitude{$region}{"latitude_min"};
	$latitude <= $region_latitude_longitude{$region}{"latitude_max"};
	$latitude += $latitude_increment)
{
	my $longitude;
	for ($longitude = $region_latitude_longitude{$region}{"longitude_min"};
		$longitude <= $region_latitude_longitude{$region}{"longitude_max"};
		$longitude += $longitude_increment)
	{
		# The list of stations and data about them 'near' $latitude/$longitude in json format is to be had by doing a get request to this URL
		my $url = "http://api.wunderground.com/api/$wukey/geolookup/q/$latitude,$longitude.json";

		my $data = get $url;
		if (! defined $data)
		{
			print LOGFILE "Can't access url $url\n";
			next;
		}
		print LOGFILE "accessed $url\n";

		# Write out the json text structure unprocessed, we will deal with its contents below
		my $outfilename = "$stations_dir/station-$latitude,$longitude-json.txt";
		my $status = open OUTFILE, ">", $outfilename;
		if (! $status)
		{
			print LOGFILE "Can't create $outfilename\n";
		}
		else
		{
			print OUTFILE $data;
			print LOGFILE "Created $outfilename\n";
		}
		close OUTFILE;

		# Decode JSON text package to reference to complex perl structure
		my $jsonpacket_ref = decode_json $data;
	
		# is this a packet with no stations?
		if (exists $jsonpacket_ref->{"response"}->{"error"})
		{
			print LOGFILE "No stations for latitude $latitude, longitude $longitude\n";
			next;
		}

		print LOGFILE "Extracting from $url data\n";

		# get reference to "location" hash
		my $location_ref = $jsonpacket_ref->{"location"};

		my $nearby_weather_stations_ref = $location_ref->{"nearby_weather_stations"};

		# reference to array of airport stations
		my $airport_ref = $nearby_weather_stations_ref->{"airport"}->{"station"};
		my $airport;
		for $airport (@$airport_ref)
		{
			# Tricky perl way to avoid duplicates in a list - we create a hash with station names as keys and idential values.
			# So duplicate station name insertions will have no effect and at the end we can just extract the keys into a list.
			$airport_stations_raw{$airport->{"icao"}} = 0;
		}

		# reference to array of pws stations
		my $pws_ref = $nearby_weather_stations_ref->{"pws"}->{"station"};
		my $pws;
		for $pws (@$pws_ref)
		{
			# Tricky perl way to avoid duplicates in a list - we create a hash with station names as keys and idential values.
			# So duplicate station name insertions will have no effect and at the end we can just extract the keys into a list.
			$pws_stations_raw{$pws->{"id"}} = 0;
		}
	}
}

# Extract the lists of airport station and pws station ids into arrays
@airport_stations = keys %airport_stations_raw;
@pws_stations = keys %pws_stations_raw;

# Now we have lists of all airport station and pws ids in the region og interest, so we requery on each member of these lists to
# get json packets of complete information about each station, then decode the packets and write out the station information to 
# csv files.

# Make the WU queries for airport stations, and write out the JSON results to text files.
my $station;
foreach $station (@airport_stations)
{
	# The data about the station in json format is to be had by doing a get request to this URL
	my $url = "http://api.wunderground.com/api/$wukey/geolookup/q/$station.json";

	my $data = get $url;
	if (! defined $data)
	{
		print LOGFILE "Can't access url $url\n";
		next;
	}
	print LOGFILE "accessed $url\n";

	# Write out the json text structure unprocessed 
	my $outfilename = "$stations_dir/station-$station-json.txt";
	my $status = open OUTFILE, ">", $outfilename;
	if (! $status)
	{
		print LOGFILE "Can't create $outfilename\n";
	}
	else
	{
		print OUTFILE $data;
		print LOGFILE "Created $outfilename\n";
	}
	close OUTFILE;

	# Decode JSON text package to reference to complex perl structure
	my $jsonpacket_ref = decode_json $data;
	
	# is this a packet with no stations?
	if (exists $jsonpacket_ref->{"response"}->{"error"})
	{
		print LOGFILE "No station in $url data\n";
		next;
	}
	print LOGFILE "Extracting from $url data\n";

	# get reference to "location" hash - it might not exist, if so just skip it
	if (defined $jsonpacket_ref->{"location"})
	{
		my $location_ref = $jsonpacket_ref->{"location"};

		# write out the station data
		print AIRPORTFILE $station, ",",
			$location_ref->{"city"}, ",", 
			$location_ref->{"country"}, ",",
			$location_ref->{"lat"}, ",", 
			$location_ref->{"lon"}, ",",
			"\n";

		# and station name
		print AIRPORTLISTFILE $station, "\n";
	}
}

# Make the WU queries for pws stations, and write out the JSON results to text files.
foreach $station (@pws_stations)
{
	# The data about the station in json format is to be had by doing a get request to this URL
	my $url = "http://api.wunderground.com/api/$wukey/geolookup/q/pws:$station.json";

	my $data = get $url;
	if (! defined $data)
	{
		print LOGFILE "Can't access url $url\n";
		next;
	}
	print LOGFILE "accessed $url\n";

	# Write out the json text structure unprocessed
	my $outfilename = "$stations_dir/station-$station-json.txt";
	my $status = open OUTFILE, ">", $outfilename;
	if (! $status)
	{
		print LOGFILE "Can't create $outfilename\n";
	}
	else
	{
		print OUTFILE $data;
		print LOGFILE "Created $outfilename\n";
	}
	close OUTFILE;

	# Decode JSON text package to reference to complex perl structure
	my $jsonpacket_ref = decode_json $data;
	
	# is this a packet with no stations?
	if (exists $jsonpacket_ref->{"response"}->{"error"})
	{
		print LOGFILE "No station in $url data\n";
		next;
	}
	print LOGFILE "Extracting from $url data\n";

	# get reference to "location" hash - it might not exist, if so just skip it
	if (defined $jsonpacket_ref->{"location"})
	{
		my $location_ref = $jsonpacket_ref->{"location"};

		print PWSFILE $station, ",",
			$location_ref->{"city"}, ",", 
			$location_ref->{"country"}, ",",
			$location_ref->{"lat"}, ",", 
			$location_ref->{"lon"}, 
			"\n";

		# and station name
		print PWSLISTFILE $station, "\n";
	}
}

close AIRPORTLISTFILE;
close PWSLISTFILE;

close AIRPORTFILE;
close PWSFILE;

close LOGFILE;


