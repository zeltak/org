#!/usr/bin/perl -w

# Process the json observations data obtained from WU.
# Get the IDs of airport and pws stations from seperate text files - one for airport stations, one for pws.
# Then get json observation data for each of these stations on each date of interest from text files previously created by WU queries.
# Finally decode the json packets, and write observation data to two text files 
# (one for airport stations and one for pws stations) as csv. 
#
# Format for airport station observation is:
#	station id, year in UTC, month in UTC, day in UTC, hour in UTC, minute in UTC,
#	tempm, tempi, dewptm, dewpti, hum, wspdm, wspdi, wgustm, wgusti, wdird, wdire,
#	vism, visi, pressurem, pressurei, windchillm, windchilli, heatindexm, heatindexi,
#	precipm, precipi, conds, icon, fog, rain, snow, hail, thunder, tornado. metar
#
# Format for pws station observation is:
#	station id, year in UTC, month in UTC, day in UTC, hour in UTC, minute in UTC, 
#	tempm, tempi, dewptm, dewpti, hum, wspdm, wspdi, wgustm, wgusti, wdird, wdire,
#	pressurem, pressurei, windchillm, windchilli, heatindexm, heatindexi, precip_ratem,
#	precip_ratei, precip_totalm, precip_totali, solarradiation, UV, softwaretype 
#
# Copyright 2012, Paul V Carter
#

use strict;

# Perl module that processes JSON data
use JSON;

# Perl module for simple date manipulation
use Date::Simple;

##### User configuration variables

# Directory for stations data
my $stations_dir = "stationsMidAtlantic";

# Directory for observations data
my $observations_dir = "observationsMidAtlantic";

#Range of dates to get observations for
my $firstdate = Date::Simple->new('2012-08-16');
my $lastdate = Date::Simple->new('2012-08-16');	

##### End user configuration variables

my $logfile = "$observations_dir/extract-observations-data-logfile.txt";
my $airport_stations_list_file = "$stations_dir/airport-stations-list.csv";
my $airport_observations_file = "$observations_dir/airport-stations-observations.csv";
my $pws_stations_list_file = "$stations_dir/pws-stations-list.csv";
my $pws_observations_file = "$observations_dir/pws-stations-observations.csv";


my @airport_stations = ();
my @pws_stations = ();

my $station = "";

# Logfile where we note results of operations that may fail
if (! open LOGFILE, ">", $logfile)
{
	die "Can't open $logfile/n";
}

# Our two input files for station id lists
if (! open AIRPORTLISTFILE, "<", $airport_stations_list_file)
{
	die "Can't open $airport_stations_list_file/n";
}

if (! open PWSLISTFILE, "<", $pws_stations_list_file)
{
	die "Can't open $pws_stations_list_file/n";
}

# Read in the stations list files to lists
while (<AIRPORTLISTFILE>)
{
	chomp;
	push (@airport_stations, $_);
}
close AIRPORTLISTFILE;

while (<PWSLISTFILE>)
{
	chomp;
	push (@pws_stations, $_);
}
close PWSLISTFILE;

# Our two output files for observation data
if (! open AIRPORTOBSERVATIONSFILE, ">", $airport_observations_file)
{
	die "Can't open $airport_observations_file/n";
}

if (! open PWSOBSERVATIONSFILE, ">", $pws_observations_file)
{
	die "Can't open $pws_observations_file/n";
}

# print CVS header for airport stations observations
print AIRPORTOBSERVATIONSFILE
	"station,",
	"year utc,",
	"month utc,",
	"day utc,",
	"hour utc,",
	"minute utc,",
	"temperature m,", 
	"temperature i,",
	"dewpoint m,", 
	"dewpoint i,",
	"humidity,",
	"wind speed m,", 
	"wind speed i,",
	"wind gust m,", 
	"wind gust i,",
	"wind direction m,", 
	"wind direction i,",
	"visibility m,", 
	"visibility i,",
	"pressure m,", 
	"pressure i,", 
	"windchill m,", 
	"windchill i,", 
	"heat index m,", 
	"heat index i,",
	"precipitation m,", 
	"precipitation i,",
	"conditions,",
	"icon,",
	"fog,",
	"rain,",
	"snow,",
	"hail,",
	"thunder,",
	"tornado,",
	"metar",
	"\n";

# Process each airport station's observations by station id/date JSON file
foreach $station (@airport_stations)
{
	# The directory containing the observations
	my $dirname = "$observations_dir/$station";
	if (! (-d $dirname))
	{
		print LOGFILE "Directory $dirname doesn't exist\n";
		next;
	}

	my $date = $firstdate;
	while ($date <= $lastdate)
	{
		my $datestring = $date->format("%Y%m%d");
		$date++;

		# Raw observation json file obtained from WU by another script
		my $infilename = "$dirname/observation-$station-$datestring-json.txt";
		my $status = open INFILE, "<", $infilename;
		if (! $status)
		{
			print LOGFILE "Can't open $infilename\n";
			next;
		}

		# read in whole file to a string removing newlines along the way - it should be a single JSON structure
		my $data = "";
		while (<INFILE>)
		{
			chomp;
			$data = $data . $_;
		}

		# Decode JSON text package to reference to complex perl structure
		# Tricky stuff to handle decode_json failing and crashing the script - here it should just 'crash' the
		# eval block and return undef into $_. 
		my $jsonpacket_ref;
		eval { $jsonpacket_ref = decode_json $data; };
		if ($@) 
		{ 
			print LOGFILE "Can't parse $infilename\n"; 
			next; 
		}

		# is this a packet with no stations?
		if (exists $jsonpacket_ref->{"response"}->{"error"})
		{
			print LOGFILE "No station in $infilename\n";
			next;
		}
		print LOGFILE "Extracting from $infilename\n";



		# reference to array of observations
		my $observations_ref = $jsonpacket_ref->{"history"}->{"observations"};

		my $observation;
		foreach $observation (@$observations_ref)
		{
			print AIRPORTOBSERVATIONSFILE
				$station, ",",
				$observation->{"utcdate"}->{"year"}, ",",
				$observation->{"utcdate"}->{"mon"}, ",",
				$observation->{"utcdate"}->{"mday"}, ",",
				$observation->{"utcdate"}->{"hour"}, ",",
				$observation->{"utcdate"}->{"min"}, ",",
				$observation->{"tempm"}, ",", 
				$observation->{"tempi"}, ",",
				$observation->{"dewptm"}, ",", 
				$observation->{"dewpti"}, ",",
				$observation->{"hum"}, ",",
				$observation->{"wspdm"}, ",", 
				$observation->{"wspdi"}, ",",
				$observation->{"wgustm"}, ",", 
				$observation->{"wgusti"}, ",",
				$observation->{"wdird"}, ",",
				$observation->{"wdire"}, ",",
				$observation->{"vism"}, ",", 
				$observation->{"visi"}, ",",
				$observation->{"pressurem"}, ",", 
				$observation->{"pressurei"}, ",",
				$observation->{"windchillm"}, ",", 
				$observation->{"windchilli"}, ",",
				$observation->{"heatindexm"}, ",", 
				$observation->{"heatindexi"}, ",",
				$observation->{"precipm"}, ",", 
				$observation->{"precipi"}, ",",
				$observation->{"conds"}, ",",
				$observation->{"icon"}, ",",
				$observation->{"fog"}, ",",
				$observation->{"rain"}, ",",
				$observation->{"snow"}, ",",
				$observation->{"hail"}, ",",
				$observation->{"thunder"}, ",",
				$observation->{"tornado"}, ",",
				$observation->{"metar"},
				"\n";

		}
	}
	continue
	{
		close INFILE;
	}
}
close AIRPORTOBSERVATIONSFILE;

# print CVS header for pws observations file
print PWSOBSERVATIONSFILE
	"station,",
	"year utc,",
	"month utc,",
	"day utc,",
	"hour utc,",
	"minute utc,",
	"temperature m,", 
	"temperature i,",
	"dewpoint m,", 
	"dewpoint i,",
	"humidity,",
	"wind speed m,", 
	"wind speed i,",
	"wind gust m,", 
	"wind gust i,",
	"wind direction m,", 
	"wind direction i,",
	"pressure m,", 
	"pressure i,", 
	"windchill m,", 
	"windchill i,", 
	"heat index m,", 
	"heat index i,",
	"precipitation rate m,", 
	"precipitation rate i,",
	"precipitation total m,", 
	"precipitation total i,",
	"solar radiation,",
	"UV,",
	"softwaretype", 
	"\n";

# Process each pws station's observations by station id/date JSON file
foreach $station (@pws_stations)
{
	# The directory containing the observations
	my $dirname = "$observations_dir/$station";
	if (! (-d $dirname))
	{
		print LOGFILE "Directory $dirname doesn't exist\n";
		next;
	}

	my $date = $firstdate;
	while ($date <= $lastdate)
	{
		my $datestring = $date->format("%Y%m%d");
		$date++;

		# Raw observation json file obtained from WU by another script
		my $infilename = "$dirname/observation-$station-$datestring-json.txt";
		my $status = open INFILE, "<", $infilename;
		if (! $status)
		{
			print LOGFILE "Can't open $infilename\n";
			next;
		}

		# read in whole file to a string removing newlines along the way - it should be a single JSON structure
		my $data = "";
		while (<INFILE>)
		{
			chomp;
			$data = $data . $_;
		}

		# Decode JSON text package to reference to complex perl structure
		# Tricky stuff to handle decode_json failing and crashing the script - here it should just 'crash' the
		# eval block and return undef into $_. 
		my $jsonpacket_ref;
		eval { $jsonpacket_ref = decode_json $data; };
		if ($@) 
		{ 
			print LOGFILE "Can't parse $infilename\n"; 
			next; 
		}
	
		# is this a packet with no stations?
		if (exists $jsonpacket_ref->{"response"}->{"error"})
		{
			print LOGFILE "No station in $infilename\n";
			next;
		}
		print LOGFILE "Extracting from $infilename\n";


		# reference to array of observations
		my $observations_ref = $jsonpacket_ref->{"history"}->{"observations"};

		my $observation;
		foreach $observation (@$observations_ref)
		{
			print PWSOBSERVATIONSFILE
				$station, ",",
				$observation->{"utcdate"}->{"year"}, ",",
				$observation->{"utcdate"}->{"mon"}, ",",
				$observation->{"utcdate"}->{"mday"}, ",",
				$observation->{"utcdate"}->{"hour"}, ",",
				$observation->{"utcdate"}->{"min"}, ",",
				$observation->{"tempm"}, ",", 
				$observation->{"tempi"}, ",",
				$observation->{"dewptm"}, ",", 
				$observation->{"dewpti"}, ",",
				$observation->{"hum"}, ",",
				$observation->{"wspdm"}, ",", 
				$observation->{"wspdi"}, ",",
				$observation->{"wgustm"}, ",", 
				$observation->{"wgusti"}, ",",
				$observation->{"wdird"}, ",",
				$observation->{"wdire"}, ",",
				$observation->{"pressurem"}, ",", 
				$observation->{"pressurei"}, ",",
				$observation->{"windchillm"}, ",", 
				$observation->{"windchilli"}, ",",
				$observation->{"heatindexm"}, ",", 
				$observation->{"heatindexi"}, ",",
				$observation->{"precip_ratem"}, ",", 
				$observation->{"precip_ratei"}, ",",
				$observation->{"precip_totalm"}, ",",
				$observation->{"precip_totali"}, ",",
				$observation->{"solarradiation"}, ",",
				$observation->{"UV"}, ",",
				$observation->{"softwaretype"}, 
				"\n";
		}
	}
	continue
	{
		close INFILE;
	}
}
close PWSOBSERVATIONSFILE;
close LOGFILE;

