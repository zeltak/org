#!/usr/bin/perl -w

# Get from Weather Underground data about all selected US weather stations by zipcode as JSON packets, then save
# the packet for each zipcode into seperate text files.
# We do it by zipcode because it's the easiest and reliable way to cover the entire region - creating a list
# of all possible zipcodes of a list of states is easy.
#
# Then extract the IDs of airport and pws stations and save them to seperate text files.
#
# Since we don't get the exact latitude/longitude of pws stations from the zipcode query, we
# build lists of airport and pws stations, then requery on members of those lists to get exact
# data about each.
#
# Finally decode the json packets of the station data requests, and write station data to two text files 
# (one for airport stations and one for pws stations) as csv. 
#
# Format for airport stations is:
#	station,city,state,latitude,longitude
#
# Format for pws stations is:
#	station,city,state,latitude,longitude
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

my @selected_states = ("DE", "MD", "NJ", "NY", "PA", "DC");

my $stations_dir = "stationsMidAtlantic";

### End Test
##### End user configuration variables

my $logfile = "$stations_dir/get-stations-logfile.txt";
my $airport_stations_list_file = "$stations_dir/airport-stations-list.csv";
my $pws_stations_list_file = "$stations_dir/pws-stations-list.csv";
my $airport_stations_file = "$stations_dir/airport-stations-data.csv";
my $pws_stations_file = "$stations_dir/pws-stations-data.csv";

my $wukeyfile = "WUkey.txt";

#Hash of arrays of US zipcode 3 digit prefixes taken from http://en.wikipedia.org/wiki/List_of_ZIP_code_prefixes
my %zipcodeprefixes = (

	"AK" => ["99500", "99600", "99700", "99800", "99900"],

	"AL" => ["35000", "35100", "35200", "35400", "35500", 
			 "35600", "35700", "35800", "35900", "36000", 
			 "36100", "36200", "36300", "36400", "36500", 
			 "36600", "36700", "36800", "36900"], 

	"AR" => ["71600", "71700", "71800", "71900",
			 "72000", "72100", "72200", "72300", "72400",
			 "72500", "72600", "72700", "72800", "72900"],

	"AZ" => ["85000", "85100", "85200", "85300", 
			 "85500", "85600", "85700", "85900",
			 "86000", "86300", "86400",
			 "86500"],

	"CA" => ["90000", "90100", "90200", "90300", "90400",
			 "90500", "90600", "90700", "90800", 
			 "91000", "91100", "91200", "91300", "91400",
			 "91500", "91600", "91700", "91800", "91900",
			 "92000", "92100", "92200", "92300", "92400",
			 "92500", "92600", "92700", "92800", 
			 "93000", "93100", "93200", "93300", "93400",
			 "93500", "93600", "93700", "93800", "93900",
			 "94000", "94100", "94200", "94300", "94400",
			 "94500", "94600", "94700", "94800", "94900",
			 "95000", "95100", "95200", "95300", "95400",
			 "95500", "95600", "95700", "95800", "95900",
			 "96000", "96100"],

	"CO" => ["80000", "80100", "80200", "80300", "80400",
			 "80500", "80600", "80700", "80800", "80900",
			 "81000", "81100", "81200", "81300", "81400",
			 "81500", "81600"], 

    	"CT" => ["06000", "06100", "06200", "06300", "06400", 
			 "06500", "06600", "06700", "06800", "06900"],

 	"DC" => ["20000", "20200", "20300", "20400", "20500", "56900"],

	"DE" => ["19700", "19800", "19900"],

	"FL" => ["32000", "32100", "32200", "32300", "32400",
			 "32500", "32600", "32700", "32800", "32900",
			 "33000", "33100", "33200", "33300", "33400",
			 "33500", "33600", "33700", "33800", "33900", 
			 "34100", "34200", "34400", "34600", "34700", 
			 "34900"],
 
	"GA" => ["30000", "30100", "30200", "30300", "30400",
			 "30500", "30600", "30700", "30800", "30900",
			 "31000", "31100", "31200", "31300", "31400",
			 "31500", "31600", "31700", "31800", "31900",
			 "39800", "39900"], 

	"HI" => ["96700", "96800"],

	"IA" => ["50000", "50100", "50200", "50300", "50400",
			 "50500", "50600", "50700", "50800", "50900",
			 "51000", "51100", "51200", "51300", "51400",
			 "51500", "51600", 
			 "52000", "52100", "52200", "52300", "52400",
			 "52500", "52600", "52700", "52800"],

	"ID" => ["83200", "83300", "83400",
			 "83500", "83600", "83700", "83800"],

	"IL" => ["60000", "60100", "60200", "60300", "60400",
			 "60500", "60600", "60700", "60800", "60900",
			 "61000", "61100", "61200", "61300", "61400",
			 "61500", "61600", "61700", "61800", "61900",
			 "62000", "62200", "62300", "62400",
			 "62500", "62600", "62700", "62800", "62900"],

	"IN" => ["46000", "46100", "46200", "46300", "46400",
			 "46500", "46600", "46700", "46800", "46900",
			 "47000", "47100", "47200", "47300", "47400",
			 "47500", "47600", "47700", "47800", "47900"],

	"KS" => ["66000", "66100", "66200", "66400",
			 "66500", "66600", "66700", "66800", "66900",
			 "67000", "67100", "67200", "67300", "67400",
			 "67500", "67600", "67700", "67800", "67900"],

	"KY" => ["40000", "40100", "40200", "40300", "40400",
			 "40500", "40600", "40700", "40800", "40900",
			 "41000", "41100", "41200", "41300", "41400",
			 "41500", "41600", "41700", "41800", 
			 "42000", "42100", "42200", "42300", "42400",
			 "42500", "42600", "42700"],

	"LA" => ["70000", "70100", "70300", "70400",
			 "70500", "70600", "70700", "70800", 
			 "71000", "71100", "71200", "71300", "71400"],

	"MA" => ["01000", "01100", "01200", "01300", "01400", 
			 "01500", "01600", "01700", "01800", "01900", 
			 "02000", "02100", "02200", "02300", "02400", 
			 "02500", "02600", "02700", "05500"],

 	"MD" => ["20600", "20700", "20800", "20900", 
			 "21000", "21100", "21200", "21400", 
			 "21500", "21600", "21700", "21800", "21900"],

    	"ME" => ["03900", "04000", "04100", "04200", "04300", 
			 "04400", "04500", "04600", "04700", "04800", 
			 "04900"],

	"MI" => ["48000", "48100", "48200", "48300", "48400",
			 "48500", "48600", "48700", "48800", "48900",
			 "49000", "49100", "49200", "49300", "49400",
			 "49500", "49600", "49700", "49800", "49900"],

	"MN" => ["55000", "55100", "55300", "55400",
			 "55500", "55600", "55700", "55800", "55900",
			 "56000", "56100", "56200", "56300", "56400",
			 "56500", "56600", "56700"],

	"MO" => ["63000", "63100", "63300", "63400",
			 "63500", "63600", "63700", "63800", "63900",
			 "64000", "64100", "64400",
			 "64500", "64600", "64700", "64800", "64900",
			 "65000", "65100", "65200", "65300", "65400",
			 "65500", "65600", "65700", "65800"],

	"MS" => ["38600", "38700", "38800", "38900", "39000", 
			"39100", "39200", "39300", "39400", "39500", 
			"39600", "39700"],

	"MT" => ["59000", "59100", "59200", "59300", "59400",
			 "59500", "59600", "59700", "59800", "59900"],

	"NC" => ["27000", "27100", "27200", "27300", "27400",
			 "27500", "27600", "27700", "27800", "27900",
			 "28000", "28100", "28200", "28300", "28400",
			 "28500", "28600", "28700", "28800", "28900"], 

	"ND" => ["58000", "58100", "58200", "58300", "58400",
			 "58500", "58600", "58700", "58800"],

 	"NE" => ["68000", "68100", "68300", "68400",
			 "68500", "68600", "68700", "68800", "68900",
			 "69000", "69100", "69200", "69300"],

  	"NH" => ["03000", "03100", "03200", "03300", "03400", 
			 "03500", "03600", "03700", "03800"],

 	"NJ" => ["07000", "07100", "07200", "07300", "07400", 
			 "07500", "07600", "07700", "07800", "07900", 
			 "08000", "08100", "08200", "08300", "08400", 
			 "08500", "08600", "08700", "08800", "08900"],

	"NM" => ["87000", "87100", "87200", "87300", "87400",
			 "87500", "87700", "87800", "87900",
			 "88000", "88100", "88200", "88300", "88400"],

	"NV" => ["88900", 
			 "89000", "89100", "89300", "89400",
			 "89500", "89700", "89800"],

	"NY" => ["00500", "10000", "10100", "10200", "10300", "10400",
			 "10500", "10600", "10700", "10800", "10900",
			 "11000", "11100", "11200", "11300", "11400",
			 "11500", "11600", "11700", "11800", "11900",
			 "12000", "12100", "12200", "12300", "12400",
			 "12500", "12600", "12700", "12800", "12900",
			 "13000", "13100", "13200", "13300", "13400",
			 "13500", "13600", "13700", "13800", "13900",
			 "14000", "14100", "14200", "14300", "14400",
			 "14500", "14600", "14700", "14800", "14900"],

	"OH" => ["43000", "43100", "43200", "43300", "43400",
			 "43500", "43600", "43700", "43800", "43900",
			 "44000", "44100", "44200", "44300", "44400",
			 "44500", "44600", "44700", "44800", "44900",
			 "45000", "45100", "45200", "45300", "45400",
			 "45500", "45600", "45700", "45800", "45900"],

 	"OK" => ["73000", "73100", "73400",
			 "73500","73600", "73700", "73800", "73900",
			 "74000", "74100", "74300", "74400",
			 "74500", "74600", "74700", "74800", "74900"],

	"OR" => ["97000", "97100", "97200", "97300", "97400",
			 "97500", "97600", "97700", "97800", "97900"], 

	"PA" => ["15000", "15100", "15200", "15300", "15400",
			 "15500", "15600", "15700", "15800", "15900",
			 "16000", "16100", "16200", "16300", "16400",
			 "16500", "16600", "16700", "16800", "16900",
			 "17000", "17100", "17200", "17300", "17400",
			 "17500", "17600", "17700", "17800", "17900",
			 "18000", "18100", "18200", "18300", "18400",
			 "18500", "18600", "18700", "18800", "18900",
			 "19000", "19100", "19200", "19300", "19400",
			 "19500", "19600"],
 
	"RI" => ["02800", "02900"],

	"SC" => ["29000", "29100", "29200", "29300", "29400",
			 "29500", "29600", "29700", "29800", "29900"],

	"SD" => ["57000", "57100", "57200", "57300", "57400",
			 "57500", "57600", "57700"],

	"TN" => ["37000", "37100", "37200", "37300", "37400", 
			"37500", "37600", "37700", "37800", "37900", 
			"38000", "38100", "38200", "38300", "38400", "38500"],

	"TX" => ["73300", "75000", "75100", "75200", "75300", "75400",
			 "75500", "75600", "75700", "75800", "75900",
			 "76000", "76100", "76200", "76300", "76400",
			 "76500", "76600", "76700", "76800", "76900",
			 "77000", "77200", "77300", "77400",
			 "77500", "77600", "77700", "77800", "77900",
			 "78000", "78100", "78200", "78300", "78400",
			 "78500", "78600", "78700", "78800", "78900",
			 "79000", "79100", "79200", "79300", "79400",
			 "79500", "79600", "79700", "79800", "79900",
			 "88500"],

	"UT" => ["84000", "84100", "84200", "84300", "84400",
			 "84500", "84600", "84700"],

	"VA" => ["20100", "22000", "22100", "22200", "22300", 
			 "22400", "22500", "22600", "22700", "22800", 
			 "22900", "23000", "23100", "23200", "23300", 
			 "23400", "23500", "23600", "23700", "23800", 
			 "23900", "24000", "24100", "24200", "24300", 
			 "24400", "24500", "24600"],

	"VT" => ["05000", "05100", "05200", "05300", "05400", 
			 "05600", "05700", "05800", "05900"],

	"WA" => ["98000", "98100", "98200", "98300", "98400",
			 "98500", "98600", "98800", "98900",
			 "99000", "99100", "99200", "99300", "99400"],
 
	"WI" => ["53000", "53100", "53200", "53400",
			 "53500", "53700", "53800", "53900",
			 "54000", "54100", "54200", "54300", "54400",
			 "54500", "54600", "54700", "54800", "54900"],

	"WV" => ["24700", "24800", "24900", "25000", "25100", 
			 "25200", "25300", "25400", "25500", "25600", 
			 "25700", "25800", "25900", "26000", "26100", 
			 "26200", "26300", "26400", "26500", "26600", 
			 "26700", "26800"],

	"WY" => ["82000", "82100", "82200", "82300", "82400",
			 "82500", "82600", "82700", "82800", "82900",
			 "83000", "83100"]

                    );


my @zipcodes = ();
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

# Enumerate all possible Zipcodes of selected states (some may not be valid)
my $state;
foreach $state (@selected_states) 
{
	# $zipprefs is a reference to an array of the state's 3 digit zipcode prefixes, 
	# so @$zipprefs is the actual array
	my $zipprefs = $zipcodeprefixes{$state};
	my $zippref;
	foreach $zippref (@$zipprefs)
	{
		my $i;
		foreach $i (0 .. 99)
		{
			# trick to create 5 digit string with leading zeros
			push(@zipcodes, sprintf("%05d", $zippref + $i));
		}
	}
}

my $zipcode;
foreach $zipcode (@zipcodes)
{
	# The list of stations and data about them in $zipcode in json format is to be had by doing a get request to this URL
	my $url = "http://api.wunderground.com/api/$wukey/geolookup/q/$zipcode.json";

	my $data = get $url;
	if (! defined $data)
	{
		print LOGFILE "Can't access url $url\n";
		next;
	}
	print LOGFILE "accessed $url\n";

	# Write out the json text structure unprocessed, we will deal with its contents below
	my $outfilename = "$stations_dir/station-$zipcode-json.txt";
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
		print LOGFILE "No stations for zipcode $zipcode\n";
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

# Extract the lists of airport station and pws station ids into arrays and write to text files.
@airport_stations = keys %airport_stations_raw;
my $station;
foreach $station (@airport_stations)
{
	print AIRPORTLISTFILE $station, "\n";
}

@pws_stations = keys %pws_stations_raw;
foreach $station (@pws_stations)
{
	print PWSLISTFILE $station, "\n";
}

# Now we have lists of all airport station and pws ids in the states of interest, so we requery on each member of these lists to
# get json packets of complete information about each station, then decode the packets and write out the station information to 
# csv files.

# Make the WU queries for airport stations, and write out the JSON results to text files.
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

	# get reference to "location" hash
	my $location_ref = $jsonpacket_ref->{"location"};

	# write out the station data
	print AIRPORTFILE $station, ",",
			$location_ref->{"city"}, ",", 
			$location_ref->{"state"}, ",",
			$location_ref->{"lat"}, ",", 
			$location_ref->{"lon"}, ",",
			"\n";
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

	# get reference to "location" hash
	my $location_ref = $jsonpacket_ref->{"location"};

	print PWSFILE $station, ",",
			$location_ref->{"city"}, ",", 
			$location_ref->{"state"}, ",",
			$location_ref->{"lat"}, ",", 
			$location_ref->{"lon"}, 
			"\n";
}

close AIRPORTLISTFILE;
close PWSLISTFILE;

close AIRPORTFILE;
close PWSFILE;

close LOGFILE;


