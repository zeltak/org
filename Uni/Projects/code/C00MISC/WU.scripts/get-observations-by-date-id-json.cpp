/* get-observations-by-date-id-json.cpp
 *
 *
 * Copyright 2012 Paul V Carter
 *
 */

#include "get-observations-by-date-id-json.h"

#include <cstdlib>
#include <ctime>
#include <cerrno>

#include <sys/stat.h>

#include <vector>
#include <queue>
#include <iterator>

#include <sstream>

#include <curl/curl.h>


/******** Configuration constants 
 * configuration file contains in this order
 *	# worker threads
 *	Name of file containing Weather Underground key
 *	Name of log file
 *	Name of file containing list of airport stations 
 *	Name of file containing list of pws stations 
 *	Name of directory where observations will be stored
 *	Starting day of observation set (1..31)
 *	Starting month of observation set (1..12)
 *	Starting year of observation set
 *	Ending day of observation set (1..31)
 *	Ending month of observation set (1..12)
 *	Ending year of observation set
 *	Get airport station observations (1 -> yes, 0 -> no)
 *	Get pws station observations (1 -> yes, 0 -> no)
 *	Don't collect and overwrite already existing observations (1 -> yes, 0 -> no)
 *	Delay before collecting next observation in seconds (floating point) - only accurate if
 *		# worker threads is 1
 *
 * Configuration file should not contain blank lines or white space within lines	
 */
const string configfilename = "get-obs-config.txt";

// curl callback function for writing http response to disk. This must be threadsafe.
size_t write_data(void *buffer, size_t size, size_t nmemb, void *data)
{
	FILE* pfile = static_cast<FILE*>(data);
	size_t byteswritten = 0; 

	if (NULL != pfile)
	{
		byteswritten = fwrite(buffer, size, nmemb, pfile);
	}

	return byteswritten;
}

// Thread function - gets url/filename pairs off list created by main thread (in a threadsafe manner,)
// issues http requests, saves http response in file. Terminates when master url list is exhausted.
void* threadfunction(void* arg)
{
	CURL *easyhandle = NULL;
	CURLcode ccode = CURLE_OK;
	WORKERTHREADARG* wta = static_cast<WORKERTHREADARG*>(arg);
	ostringstream outstream(ostringstream::out);

	if (NULL == (easyhandle = curl_easy_init()))
	{
		outstream << "Thread # " << wta->m_threadnumber << " curl_easy_init() failed\n";
		*(wta->m_plogfile) << outstream.str();
		return(NULL);
	}
	// Strongly advised for multithreading
	if (CURLE_OK != (ccode = curl_easy_setopt(easyhandle, CURLOPT_NOSIGNAL, 1)))
	{
		outstream <<  "Thread # " << wta->m_threadnumber <<  " curl_easy_setopt(CURLOPT_NOSIGNAL) failed ccode " << ccode << "\n";
		*(wta->m_plogfile) << outstream.str();
		curl_easy_cleanup(easyhandle);
		return(NULL);
	}

	if (CURLE_OK != (ccode = curl_easy_setopt(easyhandle, CURLOPT_WRITEFUNCTION, &write_data)))
	{
		*(wta->m_plogfile) << "Thread # " << wta->m_threadnumber <<  "Can't set curl callback function ccode " << ccode << "\n";
		curl_easy_cleanup(easyhandle);
		return(NULL);
	}


	// Get url/file pair from master queue - quit when it is empty
	URLFILE urf;
	FILE* outfile=NULL;
	while (1)
	{
		if (outfile)
		{
			fclose(outfile);
			outfile = NULL;
		}

		// Get next url/filename pair in threadsafe manner
		(void)pthread_mutex_lock(wta->m_purlslock);
		bool isempty = wta->m_purls->empty();
		int qsize = int(wta->m_purls->size());
		if (!isempty)
		{
			urf = wta->m_purls->front();
			wta->m_purls->pop();
		}
		(void)pthread_mutex_unlock(wta->m_purlslock);
		if (isempty)
			break;

// Logfile gets too big
//		outstream << "Thread # " << wta->m_threadnumber << " queue size " << qsize << 
//			" url: " << urf.url << " file: " << urf.filename << "\n";	
//		*(wta->m_plogfile) << outstream.str();

		// Wait for certain number of milliseconds before continuing if the config file
		// specified it - for throttling back the rate of observation requests.
		usleep(wta->m_mseconds_delay*1000);

		if (NULL == (outfile = fopen(urf.filename.c_str(), "w")))
		{
			outstream << "Thread # " << wta->m_threadnumber << " Can't open file " << urf.filename <<
				" error " << errno << "\n";
			*(wta->m_plogfile) << outstream.str();
			continue;
		}

		// Give the curl callback function the pointer to open FILE* for this handle 
		if (CURLE_OK != (ccode = curl_easy_setopt(easyhandle, CURLOPT_WRITEDATA, outfile)))
		{
			outstream << "Thread # " << wta->m_threadnumber << " Can't set callback data buffer  ccode " << ccode << "\n";
			*(wta->m_plogfile) << outstream.str();
			break;
		}

		// Tell curl the url to read from for this handle
		if (CURLE_OK != (ccode = curl_easy_setopt(easyhandle, CURLOPT_URL, urf.url.c_str())))
		{
			outstream << "Thread # " << wta->m_threadnumber << " Can't set url " << urf.url << " failed ccode " << ccode << "\n";
			*(wta->m_plogfile) << outstream.str();
			continue;
		}

		// issue http request, block here until http response is processed
		if (CURLE_OK != (ccode = curl_easy_perform(easyhandle)))
		{
			outstream << "Thread # " << wta->m_threadnumber << " reading URL " << urf.url << " failed ccode " << ccode << "\n";
			*(wta->m_plogfile) << outstream.str();
		}
	}

	if (outfile)
	{
		fclose(outfile);
		outfile = NULL;
	}

	curl_easy_cleanup(easyhandle);
	return(NULL);
}

urlfile& urlfile::operator=(const urlfile& uf)
{
	if (this != &uf)
	{
		url = uf.url;
		filename = uf.filename;
	}

	return *this;
}

// Threadsafe write to logfile
LogFile& LogFile::operator<<(const string& s)
{
	(void)pthread_mutex_lock(&m_lock);
	m_file << s;
	(void)pthread_mutex_unlock(&m_lock);

	return *this;
}

LogFile& LogFile::operator<<(const char* s)
{
	return operator<<(string(s));
}

LogFile& LogFile::operator<<(int i)
{
	(void)pthread_mutex_lock(&m_lock);
	m_file << i;
	(void)pthread_mutex_unlock(&m_lock);

	return *this;
}

int Application::Run()
{
	LogFile logfile(m_logfilename);

	if (logfile.Fail())
	{
		cerr << "Can't create logfile " << m_logfilename << endl;
		return(-1);
	}

	// Get WU key from text file
	string wukey;
	{
		ifstream wukeyfile(m_wukeyfilename.c_str());
		if (wukeyfile.fail())
		{
			logfile << "Can't open WU key file " << m_wukeyfilename << "\n";
			return(-1);
		}
		wukeyfile >> wukey;
		if ("" == wukey)
		{
			logfile << "Can't read WU key file " << m_wukeyfilename << "\n";
			return(-1);
		}
	}

	// If the station's observations subdirectory doesn't exist, create it
	string obsdirname = m_observations_directory;
	if (0 != mkdir(obsdirname.c_str(), S_IRUSR|S_IRGRP|S_IROTH|S_IWUSR|S_IWGRP|S_IWOTH|S_IXUSR|S_IXGRP|S_IXOTH))
	{
		// If the directory exists, we are all right, otherwise we couldn't create it, so bomb out
		if (EEXIST != errno)
		{
			logfile << "Can't create observations directory " << obsdirname << "\n";
			return -1;
		}
	}
	else
	{
		logfile << "Created observations directory " << obsdirname << "\n";
	}

	string s;
	// Get list of airport stations from text file
	vector<string> airports;

	if (m_getairportobs)
	{
		ifstream airportsfile(m_airportsfilename.c_str());
		if (airportsfile.fail())
		{
			logfile << "Can't open airport stations file " << m_airportsfilename << "\n";
			return(-1);
		}
		while (!airportsfile.eof())
		{
			airportsfile >> s;
			if ("" != s)
			{
				airports.push_back(s);
			}
		}
	}

	// Get list of pws stations from text file
	vector<string> pwss;

	if (m_getpwsobs)
	{
		ifstream pwssfile(m_pwsfilename.c_str());
		if (pwssfile.fail())
		{
			logfile << "Can't open pws stations file " << m_pwsfilename << "\n";
			return(-1);
		}
		while (!pwssfile.eof())
		{
			pwssfile >> s;
			if ("" != s)
			{
				pwss.push_back(s);
			}
		}
	}

	// Enumerate the dates from start to end
	// Using ctime functions is kind of klunky but they're there and they work
	vector<string> dates;

	struct tm startdate = {0};
	startdate.tm_mday = m_startday;
	startdate.tm_mon = m_startmonth;
	startdate.tm_year = m_startyear;
	time_t thedatesec = mktime(&startdate);

	struct tm enddate = {0};
	enddate.tm_mday = m_endday;
	enddate.tm_mon = m_endmonth;
	enddate.tm_year = m_endyear;
	const time_t enddatesec = mktime(&enddate);

	const int cdatestrlen = 120;
	char cdatestr[cdatestrlen];

	while (thedatesec <= enddatesec)
	{
		struct tm* t = localtime(&thedatesec);
		(void)strftime(cdatestr, cdatestrlen, "%Y%m%d", t);
		dates.push_back(string(cdatestr));

		// Advance one day
		t->tm_mday++;
		thedatesec = mktime(t);
	}



	// Now create for each <station, date> pair a url to retrieve the WU observation at that station at that date
	// and the filename the observation will be stored.
	// Also for each station, create an empty directory under the observations directory if it doesn't exist.
	// Airport and pws observations have different url structures, so iterate through each
	// station list seperately.
	// We store the generated url/filename pair in a queue, whose contents will be consumed
	// by the worker threads.
	queue<URLFILE> urls;
	vector<string>::iterator station_iter;
	vector<string>::iterator datestring_iter;

	cout << "Creating url/filename collection queue\n";

	for (station_iter = airports.begin(); station_iter < airports.end(); station_iter++)
	{
		string station = *station_iter;
		// If the station's observations subdirectory doesn't exist, create it
		string dirname = m_observations_directory + "/" + station;
		if (0 != mkdir(dirname.c_str(), S_IRUSR|S_IRGRP|S_IROTH|S_IWUSR|S_IWGRP|S_IWOTH|S_IXUSR|S_IXGRP|S_IXOTH))
		{
			// If the directory exists, we are all right, otherwise don't process the station
			if (EEXIST != errno)
			{
				logfile << "Can't create directory " << dirname << " skipping station " << station << "\n";
				continue;
			}
		}
		else
		{
			logfile << "Created airport station directory " << station << "\n";
		}

		for (datestring_iter = dates.begin(); datestring_iter < dates.end(); datestring_iter++)
		{
			string datestring = *datestring_iter;
			string url = "http://api.wunderground.com/api/" + wukey + "/history_" + datestring + "/q/" + station + ".json";
			string filename = dirname + "/observation-" + station + "-" + datestring + "-json.txt";
			URLFILE uf(url, filename);

			// If we are configured to not overwrite existing observation files, check if the file exists by trying to open it.
			// If we can, then skip adding this url/filename pair to the master urls queue.
			if (m_nooverwrite)
			{
				ifstream checkfile(filename.c_str());
				if (!checkfile.fail())
				{
					continue;
				}
			}

			urls.push(uf);
		}
	}

	for (station_iter = pwss.begin(); station_iter < pwss.end(); station_iter++)
	{
		string station = *station_iter;
		// If the station's observations subdirectory doesn't exist, create it
		string dirname = m_observations_directory + "/" + station;
		if (0 != mkdir(dirname.c_str(), S_IRUSR|S_IRGRP|S_IROTH|S_IWUSR|S_IWGRP|S_IWOTH|S_IXUSR|S_IXGRP|S_IXOTH))
		{
			// If the directory exists, we are all right, otherwise don't process the station
			if (EEXIST != errno)
			{
				logfile << "Can't create directory " << dirname << " skipping station " << station << "\n";
				continue;
			}
		}
		else
		{
			logfile << "Created pws station directory " << station << "\n";
		}

		for (datestring_iter = dates.begin(); datestring_iter < dates.end(); datestring_iter++)
		{
			string datestring = *datestring_iter;
			string url = "http://api.wunderground.com/api/" + wukey + "/history_" + datestring + "/q/pws:" + station + ".json";
			string filename = dirname + "/observation-" + station + "-" + datestring + "-json.txt";
			URLFILE uf(url, filename);

			// If we are configured to not overwrite existing observation files, check if the file exists by trying to open it.
			// If we can, then skip adding this url/filename pair to the master urls queue.
			if (m_nooverwrite)
			{
				ifstream checkfile(filename.c_str());
				if (!checkfile.fail())
				{
					continue;
				}
			}

			urls.push(uf);
		}
	}

	logfile << "airport stations: " << airports.size() << "   pws stations: " << pwss.size() << "   dates: " << dates.size() << "   urls: " << urls.size() << "\n";
	cout << "airport stations: " << airports.size() << "   pws stations: " << pwss.size() << "   dates: " << dates.size() << "   urls: " << urls.size() << "\n";
	if (CURLE_OK != curl_global_init(CURL_GLOBAL_ALL))
	{
		logfile << "Curl library won't initialize" << "\n";
		return(-1);
	}


	// Create and start worker threads who actually do the http request/response and store the
	// results to file.
	cout << "starting " << m_nThreads << " observation collection threads\n";
	pthread_t workerthreads[m_nThreads];
	WORKERTHREADARG wtas[m_nThreads];
	for (int nThread=0; nThread<m_nThreads; nThread++)
	{
		wtas[nThread].m_plogfile = &logfile;
		wtas[nThread].m_purlslock = &m_urlslock;
		wtas[nThread].m_purls = &urls;
		wtas[nThread].m_threadnumber = nThread;
		wtas[nThread].m_mseconds_delay = m_mseconds_delay;
		if (0 != pthread_create(&workerthreads[nThread], NULL, threadfunction, &wtas[nThread]))
		{
			logfile << "Can't create thread # " << nThread << "\n";
		}
	}

	// Give continual progress status to cout while waiting for worker threads to exhaust url queue
	while (1)
	{
		sleep(5);
		(void)pthread_mutex_lock(&m_urlslock);
		int qsize = int(urls.size());
		(void)pthread_mutex_unlock(&m_urlslock);
		cout << qsize << " observations to go before finish\n";
		if (qsize == 0)
			break;
	}

	cout << "All observations collected, waiting for worker threads to terminate\n";

	// Wait for all the threads to finish then we are done.
	for (int nThread=0; nThread<m_nThreads; nThread++)
	{
		(void)pthread_join(workerthreads[nThread], NULL);
	}

	return(0);	
}

Application::~Application()
{
	curl_global_cleanup();
	pthread_mutex_destroy(&m_urlslock);
}

int main(int ac, char** av)
{
	// Read in configuration file
	ifstream configfile(configfilename.c_str());
	if (configfile.fail())
	{
		cerr << "Can't open configuration file " << configfilename << "\n";
		return(-1);
	}

	int nThreads = 20;
	configfile >> nThreads;	
	
	string wukeyfilename;
	configfile >> wukeyfilename;

	string logfilename;
	configfile >> logfilename;

	string airport_stations_list_filename;
	configfile >> airport_stations_list_filename;

	string pws_stations_list_filename;
	configfile >> pws_stations_list_filename;

	string observations_directory;
	configfile >> observations_directory;

	int startday;
	configfile >> startday;

	int startmonth;
	configfile >> startmonth;
	startmonth--;		// ctime months are zero-based
	
	int startyear;
	configfile >> startyear;
	startyear -= 1900;	// ctime years are 1900-based

	int endday;
	configfile >> endday;

	int endmonth;
	configfile >> endmonth;
	endmonth--;		// ctime months are zero-based
	
	int endyear;
	configfile >> endyear;
	endyear -= 1900;	// ctime years are 1900-based

	int getairportobs;
	configfile >> getairportobs;

	int getpwsobs;
	configfile >> getpwsobs;

	int nooverwrite;
	configfile >> nooverwrite;

	double sec_delay;
	configfile >> sec_delay;

// TEST
#if 0
	cout << nThreads << "\n";
	cout << wukeyfilename << "\n";
	cout << logfilename << "\n";
	cout << airport_stations_list_filename << "\n";
	cout << pws_stations_list_filename << "\n";
	cout << observations_directory << "\n";
	cout << startday << "\n";
	cout << startmonth << "\n";
	cout << startyear << "\n";
	cout << endday << "\n";
	cout << endmonth << "\n";
	cout << endyear << "\n";
	cout << getairportobs << "\n";
	cout << getpwsobs << "\n";
	cout << nooverwrite << "\n";
	cout << sec_delay << "\n";
	cout << int(sec_delay * 1000.0) << "\n";

	return(0);
#endif
// TEST
	try
	{
		Application app(logfilename, 
				airport_stations_list_filename, 
				pws_stations_list_filename,
				wukeyfilename, 
				observations_directory, 
				nThreads, 
				startday, startmonth, startyear, 
				endday, endmonth, endyear,
				getairportobs,
				getpwsobs,
				nooverwrite,
				int(sec_delay * 1000.0));
		return (app.Run());
	}
	catch (string& err)
	{
		cerr << err;
		return(-1);
	}

}


