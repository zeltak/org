#if !defined(__GET_OBSERVATION_BY_DATE_ID_JSON_H__)
#define __GET_OBSERVATION_BY_DATE_ID_JSON_H__

#include <iostream>
#include <fstream>
#include <string>
#include <queue>

using namespace std;

#include <pthread.h>

typedef struct urlfile {
	string url;
	string filename;

	urlfile() : url(""), filename("") {};
	urlfile(const string& u, const string& f) : url(u), filename(f) {};
	urlfile(const urlfile& uf) : url(uf.url), filename(uf.filename) {};
	urlfile& operator=(const urlfile& uf);
	
} URLFILE;

class LogFile
{
public:
	LogFile(const string& filename) : m_filename(filename) 
		{ 
		if (0 != pthread_mutex_init(&m_lock, NULL)) throw string("Can't create mutex in Application\n");
		m_file.open(filename.c_str(), ios::out);
		if (m_file.fail()) throw string("Can't open logfile " + m_filename + "\n");
		};
	~LogFile()
		{ pthread_mutex_destroy(&m_lock); };

	LogFile& operator<<(const string& s);
	LogFile& operator<<(const char* s);
	LogFile& operator<<(int i);

	bool Fail() 
		{return m_file.fail();};
 
private:
	const string m_filename;
	ofstream m_file;
	pthread_mutex_t m_lock;
};

typedef struct workerthreadarg
{
	LogFile* m_plogfile;
	pthread_mutex_t* m_purlslock;
	queue<URLFILE>* m_purls;
	int m_threadnumber;
	int m_mseconds_delay;
} WORKERTHREADARG;


class Application
{
public:
	Application(const string& logfilename, 
			const string& airportsfilename, 
			const string& pwsfilename, 
			const string& wukeyfilename, 
			const string& observations_directory, 
			int nThreads,
			int startday,
			int startmonth,
			int startyear,
			int endday,
			int endmonth,
			int endyear,
			int getairportobs,
			int getpwsobs,
			int nooverwrite,
			int mseconds_delay) : 
			m_logfilename(logfilename), 
			m_airportsfilename(airportsfilename), 
			m_pwsfilename(pwsfilename), 
			m_wukeyfilename(wukeyfilename), 
			m_observations_directory(observations_directory), 
			m_nThreads(nThreads),
			m_startday(startday),
			m_startmonth(startmonth),
			m_startyear(startyear),
			m_endday(endday),
			m_endmonth(endmonth),
			m_endyear(endyear),
			m_getairportobs(getairportobs),
			m_getpwsobs(getpwsobs),
			m_nooverwrite(nooverwrite),
			m_mseconds_delay(mseconds_delay)

		{ if (0 != pthread_mutex_init(&m_urlslock, NULL)) throw string("Can't create mutex in Application\n"); };
	~Application();

	int Run();

private:
	const string m_logfilename;
	const string m_airportsfilename;
	const string m_pwsfilename;
	const string m_wukeyfilename;
	const string m_observations_directory;

	const int m_getairportobs;
	const int m_getpwsobs; 

	const int m_nooverwrite;

	const int m_startday;
	const int m_startmonth;
	const int m_startyear;
	const int m_endday;
	const int m_endmonth;
	const int m_endyear;

	const int m_nThreads;

	const int m_mseconds_delay;

	// Lock for url queue - each worker thread will get a reference to it.
	pthread_mutex_t m_urlslock;
};

#endif

