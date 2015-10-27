get-observations-by-date-id-json: get-observations-by-date-id-json.o
	g++ get-observations-by-date-id-json.o -o get-observations-by-date-id-json -L/usr/lib/x86_64-linux-gnu -lpthread -lcurl -Wl,-Bsymbolic-functions -Wl,-z,relro
	chmod 0755 get-observations-by-date-id-json

get-observations-by-date-id-json.o: get-observations-by-date-id-json.cpp get-observations-by-date-id-json.h
	g++ -c get-observations-by-date-id-json.cpp

clean:
	rm get-observations-by-date-id-json.o get-observations-by-date-id-json


