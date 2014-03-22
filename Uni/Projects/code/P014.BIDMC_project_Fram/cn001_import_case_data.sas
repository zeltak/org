libname tmp "Z:\";


data geocodes_offspring;
set	tmp.geocodes_offspring;
run;

data geocodes_gen3;
set	tmp.geocodes_gen3;
run;

libname poll "H:\";


data poll;
set	poll.poll_0008;
run;


proc summary nway data=	geocodes_offspring;
class ranid	;
var long lat;
output out=offspring mean=long lat;
run;


proc summary nway data=	geocodes_gen3;
class ranid	;
var long lat;
output out=gen3 mean=long lat;
run;



data offspring;
set offspring;
if long=. then delete;
run;


proc freq data=offspring;
table ranid /list;
run;
