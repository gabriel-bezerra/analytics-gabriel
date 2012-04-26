#netstat -t | grep lcc2 | cut -f 17  -d ' ' | cut -f 1 -d : | sed s/$/.lcc.ufcg.edu.br/ | sort | uniq
netstat -t | grep lcc2 | cut -f 17  -d ' ' | cut -f 1 -d : | sort | uniq
