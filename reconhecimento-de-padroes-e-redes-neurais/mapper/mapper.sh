#!/bin/bash

if [ $# -ne 3 ]; then
    echo "Usage: $0 <login> <hosts-list-script> <commands-file>"
    exit 1
fi

USER=$1
HOSTS_SCRIPT=$2
COMMANDS_FILE=$3


machine=0  # first machine to be used

HOSTS=$($HOSTS_SCRIPT)

function machine_n {
    N_HOSTS=$(echo $HOSTS | wc -w)

    ACTUAL_HOST_N=$((($1%$N_HOSTS) + 1))
    ACTUAL_HOST=$(echo $HOSTS | cut -d ' ' -f $ACTUAL_HOST_N)

    echo $ACTUAL_HOST
}


while read line
do
    PREFIX=$(echo $line | cut -f 1 -d '|')
    N_RUNS=$(echo $line | cut -f 2 -d '|')
    COMMAND=$(echo $line | cut -f 3 -d '|' | sed -e s/\$prefix/$PREFIX/g)

    for i in $(seq $N_RUNS); do
        RUN_COMMAND=${COMMAND//'$run'/$i}
        STD_OUTPUT_FILE=$PREFIX-$i.std.out
        ERR_OUTPUT_FILE=$PREFIX-$i.err.out

        echo "ssh -n "$USER@$(machine_n $machine)" $RUN_COMMAND > $STD_OUTPUT_FILE 2> $ERR_OUTPUT_FILE &"
        ssh -n "$USER@$(machine_n $machine)" "$RUN_COMMAND" > $STD_OUTPUT_FILE 2> $ERR_OUTPUT_FILE &

        # set next machine to be used
        machine=$(($machine + 1))
    done
done < $COMMANDS_FILE

