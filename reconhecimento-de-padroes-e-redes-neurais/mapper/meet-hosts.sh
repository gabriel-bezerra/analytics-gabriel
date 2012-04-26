#!/bin/bash

for i in $(./hosts.sh); do
    ssh -n $i exit
done

