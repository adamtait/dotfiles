#!/usr/bin/env bash

# increase the max number of procs that [single user = 2000] & [all
# users = 2500] can run at a time
#   NEEDED to run react native on simulator in xcode
alias increase-max-procs="sudo launchctl limit maxproc 2000 2500"
