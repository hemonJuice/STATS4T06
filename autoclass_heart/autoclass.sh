
# the "y" parameter validates warning
# in case of a reproducible run
autoclass -search autoclass.db2 autoclass.hd2 autoclass.model autoclass.s-params >autoclass-search.log 2>&1 <<EOF
y
EOF
autoclass -reports autoclass.results-bin autoclass.search autoclass.r-params >autoclass-report.log 2>&1

if [ $? -eq 0 ]
then
    touch autoclass-run-success
else
    touch autoclass-run-failure
fi
