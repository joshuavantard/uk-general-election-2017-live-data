wget  -Ofull.json https://interactive.guim.co.uk/2017/06/ukelection2017-data/snap/full.json
python compare.py
R --vanilla --no-save < report.R

