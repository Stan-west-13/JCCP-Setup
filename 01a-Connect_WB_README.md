Connect to Wordbank Database
================

Data used in these analyses are pulled from an archival version of the
Wordbank database (July 7, 2022).

Accessing this version first requires installation of MySQL server to
your local machine.

Then, in your native command line run the following commands:

``` text
## For Windows:
wget https://wordbank-versioning.s3.amazonaws.com/wordbank_20220722.sql -O wordbank_20220722.sql

## For Linux: 
# download .sql file
wget https://wordbank-versioning.s3.amazonaws.com/wordbank_20220722.sql
```

Before accessing the database in R, you will need to initiate the
connection in MySQL. Once you launch MySQL, you will run the following
command to initiate the connection:

``` text
## In MySQL Command Line Client
\. PATH-TO-DATABASE\wordbank_20220722.sql 
```

Once you have the connection, move to script 1b to pull from the
database.
