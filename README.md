# About
punch is a commandline time tracking repl.

# Usage

```
Usage: punch [command or project]

Commands:

help                                     show help
[project]                                start tracking in project
rm [project]                             remove project
ls                                       list projects

Tracking Commands:

help                                     show help
now [activity]                           start tracking activity
punch [project]                          switch to project
stop                                     stop tracking activity
exit                                     leave project
time [activity]                          show time spend on activity

ls                                       list activities
    ls -d                                list activities today
    ls -w                                list activities this week

sum                                      show summary today
    sum -d[n]                            show summary today - n days  
    sum -w[n]                            show summary this week - n weeks

agenda                                   show agenda today
    agenda -d[n]                         show agenda today - n days  
    agenda -w[n]                         show agenda this week - n weeks

rm [activity]                            remove activity

add [activity] [d[.m][.y]] h[:mm]-h[:mm] add activity manually

    add x 10-11                          adds activity x today 10:00 to 11:00
    add x 10 10:30-11:30                 adds activity x at 10th day of current month
    add x 10.1.1999 10-11                adds activity x on specified date

use <TAB> to autocomplete activity and project names
```