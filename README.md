[logcat-mode](https://github.com/dcolascione/logcat-mode) is a tool
for viewing Android system log output.  It supports filtering output
by a configurable set of rules, pushing and popping rule sets,
visiting Java files referenced in log messages, and viewing metadata
about log entries.

![Typical screenshot](/screenshot1.png?raw=true)

You can configure the display style and individually enable and
disable showing fields of log records.  These changes apply to log
records already in the buffer as well as log entries
subsequently received.

![Brief style](/screenshot2.png?raw=true)

Another convenient feature is calculating the time difference between
log entries.

![Time difference](/screenshot3.png?raw=true)

You will need [fb-adb](https://github.com/facebook/fb-adb) in order
for logcat-mode to work.

