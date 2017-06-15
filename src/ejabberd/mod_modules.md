## module running parameters
To get current parameters from a running module you can do this:
- Go to web admin interface (example: http://127.0.0.1:9090/admin/)
- In the "Virtual Hosts" section select the desired domain
- Select "Nodes" section
- Select the running node, probably something like "ejabberd@localhost" (default)
- Select "Modules" section
- Inside it there are all the running modules for that node/host, just detect where your module is (Table Header: module)
- Take a look at its associated Options row, select the erlang blob from there, no matter if it's in a binary form, just get it from there
- Put it in an erlang shell to get it in a human readable form, you may nicely format it in this way
```erlang
Params = "PASTE HERE YOUR CODE WITHOUT QUOTES".
io:fwrite("And here are your module running params:~n~p~n", [Params]).
```
