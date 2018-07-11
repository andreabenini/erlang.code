Suggestion for dicts:
- Multiple processes on multiple VMs -> use mnesia
- Multiple processes on one VM -> use ets/dets
- One process -> use bag/dict/...


Useful Links:
- Creating a lightweight in-memory key/value storage system
  - http://erlang-tutorials.colefichter.ca/kv1
  - http://erlang-tutorials.colefichter.ca/kv2
  - http://erlang-tutorials.colefichter.ca/kv3
