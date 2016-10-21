# Useful links
Here are some docs for general usage and daily programming

## OTP Docs

### supervisor
* This is a really simple guide with hints for a newbie on what a supervisor is, basic usage and some source code as well
  - https://pdincau.wordpress.com/2010/01/28/supervisors-in-erlang-otp/
* StackOverflow question about adding a child process to a supervisor (simple_one_for_one), simple tricks and notes from users
  - http://stackoverflow.com/questions/4837196/erlang-supervisor3-adding-a-child-process
* Supervisor design principles: changing, code hot loading, restart/upgrade (from official docs)
  - http://erlang.org/doc/design_principles/appup_cookbook.html

### gen_server
* This one is a beginner guide for gen_server, very well written and simple
  - http://bytefilia.com/erlang-otp-gen_server-template-example/
* A clean and good sample for gen_server
  - http://20bits.com/article/erlang-a-generic-server-tutorial

## Other libraries

### lager
* A logging framework for Erlang/OTP from Basho
  - http://basho.com/posts/technical/introducing-lager-a-new-logging-framework-for-erlangotp/
  - https://pdincau.wordpress.com/2012/08/11/how-to-log-your-stuff-with-lager/

### fast xml
* Fast Expat based Erlang XML parsing library
  - https://github.com/processone/fast_xml
  - This library might be used without dependencies but see my comment [in this issue](https://github.com/processone/fast_xml/issues/10). Copy p1_nif_utils.erl in the same project to compile it
