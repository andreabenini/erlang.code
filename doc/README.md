# Useful links
Here are some docs for general usage and daily programming

## System & General documentation
* System Principles
  - http://erlang.org/doc/system_principles/system_principles.html
* Free Books and references
  - http://opensource.erlang-solutions.com/erlang-handbook/
  - http://learnyousomeerlang.com/ (LYSE: Learn You Some Erlang)
* Articles
  - http://www.theerlangelist.com/article/spawn_or_not To spawn, or not to spawn


## BEAM
### The BEAM Book
* This excellent book was made from one of the creators of the BEAM VM, definitively a must read !
  - https://github.com/happi/theBeamBook
* 10 Ways to stop the virtual machine
  - https://medium.com/erlang-battleground/10-ways-to-stop-an-erlang-vm-7016bd593a5


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

### general OTP
* Design systems with asynchronous message passing between processes
  - https://www.youtube.com/watch?v=6sBL1kHoMoo

## Other libraries

### lager
* A logging framework for Erlang/OTP from Basho
  - http://basho.com/posts/technical/introducing-lager-a-new-logging-framework-for-erlangotp/
  - https://pdincau.wordpress.com/2012/08/11/how-to-log-your-stuff-with-lager/

### fast xml
* Fast Expat based Erlang XML parsing library
  - https://github.com/processone/fast_xml
  - This library might be used without dependencies but see my comment [in this issue](https://github.com/processone/fast_xml/issues/10). Copy p1_nif_utils.erl in the same project to compile it

### httpc
* httpc example with SSL connection
  - https://github.com/processone/ejabberd/issues/1506

### mnesia
* mnesia generic manual
  - http://ftp.stu.edu.tw/FreeBSD/distfiles/erlang/mnesia-4.0.2.pdf
* backup/restore single table from/to mnesia
  - https://stackoverflow.com/questions/3709051/how-to-backup-restore-only-single-table-from-to-mnesia
* rename the Node running a mnesia Database
  - https://stackoverflow.com/questions/463400/how-to-rename-the-node-running-a-mnesia-database
  - http://blog.ikura.co/posts/prod-to-dev-with-mnesia.html
* proper way to backup/restore a mnesia database?
  - https://stackoverflow.com/questions/10154414/what-is-the-proper-way-to-backup-restore-a-mnesia-database
* mnesia database backup/restore with checkpoints
  - http://www.programering.com/a/MzNyYTMwATE.html


## Profiling, Rebar, General Tools, utilities
* Erlang Intro, Creating a simple library, Rebar basics
  - https://howistart.org/posts/erlang/1/index.html
* A set of functions for time profiling of Erlang programs to find out how the execution time is used
  - http://erlang.org/doc/man/eprof.html
* Code snippets and practices that helped developers debug production systems that were built in Erlang
  - https://www.erlang-in-anger.com/
* Recon is a set of tools usable in production to diagnose Erlang problems or inspect production environment safely
  - https://github.com/ferd/recon
* An informative youtube channel on Erlang Engine tuning (Eugene Kalinin)
  - https://www.youtube.com/playlist?list=PL9MhXsiBgon5QVFi3rQDZAlftwPCW8gLc


## Speechs, Presentations
* Saša Jurić
  - https://vimeo.com/130867470 High Availability ElixirConf EU 2015
  - https://www.youtube.com/watch?v=Ba3aCm3A0o8, HA with Elixir and Erlang (FullStack Fest 2016)
* Garrett Smith
  - https://www.youtube.com/watch?v=BO-8Hx8kPtA Building a web app in Erlang
* Jamie Winsor
  - https://www.youtube.com/watch?v=eZtkDLellyM Building server and porting them into production [elixir]


## Massive Data Exchange (Global Process Registries)
* Massive interconnection of multiple devices and message passing between them
  - http://www.ostinelli.net/an-evaluation-of-erlang-global-process-registries-meet-syn/
  
