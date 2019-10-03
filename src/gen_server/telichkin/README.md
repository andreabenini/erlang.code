# OTP CheatSheet

###### [Credits to: https://github.com/Telichkin/otp_cheatsheet]

Every OTP behavior has main parts in its API: client, server, possible inputs and possible outputs. But a one-dimensional structure of standard documentation (from top to bottom) can't present all these parts in one place which leads to loss of a context and longer learn-curve.

This cheat sheet is an attempt to present common parts of OTP behaviors in one place using opportunities of a two-dimensional structure.

## Table of Contents
- Supervisor
  - Init
- Gen Server
  - Init
  - Sync operation
  - Async operation
  - Info message
  - Terminate

## Supervisor
#### Init
![Supervisor Init](./supervisor_init.svg)

## Gen Server
#### Init
![gen_server_init.svg](./gen_server_init.svg)
#### Sync operation
![gen_server_call.svg](./gen_server_call.svg)
#### Async operation
![gen_server_cast.svg](./gen_server_cast.svg)
#### Info message
![gen_server_info.svg](./gen_server_info.svg)
#### Terminate
![gen_server_terminate.svg](./gen_server_terminate.svg)

