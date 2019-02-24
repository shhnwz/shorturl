shorturl
=====

An OTP application

Pre-requisites
--------------
* riak docker setup commands
    - Load and run riak-kv docker image
    ```
    docker run --name=riak -d -p 8087:8087 -p 8098:8098 basho/riak-kv
    ```
    - Login to riak container console for issuing admin commands.
    ```
    bash -c "clear && docker exec -it riak sh"
    ```
    - Create and activate CRDT bucket type 
    ```
    #>riak-admin bucket-type create crdt_counter_type '{"props":{"datatype":"counter"}}'
    #>riak-admin bucket-type activate crdt_counter_type
    ```

Build
-----
    $ sh pre_compile.sh

    $ rebar3 compile

Eunit
-----
    $ rebar3 eunit

Expected Compilation Issue/s (Even after executing pre_compile.sh)
---------------------------
Compiling _build/default/lib/riakc/src/riakc_pb_socket.erl failed
_build/default/lib/riakc/src/riakc_pb_socket.erl:2384: call to crypto:rand_bytes/1 will fail, since it was removed in 20.0; use crypto:strong_rand_bytes/1

Fix
---
open rebar.config of riakc

vi _build/default/lib/riakc/rebar.config

change two lines as per 'riakc_erl20.diff' file

diff --git a/rebar.config b/rebar.config
index 5fb1e7f..5c6394f 100644
--- a/rebar.config
+++ b/rebar.config
@@ -1,4 +1,4 @@
-{require_otp_vsn, "R15|R16|17|18|19"}.
+{require_otp_vsn, "R15|R16|17|18|19|20"}.
 
 {cover_enabled, true}.
 
@@ -9,7 +9,7 @@
     warnings_as_errors,
     {platform_define, "^[0-9]+", namespaced_types},
     {platform_define, "(?=^[0-9]+)(?!^17)", deprecated_now},
-    {platform_define, "^19", deprecated_19}
+    {platform_define, "^(19|20)", deprecated_19}
 ]}.
 

Starting the Application
-----------------------
In shell mode

$ rebar3 shell

Test Curl Commands
------------------
```
curl -X POST -d 'www.google.com'  http://localhost:8080/shorturl/generate_token
```
Output Token  -> ChN7N1zDAA==

```
curl -X GET -d 'ChN7N1zDAA=='  http://localhost:8080/shorturl/redirect
```
Output URL -> www.google.com


    
