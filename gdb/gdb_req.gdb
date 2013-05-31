
define service
       dump binary value req.bin gdb_request
       shell lua gdb_req.lua
       source reply.gdb
       continue
end

define start_service
       break gdb_call
       tbreak main
       run
       while 1
              service
       end
end

start_service
