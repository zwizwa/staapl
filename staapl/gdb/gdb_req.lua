

-- generate GDB command to init array
function gdb_reply(gdb_req)
    local f = io.open("reply.gdb","w+")
    f:write (string.format("set gdb_reply={%d", gdb_req[1]))
    for i=2,#gdb_req do
       f:write (string.format(",%d", gdb_req[i]))
    end
    f:write("}\n")
    g:close()
 end

-- read request
function gdb_request()
   local f = io.open("request.bin", "r")
   -- use parse-bin.lua
end


-- gdb_reply(handle(gdb_request()))
