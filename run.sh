# Set UDP Buffer Limits
# https://access.redhat.com/documentation/en-US/JBoss_Enterprise_Web_Platform/5/html/Administration_And_Configuration_Guide/jgroups-perf-udpbuffer.html 
# max: 2^23-1 = 4294967295
# 50*1024*1024
sysctl -w net.core.rmem_max=52428800

echo "-- compiling --"
rebar3 clean && rm _build/default/lib/udpserver -rf 2>/dev/null && rebar3 compile

echo "-- running --"
erl -env ERL_LIBS _build/default/lib -eval 'application:ensure_all_started(udpserver).' -noshell

# production
#./bin/udpserver foreground