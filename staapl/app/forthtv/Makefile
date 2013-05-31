all:

bootstrap: monitor.state

# don't generate it if it exists
monitor.state:
	cp init-monitor.state monitor.state

clean:
	rm -f *~
	rm -f monitor.hex monitor.state
