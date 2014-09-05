#!/usr/bin/env python

import sys
import os
import socket
import select
import threading
import traceback
import time
import re
import datetime

from optparse import OptionParser

try:
    import readline
except:
    pass

havePIL = False

try:
    from PIL import Image
    havePIL = True
except:
    pass


class SocketThread( threading.Thread ):

    def __init__( self , host , port , script , filter , log ):

        threading.Thread.__init__( self )

        self.done = False
        self.host = host
        self.port = port
        self.script = script
        self.filter = filter
        self.log = None

        if log is not None:

            try:

                self.log = open( log , "at" )

            except Exception , e :

                print "FAIL TO OPEN" , log , ":" , str( e )

        self.sock = None

    def quit(self):
        self.done = True
        self.sock.close();

    def send(self, s):
                if s.startswith( ":" ):

                    command = s[ 1 : ]
                    if command[0:5].lower() == "sleep":
                        delay = 1
                        print "SLEEPING", delay
                        time.sleep( delay )
                    elif command[0:10].lower() == "disconnect":
                        print "QUIT"
                        self.quit()
                    else:

                        print "RUNNING" , command

                        try:

                            execfile( command )

                        except KeyboardInterrupt:

                            pass

                        except:

                            print "THERE IS AN ERROR IN THE SCRIPT:"
                            traceback.print_exc()

                        print "FINISHED"

                else:
                    #print "SENDING :%s:" % (s)
                    self.send_internal(s)

    def send_internal( self , data ):

        if self.sock:

            try:

                self.sock.send( data )

            except:

                pass


    def run( self ):

        while not self.done:

            try:

                print "Connecting to" , self.host , self.port , "...\n" ,

                self.sock = socket.socket( socket.AF_INET , socket.SOCK_STREAM )

                self.sock.connect( ( self.host , self.port ) )

                print "Connected (%s:%d)\n" % (self.host, self.port)

            except Exception , e:

                print "failed :" , str( e )

                self.sock = None


            if not self.sock:

                print "Waiting (%s:%d)\n" % (self.host, self.port)

                time.sleep( 1 )

                continue


            if self.script:

                try:

                    for line in open( self.script , "rt" ):
                        self.send( line )

                except Exception , e:

                    print "Failed to read script" , self.script , ":" , str( e )

            inbound_data = ""

            while not self.done:
                read_ready , write_ready , errors = select.select([self.sock ] , [] , [ self.sock ], 0.5 )

                if self.sock in read_ready:

                    try:

                        data_received = self.sock.recv( 4096 )

                    except:

                        break

                    if len( data_received ) == 0:

                        break

                    # Split it into lines

                    inbound_data += data_received

                    while True:

                        line , separator , rest = inbound_data.partition( "\n" )

                        if len( line ) > 0:

                            if ( not self.filter ) or ( self.filter and self.filter.search( line ) ):

                                print line

                                if self.log:
                                    self.log.write( line + "\n" )

                        inbound_data = rest

                        if len( separator ) == 0:

                            break


                if self.sock in errors:

                    break


            try:
                self.sock.shutdown( socket.SHUT_RDWR )
                self.sock.close()
            except:
                pass

            self.sock = None
            print "Disconnected (%s:%d)\n" % (self.host, self.port)

def main( host , port , script , filter , log ):

    try:

        thread = SocketThread( host , port , script , filter , log )
        thread.start()

        def send( s ):
            if not s.endswith( "\n" ):
                s += "\n"
            thread.send( s )

        # Load readline history

        history_file_name = os.path.join( os.path.expanduser('~') , ".pyconsole.history" )

        try:

            readline.read_history_file( history_file_name )

        except:

            pass

        while not thread.done:
            s = raw_input( )

            if len( s ) > 0:
                send( s )

    except (EOFError, KeyboardInterrupt):
        thread.quit()
        readline.write_history_file( history_file_name )
        return 0;

    try:

        readline.write_history_file( history_file_name )

    except:

        pass

    thread.quit()
    return 0



if __name__ == "__main__":

    parser = OptionParser( "usage: %prog [options] host" )

    parser.add_option( "-s" , "--script" , help="Script to run after connection" )
    parser.add_option( "-f" , "--filter" , help="Regular expression to filter output" )
    parser.add_option( "-p" , "--port"   , help="Port (defaults to 9536)" , type="int" , default=9536)
    parser.add_option( "-l" , "--log"    , help="Log to a file" )


    ( options , args ) = parser.parse_args()

    if len( args ) < 1:

        parser.error( "You must pass the host" )

    filter = None

    if options.filter:

        try:

            filter = re.compile( options.filter )

        except Exception , e:

            parser.error( "Invalid filter expression : " + str( e ) )


    e = main( args[ 0 ] , options.port , options.script , filter , options.log )










