# Copyright (C) 2011-2012 Chiel ten Brinke <ctenbrinke@gmail.com>
#                         Google Inc.
#               2017      ycmd contributors
#
# This file is part of ycmd.
#
# ycmd is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ycmd is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ycmd.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function
from __future__ import division
# Not installing aliases from python-future; it's unreliable and slow.
from builtins import *  # noqa

from collections import defaultdict
from future.utils import iteritems
from future.utils import itervalues
import logging
import os
import errno
import time
import re
import requests
import threading

from ycmd.completers.completer import Completer
from ycmd.utils import ( ForceSemanticCompletion, CodepointOffsetToByteOffset,
                         ToUnicode, urljoin )
from ycmd import responses
from ycmd import utils
from ycmd.completers.completer_utils import GetFileContents
from requests import ( Session )
import logging
from ycmd.completers.cs import solutiondetection
import threading
import traceback
from subprocess import ( PIPE )

SERVER_NOT_FOUND_MSG = ( 'OmniSharp server binary not found at {0}. ' +
                         'Did you compile it? You can do so by running ' +
                         '"./install.py --omnisharp-completer".' )
MAX_PENDING_REQUESTS = 3
INVALID_FILE_MESSAGE = 'File is invalid.'
NO_DIAGNOSTIC_MESSAGE = 'No diagnostic for current line!'
PATH_TO_LEGACY_OMNISHARP_BINARY = os.path.join(
  os.path.abspath( os.path.dirname( __file__ ) ),
  '..', '..', '..', 'third_party', 'OmniSharpServer',
  'OmniSharp', 'bin', 'Debug', 'OmniSharp.exe' )
ROSLYN_OMNISHARP_BINARY = 'OmniSharp'
if utils.OnWindows() or utils.OnCygwin():
  ROSLYN_OMNISHARP_BINARY = 'Omnisharp.exe'
PATH_TO_ROSLYN_OMNISHARP_BINARY = os.path.join(
  os.path.abspath( os.path.dirname( __file__ ) ),
  '..', '..', '..', 'third_party', 'omnisharp-roslyn', ROSLYN_OMNISHARP_BINARY
)
LOGFILE_FORMAT = 'omnisharp_{port}_{sln}_{std}_'


class CsharpCompleter( Completer ):
  """
  A Completer that uses the Omnisharp server as completion engine.
  """

  def __init__( self, user_options ):
    super( CsharpCompleter, self ).__init__( user_options )
    self._logger = logging.getLogger( __name__ )
    self._solution_for_file = {}
    self._completer_per_solution = {}
    self._max_diagnostics_to_display = user_options[
      'max_diagnostics_to_display' ]
    self._solution_state_lock = threading.Lock()
    self._omnisharp_path = PATH_TO_LEGACY_OMNISHARP_BINARY

    if not os.path.isfile( self._omnisharp_path ):
      raise RuntimeError(
           SERVER_NOT_FOUND_MSG.format( self._omnisharp_path ) )


  def Shutdown( self ):
    if self.user_options[ 'auto_stop_csharp_server' ]:
      for solutioncompleter in itervalues( self._completer_per_solution ):
        solutioncompleter._StopServer()


  def SupportedFiletypes( self ):
    """ Supports csharp, razor, and cshtml """
    return [ 'cs', 'razor', 'cshtml' ]


  def _GetSolutionCompleter( self, request_data ):
    """ Get the solution completer or create a new one if it does not already
    exist. Use a lock to avoid creating the same solution completer multiple
    times."""
    solution = self._GetSolutionFile( request_data[ "filepath" ] )

    with self._solution_state_lock:
      if solution not in self._completer_per_solution:
        desired_omnisharp_port = self.user_options.get( 'csharp_server_port' )
        max_diagnostics_to_display = self.user_options[
          'max_diagnostics_to_display' ]
        completer = CsharpSolutionCompleter( self._omnisharp_path,
                                             solution,
                                             desired_omnisharp_port,
                                             max_diagnostics_to_display )
        self._completer_per_solution[ solution ] = completer

    return self._completer_per_solution[ solution ]


  def ShouldUseNowInner( self, request_data ):
    return True


  def CompletionType( self, request_data ):
    return ForceSemanticCompletion( request_data )


  def ComputeCandidatesInner( self, request_data ):
    solutioncompleter = self._GetSolutionCompleter( request_data )
    completion_type = self.CompletionType( request_data )
    return [ responses.BuildCompletionData(
                completion[ 'CompletionText' ],
                completion[ 'DisplayText' ],
                completion[ 'Description' ],
                None,
                None,
                { "required_namespace_import" :
                   completion[ 'RequiredNamespaceImport' ] } )
             for completion
             in solutioncompleter._GetCompletions( request_data,
                                                   completion_type ) ]


  def FilterAndSortCandidates( self, candidates, query ):
    result = super( CsharpCompleter, self ).FilterAndSortCandidates( candidates,
                                                                     query )
    result.sort( key = _CompleteIsFromImport )
    return result


  def GetSubcommandsMap( self ):
    return {
      'StopServer'                       : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_StopServer',
                                   no_request_data = True ) ),
      'RestartServer'                    : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_RestartServer',
                                   no_request_data = True ) ),
      'ReloadSolution'                   : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_ReloadSolution',
                                   no_request_data = True ) ),
      'SolutionFile'                     : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_SolutionFile',
                                   no_request_data = True ) ),
      'GoToDefinition'                   : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GoToDefinition' ) ),
      'GoToDeclaration'                  : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GoToDefinition' ) ),
      'GoTo'                             : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GoToImplementation',
                                   fallback_to_declaration = True ) ),
      'GoToDefinitionElseDeclaration'    : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GoToDefinition' ) ),
      'GoToImplementation'               : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GoToImplementation',
                                   fallback_to_declaration = False ) ),
      'GoToImplementationElseDeclaration': ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GoToImplementation',
                                   fallback_to_declaration = True ) ),
      'GoToUsages'                       : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GoToUsages', ) ),
      'GetType'                          : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GetType' ) ),
      'FixIt'                            : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_FixIt' ) ),
      'GetDoc'                           : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_GetDoc' ) ),
      'Build'                           : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = '_Build' ) ),
      'ServerIsRunning'                  : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = 'ServerIsRunning',
                                   no_request_data = True ) ),
      'ServerIsHealthy'                  : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = 'ServerIsHealthy',
                                   no_request_data = True ) ),
      'ServerIsReady'                    : ( lambda self, request_data, args:
         self._SolutionSubcommand( request_data,
                                   method = 'ServerIsReady',
                                   no_request_data = True ) ),
      'SetOmnisharpPath'                 : ( lambda self, request_data, args:
         self._SetOmnisharpPath( request_data, args[ 0 ] ) ),
      'UseLegacyOmnisharp'                 : ( lambda self, request_data, args:
         self._SetOmnisharpPath( request_data,
                                 PATH_TO_LEGACY_OMNISHARP_BINARY ) ),
      'UseRoslynOmnisharp'                 : ( lambda self, request_data, args:
         self._SetOmnisharpPath( request_data,
                                 PATH_TO_ROSLYN_OMNISHARP_BINARY ) ),
    }


  def _SolutionSubcommand( self, request_data, method,
                           no_request_data = False, **kwargs ):
    solutioncompleter = self._GetSolutionCompleter( request_data )
    if not no_request_data:
      kwargs[ 'request_data' ] = request_data
    return getattr( solutioncompleter, method )( **kwargs )


  def OnFileReadyToParse( self, request_data ):
    solutioncompleter = self._GetSolutionCompleter( request_data )

    # Only start the server associated to this solution if the option to
    # automatically start one is set and no server process is already running.
    if ( self.user_options[ 'auto_start_csharp_server' ]
         and not solutioncompleter._ServerIsRunning() ):
      solutioncompleter._StartServer()
      return

    # Bail out if the server is unresponsive. We don't start or restart the
    # server in this case because current one may still be warming up.
    if not solutioncompleter.ServerIsHealthy():
      return

    return solutioncompleter.OnFileReadyToParse( request_data )


  def GetDetailedDiagnostic( self, request_data ):
    solutioncompleter = self._GetSolutionCompleter( request_data )
    return solutioncompleter.GetDetailedDiagnostic( request_data )


  def DebugInfo( self, request_data ):
    try:
      completer = self._GetSolutionCompleter( request_data )
    except RuntimeError:
      omnisharp_server = responses.DebugInfoServer(
        name = 'OmniSharp',
        handle = None,
        executable = self._omnisharp_path )

      return responses.BuildDebugInfoResponse( name = 'C#',
                                               servers = [ omnisharp_server ] )

    with completer._server_state_lock:
      solution_item = responses.DebugInfoItem(
        key = 'solution',
        value = completer._solution_path )

      omnisharp_server = responses.DebugInfoServer(
        name = 'OmniSharp',
        handle = completer._omnisharp_phandle,
        executable = completer._omnisharp_path,
        address = 'localhost',
        port = completer._omnisharp_port,
        logfiles = [],
        extras = [ solution_item ] )

      return responses.BuildDebugInfoResponse( name = 'C#',
                                               servers = [ omnisharp_server ] )


  def ServerIsHealthy( self ):
    """ Check if our OmniSharp server is healthy (up and serving). """
    return self._CheckAllRunning( lambda i: i.ServerIsHealthy() )


  def ServerIsReady( self ):
    """ Check if our OmniSharp server is ready (loaded solution file)."""
    return self._CheckAllRunning( lambda i: i.ServerIsReady() )


  def _CheckAllRunning( self, action ):
    solutioncompleters = itervalues( self._completer_per_solution )
    return all( action( completer ) for completer in solutioncompleters
                if completer._ServerIsRunning() )


  def _SetOmnisharpPath( self, request_data, omnisharp_path ):
    if self._omnisharp_path == omnisharp_path:
      return
    self._omnisharp_path = omnisharp_path

    if not os.path.isfile( self._omnisharp_path ):
      raise RuntimeError(
           SERVER_NOT_FOUND_MSG.format( self._omnisharp_path ) )

    solution = self._GetSolutionFile( request_data[ "filepath" ] )
    if solution in self._completer_per_solution:
      self._completer_per_solution[ solution ]._StopServer()
      del self._completer_per_solution[ solution ]


  def _GetSolutionFile( self, filepath ):
    if filepath not in self._solution_for_file:
      # NOTE: detection could throw an exception if an extra_conf_store needs
      # to be confirmed
      path_to_solutionfile = solutiondetection.FindSolutionPath(
                               utils.ConvertFilename( filepath, False ) )
      if not path_to_solutionfile:
        raise RuntimeError( 'Autodetection of solution file failed.' )
      self._solution_for_file[ filepath ] = path_to_solutionfile

    return self._solution_for_file[ filepath ]


class CsharpSolutionCompleter( object ):
  def __init__( self, omnisharp_path, solution_path, desired_omnisharp_port,
                max_diagnostics_to_display ):
    self._logger = logging.getLogger( __name__ )
    self._omnisharp_path = omnisharp_path
    self._solution_path = solution_path
    self._max_diagnostics_to_display = max_diagnostics_to_display
    self._diagnostic_store = defaultdict( lambda :
                                          defaultdict( lambda :
                                                       defaultdict( list ) ) )
    self._omnisharp_port = None
    self._omnisharp_phandle = None
    self._desired_omnisharp_port = desired_omnisharp_port
    self._server_state_lock = threading.RLock()
    self._running_commands = 0

    if not os.path.isfile( self._omnisharp_path ):
      raise RuntimeError(
           SERVER_NOT_FOUND_MSG.format( self._omnisharp_path ) )


  def OnFileReadyToParse( self, request_data ):
    if self._IsAtRequestCap():
      return

    filename = request_data[ 'filepath' ]
    if not filename:
      raise ValueError( INVALID_FILE_MESSAGE )

    errors = self._GetResponse( '/codecheck',
                                self._DefaultParameters( request_data ) )

    diagnostics = [ self._QuickFixToDiagnostic( x ) for x in
                    errors[ "QuickFixes" ] ]

    self._SetDiagnosticsInDiagStructure( diagnostics, filename, "PARSE" )

    return [ responses.BuildDiagnosticData( x ) for x in
             self._GetDiagnosticsForWholeFileFromDiagStructure( filename )
             [ : self._max_diagnostics_to_display ] ]


  def _QuickFixToDiagnostic( self, quick_fix ):
    filename = quick_fix[ "FileName" ]
    log_level = quick_fix[ "LogLevel" ]
    if log_level is None:
      log_level = "Error"

    location = responses.Location( quick_fix[ "Line" ],
                                   quick_fix[ "Column" ],
                                   filename )
    location_range = responses.Range( location, location )
    return responses.Diagnostic( list(),
                                 location,
                                 location_range,
                                 quick_fix[ "Text" ],
                                 log_level.upper() )


  def GetDetailedDiagnostic( self, request_data ):
    current_line = request_data[ 'line_num' ]
    current_column = request_data[ 'column_num' ]
    current_file = request_data[ 'filepath' ]

    if not self._diagnostic_store:
      raise ValueError( NO_DIAGNOSTIC_MESSAGE )

    diagnostics = self._GetDiagnosticsForLineFromDiagStructure( current_file,
                                                                current_line )
    if not diagnostics:
      raise ValueError( NO_DIAGNOSTIC_MESSAGE )

    closest_diagnostic = None
    distance_to_closest_diagnostic = 999

    for diagnostic in diagnostics:
      distance = abs( current_column - diagnostic.location_.column_number_ )
      if distance < distance_to_closest_diagnostic:
        distance_to_closest_diagnostic = distance
        closest_diagnostic = diagnostic

    return responses.BuildDisplayMessageResponse(
      closest_diagnostic.text_ )


  def _StartServer( self ):
    """ Start the OmniSharp server if not already running. Use a lock to avoid
    starting the server multiple times for the same solution. """
    with self._server_state_lock:
      if self._ServerIsRunning():
        return

      self._logger.info( 'Starting OmniSharp server' )

      path_to_solutionfile = self._solution_path
      self._logger.info(
          u'Loading solution file {0}'.format( path_to_solutionfile ) )

      self._ChooseOmnisharpPort()

      native_path_to_solutionfile = utils.ConvertFilename( path_to_solutionfile,
                                                           True )
      command = [ self._omnisharp_path,
                  '-p',
                  str( self._omnisharp_port ),
                  '-s',
                  u'{0}'.format( native_path_to_solutionfile ) ]

      if ( not utils.OnWindows() and not utils.OnCygwin()
           and self._omnisharp_path.endswith( '.exe' ) ):
        command.insert( 0, 'mono' )

      self._logger.info( 'Starting OmniSharp server with: ' + str( command ) )

      self._omnisharp_phandle = utils.SafePopen(
          command, stdout = PIPE, stderr = PIPE )
      for target in [
        self._GenerateOutLoop( self._omnisharp_phandle.stdout, "O" ),
        self._GenerateOutLoop( self._omnisharp_phandle.stderr, "E" )
      ]:
        threading.Thread( target = target ).start()

      self._logger.info( 'Starting OmniSharp server' )


  def _GenerateOutLoop( self, stream, type ):
    def out_loop():
      try:
        data = ""
        while not stream.closed and self._omnisharp_phandle is not None:
          new_data = os.read( stream.fileno(), 1024 * 1024 * 10 )
          if not new_data:
            time.sleep( .1 )
          data += ToUnicode( new_data )
          while "\n" in data:
            ( line, data ) = data.split( "\n", 1 )
            self._logger.info( "Omnisharp " + type + ": " + line.rstrip() )
      except Exception:
        self._logger.error( "Read error: " + traceback.format_exc() )

    return out_loop


  def _StopServer( self ):
    """ Stop the OmniSharp server using a lock. """
    with self._server_state_lock:
      self._logger.info( 'Stopping OmniSharp server' )
      self._TryToStopServer()
      self._ForceStopServer()
      self._CleanupAfterServerStop()
      self._logger.info( 'Stopped OmniSharp server' )


  def _TryToStopServer( self ):
    for _ in range( 5 ):
      try:
        self._GetResponse( '/stopserver', timeout = .5 )
      except:
        pass
      for _ in range( 10 ):
        if not self._ServerIsRunning():
          return
        time.sleep( .1 )


  def _ForceStopServer( self ):
    # Kill it if it's still up
    phandle = self._omnisharp_phandle
    if phandle is not None:
      self._logger.info( 'Killing OmniSharp server' )
      for stream in [ phandle.stderr, phandle.stdout ]:
        if stream is not None:
          stream.close()
      try:
        phandle.kill()
      except OSError as e:
        if e.errno == errno.ESRCH: # No such process
          pass
        else:
          raise


  def _CleanupAfterServerStop( self ):
    self._omnisharp_port = None
    self._omnisharp_phandle = None


  def _RestartServer( self ):
    """ Restarts the OmniSharp server using a lock. """
    with self._server_state_lock:
      self._StopServer()
      return self._StartServer()


  def _ReloadSolution( self ):
    """ Reloads the solutions in the OmniSharp server """
    self._logger.info( 'Reloading Solution in OmniSharp server' )
    return self._GetResponse( '/reloadsolution' )


  def CompletionType( self, request_data ):
    return ForceSemanticCompletion( request_data )


  def _GetCompletions( self, request_data, completion_type ):
    """ Ask server for completions """
    if self._IsAtRequestCap():
      return []
    parameters = self._DefaultParameters( request_data )
    parameters[ 'WantImportableTypes' ] = completion_type
    parameters[ 'ForceSemanticCompletion' ] = completion_type
    parameters[ 'WantDocumentationForEveryCompletionResult' ] = True
    completions = self._GetResponse( '/autocomplete', parameters )
    return completions if completions is not None else []


  def _GoToDefinition( self, request_data ):
    """ Jump to definition of identifier under cursor """
    definition = self._GetResponse( '/gotodefinition',
                                    self._DefaultParameters( request_data ) )
    if definition[ 'FileName' ] is not None:
      return responses.BuildGoToResponseFromLocation(
        _BuildLocation( request_data,
                        definition[ 'FileName' ],
                        definition[ 'Line' ],
                        definition[ 'Column' ] ) )
    else:
      raise RuntimeError( 'Can\'t jump to definition' )


  def _GoToImplementation( self, request_data, fallback_to_declaration ):
    """ Jump to implementation of identifier under cursor """
    implementation = self._GetResponse(
        '/findimplementations',
        self._DefaultParameters( request_data ) )

    if implementation[ 'QuickFixes' ]:
      if len( implementation[ 'QuickFixes' ] ) == 1:
        return responses.BuildGoToResponseFromLocation(
          _BuildLocation(
            request_data,
            implementation[ 'QuickFixes' ][ 0 ][ 'FileName' ],
            implementation[ 'QuickFixes' ][ 0 ][ 'Line' ],
            implementation[ 'QuickFixes' ][ 0 ][ 'Column' ] ) )
      else:
        return [ responses.BuildGoToResponseFromLocation(
                   _BuildLocation( request_data,
                                   x[ 'FileName' ],
                                   x[ 'Line' ],
                                   x[ 'Column' ] ) )
                 for x in implementation[ 'QuickFixes' ] ]
    else:
      if ( fallback_to_declaration ):
        return self._GoToDefinition( request_data )
      elif implementation[ 'QuickFixes' ] is None:
        raise RuntimeError( 'Can\'t jump to implementation' )
      else:
        raise RuntimeError( 'No implementations found' )


  def _GetType( self, request_data ):
    request = self._DefaultParameters( request_data )
    request[ "IncludeDocumentation" ] = False

    result = self._GetResponse( '/typelookup', request )
    message = result[ "Type" ]

    return responses.BuildDisplayMessageResponse( message )


  def _FixIt( self, request_data ):
    request = self._DefaultParameters( request_data )

    result = self._GetResponse( '/fixcodeissue', request )
    replacement_text = result[ "Text" ]
    # Note: column_num is already a byte offset so we don't need to use
    # _BuildLocation.
    location = responses.Location( request_data[ 'line_num' ],
                                   request_data[ 'column_num' ],
                                   request_data[ 'filepath' ] )
    fixits = [ responses.FixIt( location,
                                _BuildChunks( request_data,
                                              replacement_text ) ) ]

    return responses.BuildFixItResponse( fixits )


  def _GetDoc( self, request_data ):
    request = self._DefaultParameters( request_data )
    request[ "IncludeDocumentation" ] = True

    result = self._GetResponse( '/typelookup', request )
    message = result[ "Type" ]
    if ( result[ "Documentation" ] ):
      message += "\n" + result[ "Documentation" ]

    return responses.BuildDetailedInfoResponse( message )


  def _GoToUsages( self, request_data ):
    """ Jump to usages of identifier under cursor """
    usages = self._GetResponse( '/findusages',
                                self._DefaultParameters( request_data ) )

    if usages[ 'QuickFixes' ]:
      if len( usages[ 'QuickFixes' ] ) == 1:
        return responses.BuildGoToResponseFromLocation(
          _BuildLocation(
            request_data,
            usages[ 'QuickFixes' ][ 0 ][ 'FileName' ],
            usages[ 'QuickFixes' ][ 0 ][ 'Line' ],
            usages[ 'QuickFixes' ][ 0 ][ 'Column' ],
          ),
          _DecodeDescription( usages[ 'QuickFixes' ][ 0 ][ 'Text' ] ) )
      else:
        return [ responses.BuildGoToResponseFromLocation(
            _BuildLocation(
              request_data,
              x[ 'FileName' ],
              x[ 'Line' ],
              x[ 'Column' ],
            ),
            _DecodeDescription( x[ 'Text' ] ) )
          for x in usages[ 'QuickFixes' ] ]
    else:
      if usages[ 'QuickFixes' ] is None:
        raise RuntimeError( 'Can\'t jump to usage' )
      else:
        raise RuntimeError( 'No usages found' )


  def _Build( self, request_data ):
    build_errors = self._GetResponse( '/build',
                                self._DefaultParameters( request_data ) )

    diagnostics = [ self._QuickFixToDiagnostic( x ) for x in
                    build_errors[ "QuickFixes" ] ]

    self._SetAllFileDiagnosticsInDiagStructure( diagnostics, "BUILD" )

    return [ responses.BuildDiagnosticData( x ) for x in
             self._GetDiagnosticsForAllFilesFromDiagStructure( "BUILD" ) ]


  def _DefaultParameters( self, request_data ):
    """ Some very common request parameters """
    parameters = {}
    parameters[ 'line' ] = request_data[ 'line_num' ]
    parameters[ 'column' ] = request_data[ 'column_codepoint' ]

    filepath = request_data[ 'filepath' ]
    parameters[ 'buffer' ] = (
      request_data[ 'file_data' ][ filepath ][ 'contents' ] )
    parameters[ 'filename' ] = filepath
    return parameters


  def _ServerIsRunning( self ):
    """ Check if our OmniSharp server is running (process is up)."""
    return utils.ProcessIsRunning( self._omnisharp_phandle )


  def ServerIsHealthy( self ):
    """ Check if our OmniSharp server is healthy (up and serving)."""
    if not self._ServerIsRunning():
      return False

    try:
      return self._GetResponse( '/checkalivestatus', timeout = 3 )
    except Exception:
      return False


  def ServerIsReady( self ):
    """ Check if our OmniSharp server is ready (loaded solution file)."""
    if not self._ServerIsRunning():
      return False

    try:
      return self._GetResponse( '/checkreadystatus', timeout = .2 )
    except Exception:
      return False


  def _SolutionFile( self ):
    """ Find out which solution file server was started with """
    return self._solution_path


  def _ServerLocation( self ):
    # We cannot use 127.0.0.1 like we do in other places because OmniSharp
    # server only listens on localhost.
    return 'http://localhost:' + str( self._omnisharp_port )


  def _IsAtRequestCap ( self ):
    return self._running_commands >= MAX_PENDING_REQUESTS


  def _GetResponse( self, handler, parameters = {}, timeout = None ):
    """ Handle communication with server """
    if self._IsAtRequestCap():
      raise RuntimeError( "Too many commands running, try again later" )
    self._running_commands += 1
    try:
      target = urljoin( self._ServerLocation(), handler )
      self._logger.info( u'Sending request' )
      response = requests.post( target,
                                json = parameters,
                                timeout = timeout )
      self._logger.info( u'Received response request' )
      return response.json()
    finally:
      self._running_commands -= 1


  def _ChooseOmnisharpPort( self ):
    if not self._omnisharp_port:
        if self._desired_omnisharp_port:
            self._omnisharp_port = int( self._desired_omnisharp_port )
        else:
            self._omnisharp_port = utils.GetUnusedLocalhostPort()
    self._logger.info( u'using port {0}'.format( self._omnisharp_port ) )


  def _SetDiagnosticsInDiagStructure( self, diagnostics, filename, source ):
    for key, line in iteritems(self._diagnostic_store[ filename ][ source ]):
      del line[:]

    for diagnostic in diagnostics:
        source_store = self._diagnostic_store[ filename ][ source ]
        source_store[ diagnostic.location_.line_number_ ] .append( diagnostic )

  def _SetAllFileDiagnosticsInDiagStructure( self, diagnostics, source ):
    for filename in self._diagnostic_store:
      for key, line in iteritems(self._diagnostic_store[ filename ][ source ]):
        del line[:]

    for diagnostic in diagnostics:
      filename = diagnostic.location_.filename_
      source_store = self._diagnostic_store[ filename ][ source ]
      source_store[ diagnostic.location_.line_number_ ] .append( diagnostic )


  def _GetDiagnosticsForLineFromDiagStructure( self, filename, line ):
    result = []
    for source in self._diagnostic_store[ filename ]:
        for item in self._diagnostic_store[ filename ][ source ][ line ]:
          result.append( item )
    return result


  def _GetDiagnosticsForWholeFileFromDiagStructure( self, filename ):
    result = []
    for source in self._diagnostic_store[ filename ]:
      for line in self._diagnostic_store[ filename ][ source ]:
        for item in self._diagnostic_store[ filename ][ source ][ line ]:
          result.append( item )
    return result


  def _GetDiagnosticsForAllFilesFromDiagStructure( self, source ):
    result = []
    for filename in self._diagnostic_store:
      for line in self._diagnostic_store[ filename ][ source ]:
        for item in self._diagnostic_store[ filename ][ source ][ line ]:
          result.append( item )
    return result


  def _SetOmnisharpPath( self, request_data, path ):
    self._omnisharp_path = path

    if not os.path.isfile( self._omnisharp_path ):
      raise RuntimeError(
           SERVER_NOT_FOUND_MSG.format( self._omnisharp_path ) )


def _DecodeDescription( string ):
    return string.decode( "string-escape" ).strip()


def _CompleteIsFromImport( candidate ):
  try:
    return candidate[ "extra_data" ][ "required_namespace_import" ] is not None
  except ( KeyError, TypeError ):
    return False


def _BuildChunks( request_data, new_buffer ):
  filepath = request_data[ 'filepath' ]
  old_buffer = request_data[ 'file_data' ][ filepath ][ 'contents' ]
  new_buffer = _FixLineEndings( old_buffer, new_buffer )

  new_length = len( new_buffer )
  old_length = len( old_buffer )
  if new_length == old_length and new_buffer == old_buffer:
    return []
  min_length = min( new_length, old_length )
  start_index = 0
  end_index = min_length
  for i in range( 0, min_length - 1 ):
    if new_buffer[ i ] != old_buffer[ i ]:
      start_index = i
      break
  for i in range( 1, min_length ):
    if new_buffer[ new_length - i ] != old_buffer[ old_length - i ]:
      end_index = i - 1
      break
  # To handle duplicates, i.e aba => a
  if ( start_index + end_index > min_length ):
    start_index -= start_index + end_index - min_length

  replacement_text = new_buffer[ start_index : new_length - end_index ]

  ( start_line, start_column ) = _IndexToLineColumn( old_buffer, start_index )
  ( end_line, end_column ) = _IndexToLineColumn( old_buffer,
                                                 old_length - end_index )

  # No need for _BuildLocation, because _IndexToLineColumn already converted
  # start_column and end_column to byte offsets for us.
  start = responses.Location( start_line, start_column, filepath )
  end = responses.Location( end_line, end_column, filepath )
  return [ responses.FixItChunk( replacement_text,
                                 responses.Range( start, end ) ) ]


def _FixLineEndings( old_buffer, new_buffer ):
  new_windows = "\r\n" in new_buffer
  old_windows = "\r\n" in old_buffer
  if new_windows != old_windows:
    if new_windows:
      new_buffer = new_buffer.replace( "\r\n", "\n" )
      new_buffer = new_buffer.replace( "\r", "\n" )
    else:
      new_buffer = re.sub( "\r(?!\n)|(?<!\r)\n", "\r\n", new_buffer )
  return new_buffer


# Adapted from http://stackoverflow.com/a/24495900
def _IndexToLineColumn( text, index ):
  """Get 1-based (line_number, col) of `index` in `string`, where string is a
  unicode string and col is a byte offset."""
  lines = text.splitlines( True )
  curr_pos = 0
  for linenum, line in enumerate( lines ):
    if curr_pos + len( line ) > index:
      return ( linenum + 1,
               CodepointOffsetToByteOffset( line, index - curr_pos + 1 ) )
    curr_pos += len( line )
  assert False


def _BuildLocation( request_data, filename, line_num, column_num ):
  if line_num <= 0 or column_num <= 0:
    return None
  contents = utils.SplitLines( GetFileContents( request_data, filename ) )
  line_value = contents[ line_num - 1 ]
  return responses.Location(
      line_num,
      CodepointOffsetToByteOffset( line_value, column_num ),
      filename )
