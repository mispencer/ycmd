# Copyright (C) 2016 ycmd contributors
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

from __future__ import unicode_literals
from __future__ import print_function
from __future__ import division
from __future__ import absolute_import
from future import standard_library
standard_library.install_aliases()
from builtins import *  # noqa

from contextlib import contextmanager
import functools
import os

from ycmd import handlers
from ycmd.tests.test_utils import ( ClearCompletionsCache, SetUpApp,
                                    StartCompleterServer, StopCompleterServer,
                                    WaitUntilCompleterServerReady )

shared_legacy_app = None
shared_roslyn_app = None
shared_app_server_state = {}
shared_app_filepaths = {}


def PathToTestFile( *args ):
  dir_of_current_script = os.path.dirname( os.path.abspath( __file__ ) )
  return os.path.join( dir_of_current_script, 'testdata', *args )


def setUpPackage():
  """Initializes the ycmd server as a WebTest application that will be shared
  by all tests using the SharedYcmd decorator in this package. Additional
  configuration that is common to these tests, like starting a semantic
  subserver, should be done here."""
  global shared_legacy_app, shared_roslyn_app, shared_app_server_state

  shared_legacy_app = SetUpApp()
  shared_app_server_state[ shared_legacy_app ] = handlers._server_state
  shared_legacy_app.post_json(
    '/ignore_extra_conf_file',
    { 'filepath': PathToTestFile( '.ycm_extra_conf.py' ) } )
  shared_app_filepaths[ shared_legacy_app ] = []

  shared_roslyn_app = SetUpApp()
  shared_app_server_state[ shared_roslyn_app ] = handlers._server_state
  shared_roslyn_app.post_json(
    '/ignore_extra_conf_file',
    { 'filepath': PathToTestFile( '.ycm_extra_conf.py' ) } )
  shared_app_filepaths[ shared_roslyn_app ] = []


def tearDownPackage():
  """Cleans up the tests using the SharedYcmd decorator in this package. It is
  executed once after running all the tests in the package."""
  global shared_app, shared_filepaths

  for app in shared_app_filepaths:
    old_server_state = handlers._server_state
    try:
      handlers._server_state = shared_app_server_state[ app ]
      for filepath in shared_app_filepaths[ app ]:
        StopCompleterServer( app, 'cs', filepath )
    finally:
      shared_app_server_state[ app ] = handlers._server_state
      handlers._server_state = old_server_state


@contextmanager
def WrapOmniSharpServer( app, filepath ):
  global shared_filepaths

  SetRoslynState( app, filepath, use_roslyn )
  if ( app not in shared_app_filepaths
       or filepath not in shared_app_filepaths[ app ] ):
    StartCompleterServer( app, 'cs', filepath )
    if app in shared_app_filepaths:
      shared_app_filepaths[ app ].append( filepath )
  WaitUntilCompleterServerReady( app, 'cs' )
# WaitUntilCompleterServerReady( app, 'cs', filepath )
  yield


def SharedYcmd( test ):
  """Defines a decorator to be attached to tests of this package. This decorator
  passes the shared ycmd application as a parameter.

  Do NOT attach it to test generators but directly to the yielded tests."""
  global shared_legacy_app, shared_roslyn_app

  ( argspec, _, _, _ ) = getargspec( test )
  try:
    argindex = argspec.index( 'use_roslyn' )
  except ValueError:
    argindex = -1

  @functools.wraps( test )
  def Wrapper( *args, **kwargs ):
    global shared_legacy_app, shared_roslyn_app, shared_app_server_state

    ( argspec, _, _, _ ) = getargspec( test )
    try:
      argindex = argspec.index( 'use_roslyn' )
      use_roslyn = args[ argindex - 1 ]
    except:
      if 'use_roslyn' in kwargs:
        use_roslyn = kwargs[ 'use_roslyn' ]
      else:
        use_roslyn = False
    app = shared_roslyn_app if use_roslyn else shared_legacy_app

    old_server_state = handlers._server_state

    try:
      handlers._server_state = shared_app_server_state[ app ]
      ClearCompletionsCache()
      return test( app, *args, **kwargs )
    finally:
      shared_app_server_state[ app ] = handlers._server_state
      handlers._server_state = old_server_state
  return Wrapper


def IsolatedYcmd( test ):
  """Defines a decorator to be attached to tests of this package. This decorator
  passes a unique ycmd application as a parameter. It should be used on tests
  that change the server state in a irreversible way (ex: a semantic subserver
  is stopped or restarted) or expect a clean state (ex: no semantic subserver
  started, no .ycm_extra_conf.py loaded, etc).

  Do NOT attach it to test generators but directly to the yielded tests."""
  @functools.wraps( test )
  def Wrapper( *args, **kwargs ):
    old_server_state = handlers._server_state

    try:
      app = SetUpApp()
      app.post_json(
        '/ignore_extra_conf_file',
        { 'filepath': PathToTestFile( '.ycm_extra_conf.py' ) } )
      test( app, *args, **kwargs )
    finally:
      handlers._server_state = old_server_state
  return Wrapper
