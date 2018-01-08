# coding: utf-8
#
# Copyright (C) 2015 ycmd contributors
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

from hamcrest import ( assert_that, calling, empty, greater_than, has_item,
                       has_items, has_entries, raises )
from nose import SkipTest
from nose.tools import eq_
from webtest import AppError

from ycmd.tests.cs import PathToTestFile, SharedYcmd, WrapOmniSharpServer
from ycmd.tests.test_utils import ( BuildRequest, CompletionEntryMatcher )
from ycmd.utils import ReadFile


def GetCompletions_Basic_test():
  yield _GetCompletions_Basic_test, True
  yield _GetCompletions_Basic_test, False


@SharedYcmd
def _GetCompletions_Basic_test( app, use_roslyn ):
  filepath = PathToTestFile( 'testy', 'Program.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 10,
                                    column_num = 12 )
    response_data = app.post_json( '/completions', completion_data ).json
    assert_that( response_data[ 'completions' ],
                 has_items( CompletionEntryMatcher( 'CursorLeft' ),
                            CompletionEntryMatcher( 'CursorSize' ) ) )
    eq_( 12, response_data[ 'completion_start_column' ] )


def GetCompletions_Unicode_test():
  yield _GetCompletions_Unicode_test, True
  yield _GetCompletions_Unicode_test, False


@SharedYcmd
def _GetCompletions_Unicode_test( app, use_roslyn ):
  raise SkipTest( "Unicode is not working (yet) for both" )
  filepath = PathToTestFile( 'testy', 'Unicode.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 43,
                                    column_num = 26 )
    response_data = app.post_json( '/completions', completion_data ).json
    method_completion = 'DoATest' if use_roslyn else 'DoATest()'
    assert_that( response_data[ 'completions' ],
                 has_items(
                   CompletionEntryMatcher( method_completion ),
                   CompletionEntryMatcher( 'an_int' ),
                   CompletionEntryMatcher( 'a_unicøde' ),
                   CompletionEntryMatcher( 'øøø' ) ) )

    eq_( 26, response_data[ 'completion_start_column' ] )


def GetCompletions_MultipleSolution_test():
  yield _GetCompletions_MultipleSolution_test, True
  yield _GetCompletions_MultipleSolution_test, False


@SharedYcmd
def _GetCompletions_MultipleSolution_test( app, use_roslyn ):
  filepaths = [ PathToTestFile( 'testy', 'Program.cs' ),
                PathToTestFile( 'testy-multiple-solutions',
                                'solution-named-like-folder',
                                'testy',
                                'Program.cs' ) ]
  for filepath in filepaths:
    with WrapOmniSharpServer( app, filepath, use_roslyn ):
      contents = ReadFile( filepath )

      completion_data = BuildRequest( filepath = filepath,
                                      filetype = 'cs',
                                      contents = contents,
                                      line_num = 10,
                                      column_num = 12 )
      response_data = app.post_json( '/completions',
                                     completion_data ).json
      assert_that( response_data[ 'completions' ],
                   has_items( CompletionEntryMatcher( 'CursorLeft' ),
                              CompletionEntryMatcher( 'CursorSize' ) ) )
      eq_( 12, response_data[ 'completion_start_column' ] )


def GetCompletions_PathWithSpace_test():
  yield _GetCompletions_PathWithSpace_test, True
  yield _GetCompletions_PathWithSpace_test, False


@SharedYcmd
def _GetCompletions_PathWithSpace_test( app, use_roslyn ):
  filepath = PathToTestFile( u'неприличное слово', 'Program.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 9,
                                    column_num = 12 )
    response_data = app.post_json( '/completions', completion_data ).json
    assert_that( response_data[ 'completions' ],
                 has_items( CompletionEntryMatcher( 'CursorLeft' ),
                            CompletionEntryMatcher( 'CursorSize' ) ) )
    eq_( 12, response_data[ 'completion_start_column' ] )


def GetCompletions_HasBothImportsAndNonImport_test():
  # Roslyn doesn't have this feature
  # yield _GetCompletions_HasBothImportsAndNonImport_test, True
  yield _GetCompletions_HasBothImportsAndNonImport_test, False


@SharedYcmd
def _GetCompletions_HasBothImportsAndNonImport_test( app, use_roslyn ):
  filepath = PathToTestFile( 'testy', 'ImportTest.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 9,
                                    column_num = 12,
                                    force_semantic = True,
                                    query = 'Date' )
    response_data = app.post_json( '/completions', completion_data ).json

    assert_that(
      response_data[ 'completions' ],
      has_items( CompletionEntryMatcher( 'DateTime' ),
                 CompletionEntryMatcher( 'DateTimeStyles' ) )
    )


def GetCompletions_ImportsOrderedAfter_test():
  # Roslyn doesn't have this feature
  # yield _GetCompletions_ImportsOrderedAfter_test, True
  yield _GetCompletions_ImportsOrderedAfter_test, False


@SharedYcmd
def _GetCompletions_ImportsOrderedAfter_test( app, use_roslyn ):
  filepath = PathToTestFile( 'testy', 'ImportTest.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 9,
                                    column_num = 12,
                                    force_semantic = True,
                                    query = 'Date' )
    response_data = app.post_json( '/completions', completion_data ).json

    min_import_index = min(
      loc for loc, val
      in enumerate( response_data[ 'completions' ] )
      if val[ 'extra_data' ][ 'required_namespace_import' ]
    )

    max_nonimport_index = max(
      loc for loc, val
      in enumerate( response_data[ 'completions' ] )
      if not val[ 'extra_data' ][ 'required_namespace_import' ]
    )

    assert_that( min_import_index, greater_than( max_nonimport_index ) ),


def GetCompletions_ForcedReturnsResults_test():
  # Roslyn doesn't have this feature
  # yield _GetCompletions_ForcedReturnsResults_test, True
  yield _GetCompletions_ForcedReturnsResults_test, False


@SharedYcmd
def _GetCompletions_ForcedReturnsResults_test( app, use_roslyn ):
  filepath = PathToTestFile( 'testy', 'ContinuousTest.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 9,
                                    column_num = 21,
                                    force_semantic = True,
                                    query = 'Date' )
    response_data = app.post_json( '/completions', completion_data ).json

    assert_that( response_data[ 'completions' ],
                 has_items( CompletionEntryMatcher( 'String' ),
                            CompletionEntryMatcher( 'StringBuilder' ) ) )


def GetCompletions_NonForcedReturnsNoResults_test():
  # Roslyn doesn't have this feature
  # yield _GetCompletions_NonForcedReturnsNoResults_test, True
  yield _GetCompletions_NonForcedReturnsNoResults_test, False


@SharedYcmd
def _GetCompletions_NonForcedReturnsNoResults_test( app, use_roslyn ):
  filepath = PathToTestFile( 'testy', 'ContinuousTest.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )
    event_data = BuildRequest( filepath = filepath,
                               filetype = 'cs',
                               contents = contents,
                               event_name = 'FileReadyToParse' )

    app.post_json( '/event_notification', event_data )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 9,
                                    column_num = 21,
                                    force_semantic = False,
                                    query = 'Date' )
    results = app.post_json( '/completions', completion_data ).json

    # There are no semantic completions. However, we fall back to identifier
    # completer in this case.
    assert_that( results, has_entries( {
      'completions': has_item( has_entries( {
        'insertion_text' : 'String',
        'extra_menu_info': '[ID]',
      } ) ),
      'errors': empty(),
    } ) )


def GetCompletions_ForcedDividesCache_test():
  # Roslyn doesn't have this feature
  # yield _GetCompletions_ForcedDividesCache_test, True
  yield _GetCompletions_ForcedDividesCache_test, False


@SharedYcmd
def _GetCompletions_ForcedDividesCache_test( app, use_roslyn ):
  filepath = PathToTestFile( 'testy', 'ContinuousTest.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    contents = ReadFile( filepath )
    event_data = BuildRequest( filepath = filepath,
                               filetype = 'cs',
                               contents = contents,
                               event_name = 'FileReadyToParse' )

    app.post_json( '/event_notification', event_data )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 9,
                                    column_num = 21,
                                    force_semantic = True,
                                    query = 'Date' )
    results = app.post_json( '/completions', completion_data ).json

    assert_that( results[ 'completions' ], not( empty() ) )
    assert_that( results[ 'errors' ], empty() )

    completion_data = BuildRequest( filepath = filepath,
                                    filetype = 'cs',
                                    contents = contents,
                                    line_num = 9,
                                    column_num = 21,
                                    force_semantic = False,
                                    query = 'Date' )
    results = app.post_json( '/completions', completion_data ).json

    # There are no semantic completions. However, we fall back to identifier
    # completer in this case.
    assert_that( results, has_entries( {
      'completions': has_item( has_entries( {
        'insertion_text' : 'String',
        'extra_menu_info': '[ID]',
      } ) ),
      'errors': empty(),
    } ) )


def GetCompletions_ReloadSolution_Basic_test():
  # Roslyn doesn't have this endpoint (yet)
  # yield _GetCompletions_ReloadSolution_Basic_test, True
  yield _GetCompletions_ReloadSolution_Basic_test, False


@SharedYcmd
def _GetCompletions_ReloadSolution_Basic_test( app, use_roslyn ):
  filepath = PathToTestFile( 'testy', 'Program.cs' )
  with WrapOmniSharpServer( app, filepath, use_roslyn ):
    result = app.post_json(
      '/run_completer_command',
      BuildRequest( completer_target = 'filetype_default',
                    command_arguments = [ 'ReloadSolution' ],
                    filepath = filepath,
                    filetype = 'cs' ) ).json

    eq_( result, True )


def GetCompletions_ReloadSolution_MultipleSolution_test():
  # Roslyn doesn't have this endpoint (yet)
  # yield _GetCompletions_ReloadSolution_MultipleSolution_test, True
  yield _GetCompletions_ReloadSolution_MultipleSolution_test, False


@SharedYcmd
def _GetCompletions_ReloadSolution_MultipleSolution_test( app, use_roslyn ):
  filepaths = [ PathToTestFile( 'testy', 'Program.cs' ),
                PathToTestFile( 'testy-multiple-solutions',
                                'solution-named-like-folder',
                                'testy',
                                'Program.cs' ) ]
  for filepath in filepaths:
    with WrapOmniSharpServer( app, filepath, use_roslyn ):
      result = app.post_json(
        '/run_completer_command',
        BuildRequest( completer_target = 'filetype_default',
                      command_arguments = [ 'ReloadSolution' ],
                      filepath = filepath,
                      filetype = 'cs' ) ).json

      eq_( result, True )


def SolutionSelectCheck( app, sourcefile, reference_solution,
                         extra_conf_store = None ):
  # reusable test: verify that the correct solution (reference_solution) is
  #   detected for a given source file (and optionally a given extra_conf)
  if extra_conf_store:
    app.post_json( '/load_extra_conf_file',
                   { 'filepath': extra_conf_store } )

  result = app.post_json( '/run_completer_command',
                          BuildRequest( completer_target = 'filetype_default',
                                        command_arguments = [ 'SolutionFile' ],
                                        filepath = sourcefile,
                                        filetype = 'cs' ) ).json

  # Now that cleanup is done, verify solution file
  eq_( reference_solution, result)


def GetCompletions_UsesSubfolderHint_test():
  yield _GetCompletions_UsesSubfolderHint_test, True
  yield _GetCompletions_UsesSubfolderHint_test, False


@SharedYcmd
def _GetCompletions_UsesSubfolderHint_test( app, use_roslyn ):
  SolutionSelectCheck( app,
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-named-like-folder',
                                       'testy', 'Program.cs' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-named-like-folder',
                                       'testy.sln' ) )


def GetCompletions_UsesSuperfolderHint_test():
  yield _GetCompletions_UsesSuperfolderHint_test, True
  yield _GetCompletions_UsesSuperfolderHint_test, False


@SharedYcmd
def _GetCompletions_UsesSuperfolderHint_test( app, use_roslyn ):
  SolutionSelectCheck( app,
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-named-like-folder',
                                       'not-testy', 'Program.cs' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-named-like-folder',
                                       'solution-named-like-folder.sln' ) )


def GetCompletions_ExtraConfStoreAbsolute_test():
  yield _GetCompletions_ExtraConfStoreAbsolute_test, True
  yield _GetCompletions_ExtraConfStoreAbsolute_test, False


@SharedYcmd
def _GetCompletions_ExtraConfStoreAbsolute_test( app, use_roslyn ):
  SolutionSelectCheck( app,
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-abs',
                                       'testy', 'Program.cs' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'testy2.sln' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-abs',
                                       '.ycm_extra_conf.py' ) )


def GetCompletions_ExtraConfStoreRelative_test():
  yield _GetCompletions_ExtraConfStoreRelative_test, True
  yield _GetCompletions_ExtraConfStoreRelative_test, False


@SharedYcmd
def _GetCompletions_ExtraConfStoreRelative_test( app, use_roslyn ):
  SolutionSelectCheck( app,
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-rel',
                                       'testy', 'Program.cs' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-rel',
                                       'testy2.sln' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-rel',
                                       '.ycm_extra_conf.py' ) )


def GetCompletions_ExtraConfStoreNonexisting_test():
  yield _GetCompletions_ExtraConfStoreNonexisting_test, True
  yield _GetCompletions_ExtraConfStoreNonexisting_test, False


@SharedYcmd
def _GetCompletions_ExtraConfStoreNonexisting_test( app, use_roslyn ):
  SolutionSelectCheck( app,
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-bad',
                                       'testy', 'Program.cs' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-bad',
                                       'testy2.sln' ),
                       PathToTestFile( 'testy-multiple-solutions',
                                       'solution-not-named-like-folder',
                                       'extra-conf-bad',
                                       'testy', '.ycm_extra_conf.py' ) )


def GetCompletions_DoesntStartWithAmbiguousMultipleSolutions_test():
  yield _GetCompletions_DoesntStartWithAmbiguousMultipleSolutions_test, True
  yield _GetCompletions_DoesntStartWithAmbiguousMultipleSolutions_test, False


@SharedYcmd
def _GetCompletions_DoesntStartWithAmbiguousMultipleSolutions_test( app,
                                                                use_roslyn ):
  filepath = PathToTestFile( 'testy-multiple-solutions',
                             'solution-not-named-like-folder',
                             'testy', 'Program.cs' )
  contents = ReadFile( filepath )
  event_data = BuildRequest( filepath = filepath,
                             filetype = 'cs',
                             contents = contents,
                             event_name = 'FileReadyToParse' )

  assert_that(
    calling( app.post_json ).with_args( '/event_notification', event_data ),
    raises( AppError, 'Autodetection of solution file failed' ),
    "The Omnisharp server started, despite us not being able to find a "
    "suitable solution file to feed it. Did you fiddle with the solution "
    "finding code in cs_completer.py? Hopefully you've enhanced it: you need "
    "to update this test then :)" )
