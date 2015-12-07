%% @author Grant
%% @doc @todo Add description to main.


-module(main).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

run_network(AuthorName)->
	Works = rdf_processor:find_author_works("Dickens, Charles"),
	html_processor:process_file_numbers(Works),
	stats:chapter_stats(Text)

all_stats([])->[];
all_stats([Work|Works])->

%% ====================================================================
%% Internal functions
%% ====================================================================


