%% @author Grant
%% @doc @todo Add description to main.


-module(main).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run_network/0, test_network_on_file/2,contains_/1,all_stats/2, generate_list_outputs/2]).

%% NONE OF THIS WILL WORK IF YOU DO NOT HAVE THE PROPER DIRECTORY STRUCTURE
%% TO MAKE IT RUN YOU WILL NEED TO ENTER PROPER DIRECTORY STRUCTURE MANUALLY IN main.erl, html_processor.erl, rdf_processor.erl, and paragraph.erl, AND FOLLOW ANY ADDITIONAL INSTRUCTIONS LISTED IN THOSE FILES.

test_network_on_file(Network, FilePath)->
	File = file:read_file(FilePath),
	Text = binary_to_list(File),
	Stats = stats:chapter_stats(Text),
	ann:feed_forward(Network, Stats).
generate_works_dickens(DIR1)->
	Works1 = rdf_processor:find_author_works("Dickens, Charles"),
	DIR1 = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Dickens",
	html_processor:process_file_numbers(Works1,DIR1 ).
generate_works_tolstoy(DIR2)->
	Works2 = rdf_processor:find_author_works("Tolstoy, Leo, graf"),
	DIR2 = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Tolstoy",
	html_processor:process_file_numbers(Works2, DIR2).

run_network()->
	DIR1 = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Dickens",
	DIR2 = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Tolstoy",
	%%generate_works_dickens(),
	%%generate_works_tolstoy(),
	io:write("file processing complete, beginning stat calculations"),
	{ok, Files1} = file:list_dir(DIR1),
	Chapters1 = lists:filter(fun contains_/1, Files1),
	{ok, Files2} = file:list_dir(DIR2),
	Chapters2 = lists:filter(fun contains_/1, Files2),
	Stats1 = all_stats(Chapters1,DIR1),
	Stats2 = all_stats(Chapters2,DIR2),
	io:write("stat calculation complete, beginning ann calculations"),
	{Network, TrainingError} = ann:training_session_parallelism(lists:append(Stats1, Stats2), lists:append(generate_list_outputs(Stats1, [1,0]), generate_list_outputs(Stats2, [0,1])), 2).

contains_(String)->	case string:chr(String, 95) of
						0-> false;
						_->true
					end.

%%creates the output list for a list of stats, given Output is [isAuthor1, isAuthor2...]
generate_list_outputs([],_)->[];
generate_list_outputs([_|Xs], Output)->
	[Output|generate_list_outputs(Xs,Output)].

all_stats([],_)->[];
all_stats([Work|Works],DIR)->
	{ok,File} = file:read_file(string:concat(string:concat(DIR,"\\"),Work)),
	Text = binary_to_list(File),
	%%io:write(string:concat("read ",Work)),
	try stats:chapter_stats(Text) of
		S -> Stats = tuple_to_list(S),
			 [Stats|all_stats(Works,DIR)]
		catch 
			Exception:Reason	->	all_stats(Works,DIR)
	end.
	

%% ====================================================================
%% Internal functions
%% ====================================================================


