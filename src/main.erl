%% @author Grant
%% @doc @todo Add description to main.


-module(main).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run_network/0, test_network_on_file/2]).

test_network_on_file(Network, FilePath)->
	File = file:read_file(FilePath),
	Text = binary_to_list(File),
	Stats = stats:chapter_stats(Text),
	ann:feed_forward(Network, Stats).

run_network()->
	Works1 = rdf_processor:find_author_works("Dickens, Charles"),
	DIR1 = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Dickens",
	html_processor:process_file_numbers(Works1,DIR1 ),
	Works2 = rdf_processor:find_author_works("Tolstoy, Leo, graf"),
	DIR2 = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Tolstoy",
	html_processor:process_file_numbers(Works2, DIR2),
	io:write("file processing complete, beginning stat calculations"),
	{ok, Files1} = file:list_dir(DIR1),
	Chapters1 = lists:filter(contains_, Files1),
	{ok, Files2} = file:list_dir(DIR2),
	Chapters2 = lists:filter(contains_, Files2),
	Stats1 = all_stats(Chapters1),
	Stats2 = all_stats(Chapters2),
	io:write("stat calculation complete, beginning ann calculations"),
	{Network, TrainingError} = ann:training_session_parallelism(lists:append(Stats1, Stats2), lists:append(generate_list_outputs(Stats1, [1,0]), generate_list_outputs(Stats2, [0,1])), 2).

contains_(String)->	lists:member("_", String).

%%creates the output list for a list of stats, given Output is [isAuthor1, isAuthor2...]
generate_list_outputs([],_)->[];
generate_list_outputs([_|Xs], Output)->
	[Output|generate_list_outputs(Xs,Output)].

all_stats([])->[];
all_stats([Work|Works])->
	[stats:chapter_stats(binary_to_list(file:read_file(Work)))|all_stats(Works)].


%% ====================================================================
%% Internal functions
%% ====================================================================


