%% @author Grant
%% @doc @todo Add description to rdf_processor.


-module(rdf_processor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([find_author_works/1]).
%% requires that you have the gutenburg rdf catalog (http://www.gutenberg.org/wiki/Gutenberg:Feeds) unpacked, such that DIR points to rdf-files.tar\cache\epub

% Input: String AuthorName, in format ""Last Name, FIrst name" ex. "Dickens, Charles"
% Output: list of Natural, each element in the list corresponds to a file number of one of the author's works.
find_author_works(AuthorName)-> find_author(AuthorName,1,[]).

find_author(_,50543, Works)-> Works;
find_author(AuthorName, 50283, Works)-> find_author(AuthorName, 50284, Works);
find_author(AuthorName, 50465, Works)-> find_author(AuthorName, 50466, Works);
find_author(AuthorName, 50541, Works)-> find_author(AuthorName, 50542, Works);
find_author(AuthorName, Num,Works)->
	DIR = "C:\\Users\\Grant\\Desktop\\Gutenburg RDF Files\\rdf-files.tar\\cache\\epub",    %% the location of the RDF files from Project Gutenburg
	FileAuthor = file_read_author(make_path_local(DIR,Num)),
	if
		FileAuthor =:= AuthorName->
			find_author(AuthorName, Num+1,[ Num|Works]);
		true->
			find_author(AuthorName, Num+1, Works)
			end.
%% ====================================================================
%% Internal functions
%% ====================================================================

%RDF files location: C:\Users\Grant\Desktop\Gutenburg RDF Files\rdf-files.tar\cache\epub
%RDF minimum file num: 1
%RDF maximum file num:50543
%file number 50283,50465,50541 don't exist

file_read_author(FilePath)->
	case file:read_file(FilePath) of
		{ok, File}  -> {ok,File};
		{error, enoent} -> File = FilePath
	end,
	FileString = binary_to_list(File),
	case		string:str(FileString,"<pgterms:name>")=:= 0 of
		true->
			"Error: No Author Found";
		false->
			string:sub_string(FileString, (string:str(FileString,"<pgterms:name>")+14),(string:str(FileString, "</pgterms:name>")-1))
	end.

make_path_local(DIR, Num)->
	string:concat(string:concat(string:concat(string:concat(string:concat(DIR,"\\"),integer_to_list(Num)),"\\pg"),integer_to_list(Num)),".rdf").