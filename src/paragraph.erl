%% @author Grant
%% @doc @todo Add description to paragraph.


-module(paragraph).

%% ====================================================================
%% API functions
%% ====================================================================
-export([process_file_test/0,process_file/2,remove_preface_etc/1]).
-import(re,[split/3,replace/4]).
%document storage location: C:\Users\Grant\Erlang Workspace\DocumentProcessor\Documents
process_file_test()->
	{ok, File} = file:read_file("C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\src\\Practice.txt"),
	process_paragraphs(remove_rn(File),"Practice", "CHAPTER").

%FilePath is the directory that the file is in. Name is the name of the file, without extension ("practice", "testFile", etc).
process_file(FilePath,Name)->
	{ok, File} = file:read_file(string:concat(string:concat(FilePath,Name),".txt")),
	case string:str(binary_to_list(File), "\r\nCHAPTER") of
		0->Split = "\r\n\r\n\r\n[IVXL]+(.|  )";
		_Else->Split = "CHAPTER"
	end,
	process_paragraphs(remove_rn(File),Name, Split).

remove_preface_etc(FullFilePath)->
	{ok, File} = file:read_file(FullFilePath),
	FileString = binary_to_list(File),
	TrimmedFile = string:substr(FileString, get_start(FileString)),
	file:write_file(FullFilePath, TrimmedFile).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_start(File)->
%%	Starters = ["\r\n\r\nCHAPTER 1","\r\n\r\nCHAPTER ONE","\r\n\r\nCHAPTER I", "\r\n\r\nSTAVE ONE"],
	case string:str(File, "\r\n\r\nCHAPTER 1") of
		0 ->
			case string:str(File,"\r\n\r\nCHAPTER ONE") of
				0 ->
					case string:str(File, "\r\n\r\nCHAPTER I") of
						0 ->
							case string:str(File, "\r\n\r\nSTAVE ONE") of
								0->
									case string:str(File, "\r\n\r\nI.") of
										0->
											1;
										N->
											N end;
								N->
									N end;
						N->
							N end;
				N->
					N end;
		N->
			N end.

process_paragraphs(Document,Name,Split)->
	Paragraphs = split(Document, Split,[{return,list}]),
	write_files(lists:map(fun remove_rn/1, Paragraphs),string:concat(Name,"_"),"0").

%DIR is the location that you want to write the individual paragraph files to.
write_files([],_,_)->ok;
write_files([ Head |Paragraphs], Name, Num)->
	Length = length(Head),
	if
		Length > 50 ->
			DIR = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Documents\\",  %%Where you want the PARSED documents to be stored
			file:write_file(string:concat(string:concat(DIR, string:concat(Name, Num)),".txt"), Head),
			{N,_} = string:to_integer(Num),
			write_files(Paragraphs, Name, integer_to_list(N+1));
		true->
			write_files(Paragraphs, Name, Num)
			end.

remove_rn(Document)->
	 replace(Document,"\r\n"," ",[{return,list}]).