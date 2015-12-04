%% @author Grant
%% @doc @todo _Elsedd description to html_processor.
%%
% https://www.gutenberg.org/ebooks/search/?query=charles+dickens&go=Go
%https://www.gutenberg.org/ebooks/19337
%http://www.gutenberg.org/cache/epub/19337/pg19337.txt



-module(html_processor).
-import(httpc,[request/1]).

%% ====================================================================
%%API functions
%% ====================================================================
%Document Directory: C:\Users\Grant\Erlang Workspace\DocumentProcessor\Documents
%download mirror: http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/
-export([process_file_numbers/1]).

% Input: list of Natural, the file numbers of the desired documents.
% Downloads, processes and writes the files to location DIR.
process_file_numbers([])->inets:stop(),
						  ok;
process_file_numbers([Head|Tail])->
	inets:start(),
	Mirror = "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/",  %% don't change this
	DIR = "C:\\Users\\Grant\\Erlang Workspace\\cpsc311project\\Documents",  %% Where you want the ORIGINAL, downloaded documents to be stored
	Name = integer_to_list(Head),
	ShortPath = string:concat(string:concat(DIR,"\\"),Name),
	FilePath = string:concat(ShortPath ,".txt"),
	FileContents = read_URL(string:concat(Mirror, construct_extension(Name, Name))),
	case  string:equal("error: no txt file",FileContents ) of
		false-> file:write_file(FilePath,FileContents),
    	paragraph:remove_preface_etc(FilePath),
		paragraph:process_file(string:concat(DIR,"\\"), Name),
		inets:stop(),
		process_file_numbers(Tail);
		true-> process_file_numbers(Tail)
		end.

read_URL(URL)->
	[BinURL|_] = re:replace(URL, ".txt", ""),
	BaseURL = binary_to_list(BinURL),
	case httpc:request(get,{string:concat(BaseURL,".txt"), []},[], []) of
		{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} ->Body1;
		_Else1 -> 
			case  httpc:request(get,{string:concat(BaseURL,"-0.txt"), []},[], []) of
				{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} -> Body2;
				_Else2 ->
					case	 httpc:request(get,{string:concat(BaseURL,"-1.txt"), []},[], []) of
						{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} -> Body3;
						_Else3 ->
							case	 httpc:request(get,{string:concat(BaseURL,"-2.txt"), []},[], []) of
								{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} -> Body4;
								_Else4 ->
									case	 httpc:request(get,{string:concat(BaseURL,"-3.txt"), []},[], []) of
										{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} -> Body5;
										_Else5 ->
											case	 httpc:request(get,{string:concat(BaseURL,"-4.txt"), []},[], []) of
												{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} -> Body6;
												_Else6 ->
													case	 httpc:request(get,{string:concat(BaseURL,"-5.txt"), []},[], []) of
														{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} -> Body7;
														_Else7 ->
															case	 httpc:request(get,{string:concat(BaseURL,"-6.txt"), []},[], []) of
																{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} -> Body8;
																	_Else8 ->
																		case	 httpc:request(get,{string:concat(BaseURL,"-7.txt"), []},[], []) of
																			{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} -> Body9;
																			_Else9 ->
																				case	 httpc:request(get,{string:concat(BaseURL,"-8.txt"), []},[], []) of
																					{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} -> Body10;
																					_Else10 ->
																						case	 httpc:request(get,{string:concat(BaseURL,"-9.txt"), []},[], []) of
																							{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} -> Body11;
																							_Else11 ->"error: no txt file" end%throw(string:concat("tried to download non-existent file:", BaseURL)) end
																				end
																		end
																end
														end
											end
									end
							end
					end
			end
	end.
											
%% ====================================================================
%% Internal functions
%% ====================================================================

%Document Directory: C:\Users\Grant\Erlang Workspace\DocumentProcessor\Documents
%download mirror: http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/

construct_extension(FileNum,[_])-> string:concat(string:concat("/0/",FileNum),".txt");
construct_extension([_],FileNum)->string:concat(string:concat(FileNum, "/"), string:concat(FileNum,".txt"));
construct_extension([Head|Tail],FileNum)->
	string:concat(string:concat([Head],"/") ,construct_extension(Tail, FileNum)).