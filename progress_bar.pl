:- module(progress_bar, [
	simple_spinner//1,
	default_spinner//2,
	fancy_spinner//3,
	spinner//10,
	spinner_end//0,
	simple_progress_bar//2,
	default_progress_bar//4,	% default_progress_bar(+Index:int, +Total:int, IntroText:text,OutroText:text)
	fancy_progress_bar//7,
	progress_bar//12
	]).

/** <module> progress_bar - DCG rules that render a progress bar, or spinner to indicate progress of a specific predicate

 	@todo 

 	Example

 		... 
 		length(Items,Total),
 		forall(nth1(Index,Items,Item), (
 			print_message(informational,my_application_does_stuff(Index,Total)
 			%  do something with Item
			),
		... 

 	my_application_does_stuff(Index,Total) -->
 		simple_progress_bar(Index,Total).
		

*/

:- use_module(library(dcg/basics)).
:- use_module(library(error)).

:- use_module(library(debug)).
:- debug(progress_bar).

:- multifile prolog:message//1.


spinner(none,['']).
spinner(classic,['|','/','—','\\']).
spinner(mini,['x','+']).
spinner(dots1,['.  ','.. ','...',' ..','  .']).
spinner(dots2,['.  ','.. ','...']).
spinner(bar,['▁','▃','▄','▅','▆','▇','█','▇','▆','▅','▄','▃']).
spinner(dqpb,['d', 'q', 'p', 'b']).

test_progress_bar(Label,Total) :-
    Sleep is 10 / Total,
    get_time(TS),
    forall(
        (between(1,Total,Index),sleep(Sleep)),
        print_message(informational,pb(demo_progress_bar(Label,Index/Total,TS)))).

test_spinner(Label,Total) :-
    Sleep is 10 / Total,
    get_time(TS),
    forall(
        (between(1,Total,Progress),sleep(Sleep)),
        print_message(informational,pb(demo_spinner(Label,Progress,TS)))).

prolog:message(pb(Msg)) --> Msg.

demo_progress_bar(simple,Progress,_) -->
	{
		Progress = Index/Total
	},
	simple_progress_bar(Index,Total).

demo_progress_bar(default,Progress,TS0) -->
	{
		Progress = Index/Total,
		IntroText = 'default demo ',
		get_time(TS),RunningTime is TS - TS0, format(string(OutroText)," ~2f seconds",[RunningTime])
	},
	default_progress_bar(Index,Total,IntroText,OutroText).

demo_progress_bar(fancy,Progress,TS0) -->
	{
		Progress = Index/Total,
		IntroText = 'fancy demo ',
		Items = [administration, modernize ,excitement ,valid ,choke ,pigeon ,ministry ,atmosphere ,nominate ,hide ,few ,goalkeeper ,plan ,seminar ,blue ,coma ,incredible ,surprise ,important ,cheat],
		length(Items,N), 
		I is Index mod N, nth0(I,Items,Item),(Index < Total -> format(string(StartText),"Processing: \"~w\" ",[Item]) ; StartText = ""),	
		J is (Index + 1) mod N, nth0(J,Items,Next),(Index < Total ->  format(string(EndText)," Up next: \"~w\" ",[Next]) ; EndText = ""),
		get_time(TS),RunningTime is TS - TS0, format(string(OutroText)," ~2f seconds",[RunningTime]),
		Percentage is (Index/Total) * 100, format(string(TodoText)," ~0f%",[Percentage])
	},
	fancy_progress_bar(Index,Total,IntroText,OutroText,StartText,TodoText,EndText).

demo_spinner(simple,Progress,_) --> simple_spinner(Progress).

demo_spinner(default,Progress,_) --> default_spinner(Progress,"Busy doing great things ").

demo_spinner(fancy,Progress,TS0) -->
	{
		Items = [administration, modernize ,excitement ,valid ,choke ,pigeon ,ministry ,atmosphere ,nominate ,hide ,few ,goalkeeper ,plan ,seminar ,blue ,coma ,incredible ,surprise ,important ,cheat],
		random_member(Item,Items),
		get_time(TS),RunningTime is TS - TS0, format(string(TextRight),"[~2f seconds]",[RunningTime]),
		format(string(TextLeft)," Processing '~w'",[Item])
	},
	fancy_spinner(Progress,TextLeft,TextRight).

%!	simple_spinner(+Progress:int) is det.
%	renders a spinner message on the center of the screen-line
simple_spinner(Progress) --> 
	{
		SpinLeftLeft = none,
		TextLeft = "",
		SpinLeftRight = none,
		SpinCenterLeft = dots1,
		TextCenter = " Processing ",
		SpinCenterRight = dots1,
		SpinRightLeft = none,
		TextRight = "",
		SpinRightRight = none
	},
	spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight).

%!	default_spinner(+Progress:int,TextLeft:text) is det.
%	renders a text message followed by a spinner 
default_spinner(Progress,TextLeft) --> 
	{
		SpinLeftLeft = none,
		SpinLeftRight = classic,
		SpinCenterLeft = none,
		TextCenter = "",
		SpinCenterRight = none,
		SpinRightLeft = none,
		TextRight = "",
		SpinRightRight = none
	},
	spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight).

%!	fancy_spinner(+Progress:int, +TextLeft:text, +TextRight:text) is det.
%	fancy_spinner renders a (dynamic) message on the left of the screen (including a spinner) and rights-aligned message
fancy_spinner(Progress,TextLeft,TextRight) --> 
	{
		SpinLeftLeft = bar,
		SpinLeftRight = none,
		SpinCenterLeft = none,
		TextCenter = "",
		SpinCenterRight = none,
		SpinRightLeft = none,
		SpinRightRight = none
	},
	spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight).

%!	spinner(+Progress:int,
%!		+SpinLeftLeft:atom,+TextLeft:text,SpinLeftRight:atom,
%!		+SpinCenterLeft:atom,+TextCenter:text,+SpinCenterRight:atom,
%!		+SpinRightLeft:atom,+TextRight:text,+SpinRightRight:atom)
%
%	Generates a spinner message that is meant to be called repeatedly by a task to indicate its progress 
%	(while its completion cannot be determined ahead of timer, otherwise a progess bar would be more appropriate)
%
%	%	The layout of the spinner message is setup according to the following schema:	
%	SLL TL SLR 			SCL TC SCR 			SRL TR SRR 		
% 
%	SLL stands for Spinner-Left-Left, TL stands for Text-Left, SLR stands for Spinner-Left-Right
%	The other abreviation follow the same schema
%
%	Progress is an possitive integer that represents advancement of a task (it is assumed to grow 1 with every call)
%	TL is a left-aligned text,
%	TC is a center-aligned text,
% 	TR is a right-aligned text
%	SLL, SLR, SCL,SCR,SRL and SRR are atoms represent a spinner Id (e.g. 'classic', 'dots', .. ). 
%	Noting 'none' denotes absence of a spinner
%
%	As the termination of a spinner us unnknown a newline should be emmited when done, this can be done using end_spinner.
spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight) --> 
	{
		findall(Id,spinner(Id,_),Ids),
		must_be(nonneg, Progress),
		must_be(oneof(Ids),SpinLeftLeft),
		must_be(any,TextLeft),
		must_be(oneof(Ids),SpinLeftRight),
		must_be(oneof(Ids),SpinCenterLeft),
		must_be(any,TextCenter),
		must_be(oneof(Ids),SpinCenterRight),
		must_be(oneof(Ids),SpinRightLeft),
		must_be(any,TextRight),
		must_be(oneof(Ids),SpinRightRight)
	},
	remove_line_content, 
	prefix_line, 
	full_width_spinner(Progress,SpinLeftLeft,TextLeft,SpinLeftRight,SpinCenterLeft,TextCenter,SpinCenterRight,SpinRightLeft,TextRight,SpinRightRight),
	[flush].

spinner_end --> [nl].

full_width_spinner(Progress,SLL,TL,SLR,SCL,TC,SCR,SRL,TR,SRR) -->
   {
    	tty_size(_,W0),
    	Position = 0,
        Width is W0 - 3 % '% ' prefix + cursor
   },
   render_spinner(Progress,Position,Width,SLL,TL,SLR,SCL,TC,SCR,SRL,TR,SRR).

% PosL				   PosC 			PosR
% SLL TL SLR 		SCL TC SCR 	  SRL TR SRR
render_spinner(Progress,Position,Width,SLL,TL,SLR,SCL,TC,SCR,SRL,TR,SRR) -->
	{
		must_be(nonneg,Position),
		must_be(nonneg,Width),
		PosL = Position,	
		spinner(SCL,[Frame_SCL|_]),atom_length(Frame_SCL,Len_SCL),
		string_length(TC,Len_TC),
		spinner(SCR,[Frame_SCR|_]),
		spinner(SCR,[Frame_SCR|_]),atom_length(Frame_SCR,Len_SCR),
		PosC is PosL + round(Width/2) - round((Len_SCL + Len_TC + Len_SCR) / 2) - 1,
		spinner(SRL,[Frame_SRL|_]),atom_length(Frame_SRL,Len_SRL),
		string_length(TR,Len_TR),
		spinner(SRR,[Frame_SRR|_]),atom_length(Frame_SRR,Len_SRR),
		PosR is Position + Width - Len_SRL - Len_TR - Len_SRR
	},  
	do_render_spinner(Progress,PosL,SLL,TL,SLR,PosC,SCL,TC,SCR,PosR,SRL,TR,SRR).

do_render_spinner(Progress,PosL,SLL,TL,SLR,PosC,SCL,TC,SCR,PosR,SRL,TR,SRR) --> 
	{
		spinner(SLL,Frames_SLL),length(Frames_SLL,N_SLL), I_SLL is Progress mod N_SLL, nth0(I_SLL,Frames_SLL,Frame_SLL),
		spinner(SLR,Frames_SLR),length(Frames_SLR,N_SLR), I_SLR is Progress mod N_SLR, nth0(I_SLR,Frames_SLR,Frame_SLR),
		spinner(SCL,Frames_SCL),length(Frames_SCL,N_SCL), I_SCL is Progress mod N_SCL, nth0(I_SCL,Frames_SCL,Frame_SCL),
		spinner(SCR,Frames_SCR),length(Frames_SCR,N_SCR), I_SCR is Progress mod N_SCR, nth0(I_SCR,Frames_SCR,Frame_SCR),
		spinner(SRL,Frames_SRL),length(Frames_SRL,N_SRL), I_SRL is Progress mod N_SRL, nth0(I_SRL,Frames_SRL,Frame_SRL),
		spinner(SRR,Frames_SRR),length(Frames_SRR,N_SRR), I_SRR is Progress mod N_SRR, nth0(I_SRR,Frames_SRR,Frame_SRR),

		MetaTabSpec = "~~~w|~~w~~w~~w~~~w|~~w~~w~~w~~~w|~~w~~w~~w",
		format(atom(TabSpec),MetaTabSpec,[PosL,PosC,PosR])
	},
	[TabSpec-[Frame_SLL,TL,Frame_SLR,Frame_SCL,TC,Frame_SCR,Frame_SRL,TR,Frame_SRR]].


%! simple_progress_bar(+Index:int, +Total:int)
%	simple_progress_bar renders a progress bar, and the percentage completed
simple_progress_bar(Index,Total) --> 
	{
		IntroText = '',
		Percentage is (Index/Total) * 100, format(string(OutroText),"[~0f%]",[Percentage]),
		StartMarker = '[',
		StartText = '',
		DoneChar = '*',
		DoneText = '',
		TodoText = '',
		TodoChar = ' ',
		EndText = '',
		EndMarker = ']'
	},
	progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker).

%! default_progress_bar(+Index:int, +Total:int, IntroText:text,OutroText:text)
%	default_progress_bar renders a progress bar, including an IntroText, OutroText (that may be dynamically updated)
default_progress_bar(Index,Total,IntroText,OutroText) --> 
	{
		StartMarker = '[',
		StartText = '',
		DoneChar = '=',
		DoneText = '>',
		TodoText = '',
		TodoChar = ' ',
		EndText = '',
		EndMarker = ']'
	},
	progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker).

%! fancy_progress_bar(+Index:int, +Total:int, IntroText:text,OutroText:text,StartText,TodoText:text)
%	default_progress_bar renders a progress bar, including an IntroText, OutroText, StartText and TodoText (that may be dynamically updated)
fancy_progress_bar(Index,Total,IntroText,OutroText,StartText,TodoText,EndText) -->
	{
		StartMarker = '\u2503',		% ┃
		DoneChar = '\u25A0',		% ■
		DoneText = '\u25BA',		% ►
		TodoChar = '\u25A1',		% □
		EndMarker = '\u2503'		% ┃
	},
	progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker).

%! progress_bar(+Index:int,+Total:int,+IntroText:text,+OutroText:text,
%!		+StartMarker:char,+StartText:text,+DoneChar:char,+DoneText:text,
%!		+TodoText:text,+TodoChar:char,+EndText:text,+EndMarker:char) is det. 
%	progress_bar renders the progress of a process using the full width of the terminal
%	where Index represent the current advancement, and Total represent completion. (i.e. Index =< Total) 
%
%	The layout of the bar is setup according to the following schema:	
%	Intro [Start+++++++++++++++><---------End] Outro
%
%	IntroText ("Intro" in the schema) represents the text that is printed before the bar
%	StartMarker ("[") defined the character used to render the left boundary of the bar
%	StartText ("Start") is a text printed at the right side of the StartMarker, provided there is sufficient space 
%	DoneChar ("+") is the charcater used to render the advancement completed
%	DoneText (">") is a text that is printed (provided there is space) at the left side ('i.e done') of the current advancement
%	TodoText ("<") is a text that is printed (provided there is space) at the right side (i.e. todo') of the current advancement
%	TodoChar ("-") is the character used to render the advacement that remains to be made 
%	EndText ("End") is a text printed at the left side of the EndMarker, provided there is sufficient space
%	EndMarker ("]") defined the character used to render the right boundary of the bar
%	OutroText ("Outro") represents the text that is printed after the bar	
progress_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker) --> 
	{
		must_be(nonneg,Index),
		must_be(nonneg,Total),
        must_be(any,IntroText),
        must_be(any,OutroText),
		must_be(char,StartMarker),
        must_be(any,StartText),
        must_be(char,DoneChar),
        must_be(any,DoneText),
        must_be(any,TodoText),
        must_be(char,TodoChar),
        must_be(any,EndText),
        must_be(char,EndMarker)
	},
	remove_line_content,
	prefix_line,
	full_width_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker),
	finished(Index,Total).

remove_line_content --> [at_same_line,'\r'].
prefix_line --> ['% '].
finished(Total,Total) --> !,[nl].
finished(_,_) --> [flush].


% Renders an instance of the progress_bar using the full width of the terminal
full_width_bar(Index,Total,IntroText,OutroText,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker) --> 
    {
    	tty_size(_,W0),
        string_length(IntroText,LStart),
        string_length(OutroText,LEnd),
        Width is W0 - LStart - LEnd - 5 % '% ' prefix + cursor + StartMarker + EndMarker
        % debug(progress_bar,"tty:~w, sm:~w, sl:~w, em:~w, el:~w",[W0,IntroText,LStart,EndStr,LEnd])
    },
    [IntroText],render_bar(0,Width,Index,Total,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker),[OutroText].

% Renders a bar of Width starting at StartPosition
render_bar(StartPosition,Width,Index,Total,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker) -->
    {
    	must_be(nonneg,StartPosition),
        must_be(nonneg,Width),
        (Index == Total -> % for rounding errors
        	(DoneWidth = Width, TodoWidth = 0) 
        	;
        	(
        		DoneWidth is floor(Index * (Width / Total)),
        		TodoWidth is Width - DoneWidth
        	)
        )
        % debug(progress_bar,"progress:~w, width:~w, DoneWidth:~w, TodoWidth:~w",[Index/Total,Width,DoneWidth,TodoWidth])
    },
    do_render_bar(StartPosition,DoneWidth,TodoWidth,StartMarker,StartText,DoneChar,DoneText,TodoText,TodoChar,EndText,EndMarker).


%! do_render_bar(
%!		+StartPosition:int,+DoneWidth:int,+ToDoWidth:int,
%!		+StartMarker,+StartText:text,+DoneChar:char,+DoneText:text,
%!		+TodoText:text,+TodoChar:char,+EndText:text, +EndMarker:char)
%
%	render_bar does the actual rendering: It generates Template for format/3, and processes the arguments
%	StartPosition represents the position to start the rendering
%	DoneWidth represents the length (in characters) of the Done area
%	TodoWidth represents the length (in characters) of the Todo area   
do_render_bar(StartPosition,DoneWidth,TodoWidth,StartMarker,StartText0,DoneChar,DoneText0,TodoText0,TodoChar,EndText0,EndMarker) -->
    {
        must_be(nonneg,DoneWidth),
        must_be(nonneg,TodoWidth),

        string_length(DoneText0,DoneTextLength),
        string_length(StartText0,StartTextLength),
        string_length(EndText0,EndTextLength),
        string_length(TodoText0,TodoTextLength),
        ((DoneTextLength >= DoneWidth ; TodoWidth == 0) -> DoneText = '' ; DoneText = DoneText0),
        (DoneTextLength + StartTextLength >= DoneWidth -> StartText = '' ; StartText = StartText0),
        (TodoTextLength >= TodoWidth -> TodoText = '' ; TodoText = TodoText0),
        (TodoTextLength + EndTextLength >= TodoWidth -> EndText = '' ; EndText = EndText0),
        MetaTabSpec = '~~~w|~w~w~~`~wt~w~~~w+~w~~`~wt~w~~~w+~w',
        format(atom(TabSpec),MetaTabSpec,[StartPosition,StartMarker,StartText,DoneChar,DoneText,DoneWidth,TodoText,TodoChar,EndText,TodoWidth,EndMarker]) 
        % Example  = '~0|[start~`+t>~100+<~`-tend~48+]',format(Example)
        % MetaTabSpec = '~~~w|~w~w~~`~wt~w~~~w+~w~~`~wt~w~~~w+~w',TabArgs = [0,'[','start','+','>',100,'<','-','end',48,']'],format(atom(Example2),MetaTabSpec,TabArgs),format(Example2).
    },
    [TabSpec].


