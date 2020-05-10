
:- use_module(progress_bar).

demo_progress_bar :- test_progress_bar(simple,100).
demo_progress_bar :- test_progress_bar(default,100).
demo_progress_bar :- test_progress_bar(fancy,100).

demo_spinner :- test_spinner(simple,100).
demo_spinner :- test_spinner(default,100).
demo_spinner :- test_spinner(fancy,100).

test_progress_bar(Label,Total) :-
    Sleep is 5 / Total,
    get_time(TS),
    forall(
        (between(1,Total,Index),sleep(Sleep)),
        print_message(informational,pb(demo_progress_bar(Label,Index/Total,TS)))).

test_spinner(Label,Total) :-
    Sleep is 5 / Total,
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