# Progress-bar and spinner for SWIPL (progress_bar)

Progress-bar and spinner portray the progress of a user defined process for SWI-Prolog (https://www.swi-prolog.org/). Several options
are availe to adapt the visual appearance of the progress-bar (see the pldoc documentation for details).

It is meant to be hooked to the prolog messageing system (see https://www.swi-prolog.org/pldoc/man?predicate=print_message/2).


## Installation

```swipl
pack_install(progress_bar).
```
## Usage

In your SWIPL programme include the directive: 

```swipl
:- use_module(progress_bar).
prolog:message(demo(Msg)) --> Msg.
```

As an example, run the following query:

```swipl
Total =  1000,
forall(
        between(1,Total,Index),
        print_message(informational,demo(simple_progress_bar(Index,Total)))).
```

## Examples

In the file `examples\demo.pl` a number of demos are provided 

```swipl
:- ['./examples/demo.pl']. 	% loads the demo
:- demo_progress_bar.		% runs demo simple_progress_bar, default_progress_bar and fancy_progress_bar
:- demo_spinner.			% runs demo simple_spinner, default_spinner and fancy_spinner
```

## Documentation

See the pldoc documentation for additional information

## Files

```
progress_bar.pl - the progress_bar module
examples/demo.pl - a number of progress_bar and spinner examples
```

