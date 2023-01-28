%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2023, Fred Youhanaie
%%% @doc
%%%
%%% The `espace' tuple record types and I/O macros.
%%%
%%% @end
%%% Created : 28 Jan 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-record(boxcast,  {row, col, msg}).
-record(cell,     {row, col, msg}).
-record(cellcast, {row, col, num}).
-record(colcast,  {col, msg}).
-record(done,     {solution}).
-record(rowcast,  {row, msg}).
-record(solved,   {row, col, num}).

%%-------------------------------------------------------------------

-define( OUT(Rec               ), espace:out(#Rec               ) ).
-define( OUT(Rec, P_1          ), espace:out(#Rec{P_1          }) ).
-define( OUT(Rec, P_1, P_2     ), espace:out(#Rec{P_1, P_2     }) ).
-define( OUT(Rec, P_1, P_2, P_3), espace:out(#Rec{P_1, P_2, P_3}) ).

%%-------------------------------------------------------------------

-define( INP(Rec     ), espace:inp(#Rec{_='_'     }) ).
-define( INP(Rec, P_1), espace:inp(#Rec{_='_', P_1}) ).

%%-------------------------------------------------------------------

-define( IN(Rec               ), espace:in(#Rec{_='_'               }) ).
-define( IN(Rec, P_1          ), espace:in(#Rec{_='_', P_1          }) ).
-define( IN(Rec, P_1, P_2     ), espace:in(#Rec{_='_', P_1, P_2     }) ).
-define( IN(Rec, P_1, P_2, P_3), espace:in(#Rec{_='_', P_1, P_2, P_3}) ).

%%-------------------------------------------------------------------
