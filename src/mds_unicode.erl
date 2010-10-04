-module(mds_unicode).

-export([utf8_to_cp1251/1, list_to_urlencode_string/1]).

utf8_to_cp1251([]) ->
   [];
utf8_to_cp1251([I|Rest]) ->
   [ch_to_cp1251(I) | utf8_to_cp1251(Rest)].

ch_to_cp1251(Ch) when Ch < 127 ->
   Ch;
ch_to_cp1251(Ch) when (Ch >= 16#410) and (Ch =< 16#44F) ->
   Ch - 848;
ch_to_cp1251(Ch) ->
   ChTable = [{16#402, 16#80}, {16#403, 16#81}, {16#201A, 16#82}, {16#453, 16#83},
              {16#201E, 16#84}, {16#2026, 16#85}, {16#2020, 16#86}, {16#2021, 16#87},
              {16#20AC, 16#88}, {16#2030, 16#89}, {16#409, 16#8A}, {16#2039, 16#8B},
              {16#40A, 16#8C}, {16#40C, 16#8D}, {16#40B, 16#8E}, {16#40F, 16#8F},
              {16#452, 16#90}, {16#2018, 16#91}, {16#2019, 16#92}, {16#201C, 16#93},
              {16#201D, 16#94}, {16#2022, 16#95}, {16#2013, 16#96}, {16#2014, 16#97},
              {16#2122, 16#99}, {16#459, 16#9A}, {16#203A, 16#9B}, {16#45A, 16#9C},
              {16#45C, 16#9D}, {16#45B, 16#9E}, {16#45F, 16#9F}, {16#A0, 16#A0},
              {16#40E, 16#A1}, {16#45E, 16#A2}, {16#408, 16#A3}, {16#A4, 16#A4},
              {16#490, 16#A5}, {16#A5, 16#A6}, {16#A7, 16#A7}, {16#401, 16#A8},
              {16#A9, 16#A9}, {16#404, 16#AA}, {16#AB, 16#AB}, {16#AC, 16#AC},
              {16#AD, 16#AD}, {16#AE, 16#AE}, {16#407, 16#AF}, {16#B0, 16#B0},
              {16#B1, 16#B1}, {16#405, 16#B2}, {16#456, 16#B3}, {16#491, 16#B4},
              {16#B5, 16#B5}, {16#B6, 16#B6}, {16#B7, 16#B7}, {16#451, 16#451},
              {16#2116, 16#B9}, {16#454, 16#BA}, {16#BB, 16#BB}, {16#458, 16#BC},
              {16#405, 16#BD}, {16#455, 16#BE}, {16#457, 16#BF}],
   case lists:keyfind(Ch, 1, ChTable) of
      false ->
         erlang:error({unknown_character, Ch});
      {Ch, CpCh} ->
         CpCh
   end.

list_to_urlencode_string([]) ->
   [];
list_to_urlencode_string([I | Tail]) when I < 127 ->
   [I | list_to_urlencode_string(Tail)];
list_to_urlencode_string([I | Tail]) ->
   "&#" ++ erlang:integer_to_list(I) ++ ";" ++ list_to_urlencode_string(Tail).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

list_to_urlencode_string_test() ->
   List = [46, 59, 32, 1055, 1068],
   ?assertEqual(".; &#1055;&#1068;", list_to_urlencode_string(List)).

-endif.
