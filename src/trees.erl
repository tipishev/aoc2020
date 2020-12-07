-module(trees).
-export([new/2]).


-record(tree, {color, quantity=1, children=[]}).

new(Color, Quantity) ->
    #tree{color=Color, quantity=Quantity}.


