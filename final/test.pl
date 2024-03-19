price(amazon,a,2).
price(amazon,b,3).
price(amazon,d,7).
price(bestbuy,a,3).
price(bestbuy,c,5).
price(walmart,a,4).
price(walmart,b,5).
price(walmart,d,6).

% price(Store, a, Price), price < 4

beats(Store1, Store2, Item) :-
    price(Store1, Item, Price1),
    price(Store2, Item, Price2),
    Price1 < Price2.
