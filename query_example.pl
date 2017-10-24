/*
Sample queries you can make to the restaurant bot
With expected responses:

?- ask([i,like,to,eat,pizza], Res, Food).
Res = ['Pizza Hut'],
Food = [pizza].

Pizza Hut is the only place that sells pizza

?- ask([i,like,to,eat,ramen,and,sushi], Res, Food).
false.

No place sells both ramen and sushi in our knowledge base

?- ask([i,like,ramen,or,pizza], Res, Food).
Res = ['Pizza Hut', 'Bento Sushi'],
Food = [pizza, sushi] ;
Res = ['Pizza Hut', 'Bento Sushi'],
Food = [pizza, ramen] ;
false.

Pizza Hut sells pizza, Bento Sushi sells sushi
the user likes to eat which ever pizza or sushi so we return both restaurants

?- ask([i,do,not,want,to,eat,noodle,and,sushi], Res, Food).
Res = ['McDonalds'],
Food = [burgers] ;
Res = ['McDonalds'],
Food = [fries] ;
Res = ['McDonalds'],
Food = [icecream] ;
Res = ['Starbucks'],
Food = [coffee] ;
Res = ['Starbucks'],
Food = [cookie] ;
Res = ['Pizza Hut'],
Food = [pizza] ;
Res = ['Sage Bistro'],
Food = [seafood] ;
Res = Food, Food = [] ;
false.

Excludes all restaurants that contain noodle or sushi

ask([i,want,to,eat,cheap,food], Res, Food).
Res = ['McDonalds'],
Food = [burgers] ;
Res = ['McDonalds'],
Food = [fries] ;
Res = ['McDonalds'],
Food = [icecream] ;
false.

McDonalds is the only place that sells cheap food in our knowledge base.
So it is the only restaurant that is returned.
*/


