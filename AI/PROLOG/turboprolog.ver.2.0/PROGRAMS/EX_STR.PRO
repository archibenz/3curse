%trace
predicates
   exist_str(string,string,string,string)
   start
goal
   start.
clauses
   exist_str(L,"","",L):-!.   
   exist_str(LS,S,L1,L2):-
       frontchar(LS,C1,LLS),
       frontchar(S,C2,SS),
       C1 = C2,
       exist_str(LLS,SS,L1,L2).
   exist_str("",_,"",""):-!.
   exist_str(LS,S,L1,L2):-
       frontchar(LS,C,LLS),
       exist_str(LLS,S,LO,L2),
       frontchar(L1,C,LO).                
   start:-
       exist_str("ººººasdºººººfgjºººººkl","ººklºº",L1,L2),
       !,write("L1= ",L1,"   L2= ",L2).
       