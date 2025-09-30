/* 
ﬁﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›      
ﬁ  èpÆ£p†¨¨† „Øp†¢´•≠®Ô ¨•≠Ó, Æ™≠†¨® ¢¢Æ§† ® ®≠‰Æp¨†Ê®Æ≠≠Î¨® ›
ﬁ  Æ™≠†¨® §´Ô Vitamina-TPrploga.                             ›
ﬁ‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹› 
                                                                 */
PREDICATES
  menu(symbol,integer,symbol,integer,integer,integer,integer,
       string,integer,integer,integer,integer) 
  menu_field(symbol,integer,integer,integer,integer,symbol)
  display_menu(symbol,integer)
  init_menu(symbol)
  writefield(symbol,integer)
  setbar(symbol,integer)
  testkey(symbol,KEY,integer,symbol)
  menu_type(symbol,KEY,integer,integer,integer)
  input(symbol,string,string,KEY) 
  display_msg(integer,string)
  getchoice(symbol,integer)
  inpline(string,KEY)
  
CLAUSES
  display_menu(MENUNAME,CHOICE):-
              cursorform(9,9),
              init_menu(MENUNAME),
              repeat,
              readkey(KEY),
              testkey(MENUNAME,KEY,CHOICE,CONT),
              not(CONT = cont),!,
              cursorform(7,8).

  init_menu(MENUNAME):-
              menu(MENUNAME,NEWNO,_,_,_,_,_,_,_,_,_,_),
              existwindow(NEWNO),
              shiftwindow(NEWNO),!.
  init_menu(MENUNAME):-
              menu(MENUNAME,MENUNO,_,MAXFIELD,STARTCHOICE,WATTR,FATTR,
                   HEADER,ROW,COL,HEIGHT,WIGHT),
              makewindow(MENUNO,WATTR,FATTR,HEADER,ROW,COL,HEIGHT,WIGHT),
              writefield(MENUNAME,MAXFIELD),
              STARTCHOICE <> 0,
              menu_field(MENUNAME,STARTCHOICE,ATTR,ROW1,COL1,FIELDNAME),
              str_len(FIELDNAME,FIELDLEN),
              reverseattr(ATTR,REVATTR),
              field_attr(ROW1,COL1,FIELDLEN,REVATTR),
              cursor(ROW1,COL1),!.
  init_menu(_):-!.
         

   writefield(_,0):-!.
   writefield(MENUNAME,MAXFIELD):-
              menu_field(MENUNAME,MAXFIELD,FATTR,ROW,COL,FIELDNAME),
              str_len(FIELDNAME,FIELDLEN),
              
              field_str(ROW,COL,FIELDLEN,FIELDNAME),
              field_attr(ROW,COL,FIELDLEN,FATTR),
              L1=MAXFIELD-1,
              writefield(MENUNAME,L1).

   setbar(MENUNAME,NEWCHOICE):-
              getchoice(MENUNAME,CURENTCHOICE), 
              menu_field(MENUNAME,CURENTCHOICE,FATTR1,ROW1,COL1,FIELDNAME1),
              str_len(FIELDNAME1,FIELDLEN1),
              menu_field(MENUNAME,NEWCHOICE,FATTR2,ROW2,COL2,FIELDNAME2),
              str_len(FIELDNAME2,FIELDLEN2),
              reverseattr(FATTR2,REVATTR2),
              field_attr(ROW1,COL1,FIELDLEN1,FATTR1),
              field_attr(ROW2,COL2,FIELDLEN2,REVATTR2),
              cursor(ROW2,COL2).


   testkey(_,esc,CHOICE,esc):-
              CHOICE=0,!.
   testkey(_,fkey(10),CHOICE,stop):-
              CHOICE=0,!.
   testkey(MENUNAME,cr,CHOICE,selection):-
              getchoice(MENUNAME,CHOICE),!.
   testkey(MENUNAME,home,NEWCHOICE,cont):-
              NEWCHOICE=1,
              setbar(MENUNAME,NEWCHOICE),!.
   testkey(MENUNAME,end,NEWCHOICE,cont):-
              menu(MENUNAME,_,_,NEWCHOICE,_,_,_,_,_,_,_,_),
              setbar(MENUNAME,NEWCHOICE),!.
   testkey(MENUNAME,KEY,NEWCHOICE,cont):-
              getchoice(MENUNAME,CURENTCHOICE),
              menu(MENUNAME,_,TYPE,MAXFIELD,_,_,_,_,_,_,_,_),
              menu_type(TYPE,KEY,MAXFIELD,CURENTCHOICE,NEWCHOICE),
              setbar(MENUNAME,NEWCHOICE),!.
   
   getchoice(MENUNAME,CURENTCHOICE):-
              cursor(ROW,COL),              
              menu_field(MENUNAME,CURENTCHOICE,_,ROW,COL,_),!.
   getchoice(MENUNAME,CURENTCHOICE):-
              menu(MENUNAME,_,_,_,CURENTCHOICE,_,_,_,_,_,_,_),!.


   menu_type(horiz,left,_,CURENTCHOICE,NEWCHOICE):-
              CURENTCHOICE > 1,!,
              NEWCHOICE=CURENTCHOICE-1.
   menu_type(horiz,left,MAXFIELD,1,MAXFIELD):-!.           
   menu_type(horiz,right,MAXFIELD,CURENTCHOICE,NEWCHOICE):-
              CURENTCHOICE < MAXFIELD,!,
              NEWCHOICE=CURENTCHOICE+1.
   menu_type(horiz,right,MAXFIELD,MAXFIELD,1):-!.
   menu_type(horiz,_,_,CURENTCHOICE,CURENTCHOICE):-!.

   menu_type(vert,up,_,CURENTCHOICE,NEWCHOICE):-
              CURENTCHOICE > 1,!,
              NEWCHOICE=CURENTCHOICE-1.
   menu_type(vert,up,MAXFIELD,1,MAXFIELD):-!.
   menu_type(vert,pgup,_,CURENTCHOICE,NEWCHOICE):-
              CURENTCHOICE > 1,!,
              NEWCHOICE=CURENTCHOICE-1.
   menu_type(vert,pgup,MAXFIELD,1,MAXFIELD):-!.
   menu_type(vert,down,MAXFIELD,CURENTCHOICE,NEWCHOICE):- 
              CURENTCHOICE < MAXFIELD,!,
              NEWCHOICE=CURENTCHOICE+1.
   menu_type(vert,down,MAXFIELD,MAXFIELD,1):-!.
   menu_type(vert,pgdn,MAXFIELD,CURENTCHOICE,NEWCHOICE):-
              CURENTCHOICE < MAXFIELD,!,
              NEWCHOICE = CURENTCHOICE+1.
   menu_type(_,_,_,CURENTCHOICE,CURENTCHOICE).


  input(WINDOWNAME,_,INPUT,KEY):-
              menu(WINDOWNAME,WNO,_,_,_,_,_,_,_,_,_,_),
              existwindow(WNO),
              inpline(INPUT,KEY),!.
  input(WINDOWNAME,DEFLINE,INPUT,KEY):-
              init_menu(WINDOWNAME),
              str_len(DEFLINE,FIELDLEN),
              field_str(0,0,FIELDLEN,DEFLINE),
              cursor(0,0), 
              inpline(INPUT,KEY),!.

  inpline(T,KEY):-
              cursorform(7,9),
              readln(TY),
              T=TY,
              KEY=cr,!.
  inpline("",esc).

  display_msg(WNO,MESSAGE):-
              shiftwindow(N),
              existwindow(WNO),
              shiftwindow(WNO),
              makewindow(N1,AW,AF,STR,R,C,NR,NC,CW,FSP,SB),
              REMOVEWINDOW(WNO,0),
              makewindow(N1,AW,AF,STR,R,C,NR,NC,CW,FSP,SB),
              window_str(MESSAGE),
              shiftwindow(N).
  
  
  
/********************** èêàåÖê åÖçû **********************************

éØ®·†≠®•: menu(®¨Ô_¨•≠Ó, ≠Æ¨_Æ™≠†_¨•≠Ó, horiz/vert, ™Æ´_ØÆ´•©_¢_¨•≠Ó,
               ·‚†p‚_ØÆß(1...) , †‚p_Æ™≠†, †‚p_Æ™†≠‚, ·‚pÆ™†_·ÆÆ°È,
               ØÆ´Æ¶•≠®• ® p†ß¨•pÎ Æ™≠†).
               
         ù‚Æ‚ ‰†™‚ ÆØ®·Î¢†•‚ Æ™≠Æ ¨•≠Ó. Ñ†´•• ÆØ®·Î¢†•‚·Ô ™†¶§Æ• ØÆ´•
      ¨•≠Ó ‰†™‚†¨®:
      
          menu_field( ®¨Ô ¨•≠Ó, ≠Æ¨_ØÆ´Ô, †‚p_ØÆ´Ô, N ·‚pÆ™®, N ·‚Æ´°Ê† ¢
                      Æ™≠• ¨•≠Ó, " àåü èéãü ").   
               
                     
   menu(msgwindow,2,horiz,1,0,81,0,"",0,0,1,80).
   menu(mainmenu,3,horiz,5,1,19,31," mainmenu ",1,0,3,80).
   menu(filemenu,4,vert,4,1,49,103," file ",1,3,6,8).
% msgwindow
       menu_field(msgwindow,1,81,0,0,"").
% mainmenu   
       menu_field(mainmenu,1,19,0,4," FILE ").
       menu_field(mainmenu,2,19,0,20," EDIT ").
       menu_field(mainmenu,3,19,0,36," CONV ").
       menu_field(mainmenu,4,19,0,52," PRINT ").
       menu_field(mainmenu,5,19,0,68," OPTIONS ").
% filemenu
       menu_field(filemenu,1,49,0,0," LOAD ").
       menu_field(filemenu,2,49,1,0," SAVE ").
       menu_field(filemenu,3,49,2,0," DIR  ").
       menu_field(filemenu,4,49,3,0," NEW  ").

******* ß†§†©‚• §´Ô Øp®¨•p† Ê•´Ï :

 Goal: dysplay_menu(mainmenu,X),display_menu(filemenu)
*/



     
