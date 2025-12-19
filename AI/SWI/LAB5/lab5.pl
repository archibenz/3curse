% ЛР №5: База данных потолочных панелей
% выполнил Коломиец Александр Романович ИКПИ 33

:- use_module(library(pce)).
:- use_module(library(csv)).
:- use_module(library(readutil)).  % read_line_to_string/2
:- dynamic panels/5.

% panels(Article, Brand, Material, Size, Price).

% =========================================================
% SAFE GUI CALLS (важно для XPCE: не отдаём fail/exception наружу)
% =========================================================

safe_call(Goal) :-
    catch(call(Goal), validation_failed, true),
    catch(true, _, true).

require_field(Cond, FieldObj, Title, Msg) :-
    (   Cond
    ->  true
    ;   show_error_on_field(Title, Msg, FieldObj),
        throw(validation_failed)
    ).

% =========================================================
% MAIN MENU
% =========================================================

start :-
    new(MainDialog, dialog('Ceiling Panels Database System')),
    send(MainDialog, size, size(650, 450)),

    new(Title, text(' CEILING PANELS DATABASE SYSTEM')),
    send(Title, font, font(roman, bold, 18)),
    send(Title, colour, black),
    send(Title, alignment, left),
    send(MainDialog, append, Title),

    send(MainDialog, append, new(_, label('', ''))),

    new(Subtitle, label(subtitle, ' MAIN MENU:')),
    send(Subtitle, font, font(normal, bold, 20)),
    send(Subtitle, colour, blue),
    send(Subtitle, alignment, left),
    send(MainDialog, append, Subtitle),

    send(MainDialog, append, new(_, label('', ''))),
    send(MainDialog, append, new(_, label('', ''))),

    new(B1, button('1. ADD NEW PANEL ', message(@prolog, add_panel_dialog))),
    send(B1, alignment, left),
    send(MainDialog, append, B1),
    send(MainDialog, append, new(_, label('', ''))),

    new(B2, button('2. VIEW DATABASE ', message(@prolog, view_database_simple))),
    send(B2, alignment, left),
    send(MainDialog, append, B2),
    send(MainDialog, append, new(_, label('', ''))),

    new(B3, button('3. IMPORT FROM CSV ', message(@prolog, import_csv_dialog))),
    send(B3, alignment, left),
    send(MainDialog, append, B3),
    send(MainDialog, append, new(_, label('', ''))),

    new(B4, button('4. EXPORT TO CSV ', message(@prolog, export_csv_dialog))),
    send(B4, alignment, left),
    send(MainDialog, append, B4),
    send(MainDialog, append, new(_, label('', ''))),

    new(B5, button('5. EDIT PANEL ', message(@prolog, edit_panel_dialog))),
    send(B5, alignment, left),
    send(MainDialog, append, B5),
    send(MainDialog, append, new(_, label('', ''))),

    new(B6, button('6. DELETE PANEL ', message(@prolog, delete_panel_dialog))),
    send(B6, alignment, left),
    send(MainDialog, append, B6),
    send(MainDialog, append, new(_, label('', ''))),

    new(B7, button('7. CLEAR DATABASE ', message(@prolog, clear_database_dialog))),
    send(B7, alignment, left),
    send(MainDialog, append, B7),

    send(MainDialog, append, new(_, label('', ''))),
    send(MainDialog, append, new(_, label('', ''))),

    new(ExitButton, button(' EXIT ', message(MainDialog, destroy))),
    send(ExitButton, alignment, left),
    send(ExitButton, colour, red),
    send(MainDialog, append, ExitButton),

    send(MainDialog, open_centered).

% =========================================================
% VIEW DATABASE
% =========================================================

view_database_simple :-
    findall([A,B,M,S,P], panels(A,B,M,S,P), Items),
    ( Items = []
    -> show_message('Database', 'Database is empty')
    ;  new(D, dialog('Database Contents - Table View')),
       send(D, size, size(980, 600)),

       new(Title, label(title, 'CEILING PANELS DATABASE')),
       send(Title, font, font(normal, bold, 14)),
       send(Title, alignment, center),
       send(D, append, Title),
       send(D, append, new(_, label('', ''))),

       new(Text, text('', left, normal)),
       send(Text, font, font(courier, roman, 10)),
       send(Text, width, 940),
       send(Text, height, 25),

       format(string(Headers),
              '~w~t~18+~w~t~40+~w~t~65+~w~t~85+~w~n',
              ['Article','Brand','Material','Size','Price']),
       send(Text, append, Headers),
       send(Text, append,
            string('-----------------------------------------------------------------------------------------------\n')),

       forall(member([Article, Brand, Material, Size, Price], Items),
              (
                  shorten_atom(Brand, 18, BrandShort),
                  shorten_atom(Material, 22, MatShort),
                  shorten_atom(Size, 12, SizeShort),
                  format(string(Row),
                         '~w~t~18+~w~t~40+~w~t~65+~w~t~85+~w~n',
                         [Article, BrandShort, MatShort, SizeShort, Price]),
                  send(Text, append, Row)
              )),

       send(D, append, Text),
       send(D, append, new(_, label('', ''))),

       length(Items, Count),
       format(string(CountLabel), 'Total records: ~w', [Count]),
       new(CountLabelObj, label(count, CountLabel)),
       send(CountLabelObj, font, font(normal, bold, 12)),
       send(CountLabelObj, alignment, center),
       send(D, append, CountLabelObj),

       send(D, append, new(_, label('', ''))),

       new(ButtonBox, dialog_group(buttons, group)),
       send(ButtonBox, gap, size(30, 10)),
       send(ButtonBox, append, button('CLOSE', message(D, destroy))),
       send(ButtonBox, alignment, center),
       send(D, append, ButtonBox),

       send(D, open_centered)
    ).

shorten_atom(Atom, Max, Out) :-
    ( atom(Atom), atom_length(Atom, Len), Len > Max
    -> Keep is Max - 3,
       sub_atom(Atom, 0, Keep, _, Prefix),
       atom_concat(Prefix, '...', Out)
    ; Out = Atom
    ).

% =========================================================
% ADD PANEL
% =========================================================

add_panel_dialog :-
    new(D, dialog('Add New Ceiling Panel')),
    send(D, size, size(520, 420)),

    new(Title, label(title, 'ADD NEW CEILING PANEL')),
    send(Title, font, font(normal, bold, 14)),
    send(Title, alignment, center),
    send(D, append, Title),

    send(D, append, new(_, label('', ''))),

    send(D, append, new(ArticleObj, text_item('Article:', ''))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(BrandObj, text_item('Brand:', ''))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(MaterialObj, text_item('Material:', ''))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(SizeObj, text_item('Size (e.g. 600x600):', ''))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(PriceObj, text_item('Price:', ''))),

    send(D, append, new(_, label('', ''))),

    new(ButtonBox, dialog_group(buttons, group)),
    send(ButtonBox, gap, size(20, 0)),
    send(ButtonBox, append,
         button('SAVE',
                message(@prolog, validate_and_save_panel_safe,
                        D, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj))),
    send(ButtonBox, append, button('CANCEL', message(D, destroy))),
    send(ButtonBox, alignment, center),
    send(D, append, ButtonBox),

    send(D, open_centered).

% XPCE вызывает только "обычный" предикат -> внутри уже safe_call
validate_and_save_panel_safe(D, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj) :-
    safe_call(validate_and_save_panel_core(D, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj)).

validate_and_save_panel_core(D, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj) :-
    get(ArticleObj, selection, Article),
    get(BrandObj, selection, Brand),
    get(MaterialObj, selection, Material),
    get(SizeObj, selection, Size),
    get(PriceObj, selection, Price),

    require_field(Article \= '', ArticleObj, 'Validation Error', 'Article cannot be empty!'),

    normalize_article(Article, NormArticle),
    require_field(\+ panels(NormArticle, _, _, _, _), ArticleObj,
                  'Validation Error', 'Panel with this article already exists!'),

    require_field(validate_name_field(Brand), BrandObj,
                  'Validation Error', 'Brand must contain only letters/spaces!'),

    require_field(validate_material_field(Material), MaterialObj,
                  'Validation Error', 'Material must contain only letters/spaces/hyphen!'),

    require_field(validate_size(Size), SizeObj,
                  'Validation Error', 'Size must look like 600x600 (numbers x numbers)!'),

    require_field(validate_price(Price), PriceObj,
                  'Validation Error', 'Price must be a positive integer!'),

    assertz(panels(NormArticle, Brand, Material, Size, Price)),
    show_message('Success', 'Ceiling panel added successfully!'),
    send(D, destroy).

% =========================================================
% EDIT PANEL
% =========================================================

edit_panel_dialog :-
    new(D, dialog('Edit Ceiling Panel')),
    send(D, size, size(520, 200)),

    new(Title, label(title, 'EDIT CEILING PANEL')),
    send(Title, font, font(normal, bold, 14)),
    send(Title, alignment, center),
    send(D, append, Title),

    send(D, append, new(_, label('', ''))),
    send(D, append, new(Article, text_item('Article to edit:', ''))),
    send(D, append, new(_, label('', ''))),

    new(ButtonBox, dialog_group(buttons, group)),
    send(ButtonBox, gap, size(20, 0)),
    send(ButtonBox, append,
         button('FIND',
                and(message(@prolog, find_panel_for_edit, Article?selection),
                    message(D, destroy)))),
    send(ButtonBox, append, button('CANCEL', message(D, destroy))),
    send(ButtonBox, alignment, center),
    send(D, append, ButtonBox),

    send(D, open_centered).

find_panel_for_edit(ArticleArg) :-
    normalize_article(ArticleArg, NormArticle),
    ( panels(NormArticle, B, M, S, P)
    -> edit_panel_details(NormArticle, B, M, S, P)
    ;  show_error('Error', 'Panel not found!')
    ).

edit_panel_details(OldArticle, OldBrand, OldMaterial, OldSize, OldPrice) :-
    new(D, dialog('Edit Panel Details')),
    send(D, size, size(520, 440)),

    new(Title, label(title, 'EDIT PANEL DETAILS')),
    send(Title, font, font(normal, bold, 14)),
    send(Title, alignment, center),
    send(D, append, Title),

    send(D, append, new(_, label('', ''))),

    send(D, append, new(ArticleObj, text_item('Article:', OldArticle))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(BrandObj, text_item('Brand:', OldBrand))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(MaterialObj, text_item('Material:', OldMaterial))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(SizeObj, text_item('Size (e.g. 600x600):', OldSize))),
    send(D, append, new(_, label('', ''))),

    send(D, append, new(PriceObj, text_item('Price:', OldPrice))),
    send(D, append, new(_, label('', ''))),

    new(ButtonBox, dialog_group(buttons, group)),
    send(ButtonBox, gap, size(20, 0)),
    % окно закроется только в save_edit_core при успехе
    send(ButtonBox, append,
         button('SAVE',
                message(@prolog, save_edit_safe,
                        D, OldArticle, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj))),
    send(ButtonBox, append, button('CANCEL', message(D, destroy))),
    send(ButtonBox, alignment, center),
    send(D, append, ButtonBox),

    send(D, open_centered).

save_edit_safe(Dialog, OldArticle, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj) :-
    safe_call(save_edit_core(Dialog, OldArticle, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj)).

save_edit_core(Dialog, OldArticle, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj) :-
    validate_and_update_panel_core(OldArticle, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj),
    show_message('Success', 'Panel updated successfully!'),
    send(Dialog, destroy).

validate_and_update_panel_core(OldArticle, ArticleObj, BrandObj, MaterialObj, SizeObj, PriceObj) :-
    get_text_item_string(ArticleObj, Article),
    get_text_item_string(BrandObj, Brand),
    get_text_item_string(MaterialObj, Material),
    get_text_item_string(SizeObj, Size),
    get_text_item_string(PriceObj, Price),

    require_field(Article \= '', ArticleObj, 'Validation Error', 'Article cannot be empty!'),

    normalize_article(Article, NormArticle),

    ( NormArticle \== OldArticle
    -> require_field(\+ panels(NormArticle, _, _, _, _), ArticleObj,
                     'Validation Error', 'Panel with this article already exists!')
    ;  true
    ),

    require_field(validate_name_field(Brand), BrandObj,
                  'Validation Error', 'Brand must contain only letters/spaces!'),

    require_field(validate_material_field(Material), MaterialObj,
                  'Validation Error', 'Material must contain only letters/spaces/hyphen!'),

    require_field(validate_size(Size), SizeObj,
                  'Validation Error', 'Size must look like 600x600 (numbers x numbers)!'),

    require_field(validate_price(Price), PriceObj,
                  'Validation Error', 'Price must be a positive integer!'),

    retract(panels(OldArticle, _, _, _, _)),
    assertz(panels(NormArticle, Brand, Material, Size, Price)).

% =========================================================
% DELETE PANEL
% =========================================================

delete_panel_dialog :-
    new(D, dialog('Delete Ceiling Panel')),
    send(D, size, size(520, 200)),

    new(Title, label(title, 'DELETE CEILING PANEL')),
    send(Title, font, font(normal, bold, 14)),
    send(Title, alignment, center),
    send(D, append, Title),

    send(D, append, new(_, label('', ''))),
    send(D, append, new(Article, text_item('Article to delete:', ''))),
    send(D, append, new(_, label('', ''))),

    new(ButtonBox, dialog_group(buttons, group)),
    send(ButtonBox, gap, size(20, 0)),
    send(ButtonBox, append, button('DELETE', message(@prolog, delete_panel, Article?selection))),
    send(ButtonBox, append, button('CANCEL', message(D, destroy))),
    send(ButtonBox, alignment, center),
    send(D, append, ButtonBox),

    send(D, open_centered).

delete_panel(Article) :-
    normalize_article(Article, NormArticle),
    ( retract(panels(NormArticle, _, _, _, _))
    -> show_message('Success', 'Panel deleted successfully!')
    ;  show_error('Error', 'Panel not found!')
    ).

% =========================================================
% CLEAR DATABASE
% =========================================================

clear_database_dialog :-
    new(D, dialog('Clear Database')),
    send(D, size, size(520, 200)),

    new(Warning, label(warning, 'WARNING: CLEAR DATABASE')),
    send(Warning, font, font(normal, bold, 14)),
    send(Warning, colour, red),
    send(Warning, alignment, center),
    send(D, append, Warning),

    send(D, append, new(_, label('', ''))),
    new(Confirm, label(confirm, 'This will delete ALL data permanently!')),
    send(Confirm, alignment, center),
    send(D, append, Confirm),
    send(D, append, new(_, label('', ''))),

    new(ButtonBox, dialog_group(buttons, group)),
    send(ButtonBox, gap, size(20, 0)),
    send(ButtonBox, append,
         button('CONFIRM CLEAR', and(message(@prolog, clear_database), message(D, destroy)))),
    send(ButtonBox, append, button('CANCEL', message(D, destroy))),
    send(ButtonBox, alignment, center),
    send(D, append, ButtonBox),

    send(D, open_centered).

clear_database :-
    retractall(panels(_, _, _, _, _)),
    show_message('Success', 'Database cleared successfully!').

% =========================================================
% CSV IMPORT
% =========================================================

import_csv_dialog :-
    new(D, dialog('Import from CSV')),
    send(D, size, size(650, 240)),

    new(Info, label(info, 'IMPORT FROM CSV FILE')),
    send(Info, font, font(normal, bold, 14)),
    send(Info, alignment, center),
    send(D, append, Info),

    send(D, append, new(_, label('', ''))),

    new(Format, label(format, 'Format: ARTICLE;BRAND;MATERIAL;SIZE;PRICE')),
    send(Format, font, font(normal, italic, 10)),
    send(Format, colour, blue),
    send(Format, alignment, center),
    send(D, append, Format),

    send(D, append, new(_, label('', ''))),
    send(D, append, new(FilePath, text_item('File path:', ''))),
    send(D, append, new(_, label('', ''))),

    new(ButtonBox, dialog_group(buttons, group)),
    send(ButtonBox, gap, size(20, 0)),
    send(ButtonBox, append, button('IMPORT', message(@prolog, process_import, FilePath?selection))),
    send(ButtonBox, append, button('CANCEL', message(D, destroy))),
    send(ButtonBox, alignment, center),
    send(D, append, ButtonBox),

    send(D, open_centered).

process_import(FilePath) :-
    ( exists_file(FilePath)
    -> import_csv_file(FilePath, Count, Errors),
       ( Errors = []
       -> format(string(Message), 'Successfully imported ~w records from ~w', [Count, FilePath]),
          show_message('Import Result', Message)
       ;  format_all_errors(Errors, ErrorText),
          format(string(Message),
                 'Import completed with warnings:\n\n~w\nSuccessfully imported: ~w records',
                 [ErrorText, Count]),
          show_message('Import Result with Errors', Message)
       )
    ;  format(string(Message), 'File not found: ~w', [FilePath]),
       show_error('File Error', Message)
    ).

import_csv_file(File, Count, Errors) :-
    setup_call_cleanup(
        open(File, read, Stream, [encoding(utf8)]),
        import_csv_stream(Stream, 1, 0, Count, [], Errors),
        close(Stream)
    ).

import_csv_stream(Stream, LineNum, AccCount, TotalCount, AccErrors, TotalErrors) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file
    -> TotalCount = AccCount,
       TotalErrors = AccErrors
    ;  ( LineNum == 1
       -> NextLineNum is LineNum + 1,
          import_csv_stream(Stream, NextLineNum, AccCount, TotalCount, AccErrors, TotalErrors)
       ;  split_string(Line, ";", "", Tokens),
          ( Tokens = [Article, Brand, Material, Size, Price]
          -> validate_import_row(Article, Brand, Material, Size, Price, LineNum, Result, RowErrors),
             ( Result = valid
             -> normalize_article(Article, NormArticle),
                ( \+ panels(NormArticle, _, _, _, _)
                -> assertz(panels(NormArticle, Brand, Material, Size, Price)),
                   NewCount is AccCount + 1,
                   NewErrors = AccErrors
                ;  NewCount = AccCount,
                   format(string(ErrorMsg), 'Duplicate article: ~w', [NormArticle]),
                   NewErrors = [error(LineNum, ErrorMsg)|AccErrors]
                )
             ;  NewCount = AccCount,
                append(AccErrors, RowErrors, NewErrors)
             )
          ;  NewCount = AccCount,
             length(Tokens, N),
             format(string(ErrorMsg), 'Invalid number of fields: expected 5, got ~w', [N]),
             NewErrors = [error(LineNum, ErrorMsg)|AccErrors]
          ),
          NextLineNum is LineNum + 1,
          import_csv_stream(Stream, NextLineNum, NewCount, TotalCount, NewErrors, TotalErrors)
       )
    ).

validate_import_row(Article, Brand, Material, Size, Price, LineNum, Result, Errors) :-
    validate_article_import(Article, LineNum, E1),
    validate_brand_import(Brand, LineNum, E2),
    validate_material_import(Material, LineNum, E3),
    validate_size_import(Size, LineNum, E4),
    validate_price_import(Price, LineNum, E5),
    append([E1,E2,E3,E4,E5], AllErrors),
    ( AllErrors = []
    -> Result = valid, Errors = []
    ;  Result = invalid, Errors = AllErrors
    ).

validate_article_import(Article, LineNum, Errors) :-
    ( Article = ""
    -> Errors = [error(LineNum, 'Article cannot be empty')]
    ;  Errors = []
    ).

validate_brand_import(Brand, LineNum, Errors) :-
    ( validate_name_field(Brand)
    -> Errors = []
    ;  format(string(Msg), 'Brand "~w" is invalid - letters/spaces only', [Brand]),
       Errors = [error(LineNum, Msg)]
    ).

validate_material_import(Material, LineNum, Errors) :-
    ( validate_material_field(Material)
    -> Errors = []
    ;  format(string(Msg), 'Material "~w" is invalid - letters/spaces/hyphen only', [Material]),
       Errors = [error(LineNum, Msg)]
    ).

validate_size_import(Size, LineNum, Errors) :-
    ( validate_size(Size)
    -> Errors = []
    ;  format(string(Msg), 'Size "~w" is invalid - expected like 600x600', [Size]),
       Errors = [error(LineNum, Msg)]
    ).

validate_price_import(Price, LineNum, Errors) :-
    ( validate_price(Price)
    -> Errors = []
    ;  format(string(Msg), 'Price "~w" is invalid - positive integer only', [Price]),
       Errors = [error(LineNum, Msg)]
    ).

format_all_errors(Errors, Result) :-
    format_all_errors(Errors, 1, '', Result).
format_all_errors([], _, Acc, Acc).
format_all_errors([error(Line, Msg)|Rest], Index, Acc, Result) :-
    format(string(LineMsg), '~w. Line ~w: ~w\n', [Index, Line, Msg]),
    string_concat(Acc, LineMsg, NewAcc),
    NextIndex is Index + 1,
    format_all_errors(Rest, NextIndex, NewAcc, Result).

% =========================================================
% CSV EXPORT
% =========================================================

export_csv_dialog :-
    new(D, dialog('Export to CSV')),
    send(D, size, size(550, 200)),

    new(Info, label(info, 'EXPORT TO CSV FILE')),
    send(Info, font, font(normal, bold, 14)),
    send(Info, alignment, center),
    send(D, append, Info),

    send(D, append, new(_, label('', ''))),
    send(D, append, new(FilePath, text_item('File path:', ''))),
    send(D, append, new(_, label('', ''))),

    new(ButtonBox, dialog_group(buttons, group)),
    send(ButtonBox, gap, size(20, 0)),
    send(ButtonBox, append, button('EXPORT', message(@prolog, process_export, FilePath?selection))),
    send(ButtonBox, append, button('CANCEL', message(D, destroy))),
    send(ButtonBox, alignment, center),
    send(D, append, ButtonBox),

    send(D, open_centered).

process_export(FilePath) :-
    findall([A,B,M,S,P], panels(A,B,M,S,P), Items),
    ( Items = []
    -> show_error('Export Error', 'Database is empty! Nothing to export.')
    ;  setup_call_cleanup(
           open(FilePath, write, Stream, [encoding(utf8)]),
           ( write(Stream, 'ARTICLE;BRAND;MATERIAL;SIZE;PRICE'), nl(Stream),
             export_items(Stream, Items)
           ),
           close(Stream)
       ),
       length(Items, Count),
       format(string(Message), 'Exported ~w panels to:\n~w', [Count, FilePath]),
       show_message('Success', Message)
    ).

export_items(_, []).
export_items(Stream, [[A,B,M,S,P]|Rest]) :-
    format(Stream, '~w;~w;~w;~w;~w~n', [A,B,M,S,P]),
    export_items(Stream, Rest).

% =========================================================
% VALIDATION HELPERS
% =========================================================

normalize_article(Article, NormalizedAtom) :-
    ( atom(Article) -> atom_string(Article, Str)
    ; string(Article) -> Str = Article
    ; nonvar(Article) -> term_string(Article, Str)
    ; Str = ""
    ),
    string_upper(Str, UpperStr),
    normalize_space(string(NormStr), UpperStr),
    atom_string(NormalizedAtom, NormStr).

validate_name_field(String) :-
    ( atom(String) -> atom_string(String, Str)
    ; string(String) -> Str = String
    ; Str = ""
    ),
    Str \= "",
    normalize_space(string(Trim), Str),
    Trim \= "",
    string_length(Trim, L), L >= 2,
    string_chars(Trim, Chars),
    forall(member(C, Chars), (char_type(C, alpha) ; C = ' ')).

validate_material_field(String) :-
    ( atom(String) -> atom_string(String, Str)
    ; string(String) -> Str = String
    ; Str = ""
    ),
    Str \= "",
    normalize_space(string(Trim), Str),
    Trim \= "",
    string_length(Trim, L), L >= 2,
    string_chars(Trim, Chars),
    forall(member(C, Chars), (char_type(C, alpha) ; C = ' ' ; C = '-')).

validate_size(SizeIn) :-
    ( atom(SizeIn) -> atom_string(SizeIn, Size0)
    ; string(SizeIn) -> Size0 = SizeIn
    ; Size0 = ""
    ),
    Size0 \= "",
    normalize_space(string(Size), Size0),
    Size \= "",
    string_lower(Size, Low),
    ( split_string(Low, "x*", " ", [W,H])
    -> validate_pos_int_string(W),
       validate_pos_int_string(H)
    ; fail
    ).

validate_pos_int_string(Str) :-
    Str \= "",
    catch(number_string(N, Str), _, fail),
    integer(N),
    N > 0.

validate_price(String) :-
    ( atom(String) -> atom_string(String, Str)
    ; string(String) -> Str = String
    ; Str = ""
    ),
    Str \= "",
    catch(number_string(Number, Str), _, fail),
    integer(Number),
    Number > 0.

get_text_item_string(Item, Str) :-
    ( catch(get(Item, selection, Sel), _, fail)
    -> ( atom(Sel) -> atom_string(Sel, Str)
       ; string(Sel) -> Str = Sel
       ; ( catch(get(Sel, value, Val), _, fail)
         -> ( atom(Val) -> atom_string(Val, Str)
            ; string(Val) -> Str = Val
            ; term_string(Val, Str)
            )
         ; term_string(Sel, Str)
         )
       )
    ; Str = ""
    ).

% =========================================================
% MESSAGES
% =========================================================

show_error_on_field(Title, Message, FieldObj) :-
    show_error(Title, Message),
    catch(send(FieldObj, selection, ''), _, true),
    catch(send(FieldObj, value, ''), _, true),
    catch(send(FieldObj, focus, @on), _, true).

show_error(Title, Message) :-
    new(D, dialog(Title)),
    send(D, size, size(420, 180)),
    new(ErrorTitle, label(error_title, 'ERROR')),
    send(ErrorTitle, font, font(normal, bold, 14)),
    send(ErrorTitle, colour, red),
    send(ErrorTitle, alignment, center),
    send(D, append, ErrorTitle),
    send(D, append, new(_, label('', ''))),
    new(Msg, label(message, Message)),
    send(Msg, font, font(normal, roman, 12)),
    send(Msg, alignment, center),
    send(D, append, Msg),
    send(D, append, new(_, label('', ''))),
    new(OKButton, button('OK', message(D, destroy))),
    send(OKButton, alignment, center),
    send(D, append, OKButton),
    send(D, open_centered).

show_message(Title, Message) :-
    new(D, dialog(Title)),
    send(D, size, size(420, 150)),
    new(Msg, label(message, Message)),
    send(Msg, font, font(normal, bold, 12)),
    send(Msg, alignment, center),
    send(D, append, Msg),
    send(D, append, new(_, label('', ''))),
    new(OKButton, button('OK', message(D, destroy))),
    send(OKButton, alignment, center),
    send(D, append, OKButton),
    send(D, open_centered).

% =========================================================
% LOAD INFO
% =========================================================
:- write('================================='), nl,
   write(' CEILING PANELS DATABASE SYSTEM'), nl,
   write('================================='), nl,
   write('To start: ?- start.'), nl,
   write('Fields: Article, Brand, Material, Size, Price'), nl,
   write('CSV: ARTICLE;BRAND;MATERIAL;SIZE;PRICE'), nl,
   write('================================='), nl.