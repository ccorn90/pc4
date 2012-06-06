%%% c.pl -- a playground for the connect-four project.
%%% always assuming a 6 high x 7 wide grid

cpuWaitTime(0.5).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some basic grids for source data
blankGrid([-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-]).
winGrid([-,-,o,o,o,o,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-]).
gameGrid([-,-,o,o,o,x,-,-,o,o,o,o,-,-,-,-,x,o,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-]).
nearFullGrid([x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,x,o,-,o,x,o,x,o]).
fullGrid([x,o,x,o,x,o,o,x,o,x,o,x,x,o,x,o,x,o,o,x,o,x,o,x,x,o,x,o,x,o,o,x,o,x,o,x,x,o,x,o,x,o]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The CPU Player Database %%%%%%
% cpuPlayer(name,id,A,D) %%%%%%%
%%% 0 <= A <= 9, 0 <= D <= 9 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cpuPlayer('George',1,0,0).    %% George Boole - basic strategy
cpuPlayer('Richard',2,9,0).   %% Richard Stallman - aggressive style
cpuPlayer('Linus',3,0,9).     %% Linus Torvalds - defensive style
cpuPlayer('Alain',4,3,3).     %% Alain Colmerauer - equal parts aggressive, defensive, and random


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  statUserWins/1 %% statCPUWins/1 %% statTieGames/1 %%%%%%%%%%%%%%%%%%%%%%%%
%%  A method to keep track of how many times the user, cpu, or tie have won %%
%%  Are meant to be overridden using incrementXXX/0 predicate %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initial conditions for the three data points

initStatistics :-
    asserta(statUserWins(0)),
    asserta(statCPUWins(0)),
    asserta(statTieGames(0)), !.

initStatistics :- true.  % in case the asserta calls fail


% predicates to increment the record-keeping.  Cut is needed to avoid bactracking to multiple values
% include other options so that a failure to update the wincount does not crash the game
incrementUserWins :- statUserWins(Old), New is Old + 1, call(asserta(statUserWins(New))), !.
incrementUserWins :- write('Problem updating statistics.  Will proceed.\n'), true.

incrementCPUWins  :- statCPUWins(Old), New is Old + 1, call(asserta(statCPUWins(New))), !.
incrementCPUWins :- write('Problem updating statistics.  Will proceed.\n'), true.

incrementTieGames :- statTieGames(Old), New is Old + 1, call(asserta(statTieGames(New))), !.
incrementTieGames :- write('Problem updating statistics.  Will proceed.\n'), true.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% to know the terminal size, define termCols/1 and termRows/1 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in case they were not defined by the shell script launcher...
initScreenSize :-
    assertz(termCols(40)),
    assertz(termRows(40)), !.

% in case of failure
initScreenSize :- true.


% method to clear the screen
clearScreen :-
    termRows(R),
    clearScreen_printRows(R), !.

% in case of failure
clearScreen :- true.

% recursive tool to print a given number of rows.
clearScreen_printRows(0).
clearScreen_printRows(X) :-
    Y is X-1,
    nl,
    clearScreen_printRows(Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% silly predicates to make the text turn bold or not %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initColor :- assertz(colorEnable(1)).  % color is by default enabled
initColor :- true.  % in case of failure

isColorEnabled :- colorEnable(0), !, fail.
isColorEnabled :- colorEnable(1), !, true.

text_boldText :- shell('echo -n [1m'), shell('echo -n [34m').
text_boldText :- true. % in case of failure

text_regularText :- shell('echo -n [0m').
text_regularText :- true. % in case of failure

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% printGrid -- prints a linear list as a set of rows, with rows and columns labeled

doubleWide(_) :- fail.  % change to true to make it print at 2x width.  fail gives single-width printing.

printGrid(List) :-
    makeNestedByRow(List,NL),
    printHeader(_),
    printRows(0,NL), !,  % cut prevents any backtracking to try to do this again if we pass.
    nl.

printHeader(_) :- doubleWide(_), write('  1  2  3  4  5  6  7'), nl.
printHeader(_) :- write(' 1 2 3 4 5 6 7'), nl.




%% predicates to write capital letters as bold/color instead, but only if colors are enabled
writeChar('O') :-
    isColorEnabled,   % is color output enabled?
    text_boldText,
    write('o'),
    text_regularText.

writeChar('X') :-
    isColorEnabled,   % is color output enabled?
    text_boldText,
    write('x'),
    text_regularText.

writeChar(AnyChar) :-
    write(AnyChar).

printRows(_,[[]]).

printRows(RowNum,[[A1,A2,A3,A4,A5,A6,A7]|Tail]) :-
    doubleWide(_), !,
%    write(RowNum),          % print the row number
    NextRow is RowNum + 1,  
    write(' '), writeChar(A1),  % print all the data
    write(' '), writeChar(A2),
    write(' '), writeChar(A3),
    write(' '), writeChar(A4),
    write(' '), writeChar(A5),
    write(' '), writeChar(A6),
    write(' '), writeChar(A7),
    nl,
    printRows(NextRow,Tail).  % recur

printRows(RowNum,[[A1,A2,A3,A4,A5,A6,A7]|Tail]) :-
%    write(RowNum),          % print the row number
    NextRow is RowNum + 1,  
    write(' '), writeChar(A1),  % print all the data
    write(' '), writeChar(A2),
    write(' '), writeChar(A3),
    write(' '), writeChar(A4),
    write(' '), writeChar(A5),
    write(' '), writeChar(A6),
    write(' '), writeChar(A7),
    nl,
    printRows(NextRow,Tail).  % recur



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nestByColumn -- takes a linear list and creates a list of nested sublists
%    by column, left to right ... or vice virsa

nestByColumn([], []).
nestByColumn([A1,A2,A3,A4,A5,A6|Tail], [[A1,A2,A3,A4,A5,A6]|NewTail]) :-
    nestByColumn(Tail,NewTail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% makeNestedByRow -- takes a linear list and creates a list of nested sublists
%    by row, top to bottom

makeNestedByRow(List, Output) :-
    nestByColumn(List, NBColumn),
    mNBR_helper(NBColumn, Output).

mNBR_helper( [ [],[],[],[],[],[],[] ],   % if we are at the bottom of all columns
	     [ [] ]).                    % return an empty list for the bottom row

mNBR_helper( [ [A|AT],[B|BT],[C|CT],[D|DT],[E|ET],[F|FT],[G|GT] ],  % top from each column
	     [ [A,B,C,D,E,F,G] | LowerRows ]) :-
    mNBR_helper( [AT,BT,CT,DT,ET,FT,GT], LowerRows ). % recur to find lower rows
    










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% validMove(Grid,SquareNum) -- tells us a valid move in the grid, accounting for the fact that pieces fall
% this is meant to work with backtracking to give us all possible moves.
% Sweeps by columns, left to right.  Also works as checker of validity of a move.

validMove(Grid,Move) :-
    nestByColumn(Grid,Columns),           % convert grid to by-column view,
                                          %   then find the move for each column
    vM_checkAllColumns(PossibleMove,0,Columns),
    PossibleMove = Move.    % will backtrack to the previous line to generate next



% vM_checkAllColumns(MoveDepth,MoveCol,Columns).  Note:  backtracking is a good thing here!
vM_checkAllColumns(_,_,[]) :- fail.        % if we hit the rightmost list without finding a move, we fail
vM_checkAllColumns(Move,MoveCol,[C|_]) :-    % if we have a move in this column, return
    vM_checkColumn(Depth,C),
    Move is Depth + 6*MoveCol.
vM_checkAllColumns(Move,MoveCol,[_|Tail]) :-     % if we dont have a move in this column, recur.
    NewCol is MoveCol + 1,
    vM_checkAllColumns(Move,NewCol,Tail).
    
    
% simply write rules for all possible depths in a column.
vM_checkColumn(5,[-,-,-,-,-,- |_]) :- !. % column has 5 free
vM_checkColumn(4,[-,-,-,-,- |_])   :- !. % column has 4 free
vM_checkColumn(3,[-,-,-,- |_])     :- !. % column has 3 free
vM_checkColumn(2,[-,-,- |_])       :- !. % column has 2 free
vM_checkColumn(1,[-,- |_])         :- !. % column has 1 free
vM_checkColumn(0,[- |_])           :- !. % column has 0 free





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% makeMove(OldGrid,MoveNum,MoveAtom,NewGrid) -- places a tile onto the grid.
%   Can either place a given MoveNum, or if _ is given for MoveNum, will place the first possible move.

makeMove(OldGrid,N,X,NewGrid) :-
    validMove(OldGrid,N), !,              % must be a valid move.  Cut so we dont check this twice
    (X = o, ! ; X = x),                   % must be a valid character, cut so we dont check twice
    mM_placeAtom(0,OldGrid,N,X,NewGrid).  % make the move




% place the atom at the correct point in the grid -- mM_placeAtom(Counter,OldGrid,MoveNum,Atom,NewGrid)
mM_placeAtom(_,[],_,_,[]).
mM_placeAtom(Index,[_|Tail],Index,X,[X|NewTail]) :-   % the case where we are at the proper index
    NewIndex is Index + 1,
    mM_placeAtom(NewIndex,Tail,Index,X,NewTail), !.   % cut to avoid the not-copy solution.

mM_placeAtom(Index,[C|Tail],Num,X,[C|NewTail]) :-   % the case where we are at the proper index
    NewIndex is Index + 1,
    mM_placeAtom(NewIndex,Tail,Num,X,NewTail).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% moveAsAI/4 --- uses various AIs to generate a move

moveAsAI(1,Grid,Tile,Move) :- % AI number 1 is the old AI
    smarterAI(Grid,Tile,Move).

moveAsAI(ID,Grid,Tile,Move) :-
    cpuPlayer(_,ID,A,D),
    weightedAI(A,D,Grid,Tile,Move).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5/19/11 -- weightedAI/5
%%%               has ability to select varying degrees of agressiveness or defensiveness
%%%               weightedAI(Agression, Defensiveness, Grid, ComputersTile, BestComputerMove)
%%%
%%%               0 =< Agression < 10
%%%               0 =< Defensiveness < 10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% always win if there is potential to win
weightedAI(_,_,G,T,M) :-
    findWinningMove(G,T,M).


% always block a win if there is a human win to block
weightedAI(_,_,G,T,M) :-
    getOppositeTile(T,UserTile),
    findWinningMove(G,UserTile,M).

% cases to manage agression and defensiveness
weightedAI(A,D,G,T,M) :-                       
    A = D,                                     % case 1: A = D   --- equally likely to make A or D move 
    random(0,9,R),                             % choose a number R between 0 and 9   
    ( (R =< A , agressiveMoveAI(G,T,M))        % agressive if R =< A
    ; ( defensiveMoveAI(G,T,M)) ) .            % else defensive

weightedAI(A,D,G,T,M) :-
    A > D,                                     % case 2: A > D   --- more likely to make an A move
    random(0,9,R),                             % choose a number R between 0 and 9
    ( (R =< A, agressiveMoveAI(G,T,M))         % agressive if R =< A
    ; (R > 10-D, defensiveMoveAI(G,T,M))       % defensive if R > 10-D   // the idea is to allow for a D of 0 to mean *no* defensive moves
    ; (randomMoveAI(G,T,M)) ).                 % else random
    
weightedAI(A,D,G,T,M) :- 
    D > A,                                     % case 3: D > A   --- more likely to make a D move
    random(0,9,R),                             % choose a number R between 0 and 9
    ( (R =< D, defensiveMoveAI(G,T,M))         % defensive if R =< D
    ; (R > 10-D, agressiveMoveAI(G,T,M))       % agressive if R > 10-A   // the idea is to allow for an A of 0 to mean *no* agressive moves
    ; (randomMoveAI(G,T,M)) ).                 % else random

% final case -- make a random move    
weightedAI(_,_,G,T,M) :-
      randomMoveAI(G,T,M).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agressiveMoveAI/3  --- this AI will always attempt to go from 2 in a row to 3 in a row
agressiveMoveAI(Grid,Tile,Move) :-
      findPotentialWins(Grid,Tile,Move).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% defensiveMoveAI/3  --- this AI will always attempt to block the human from going from 2 in a row to 3 in a row
defensiveMoveAI(Grid,Tile,Move) :-
      getOppositeTile(Tile,UserTile),
      findPotentialWins(Grid,UserTile,Move).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% randomMoveAI/3  --- this AI will move in a random column (with weighting according to le diagram de Nicolas)
randomMoveAI(Grid,_,Move) :-
      random(0,6,ColNum),
      getMoveInColumn(Grid,ColNum,Move).
      
      
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% smarterAI/3 -- a smarter way to play (5/12)
%%      smarterAI(Grid,ComputersTile,BestComputerMove)

% check to see if computer has a winning move - if so, take that move
smarterAI(Grid,ComputersTile,BestComputerMove) :-
    findWinningMove(Grid,ComputersTile,BestComputerMove).

% check to see if user has any winning moves - if so, block
smarterAI(Grid,ComputersTile,BestComputerMove) :-
    getOppositeTile(ComputersTile, UsersTile),
    	findWinningMove(Grid,UsersTile,BestComputerMove). 

% move in a random column
smarterAI(Grid,_,BestComputerMove) :-
    random(0,6,ColNum),
    getMoveInColumn(Grid,ColNum,BestComputerMove).

% if none of those options work, move somewhere
smarterAI(Grid,_,BestComputerMove) :-
    validMove(Grid,BestComputerMove).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getCellXY -- transforms between cell numbers and XY coordinates.
%     getCellXY(CellNumber,X,Y).
%     Effectively a gigantic lookup table

getCellXY( 0, 0, 0). getCellXY( 1, 0, 1). getCellXY( 2, 0, 2). getCellXY( 3, 0, 3). getCellXY( 4, 0, 4). getCellXY( 5, 0, 5). 
getCellXY( 6, 1, 0). getCellXY( 7, 1, 1). getCellXY( 8, 1, 2). getCellXY( 9, 1, 3). getCellXY(10, 1, 4). getCellXY(11, 1, 5). 
getCellXY(12, 2, 0). getCellXY(13, 2, 1). getCellXY(14, 2, 2). getCellXY(15, 2, 3). getCellXY(16, 2, 4). getCellXY(17, 2, 5). 
getCellXY(18, 3, 0). getCellXY(19, 3, 1). getCellXY(20, 3, 2). getCellXY(21, 3, 3). getCellXY(22, 3, 4). getCellXY(23, 3, 5). 
getCellXY(24, 4, 0). getCellXY(25, 4, 1). getCellXY(26, 4, 2). getCellXY(27, 4, 3). getCellXY(28, 4, 4). getCellXY(29, 4, 5). 
getCellXY(30, 5, 0). getCellXY(31, 5, 1). getCellXY(32, 5, 2). getCellXY(33, 5, 3). getCellXY(34, 5, 4). getCellXY(35, 5, 5). 
getCellXY(36, 6, 0). getCellXY(37, 6, 1). getCellXY(38, 6, 2). getCellXY(39, 6, 3). getCellXY(40, 6, 4). getCellXY(41, 6, 5). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% neighbor/4 -- finds the cellNumber for the neighbor of a given cell.
%     neighbor(CellNumber,Xdelta,Ydelta,NeighborNumber)
%     fails if neighbor does not exist

neighbor(CellNumber,Xdelta,Ydelta,NeighborNumber) :-
    getCellXY(CellNumber,X,Y), % get some data
    NewX is X+Xdelta,
    NewY is Y+Ydelta,
    % our exclusion cases -- check all the bounds
    NewX >= 0, NewX =< 6,
    NewY >= 0, NewY =< 5,
    getCellXY(NeighborNumber,NewX,NewY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cellValue/3 -- retrieves the value of a certian cell in the grid
%     cellValue(Grid, CellNumber, Value).

cellValue(Grid, CellNumber, Value) :-
    cellValueHelper(Grid,CellNumber,0,Value).

cellValueHelper([X|_], CellNum, CellNum, X) :- !.
cellValueHelper([_|Tail], CellNumber, Counter, Value) :-
    NewCounter is Counter+1,
    cellValueHelper(Tail,CellNumber,NewCounter,Value).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isCellEmpty/2 -- true if the given cellNumber is empty
%     isCellEmpty(Grid, CellNumber)

isCellEmpty(Grid, CellNumber) :-
    cellValue(Grid, CellNumber, -).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% findWinningMove(Grid, UsersTile, Move)
%%     note that validMove(G,M) returns all possible moves
findWinningMove(Grid, UsersTile, Move) :-
    validMove(Grid, PossibleMove),
    isMoveWin(Grid, PossibleMove, UsersTile),
    Move is PossibleMove.



%%%%%%%%%%%%%%%
findPotentialWins(Grid, UsersTile, PotentialWinMove) :-
    validMove(Grid,Move),
	makeMove(Grid,Move,UsersTile,NewGhostGrid), %% place a tile 
    findWinningMove(NewGhostGrid, UsersTile, _), %% is this move a winning move ?
    PotentialWinMove is Move.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% isMoveWin/3 -- given a CellNumber in a Grid, determine if inserting UsersTile will put the board into a win state
%%     isMoveWin(Grid, CellNumber, UsersTile)

isMoveWin(Grid, CellNumber, UsersTile) :-
    checkVert(Grid, CellNumber, UsersTile).
    
isMoveWin(Grid, CellNumber, UsersTile) :-
    checkHoriz(Grid, CellNumber, UsersTile).
    
isMoveWin(Grid, CellNumber, UsersTile) :-
    checkDiagTLBR(Grid, CellNumber, UsersTile).
    
isMoveWin(Grid, CellNumber, UsersTile) :-
    checkDiagBLTR(Grid, CellNumber, UsersTile).



%% TODO: determine if backtracking below needs to be eliminated
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkHoriz/3 -- check for Horizontal win

checkHoriz(Grid, CellNumber, UsersTile) :-
	neighbor(CellNumber, -1, 0, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, -2, 0, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, -3, 0, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkHoriz(Grid, CellNumber, UsersTile) :-
	neighbor(CellNumber, -1, 0, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, -2, 0, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 1, 0, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkHoriz(Grid, CellNumber, UsersTile) :-
	neighbor(CellNumber, -1, 0, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, 1, 0, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 2, 0, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkHoriz(Grid, CellNumber, UsersTile) :-
	neighbor(CellNumber, 1, 0, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, 2, 0, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 3, 0, Cell3),
    cellValue(Grid, Cell3, UsersTile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkDiagTLBR/3 -- check for Diagonal win Top Left to Bottom Right

checkDiagTLBR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, -1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, -2, -2, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, -3, -3, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkDiagTLBR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, -1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, -2, -2, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 1, 1, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkDiagTLBR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, -1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, 1, 1, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 2, 2, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkDiagTLBR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, 1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, 2, 2, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 3, 3, Cell3),
    cellValue(Grid, Cell3, UsersTile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkDiagBLTR/3 -- check for Diagonal win Bottom Left to Top Right

checkDiagBLTR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, -1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, -2, 2, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, -3, 3, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkDiagBLTR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, -1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, -2, 2, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 1, -1, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkDiagBLTR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, -1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, 1, -1, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 2, -2, Cell3),
    cellValue(Grid, Cell3, UsersTile).

checkDiagBLTR(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, 1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
	neighbor(CellNumber, 2, -2, Cell2),
    cellValue(Grid, Cell2, UsersTile),
	neighbor(CellNumber, 3, -3, Cell3),
    cellValue(Grid, Cell3, UsersTile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkVert/3 -- check for Vertical win

checkVert(Grid, CellNumber, UsersTile) :-
    neighbor(CellNumber, 0, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    neighbor(CellNumber, 0, 2, Cell2),
    cellValue(Grid, Cell2, UsersTile),
    neighbor(CellNumber, 0, 3, Cell3),
    cellValue(Grid, Cell3, UsersTile).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% some print stuff from the spec
welcomeScreen :-
    write('Welcome to Prolog Connect 4! Have a lot of fun...\n\n'),
    aiSelect.

chooseGamePieceScreen :-
    write('x - plays first\no - plays second\n\nChoose your game piece or type "q" to quit: ').

userMoveScreen :-
    write('It\'s your turn.\nEnter a column number to place a piece or type "q" to quit.\n\nWhat would you like to do? ').
    
aiSelectScreen :-
    write('Select the Computer you\'d like to challenge: \n\n1 - George Boole (basic strategy)\n2 - Richard Stallman (aggressive style)\n3 - Linus Torvalds (defensive style)\n4 - Alain Colmerauer (random)\n\nEnter a number or "q" to quit: ').

goodbyeScreen :-
    statUserWins(UserWins),
    statCPUWins(CPUWins),
    statTieGames(Ties),
    write('Statistics\n-------------\nYour wins: '),
    write(UserWins),nl,
    write('CPU wins: '),
    write(CPUWins),nl,
    write('Tie games: '),
    write(Ties),nl,
    write('-------------\n\nThanks for playing!\n\n').

gameOverOptions :-
    write('You can do one of the following:\np - play again\nq - quit\n\nWhat would you like to do? ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fullCheck -- if the grid is full tell the user and ask what he wants to do next
fullCheck(Grid) :-
    \+validMove(Grid,_),
    printGrid(Grid),
    write('The board is full. It\'s a draw.\n\n'),
    gameOver.
    
fullCheck(_). %% if the above clause fails, fullCheck should still succeed for the purpose of backtracking.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% winCheck(Grid, Tile, CellNumber, MessageIfWin) -- return true if the board is in a win state for the given Tile
winCheck(Grid, Tile, CellNumber, 'Congratulations, you won!\n\n') :-
    isMoveWin(Grid, CellNumber, Tile),
    !,
    showWin(Grid,CellNumber,Tile,GridBold),
    clearScreen,
    printGrid(GridBold),
    write('Congratulations, you won!\n\n'),
    incrementUserWins,
    gameOver.

winCheck(Grid, Tile, CellNumber, 'Sorry, the computer won.\n\n') :-
    isMoveWin(Grid, CellNumber, Tile),
    !,
    showWin(Grid,CellNumber,Tile,GridBold),
    clearScreen,
    printGrid(GridBold),
    write('Sorry, the computer won.\n\n'),
    incrementCPUWins,
    gameOver.

winCheck(_,_,_,_). %% if the above clauses fail, winCheck should still succeed for the purpose of backtracking.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% showWin(Grid, CellNumber, Tile) %%print special characters to show winning combination
showWin(Grid, CellNumber, UsersTile, NewGrid) :-
    checkVert(Grid, CellNumber, UsersTile),
    changeVert(Grid, CellNumber, UsersTile, NewGrid).
    
showWin(Grid, CellNumber, UsersTile, NewGrid) :-
    checkHoriz(Grid, CellNumber, UsersTile),
    changeHoriz(Grid, CellNumber, UsersTile, NewGrid).

showWin(Grid, CellNumber, UsersTile, NewGrid) :-
    checkDiagTLBR(Grid, CellNumber, UsersTile),
    changeDiagTLBR(Grid, CellNumber, UsersTile, NewGrid).
    
showWin(Grid, CellNumber, UsersTile, NewGrid) :-
    checkDiagBLTR(Grid, CellNumber, UsersTile),
    changeDiagBLTR(Grid, CellNumber, UsersTile, NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% changeHoriz/3 -- change for Horizontal win

changeHoriz(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
	neighbor(CellNumber, -1, 0, Cell1),
	cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, -2, 0, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, -3, 0, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeHoriz(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
	neighbor(CellNumber, -1, 0, Cell1),
	cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, -2, 0, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 1, 0, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeHoriz(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
	neighbor(CellNumber, -1, 0, Cell1),
	cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, 1, 0, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 2, 0, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeHoriz(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
	neighbor(CellNumber, 1, 0, Cell1),
	cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, 2, 0, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 3, 0, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkDiagTLBR/3 -- check for Diagonal win Top Left to Bottom Right

changeDiagTLBR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, -1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, -2, -2, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, -3, -3, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeDiagTLBR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, -1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, -2, -2, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 1, 1, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeDiagTLBR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, -1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, 1, 1, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 2, 2, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeDiagTLBR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, 1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, 2, 2, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 3, 3, Cell3),
	cellValue(Grid, Cell13, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checkDiagBLTR/3 -- check for Diagonal win Bottom Left to Top Right

changeDiagBLTR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, -1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, -2, 2, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, -3, 3, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeDiagBLTR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, -1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, -2, 2, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 1, -1, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeDiagBLTR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, -1, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, 1, -1, Cell2),
	cellValue(Grid, Cell2, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 2, -2, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeDiagBLTR(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, 1, -1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
	neighbor(CellNumber, 2, -2, Cell2),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
	neighbor(CellNumber, 3, -3, Cell3),
	cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).

changeVert(Grid, CellNumber, UsersTile, NewGrid) :-
    getWinChar(UsersTile, UpTile),
    mM_placeAtom(0,Grid,CellNumber,UpTile,Grid1),
    neighbor(CellNumber, 0, 1, Cell1),
    cellValue(Grid, Cell1, UsersTile),
    mM_placeAtom(0,Grid1,Cell1,UpTile,Grid2),
    neighbor(CellNumber, 0, 2, Cell2),
    cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid2,Cell2,UpTile,Grid3),
    neighbor(CellNumber, 0, 3, Cell3),
    cellValue(Grid, Cell3, UsersTile),
    mM_placeAtom(0,Grid3,Cell3,UpTile,NewGrid).


%% getWinChar(UsersTile, UpTile) :- -- determine the opposite of Tile
getWinChar(x, 'X').
getWinChar(o, 'O').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gameOver -- give user options when the game is over and process input 
gameOver :-
    repeat,
    gameOverOptions,
    get(Char),
    nl,
    processGameOverInput(Char).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this terminates the game after printing a parting message
endGame :-
    goodbyeScreen,
    halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this is the entry poin for the game
play :-
    initColor,
    initScreenSize,
    initStatistics,
    welcomeScreen.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% take us to the chooseGamePieceScreen when the user enters a 'p', and end the game when they enter a 'q'
processGameOverInput(112) :- % 'p'
    aiSelect.

processGameOverInput(113) :- % 'q'
    endGame.

processGameOverInput(81) :- % 'Q'
    endGame.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% aiSelect
aiSelect :-
    repeat,
    aiSelectScreen,
    get(Char),
    nl,
    processAISelection(Char).
    
processAISelection(Char) :- % number 1-4
    Id is Char - 48,
    Id >0,Id <5,
    chooseGamePiece(Id).

processAISelection(113) :- % 'q'
    endGame.
    
processAISelection(81) :- % 'Q'
    endGame.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% chooseGamePiece -- allow the user to pick x or o to play the game with
chooseGamePiece(Ai_id) :-
    repeat,
    chooseGamePieceScreen,
    get(Char),
    nl,
    processGamePieceSelection(Ai_id, Char).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% processGamePieceSelection/1
%%    x takes us to mainLoop with a blankGrid
%%    o gets a blank grid, computer makes a move -> grid1, then go to mainLoop with grid1.
processGamePieceSelection(Ai_id,120) :- % 'x'
    blankGrid(G),
    clearScreen,
    mainLoop(Ai_id,G,x).

processGamePieceSelection(Ai_id,111) :- % 'o'
    blankGrid(G),
    clearScreen,
    computerMove(Ai_id,G,o,NewGrid),
    mainLoop(Ai_id,NewGrid,o).
    
processGamePieceSelection(_,113) :- % 'q'
	endGame.

processGamePieceSelection(_,81) :- % 'Q'
	endGame.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% mainLoop/2 -- the main loop in our program
mainLoop(AI_ID,Grid0,UserTile) :-
    !,
    printGrid(Grid0),
    userMove(Grid0, UserTile, Grid1),
    clearScreen,
    printGrid(Grid1),
    nl,nl,nl,
    cpuWaitTime(S),
    sleep(S),
    clearScreen,
    computerMove(AI_ID,Grid1, UserTile, Grid2),
    mainLoop(AI_ID,Grid2,UserTile).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% userMove/3 -- prompts for input, makes the users move, and processes win and tie conditions.
%%                  returns to mainLoop if no problems occur
userMove(Grid,UserTile,NewGrid) :-
    repeat,
    userMoveScreen,
    get(Col),
    nl,
    processColumnSelection(Grid,UserTile,NewGrid, Col).

processColumnSelection(_,_,_,113) :- % 'q'
    endGame.

processColumnSelection(_,_,_,81) :- % 'Q'
    endGame.
    
processColumnSelection(Grid,UserTile,NewGrid, Char) :-
    number(Char),
    ColNum is Char - 49,
    ColNum < 7, ColNum >= 0, % check bounds
    getMoveInColumn(Grid,ColNum,Move), !,
    % assert: the user has chosen a column to move in, and that gives a valid cell
    makeMove(Grid,Move,UserTile,NewGrid),
    winCheck(NewGrid,UserTile,Move,'Congratulations, you won!\n\n'),
    fullCheck(NewGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ComputerMove/3
computerMove(AI_ID,Grid,UserTile,NewGrid) :-
    getOppositeTile(UserTile, CompTile),
    moveAsAI(AI_ID,Grid,CompTile,BestComputerMove),
    getCellXY(BestComputerMove,X,_),
    Col is X + 1,
    cpuPlayer(Name,AI_ID,_,_),
    write(Name),
    write(' has placed a piece in column '),
    write(Col),
    write('.\n\n'),
    makeMove(Grid,BestComputerMove,CompTile,NewGrid),
    winCheck(NewGrid,CompTile,BestComputerMove,'Sorry, the computer won.\n\n'),
    fullCheck(NewGrid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getOppositeTile(Tile, ResTile)/2 -- determine the opposite of Tile
getOppositeTile(x, o).
getOppositeTile(o, x).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getMoveInColumn/3 -- a helper method which finds the lowest possible move in a column
getMoveInColumn(Grid,ColNum,Move) :-
    Move is ColNum*6 + 5,
    validMove(Grid,Move).
getMoveInColumn(Grid,ColNum,Move) :-
    Move is ColNum*6 + 4,
    validMove(Grid,Move).
getMoveInColumn(Grid,ColNum,Move) :-
    Move is ColNum*6 + 3,
    validMove(Grid,Move).
getMoveInColumn(Grid,ColNum,Move) :-
    Move is ColNum*6 + 2,
    validMove(Grid,Move).
getMoveInColumn(Grid,ColNum,Move) :-
    Move is ColNum*6 + 1,
    validMove(Grid,Move).
getMoveInColumn(Grid,ColNum,Move) :-
    Move is ColNum*6 + 0,
    validMove(Grid,Move).

