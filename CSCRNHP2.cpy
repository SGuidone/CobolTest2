000100***************************************************************** CSCRNHP2
000200*                                                               * CSCRNHP2
000300*  Copyright(C) 1998-2006 Micro Focus. All Rights Reserved.     * CSCRNHP2
000400*                                                               * CSCRNHP2
000500***************************************************************** CSCRNHP2
000600                                                                  CSCRNHP2
000700***************************************************************** CSCRNHP2
000800* CSCRNHP2.CPY                                                  * CSCRNHP2
000900*---------------------------------------------------------------* CSCRNHP2
001000* Procedure code to populate screen titles                      * CSCRNHP2
001100***************************************************************** CSCRNHP2
001200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         CSCRNHP2
001300     MOVE SCREEN-TITLE1 TO AHEAD1O IN <<SCRN>>.                   CSCRNHP2
001400     MOVE SCREEN-TITLE2 TO AHEAD2O IN <<SCRN>>.                   CSCRNHP2
001500     MOVE WS-TRAN-ID TO ATRANO IN <<SCRN>>.                       CSCRNHP2
001600     MOVE DD-TIME-OUTPUT TO ATIMEO IN <<SCRN>>.                   CSCRNHP2
001700     MOVE DDO-DATA TO ADATEO IN <<SCRN>>.                         CSCRNHP2
001800* Move in any error message                                       CSCRNHP2
001900* Move in screen specific fields                                  CSCRNHP2
002000        MOVE :OPTN:-HELP-LINE (01) TO AHLP01O IN <<SCRN>>.        CSCRNHP2
002100        MOVE :OPTN:-HELP-LINE (02) TO AHLP02O IN <<SCRN>>.        CSCRNHP2
002200        MOVE :OPTN:-HELP-LINE (03) TO AHLP03O IN <<SCRN>>.        CSCRNHP2
002300        MOVE :OPTN:-HELP-LINE (04) TO AHLP04O IN <<SCRN>>.        CSCRNHP2
002400        MOVE :OPTN:-HELP-LINE (05) TO AHLP05O IN <<SCRN>>.        CSCRNHP2
002500        MOVE :OPTN:-HELP-LINE (06) TO AHLP06O IN <<SCRN>>.        CSCRNHP2
002600        MOVE :OPTN:-HELP-LINE (07) TO AHLP07O IN <<SCRN>>.        CSCRNHP2
002700        MOVE :OPTN:-HELP-LINE (08) TO AHLP08O IN <<SCRN>>.        CSCRNHP2
002800        MOVE :OPTN:-HELP-LINE (09) TO AHLP09O IN <<SCRN>>.        CSCRNHP2
002900        MOVE :OPTN:-HELP-LINE (10) TO AHLP10O IN <<SCRN>>.        CSCRNHP2
003000        MOVE :OPTN:-HELP-LINE (11) TO AHLP11O IN <<SCRN>>.        CSCRNHP2
003100        MOVE :OPTN:-HELP-LINE (12) TO AHLP12O IN <<SCRN>>.        CSCRNHP2
003200        MOVE :OPTN:-HELP-LINE (13) TO AHLP13O IN <<SCRN>>.        CSCRNHP2
003300        MOVE :OPTN:-HELP-LINE (14) TO AHLP14O IN <<SCRN>>.        CSCRNHP2
003400        MOVE :OPTN:-HELP-LINE (15) TO AHLP15O IN <<SCRN>>.        CSCRNHP2
003500        MOVE :OPTN:-HELP-LINE (16) TO AHLP16O IN <<SCRN>>.        CSCRNHP2
003600        MOVE :OPTN:-HELP-LINE (17) TO AHLP17O IN <<SCRN>>.        CSCRNHP2
003700        MOVE :OPTN:-HELP-LINE (18) TO AHLP18O IN <<SCRN>>.        CSCRNHP2
003800        MOVE :OPTN:-HELP-LINE (19) TO AHLP19O IN <<SCRN>>.        CSCRNHP2
003900* Turn colour off if required                                     CSCRNHP2
004000     IF COLOUR-OFF                                                CSCRNHP2
004100        MOVE DFHGREEN TO ATXT01C IN <<SCRN>>                      CSCRNHP2
004200        MOVE DFHGREEN TO ASCRNC IN <<SCRN>>                       CSCRNHP2
004300        MOVE DFHGREEN TO AHEAD1C IN <<SCRN>>                      CSCRNHP2
004400        MOVE DFHGREEN TO ADATEC IN <<SCRN>>                       CSCRNHP2
004500        MOVE DFHGREEN TO ATXT02C IN <<SCRN>>                      CSCRNHP2
004600        MOVE DFHGREEN TO ATRANC IN <<SCRN>>                       CSCRNHP2
004700        MOVE DFHGREEN TO AHEAD2C IN <<SCRN>>                      CSCRNHP2
004800        MOVE DFHGREEN TO ATIMEC IN <<SCRN>>                       CSCRNHP2
004900        MOVE DFHGREEN TO AHLP01C IN <<SCRN>>                      CSCRNHP2
005000        MOVE DFHGREEN TO AHLP02C IN <<SCRN>>                      CSCRNHP2
005100        MOVE DFHGREEN TO AHLP03C IN <<SCRN>>                      CSCRNHP2
005200        MOVE DFHGREEN TO AHLP04C IN <<SCRN>>                      CSCRNHP2
005300        MOVE DFHGREEN TO AHLP05C IN <<SCRN>>                      CSCRNHP2
005400        MOVE DFHGREEN TO AHLP06C IN <<SCRN>>                      CSCRNHP2
005500        MOVE DFHGREEN TO AHLP07C IN <<SCRN>>                      CSCRNHP2
005600        MOVE DFHGREEN TO AHLP08C IN <<SCRN>>                      CSCRNHP2
005700        MOVE DFHGREEN TO AHLP09C IN <<SCRN>>                      CSCRNHP2
005800        MOVE DFHGREEN TO AHLP10C IN <<SCRN>>                      CSCRNHP2
005900        MOVE DFHGREEN TO AHLP11C IN <<SCRN>>                      CSCRNHP2
006000        MOVE DFHGREEN TO AHLP12C IN <<SCRN>>                      CSCRNHP2
006100        MOVE DFHGREEN TO AHLP13C IN <<SCRN>>                      CSCRNHP2
006200        MOVE DFHGREEN TO AHLP14C IN <<SCRN>>                      CSCRNHP2
006300        MOVE DFHGREEN TO AHLP15C IN <<SCRN>>                      CSCRNHP2
006400        MOVE DFHGREEN TO AHLP16C IN <<SCRN>>                      CSCRNHP2
006500        MOVE DFHGREEN TO AHLP17C IN <<SCRN>>                      CSCRNHP2
006600        MOVE DFHGREEN TO AHLP18C IN <<SCRN>>                      CSCRNHP2
006700        MOVE DFHGREEN TO AHLP19C IN <<SCRN>>                      CSCRNHP2
006800        MOVE DFHGREEN TO ATXT03C IN <<SCRN>>                      CSCRNHP2
006900     END-IF.                                                      CSCRNHP2
007000                                                                  CSCRNHP2
007100* $ Version 5.90a sequenced on Friday 1 Dec 2006 at 6:00pm        CSCRNHP2
