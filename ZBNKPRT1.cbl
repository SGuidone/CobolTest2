000100***************************************************************** zbnkprt1
000200*                                                               * zbnkprt1
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * zbnkprt1
000400*   This demonstration program is provided for use by users     * zbnkprt1
000500*   of Micro Focus products and may be used, modified and       * zbnkprt1
000600*   distributed as part of your application provided that       * zbnkprt1
000700*   you properly acknowledge the copyright of Micro Focus       * zbnkprt1
000800*   in this material.                                           * zbnkprt1
000900*                                                               * zbnkprt1
001000***************************************************************** zbnkprt1
001100                                                                  zbnkprt1
001200***************************************************************** zbnkprt1
001300* Program:     ZBNKPRT1.CBL                                     * zbnkprt1
001400* Function:    Print the bank statements                        * zbnkprt1
      *  
001500***************************************************************** zbnkprt1
001600 IDENTIFICATION DIVISION.                                         zbnkprt1
001700 PROGRAM-ID.                                                      zbnkprt1
001800     ZBNKPRT1.                                                    zbnkprt1
001900 DATE-WRITTEN.                                                    zbnkprt1
002000     September 2002.                                              zbnkprt1
002100 DATE-COMPILED.                                                   zbnkprt1
002200     Today.                                                       zbnkprt1
002300                                                                  zbnkprt1
002400 ENVIRONMENT DIVISION.                                            zbnkprt1
002500 INPUT-OUTPUT   SECTION.                                          zbnkprt1
002600   FILE-CONTROL.                                                  zbnkprt1
002700     SELECT EXTRACT-FILE                                          zbnkprt1
002800            ASSIGN       TO EXTRACT                               zbnkprt1
002900            ORGANIZATION IS SEQUENTIAL                            zbnkprt1
003000            ACCESS MODE  IS SEQUENTIAL                            zbnkprt1
003100            FILE STATUS  IS WS-EXTRACT-STATUS.                    zbnkprt1
003200     SELECT PRINTOUT-FILE                                         zbnkprt1
003300            ASSIGN       TO PRINTOUT                              zbnkprt1
003400            ORGANIZATION IS SEQUENTIAL                            zbnkprt1
003500            ACCESS MODE  IS SEQUENTIAL                            zbnkprt1
003600            FILE STATUS  IS WS-PRINTOUT-STATUS.                   zbnkprt1
003700                                                                  zbnkprt1
003800 DATA DIVISION.                                                   zbnkprt1
003900 FILE SECTION.                                                    zbnkprt1
004000                                                                  zbnkprt1
004100 FD  EXTRACT-FILE                                                 zbnkprt1
004200     RECORDING MODE IS V                                          zbnkprt1
004300     RECORD CONTAINS 66 TO 95 CHARACTERS.                         zbnkprt1
004400 COPY CBANKXT1.                                                   zbnkprt1
004500                                                                  zbnkprt1
004600 FD  PRINTOUT-FILE.                                               zbnkprt1
004700 01  PRINTOUT-REC                            PIC X(121).          zbnkprt1
004800                                                                  zbnkprt1
004900 WORKING-STORAGE SECTION.                                         zbnkprt1
005000 COPY CTIMERD.                                                    zbnkprt1
005100                                                                  zbnkprt1
005200 01  WS-DATE-WORK-AREA.                                           zbnkprt1
005300 COPY CDATED.                                                     zbnkprt1
005400                                                                  zbnkprt1
005500 01  WS-MISC-STORAGE.                                             zbnkprt1
005600   05  WS-PROGRAM-ID                         PIC X(8)             zbnkprt1
005700       VALUE 'ZBNKPRT1'.                                          zbnkprt1
005800   05  WS-EXTRACT-STATUS.                                         zbnkprt1
005900     10  WS-EXTRACT-STAT1                    PIC X(1).            zbnkprt1
006000     10  WS-EXTRACT-STAT2                    PIC X(1).            zbnkprt1
006100                                                                  zbnkprt1
006200   05  WS-PRINTOUT-STATUS.                                        zbnkprt1
006300     10  WS-PRINTOUT-STAT1                   PIC X(1).            zbnkprt1
006400     10  WS-PRINOUTY-STAT2                   PIC X(1).            zbnkprt1
006500                                                                  zbnkprt1
006600   05  WS-IO-STATUS.                                              zbnkprt1
006700     10  WS-IO-STAT1                         PIC X(1).            zbnkprt1
006800     10  WS-IO-STAT2                         PIC X(1).            zbnkprt1
006900                                                                  zbnkprt1
007000   05  WS-TWO-BYTES.                                              zbnkprt1
007100     10  WS-TWO-BYTES-LEFT                   PIC X(1).            zbnkprt1
007200     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            zbnkprt1
007300   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  zbnkprt1
007400                                             PIC 9(1) COMP.       zbnkprt1
007500                                                                  zbnkprt1
007600   05  WS-SAVED-EMAIL                        PIC X(30).           zbnkprt1
007700   05  WS-EMAIL-INDICATOR                    PIC X(1).            zbnkprt1
007800     88  EMAIL-REQUIRED                      VALUE 'Y'.           zbnkprt1
007900     88  EMAIL-NOT-REQUIRED                  VALUE 'N'.           zbnkprt1
008000                                                                  zbnkprt1
008100   05  WS-FIRST-REC                          PIC X(3)             zbnkprt1
008200       VALUE 'YES'.                                               zbnkprt1
008300                                                                  zbnkprt1
008400   05  WS-END-OF-FILE                        PIC X(3)             zbnkprt1
008500       VALUE 'NO '.                                               zbnkprt1
008600                                                                  zbnkprt1
008700   05  WS-RECORDS-READ                       PIC 9(5)             zbnkprt1
008800       VALUE ZERO.                                                zbnkprt1
008900                                                                  zbnkprt1
009000   05  WS-TXNS-FLAG                          PIC X(1).            zbnkprt1
009100     88  TXNS-PRINTED                        VALUE '1'.           zbnkprt1
009200     88  NO-TXNS-PRINTED                     VALUE '0'.           zbnkprt1
009300                                                                  zbnkprt1
009400   05  WS-SUB1                               PIC 9(3).            zbnkprt1
009500   05  WS-SYS-DATE                           PIC 9(5).            zbnkprt1
009600   05  WS-SYS-TIME                           PIC 9(8).            zbnkprt1
009700   05  WS-PRINTED.                                                zbnkprt1
009800     10  WS-PRINTED-DATE.                                         zbnkprt1
009900       15  FILLER                            PIC X(9)             zbnkprt1
010000           VALUE 'Printed: '.                                     zbnkprt1
010100       15  WS-PRINT-DATE                     PIC X(11)            zbnkprt1
010200           VALUE 'dd mmm yyyy'.                                   zbnkprt1
010300     10  WS-PRINTED-TIME.                                         zbnkprt1
010400       15  FILLER                            PIC X(12)            zbnkprt1
010500           VALUE SPACES.                                          zbnkprt1
010600       15  WS-PRINT-TIME.                                         zbnkprt1
010700         20  WS-PRINT-TIME-HH                PIC X(2).            zbnkprt1
010800         20  WS-PRINT-TIME-DOT1              PIC X(1).            zbnkprt1
010900         20  WS-PRINT-TIME-MM                PIC X(2).            zbnkprt1
011000         20  WS-PRINT-TIME-DOT2              PIC X(1).            zbnkprt1
011100         20  WS-PRINT-TIME-SS                PIC X(2).            zbnkprt1
011200   05  WS-TOTAL-TXNS                         PIC S9(7)V99 COMP-3. zbnkprt1
011300   05  WS-TOTAL-ASSETS                       PIC S9(7)V99 COMP-3. zbnkprt1
011400                                                                  zbnkprt1
011500                                                                  zbnkprt1
011600 01  WS-PRINT-LINES.                                              zbnkprt1
011700   05  WS-LINE1.                                                  zbnkprt1
011800     10  WS-LINE1-CC                         PIC X(1)             zbnkprt1
011900         VALUE '1'.                                               zbnkprt1
012000     10  FILLER                              PIC X(40)            zbnkprt1
012100         VALUE SPACES.                                            zbnkprt1
012200     10  WS-LINE1-HEAD                       PIC X(21)            zbnkprt1
012300         VALUE 'Micro Focus Demo Bank'.                           zbnkprt1
012400                                                                  zbnkprt1
012500   05  WS-LINE2.                                                  zbnkprt1
012600     10  WS-LINE2-CC                         PIC X(1)             zbnkprt1
012700         VALUE ' '.                                               zbnkprt1
012800     10  FILLER                              PIC X(40)            zbnkprt1
012900         VALUE SPACES.                                            zbnkprt1
013000     10  WS-LINE1-HEAD                       PIC X(20)            zbnkprt1
013100         VALUE 'Statement of Account'.                            zbnkprt1
013200                                                                  zbnkprt1
013300   05  WS-LINE3.                                                  zbnkprt1
013400     10  WS-LINE3-CC                         PIC X(1)             zbnkprt1
013500         VALUE '0'.                                               zbnkprt1
013600     10  WS-LINE3-NAME-ADDR                  PIC X(23)            zbnkprt1
013700         VALUE SPACES.                                            zbnkprt1
013800     10  FILLER                              PIC X(55)            zbnkprt1
013900         VALUE SPACES.                                            zbnkprt1
014000     10  WS-LINE3-DATE                       PIC X(20)            zbnkprt1
014100         VALUE SPACES.                                            zbnkprt1
014200                                                                  zbnkprt1
014300   05  WS-LINE4.                                                  zbnkprt1
014400     10  WS-LINE4-CC                         PIC X(1)             zbnkprt1
014500         VALUE '0'.                                               zbnkprt1
014600     10  FILLER                              PIC X(14)            zbnkprt1
014700         VALUE 'Account No.'.                                     zbnkprt1
014800     10  FILLER                              PIC X(38)            zbnkprt1
014900         VALUE 'Description '.                                    zbnkprt1
015000     10  FILLER                              PIC X(15)            zbnkprt1
015100         VALUE '    Date  '.                                      zbnkprt1
015200     10  FILLER                              PIC X(18)            zbnkprt1
015300         VALUE '      Amount '.                                   zbnkprt1
015400     10  FILLER                              PIC X(18)            zbnkprt1
015500         VALUE '     Balance '.                                   zbnkprt1
015600                                                                  zbnkprt1
015700   05  WS-LINE5.                                                  zbnkprt1
015800     10  WS-LINE5-CC                         PIC X(1).            zbnkprt1
015900     10  WS-LINE5-ACC-NO                     PIC X(9).            zbnkprt1
016000     10  FILLER                              PIC X(5).            zbnkprt1
016100     10  WS-LINE5-DESC.                                           zbnkprt1
016200       15  WS-LINE5-DESC-PT1                 PIC X(15).           zbnkprt1
016300       15  WS-LINE5-DESC-PT2                 PIC X(18).           zbnkprt1
016400     10  FILLER                              PIC X(5).            zbnkprt1
016500     10  WS-LINE5-DATE                       PIC X(11).           zbnkprt1
016600     10  FILLER                              PIC X(4).            zbnkprt1
016700     10  WS-LINE5-AMOUNT-DASH                PIC X(13).           zbnkprt1
016800     10  WS-LINE5-AMOUNT REDEFINES WS-LINE5-AMOUNT-DASH           zbnkprt1
016900                                             PIC Z,ZZZ,ZZ9.99-.   zbnkprt1
017000     10  FILLER                              PIC X(5).            zbnkprt1
017100     10  WS-LINE5-BALANCE-DASH               PIC X(13).           zbnkprt1
017200     10  WS-LINE5-BALANCE REDEFINES WS-LINE5-BALANCE-DASH         zbnkprt1
017300                                             PIC Z,ZZZ,ZZZ.99-.   zbnkprt1
017400                                                                  zbnkprt1
017500 01  WS-CONSOLE-MESSAGE                      PIC X(48).           zbnkprt1
017600                                                                  zbnkprt1
017700 01  WS-EXEC-PARM.                                                zbnkprt1
017800   05  WS-EXEC-PARM-LL                       PIC S9(4) COMP.      zbnkprt1
017900   05  WS-EXEC-PARM-DATA                     PIC X(12).           zbnkprt1
018000                                                                  zbnkprt1
018100 COPY CSTATESD.                                                   zbnkprt1
018200                                                                  zbnkprt1
018300 COPY CABENDD.                                                    zbnkprt1
018400                                                                  zbnkprt1
018500 01  WS-PARM-PTR                             POINTER.             zbnkprt1
018600 01  WS-PARM-PTR-NUM REDEFINES WS-PARM-PTR   PIC 9(4) COMP.       zbnkprt1
018700                                                                  zbnkprt1
018800 01  WS-LE-AREAS.                                                 zbnkprt1
018900   05  WS-CEE3DMP-AREAS.                                          zbnkprt1
019000     10  WS-CEE3DMP-DMP-TITLE                PIC X(80)            zbnkprt1
019100         VALUE 'CEEDUMP FROM HANDLER ROUTINE'.                    zbnkprt1
019200     10  WS-CEE3DMP-DMP-OPTIONS              PIC X(255)           zbnkprt1
019300         VALUE 'TRACE FILE VAR STOR'.                             zbnkprt1
019400     10  WS-CEE3DMP-FEEDBACK.                                     zbnkprt1
019500      15 WS-CEE3DMP-FB-SEV                   PIC S9(4) COMP.      zbnkprt1
019600      15 WS-CEE3DMP-FB-MSGNO                 PIC S9(4) COMP.      zbnkprt1
019700      15 WS-CEE3DMP-FB-CASE-SEV              PIC X(1).            zbnkprt1
019800      15 WS-CEE3DMP-FB-FAC-ID                PIC X(3).            zbnkprt1
019900      15 WS-CEE3DMP-FB-ISINFO                PIC S9(8) COMP.      zbnkprt1
020000   05  WS-CEELOCT-AREAS.                                          zbnkprt1
020100     10  WS-CEELOCT-DATE-LILIAN              PIC S9(9) BINARY.    zbnkprt1
020200     10  WS-CEELOCT-SECS-LILIAN              PIC S9(9) COMP.      zbnkprt1
020300     10  WS-CEELOCT-TIME-GREGORIAN           PIC X(17).           zbnkprt1
020400     10  WS-CEELOCT-FEEDBACK.                                     zbnkprt1
020500      15 WS-CEELOCT-FB-SEV                   PIC S9(4) COMP.      zbnkprt1
020600      15 WS-CEELOCT-FB-MSGNO                 PIC S9(4) COMP.      zbnkprt1
020700      15 WS-CEELOCT-FB-CASE-SEV              PIC X(1).            zbnkprt1
020800      15 WS-CEELOCT-FB-FAC-ID                PIC X(3).            zbnkprt1
020900      15 WS-CEELOCTRFB-ISINFO                PIC S9(8) COMP.      zbnkprt1
021000                                                                  zbnkprt1
021100 LINKAGE SECTION.                                                 zbnkprt1
021200 01  LK-EXEC-PARM.                                                zbnkprt1
021300   05  LK-EXEC-PARM-LL                       PIC S9(4) COMP.      zbnkprt1
021400   05  LK-EXEC-PARM-DATA                     PIC X(12).           zbnkprt1
021500                                                                  zbnkprt1
021600 PROCEDURE DIVISION USING LK-EXEC-PARM.                           zbnkprt1
021700                                                                  zbnkprt1
021800     PERFORM RUN-TIME.                                            zbnkprt1
021900                                                                  zbnkprt1
022000     MOVE ZEROES TO WS-EXEC-PARM-LL.                              zbnkprt1
022100     MOVE SPACES TO WS-EXEC-PARM-DATA.                            zbnkprt1
022200                                                                  zbnkprt1
022300     SET WS-PARM-PTR TO ADDRESS OF LK-EXEC-PARM.                  zbnkprt1
022400     IF WS-PARM-PTR-NUM IS NOT EQUAL TO ZEROS                     zbnkprt1
022500        MOVE LK-EXEC-PARM-LL TO WS-EXEC-PARM-LL                   zbnkprt1
022600        IF WS-EXEC-PARM-LL IS GREATER THAN                        zbnkprt1
022700             LENGTH OF WS-EXEC-PARM-DATA                          zbnkprt1
022800           MOVE LENGTH OF WS-EXEC-PARM-DATA TO WS-EXEC-PARM-LL    zbnkprt1
022900        END-IF                                                    zbnkprt1
023000        IF WS-EXEC-PARM-LL IS GREATER THAN ZERO                   zbnkprt1
023100           MOVE LK-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             zbnkprt1
023200             TO WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             zbnkprt1
023300        END-IF                                                    zbnkprt1
023400     END-IF.                                                      zbnkprt1
023500                                                                  zbnkprt1
023600     SET EMAIL-NOT-REQUIRED TO TRUE.                              zbnkprt1
023700     IF WS-EXEC-PARM-LL IS EQUAL TO ZERO                          zbnkprt1
023800        MOVE 'No exec card parm present'                          zbnkprt1
023900          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
024000        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
024100     ELSE                                                         zbnkprt1
024200       MOVE SPACES TO WS-CONSOLE-MESSAGE                          zbnkprt1
024300       STRING 'Exec parm is "' DELIMITED BY SIZE                  zbnkprt1
024400              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)               zbnkprt1
024500                DELIMITED BY SIZE                                 zbnkprt1
024600              '"' DELIMITED BY SIZE                               zbnkprt1
024700         INTO WS-CONSOLE-MESSAGE                                  zbnkprt1
024800       PERFORM DISPLAY-CONSOLE-MESSAGE                            zbnkprt1
024900     END-IF.                                                      zbnkprt1
025000                                                                  zbnkprt1
025100     IF FUNCTION UPPER-CASE(WS-EXEC-PARM-DATA) IS EQUAL TO 'EMAIL'zbnkprt1
025200        SET EMAIL-REQUIRED TO TRUE                                zbnkprt1
025300     END-IF.                                                      zbnkprt1
025400                                                                  zbnkprt1
025500     ACCEPT WS-SYS-DATE FROM DAY.                                 zbnkprt1
025600     SET DD-ENV-NULL TO TRUE.                                     zbnkprt1
025700     SET DDI-YYDDD TO TRUE.                                       zbnkprt1
025800     MOVE WS-SYS-DATE TO DDI-DATA.                                zbnkprt1
025900     SET DDO-DD-MMM-YYYY TO TRUE.                                 zbnkprt1
026000     CALL 'UDATECNV' USING WS-DATE-WORK-AREA.                     zbnkprt1
026100     MOVE FUNCTION LOWER-CASE(DDO-DATA-DD-MMM-YYYY-MMM(2:2))      zbnkprt1
026200       TO DDO-DATA-DD-MMM-YYYY-MMM(2:2).                          zbnkprt1
026300     MOVE DDO-DATA TO WS-PRINT-DATE.                              zbnkprt1
026400                                                                  zbnkprt1
026500     PERFORM EXTRACT-OPEN.                                        zbnkprt1
026600     PERFORM PRINTOUT-OPEN.                                       zbnkprt1
026700                                                                  zbnkprt1
026800     PERFORM UNTIL WS-END-OF-FILE = 'YES'                         zbnkprt1
026900       IF WS-END-OF-FILE = 'NO '                                  zbnkprt1
027000          PERFORM EXTRACT-GET                                     zbnkprt1
027100          IF WS-END-OF-FILE = 'NO '                               zbnkprt1
027200             ADD 1 TO WS-RECORDS-READ                             zbnkprt1
027300             IF WS-RECORDS-READ IS LESS THAN 6                    zbnkprt1
027400                DISPLAY BANKXT01-REC1 UPON CONSOLE                zbnkprt1
027500             ELSE                                                 zbnkprt1
027600                IF WS-RECORDS-READ IS EQUAL TO 6                  zbnkprt1
027700                   MOVE 'Suppressing record display...'           zbnkprt1
027800                      TO WS-CONSOLE-MESSAGE                       zbnkprt1
027900                   PERFORM DISPLAY-CONSOLE-MESSAGE                zbnkprt1
028000                END-IF                                            zbnkprt1
028100             END-IF                                               zbnkprt1
028200             PERFORM FORMAT-AND-PRINT                             zbnkprt1
028300          ELSE                                                    zbnkprt1
028400             PERFORM PRINT-TOTAL-TXNS                             zbnkprt1
028500             PERFORM PRINT-TOTAL-ASSETS                           zbnkprt1
028600          END-IF                                                  zbnkprt1
028700       END-IF                                                     zbnkprt1
028800     END-PERFORM.                                                 zbnkprt1
028900                                                                  zbnkprt1
029000     PERFORM EXTRACT-CLOSE.                                       zbnkprt1
029100     PERFORM PRINTOUT-CLOSE.                                      zbnkprt1
029200                                                                  zbnkprt1
029300     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkprt1
029400     MOVE 'End Of Job'                                            zbnkprt1
029500       TO WS-CONSOLE-MESSAGE.                                     zbnkprt1
029600     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkprt1
029700                                                                  zbnkprt1
029800     PERFORM RUN-TIME.                                            zbnkprt1
029900                                                                  zbnkprt1
030000     MOVE 0 TO RETURN-CODE.                                       zbnkprt1
030100                                                                  zbnkprt1
030200     GOBACK.                                                      zbnkprt1
030300                                                                  zbnkprt1
030400***************************************************************** zbnkprt1
030500* Format print lines                                            * zbnkprt1
030600***************************************************************** zbnkprt1
030700 FORMAT-AND-PRINT.                                                zbnkprt1
030800     IF BANKXT01-1-TYPE IS EQUAL TO '0'                           zbnkprt1
030900        MOVE BANKXT01-0-EMAIL TO WS-SAVED-EMAIL                   zbnkprt1
031000     END-IF.                                                      zbnkprt1
031100     IF BANKXT01-1-TYPE IS EQUAL TO '1'                           zbnkprt1
031200        PERFORM PRINT-TOTAL-TXNS                                  zbnkprt1
031300        PERFORM PRINT-TOTAL-ASSETS                                zbnkprt1
031400        IF EMAIL-REQUIRED                                         zbnkprt1
031500           MOVE SPACES TO PRINTOUT-REC                            zbnkprt1
031600           STRING 'SENDTO: ' DELIMITED BY SIZE                    zbnkprt1
031700                  WS-SAVED-EMAIL DELIMITED BY SPACE               zbnkprt1
031800             INTO PRINTOUT-REC                                    zbnkprt1
031900           PERFORM PRINTOUT-PUT                                   zbnkprt1
032000        END-IF                                                    zbnkprt1
032100        MOVE WS-LINE1 TO PRINTOUT-REC                             zbnkprt1
032200        PERFORM PRINTOUT-PUT                                      zbnkprt1
032300        MOVE WS-LINE2 TO PRINTOUT-REC                             zbnkprt1
032400        PERFORM PRINTOUT-PUT                                      zbnkprt1
032500        MOVE '0' TO WS-LINE3-CC                                   zbnkprt1
032600        MOVE BANKXT01-1-NAME TO WS-LINE3-NAME-ADDR                zbnkprt1
032700        MOVE WS-PRINTED-DATE TO WS-LINE3-DATE                     zbnkprt1
032800        MOVE WS-LINE3 TO PRINTOUT-REC                             zbnkprt1
032900        PERFORM PRINTOUT-PUT                                      zbnkprt1
033000        MOVE ' ' TO WS-LINE3-CC                                   zbnkprt1
033100        MOVE BANKXT01-1-ADDR1 TO WS-LINE3-NAME-ADDR               zbnkprt1
033200        ACCEPT WS-SYS-TIME FROM TIME                              zbnkprt1
033300        MOVE WS-SYS-TIME (1:2) TO WS-PRINT-TIME-HH                zbnkprt1
033400        MOVE ':' TO WS-PRINT-TIME-DOT1                            zbnkprt1
033500        MOVE WS-SYS-TIME (3:2) TO WS-PRINT-TIME-MM                zbnkprt1
033600        MOVE ':' TO WS-PRINT-TIME-DOT2                            zbnkprt1
033700        MOVE WS-SYS-TIME (5:2) TO WS-PRINT-TIME-SS                zbnkprt1
033800        MOVE WS-PRINTED-TIME TO WS-LINE3-DATE                     zbnkprt1
033900        MOVE WS-LINE3 TO PRINTOUT-REC                             zbnkprt1
034000        PERFORM PRINTOUT-PUT                                      zbnkprt1
034100        MOVE ' ' TO WS-LINE3-CC                                   zbnkprt1
034200        MOVE BANKXT01-1-ADDR2 TO WS-LINE3-NAME-ADDR               zbnkprt1
034300        MOVE SPACES TO WS-LINE3-DATE                              zbnkprt1
034400        MOVE WS-LINE3 TO PRINTOUT-REC                             zbnkprt1
034500        PERFORM PRINTOUT-PUT                                      zbnkprt1
034600        MOVE ' ' TO WS-LINE3-CC                                   zbnkprt1
034700        MOVE BANKXT01-1-STATE TO STATE-PROV-WK-CODE               zbnkprt1
034800        PERFORM EXPAND-STATE-PROV THRU                            zbnkprt1
034900                EXPAND-STATE-PROV-EXIT                            zbnkprt1
035000        MOVE STATE-PROV-WK-NAME TO WS-LINE3-NAME-ADDR             zbnkprt1
035100        MOVE SPACES TO WS-LINE3-DATE                              zbnkprt1
035200        MOVE WS-LINE3 TO PRINTOUT-REC                             zbnkprt1
035300        PERFORM PRINTOUT-PUT                                      zbnkprt1
035400        MOVE ' ' TO WS-LINE3-CC                                   zbnkprt1
035500        MOVE BANKXT01-1-CNTRY TO WS-LINE3-NAME-ADDR               zbnkprt1
035600        MOVE SPACES TO WS-LINE3-DATE                              zbnkprt1
035700        MOVE WS-LINE3 TO PRINTOUT-REC                             zbnkprt1
035800        PERFORM PRINTOUT-PUT                                      zbnkprt1
035900        MOVE ' ' TO WS-LINE3-CC                                   zbnkprt1
036000        MOVE BANKXT01-1-PST-CDE TO WS-LINE3-NAME-ADDR             zbnkprt1
036100        MOVE SPACES TO WS-LINE3-DATE                              zbnkprt1
036200        MOVE WS-LINE3 TO PRINTOUT-REC                             zbnkprt1
036300        PERFORM PRINTOUT-PUT                                      zbnkprt1
036400        MOVE WS-LINE4 TO PRINTOUT-REC                             zbnkprt1
036500        PERFORM PRINTOUT-PUT                                      zbnkprt1
036600        MOVE ZERO TO WS-TOTAL-TXNS                                zbnkprt1
036700        MOVE ZERO TO WS-TOTAL-ASSETS                              zbnkprt1
036800     END-IF.                                                      zbnkprt1
036900     IF BANKXT01-2-TYPE IS EQUAL TO '2'                           zbnkprt1
037000        PERFORM PRINT-TOTAL-TXNS                                  zbnkprt1
037100        MOVE SPACES TO WS-LINE5                                   zbnkprt1
037200        MOVE BANKXT01-2-ACC-NO TO WS-LINE5-ACC-NO                 zbnkprt1
037300        MOVE 'Last statement' TO WS-LINE5-DESC-PT1                zbnkprt1
037400        MOVE BANKXT01-2-ACC-DESC TO WS-LINE5-DESC-PT2             zbnkprt1
037500        MOVE BANKXT01-2-ACC-LAST-STMT-DTE TO DDI-DATA             zbnkprt1
037600        SET DD-ENV-NULL TO TRUE                                   zbnkprt1
037700        SET DDI-ISO TO TRUE                                       zbnkprt1
037800        SET DDO-DD-MMM-YYYY TO TRUE                               zbnkprt1
037900        CALL 'UDATECNV' USING WS-DATE-WORK-AREA                   zbnkprt1
038000        MOVE DDO-DATA TO WS-LINE5-DATE                            zbnkprt1
038100        MOVE BANKXT01-2-ACC-CURR-BAL TO WS-LINE5-BALANCE          zbnkprt1
038200        ADD BANKXT01-2-ACC-CURR-BAL TO WS-TOTAL-ASSETS            zbnkprt1
038300        MOVE WS-LINE5 TO PRINTOUT-REC                             zbnkprt1
038400        PERFORM PRINTOUT-PUT                                      zbnkprt1
038500     END-IF.                                                      zbnkprt1
038600     IF BANKXT01-3-TYPE IS EQUAL TO '3'                           zbnkprt1
038700        MOVE SPACES TO WS-LINE5                                   zbnkprt1
038800        MOVE BANKXT01-3-DESC TO WS-LINE5-DESC (4:30)              zbnkprt1
038900        MOVE BANKXT01-3-TIMESTAMP (1:10) TO DDI-DATA              zbnkprt1
039000        SET DD-ENV-NULL TO TRUE                                   zbnkprt1
039100        SET DDI-ISO TO TRUE                                       zbnkprt1
039200        SET DDO-DD-MMM-YYYY TO TRUE                               zbnkprt1
039300        CALL 'UDATECNV' USING WS-DATE-WORK-AREA                   zbnkprt1
039400        MOVE DDO-DATA TO WS-LINE5-DATE                            zbnkprt1
039500        MOVE BANKXT01-3-AMOUNT TO WS-LINE5-AMOUNT                 zbnkprt1
039600        ADD BANKXT01-3-AMOUNT TO WS-TOTAL-TXNS                    zbnkprt1
039700        SET TXNS-PRINTED TO TRUE                                  zbnkprt1
039800        MOVE WS-LINE5 TO PRINTOUT-REC                             zbnkprt1
039900        PERFORM PRINTOUT-PUT                                      zbnkprt1
040000     END-IF.                                                      zbnkprt1
040100                                                                  zbnkprt1
040200***************************************************************** zbnkprt1
040300* Format and print transaction totals                           * zbnkprt1
040400***************************************************************** zbnkprt1
040500 PRINT-TOTAL-TXNS.                                                zbnkprt1
040600     IF TXNS-PRINTED                                              zbnkprt1
040700        MOVE SPACES TO WS-LINE5                                   zbnkprt1
040800        MOVE '------------' TO WS-LINE5-AMOUNT-DASH               zbnkprt1
040900        MOVE WS-LINE5 TO PRINTOUT-REC                             zbnkprt1
041000        PERFORM PRINTOUT-PUT                                      zbnkprt1
041100        MOVE SPACES TO WS-LINE5-DESC                              zbnkprt1
041200        MOVE 'Total transactions' TO WS-LINE5-DESC (4:30)         zbnkprt1
041300        MOVE WS-TOTAL-TXNS TO WS-LINE5-AMOUNT                     zbnkprt1
041400        MOVE ZERO TO WS-TOTAL-TXNS                                zbnkprt1
041500        SET NO-TXNS-PRINTED TO TRUE                               zbnkprt1
041600        MOVE WS-LINE5 TO PRINTOUT-REC                             zbnkprt1
041700        PERFORM PRINTOUT-PUT                                      zbnkprt1
041800     END-IF.                                                      zbnkprt1
041900                                                                  zbnkprt1
042000                                                                  zbnkprt1
042100***************************************************************** zbnkprt1
042200* Format and print "page" totals                                * zbnkprt1
042300***************************************************************** zbnkprt1
042400 PRINT-TOTAL-ASSETS.                                              zbnkprt1
042500     IF WS-FIRST-REC IS EQUAL TO 'YES'                            zbnkprt1
042600        MOVE 'NO' TO WS-FIRST-REC                                 zbnkprt1
042700        SET NO-TXNS-PRINTED TO TRUE                               zbnkprt1
042800     ELSE                                                         zbnkprt1
042900        MOVE SPACES TO WS-LINE5                                   zbnkprt1
043000        MOVE '------------' TO WS-LINE5-BALANCE-DASH              zbnkprt1
043100        MOVE WS-LINE5 TO PRINTOUT-REC                             zbnkprt1
043200        PERFORM PRINTOUT-PUT                                      zbnkprt1
043300        MOVE SPACES TO WS-LINE5                                   zbnkprt1
043400        MOVE 'Total Assets' TO WS-LINE5-DESC                      zbnkprt1
043500        MOVE WS-TOTAL-ASSETS TO WS-LINE5-BALANCE                  zbnkprt1
043600        MOVE WS-LINE5 TO PRINTOUT-REC                             zbnkprt1
043700        PERFORM PRINTOUT-PUT                                      zbnkprt1
043800     END-IF.                                                      zbnkprt1
043900                                                                  zbnkprt1
044000***************************************************************** zbnkprt1
044100* Open the EXTRACTed data file                                 *  zbnkprt1
044200***************************************************************** zbnkprt1
044300 EXTRACT-OPEN.                                                    zbnkprt1
044400     OPEN INPUT EXTRACT-FILE.                                     zbnkprt1
044500     IF WS-EXTRACT-STATUS = '00'                                  zbnkprt1
044600        MOVE 'EXTRACT file opened OK'                             zbnkprt1
044700          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
044800        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
044900     ELSE                                                         zbnkprt1
045000        MOVE 'EXTRACT file open failure...'                       zbnkprt1
045100          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
045200        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
045300        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    zbnkprt1
045400        PERFORM DISPLAY-IO-STATUS                                 zbnkprt1
045500        PERFORM ABORT-PROGRAM                                     zbnkprt1
045600        END-IF.                                                   zbnkprt1
045700                                                                  zbnkprt1
045800***************************************************************** zbnkprt1
045900* Read a record from the EXTRACTed data file                    * zbnkprt1
046000***************************************************************** zbnkprt1
046100 EXTRACT-GET.                                                     zbnkprt1
046200     READ EXTRACT-FILE.                                           zbnkprt1
046300     IF WS-EXTRACT-STATUS NOT = '00'                              zbnkprt1
046400        IF WS-EXTRACT-STATUS = '10'                               zbnkprt1
046500           MOVE 'YES' TO WS-END-OF-FILE                           zbnkprt1
046600        ELSE                                                      zbnkprt1
046700           MOVE 'EXTRACT Error readng file ...'                   zbnkprt1
046800             TO WS-CONSOLE-MESSAGE                                zbnkprt1
046900            PERFORM DISPLAY-CONSOLE-MESSAGE                       zbnkprt1
047000            MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                zbnkprt1
047100            PERFORM DISPLAY-IO-STATUS                             zbnkprt1
047200            PERFORM ABORT-PROGRAM                                 zbnkprt1
047300        END-IF                                                    zbnkprt1
047400     END-IF.                                                      zbnkprt1
047500                                                                  zbnkprt1
047600***************************************************************** zbnkprt1
047700* Close the EXTRACTed data file                                 * zbnkprt1
047800***************************************************************** zbnkprt1
047900 EXTRACT-CLOSE.                                                   zbnkprt1
048000     CLOSE EXTRACT-FILE.                                          zbnkprt1
048100     IF WS-EXTRACT-STATUS = '00'                                  zbnkprt1
048200        MOVE 'EXTRACT file closed OK'                             zbnkprt1
048300          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
048400        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
048500     ELSE                                                         zbnkprt1
048600        MOVE 'EXTRACT file close failure...'                      zbnkprt1
048700          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
048800        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
048900        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    zbnkprt1
049000        PERFORM DISPLAY-IO-STATUS                                 zbnkprt1
049100        PERFORM ABORT-PROGRAM                                     zbnkprt1
049200     END-IF.                                                      zbnkprt1
049300                                                                  zbnkprt1
049400***************************************************************** zbnkprt1
049500* Open the seqential print file                                 * zbnkprt1
049600***************************************************************** zbnkprt1
049700 PRINTOUT-OPEN.                                                   zbnkprt1
049800     OPEN OUTPUT PRINTOUT-FILE.                                   zbnkprt1
049900     IF WS-PRINTOUT-STATUS = '00'                                 zbnkprt1
050000        MOVE 'PRINTOUT file opened OK'                            zbnkprt1
050100          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
050200        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
050300     ELSE                                                         zbnkprt1
050400        MOVE 'PRINTOUT file open failure...'                      zbnkprt1
050500          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
050600        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
050700        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   zbnkprt1
050800        PERFORM DISPLAY-IO-STATUS                                 zbnkprt1
050900        PERFORM ABORT-PROGRAM                                     zbnkprt1
051000        END-IF.                                                   zbnkprt1
051100                                                                  zbnkprt1
051200***************************************************************** zbnkprt1
051300* Write a record to the squential file                          * zbnkprt1
051400***************************************************************** zbnkprt1
051500 PRINTOUT-PUT.                                                    zbnkprt1
051600     IF PRINTOUT-REC IS NOT EQUAL TO SPACES                       zbnkprt1
051700        WRITE PRINTOUT-REC                                        zbnkprt1
051800        IF WS-PRINTOUT-STATUS NOT = '00'                          zbnkprt1
051900           MOVE 'PRINTOUT Error Writing file ...'                 zbnkprt1
052000             TO WS-CONSOLE-MESSAGE                                zbnkprt1
052100           PERFORM DISPLAY-CONSOLE-MESSAGE                        zbnkprt1
052200           MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                zbnkprt1
052300           PERFORM DISPLAY-IO-STATUS                              zbnkprt1
052400           PERFORM ABORT-PROGRAM                                  zbnkprt1
052500        END-IF                                                    zbnkprt1
052600     END-IF.                                                      zbnkprt1
052700                                                                  zbnkprt1
052800***************************************************************** zbnkprt1
052900* Close the seqential print file                                * zbnkprt1
053000***************************************************************** zbnkprt1
053100 PRINTOUT-CLOSE.                                                  zbnkprt1
053200     CLOSE PRINTOUT-FILE.                                         zbnkprt1
053300     IF WS-PRINTOUT-STATUS = '00'                                 zbnkprt1
053400        MOVE 'PRINTOUT file closed OK'                            zbnkprt1
053500          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
053600        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
053700     ELSE                                                         zbnkprt1
053800        MOVE 'PRINTOUT file close failure...'                     zbnkprt1
053900          TO WS-CONSOLE-MESSAGE                                   zbnkprt1
054000        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
054100        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   zbnkprt1
054200        PERFORM DISPLAY-IO-STATUS                                 zbnkprt1
054300        PERFORM ABORT-PROGRAM                                     zbnkprt1
054400     END-IF.                                                      zbnkprt1
054500                                                                  zbnkprt1
054600***************************************************************** zbnkprt1
054700* Display the file status bytes. This routine will display as   * zbnkprt1
054800* two digits if the full two byte file status is numeric. If    * zbnkprt1
054900* second byte is non-numeric then it will be treated as a       * zbnkprt1
055000* binary number.                                                * zbnkprt1
055100***************************************************************** zbnkprt1
055200 DISPLAY-IO-STATUS.                                               zbnkprt1
055300     IF WS-IO-STATUS NUMERIC                                      zbnkprt1
055400        MOVE SPACE TO WS-CONSOLE-MESSAGE                          zbnkprt1
055500        STRING 'File status -' DELIMITED BY SIZE                  zbnkprt1
055600               WS-IO-STATUS DELIMITED BY SIZE                     zbnkprt1
055700          INTO WS-CONSOLE-MESSAGE                                 zbnkprt1
055800        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
055900     ELSE                                                         zbnkprt1
056000        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     zbnkprt1
056100        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    zbnkprt1
056200        MOVE SPACE TO WS-CONSOLE-MESSAGE                          zbnkprt1
056300        STRING 'File status -' DELIMITED BY SIZE                  zbnkprt1
056400               WS-IO-STAT1 DELIMITED BY SIZE                      zbnkprt1
056500               '/' DELIMITED BY SIZE                              zbnkprt1
056600               WS-TWO-BYTES DELIMITED BY SIZE                     zbnkprt1
056700          INTO WS-CONSOLE-MESSAGE                                 zbnkprt1
056800        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
056900     END-IF.                                                      zbnkprt1
057000                                                                  zbnkprt1
057100***************************************************************** zbnkprt1
057200* Expand the 2 character state/prove code to its full text      * zbnkprt1
057300***************************************************************** zbnkprt1
057400 EXPAND-STATE-PROV.                                               zbnkprt1
057500     MOVE 0 TO STATE-PROV-SUB.                                    zbnkprt1
057600     DIVIDE LENGTH OF STATE-PROV-DATA (1) INTO                    zbnkprt1
057700       LENGTH OF STATE-PROV-TABLE                                 zbnkprt1
057800         GIVING STATE-PROV-COUNT.                                 zbnkprt1
057900     MOVE STATE-PROV-WK-CODE TO STATE-PROV-WK-NAME.               zbnkprt1
058000 EXPAND-STATE-PROV-LOOP.                                          zbnkprt1
058100     ADD 1 TO STATE-PROV-SUB.                                     zbnkprt1
058200     IF STATE-PROV-SUB IS GREATER THAN STATE-PROV-COUNT           zbnkprt1
058300        GO TO EXPAND-STATE-PROV-EXIT                              zbnkprt1
058400     END-IF.                                                      zbnkprt1
058500     IF STATE-PROV-WK-CODE IS EQUAL TO                            zbnkprt1
058600          STATE-PROV-CODE (STATE-PROV-SUB)                        zbnkprt1
058700        MOVE STATE-PROV-NAME (STATE-PROV-SUB) TO                  zbnkprt1
058800          STATE-PROV-WK-NAME                                      zbnkprt1
058900        GO TO EXPAND-STATE-PROV-EXIT                              zbnkprt1
059000     END-IF.                                                      zbnkprt1
059100     GO TO EXPAND-STATE-PROV-LOOP.                                zbnkprt1
059200 EXPAND-STATE-PROV-EXIT.                                          zbnkprt1
059300     EXIT.                                                        zbnkprt1
059400                                                                  zbnkprt1
059500***************************************************************** zbnkprt1
059600* 'ABORT' the program.                                          * zbnkprt1
059700* Post a message to the console and issue a goback              * zbnkprt1
059800***************************************************************** zbnkprt1
059900 ABORT-PROGRAM.                                                   zbnkprt1
060000     IF WS-CONSOLE-MESSAGE NOT = SPACES                           zbnkprt1
060100        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkprt1
060200     END-IF.                                                      zbnkprt1
060300     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.        zbnkprt1
060400     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkprt1
060500* Add some LE routines to identify but dont execute them          zbnkprt1
060600     IF RETURN-CODE IS NOT EQUAL TO RETURN-CODE                   zbnkprt1
060700        CALL 'CEE3DMP' USING WS-CEE3DMP-DMP-TITLE                 zbnkprt1
060800                             WS-CEE3DMP-DMP-OPTIONS               zbnkprt1
060900                             WS-CEE3DMP-FEEDBACK                  zbnkprt1
061000        CALL 'CEELOCT' USING WS-CEELOCT-DATE-LILIAN               zbnkprt1
061100                             WS-CEELOCT-SECS-LILIAN               zbnkprt1
061200                             WS-CEELOCT-TIME-GREGORIAN            zbnkprt1
061300                             WS-CEELOCT-FEEDBACK                  zbnkprt1
061400     END-IF.                                                      zbnkprt1
061500     MOVE 16 TO RETURN-CODE.                                      zbnkprt1
061600     GOBACK.                                                      zbnkprt1
061700                                                                  zbnkprt1
061800***************************************************************** zbnkprt1
061900* Display CONSOLE messages...                                   * zbnkprt1
062000***************************************************************** zbnkprt1
062100 DISPLAY-CONSOLE-MESSAGE.                                         zbnkprt1
062200     DISPLAY 'ZBNKPRT1 - ' WS-CONSOLE-MESSAGE                     zbnkprt1
062300       UPON CONSOLE.                                              zbnkprt1
062400     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       zbnkprt1
062500                                                                  zbnkprt1
062600 COPY CTIMERP.                                                    zbnkprt1
062700                                                                  zbnkprt1
062800* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     zbnkprt1
