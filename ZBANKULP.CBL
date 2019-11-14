000100***************************************************************** ZBANKULP
000200*                                                               * ZBANKULP
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * ZBANKULP
000400*   This demonstration program is provided for use by users     * ZBANKULP
000500*   of Micro Focus products and may be used, modified and       * ZBANKULP
000600*   distributed as part of your application provided that       * ZBANKULP
000700*   you properly acknowledge the copyright of Micro Focus       * ZBANKULP
000800*   in this material.                                           * ZBANKULP
000900*                                                               * ZBANKULP
001000***************************************************************** ZBANKULP
001100                                                                  ZBANKULP
001200***************************************************************** ZBANKULP
001300* Program:     ZBANKUPL.CBL                                     * ZBANKULP
001400* Function:    Unload SQL tables to sequential files            * ZBANKULP
001500***************************************************************** ZBANKULP
001600 IDENTIFICATION DIVISION.                                         ZBANKULP
001700 PROGRAM-ID.                                                      ZBANKULP
001800     ZBANKULP.                                                    ZBANKULP
001900 DATE-WRITTEN.                                                    ZBANKULP
002000     September 2002.                                              ZBANKULP
002100 DATE-COMPILED.                                                   ZBANKULP
002200     Today.                                                       ZBANKULP
002300 ENVIRONMENT DIVISION.                                            ZBANKULP
002400 INPUT-OUTPUT   SECTION.                                          ZBANKULP
002500   FILE-CONTROL.                                                  ZBANKULP
002600     SELECT UNLOADED-BNKACC-FILE                                  ZBANKULP
002700            ASSIGN       TO SBNKACC                               ZBANKULP
002800            ORGANIZATION IS SEQUENTIAL                            ZBANKULP
002900            ACCESS MODE  IS SEQUENTIAL                            ZBANKULP
003000            FILE STATUS  IS WS-UNLOADED-STATUS.                   ZBANKULP
003100                                                                  ZBANKULP
003200     SELECT UNLOADED-BNKATYPE-FILE                                ZBANKULP
003300            ASSIGN       TO SBNKATYP                              ZBANKULP
003400            ORGANIZATION IS SEQUENTIAL                            ZBANKULP
003500            ACCESS MODE  IS SEQUENTIAL                            ZBANKULP
003600            FILE STATUS  IS WS-UNLOADED-STATUS.                   ZBANKULP
003700                                                                  ZBANKULP
003800     SELECT UNLOADED-BNKCUST-FILE                                 ZBANKULP
003900            ASSIGN       TO SBNKCUST                              ZBANKULP
004000            ORGANIZATION IS SEQUENTIAL                            ZBANKULP
004100            ACCESS MODE  IS SEQUENTIAL                            ZBANKULP
004200            FILE STATUS  IS WS-UNLOADED-STATUS.                   ZBANKULP
004300                                                                  ZBANKULP
004400     SELECT UNLOADED-BNKTXN-FILE                                  ZBANKULP
004500            ASSIGN       TO SBNKTXN                               ZBANKULP
004600            ORGANIZATION IS SEQUENTIAL                            ZBANKULP
004700            ACCESS MODE  IS SEQUENTIAL                            ZBANKULP
004800            FILE STATUS  IS WS-UNLOADED-STATUS.                   ZBANKULP
004900                                                                  ZBANKULP
005000*    SELECT UNLOADED-BNKHELP-FILE                                 ZBANKULP
005100*           ASSIGN       TO SBNKHELP                              ZBANKULP
005200*           ORGANIZATION IS SEQUENTIAL                            ZBANKULP
005300*           ACCESS MODE  IS SEQUENTIAL                            ZBANKULP
005400*           FILE STATUS  IS WS-UNLOADED-STATUS.                   ZBANKULP
005500                                                                  ZBANKULP
005600                                                                  ZBANKULP
005700 DATA DIVISION.                                                   ZBANKULP
005800 FILE SECTION.                                                    ZBANKULP
005900 FD  UNLOADED-BNKACC-FILE.                                        ZBANKULP
006000 01  UNLOADED-BNKACC-REC                     PIC X(100).          ZBANKULP
006100                                                                  ZBANKULP
006200 FD  UNLOADED-BNKATYPE-FILE.                                      ZBANKULP
006300 01  UNLOADED-BNKATYPE-REC                   PIC X(100).          ZBANKULP
006400                                                                  ZBANKULP
006500 FD  UNLOADED-BNKCUST-FILE.                                       ZBANKULP
006600 01  UNLOADED-BNKCUST-REC                    PIC X(250).          ZBANKULP
006700                                                                  ZBANKULP
006800 FD  UNLOADED-BNKTXN-FILE.                                        ZBANKULP
006900 01  UNLOADED-BNKTXN-REC                     PIC X(400).          ZBANKULP
007000                                                                  ZBANKULP
007100*FD  UNLOADED-BNKHELP-FILE.                                       ZBANKULP
007200*01  UNLOADED-BNKHELP-REC                    PIC X(83).           ZBANKULP
007300                                                                  ZBANKULP
007400 WORKING-STORAGE SECTION.                                         ZBANKULP
007500 01  WS-MISC-STORAGE.                                             ZBANKULP
007600   05  WS-UNLOADED-STATUS.                                        ZBANKULP
007700     10  WS-UNLOADED-STAT1                   PIC X(1).            ZBANKULP
007800     10  WS-UNLOADED-STAT2                   PIC X(1).            ZBANKULP
007900                                                                  ZBANKULP
008000   05  WS-IO-STATUS.                                              ZBANKULP
008100     10  WS-IO-STAT1                         PIC X(1).            ZBANKULP
008200     10  WS-IO-STAT2                         PIC X(1).            ZBANKULP
008300                                                                  ZBANKULP
008400   05  WS-TWO-BYTES.                                              ZBANKULP
008500     10  WS-TWO-BYTES-LEFT                   PIC X(1).            ZBANKULP
008600     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            ZBANKULP
008700   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  ZBANKULP
008800                                             PIC 9(1) COMP.       ZBANKULP
008900                                                                  ZBANKULP
009000   05  WS-MSG-DATA.                                               ZBANKULP
009100     10  WS-MSG-DATA-TABLE                   PIC X(8).            ZBANKULP
009200     10  WS-MSG-DATA-FUNC                    PIC X(16).           ZBANKULP
009300     10  WS-MSG-DATA-SQLCODE                 PIC +9999.           ZBANKULP
009400                                                                  ZBANKULP
009500   05  WS-RETURN-CODE                        PIC 9(4).            ZBANKULP
009600                                                                  ZBANKULP
009700   05  WS-SQL-STATS.                                              ZBANKULP
009800     10  WS-BNKACC-SQL-ERROR                 PIC X(1).            ZBANKULP
009900     10  WS-BNKATYPE-SQL-ERROR               PIC X(1).            ZBANKULP
010000     10  WS-BNKCUST-SQL-ERROR                PIC X(1).            ZBANKULP
010100     10  WS-BNKTXN-SQL-ERROR                 PIC X(1).            ZBANKULP
010200     10  WS-BNKHELP-SQL-ERROR                PIC X(1).            ZBANKULP
010300                                                                  ZBANKULP
010400   05  WS-FILE-STATS.                                             ZBANKULP
010500     10  WS-BNKACC-REC-COUNT                 PIC 9(3).            ZBANKULP
010600     10  WS-BNKATYPE-REC-COUNT               PIC 9(3).            ZBANKULP
010700     10  WS-BNKCUST-REC-COUNT                PIC 9(3).            ZBANKULP
010800     10  WS-BNKTXN-REC-COUNT                 PIC 9(3).            ZBANKULP
010900     10  WS-BNKHELP-REC-COUNT                PIC 9(3).            ZBANKULP
011000     10  WS-BNKACC-FILE-STATUS               PIC 9(3).            ZBANKULP
011100     10  WS-BNKATYPE-FILE-STATUS             PIC 9(3).            ZBANKULP
011200     10  WS-BNKCUST-FILE-STATUS              PIC 9(3).            ZBANKULP
011300     10  WS-BNKTXN-FILE-STATUS               PIC 9(3).            ZBANKULP
011400     10  WS-BNKHELP-FILE-STATUS              PIC 9(3).            ZBANKULP
011500                                                                  ZBANKULP
011600 01  WS-TWOS-COMP.                                                ZBANKULP
011700   05  WS-TWOS-COMP-REQ                      PIC X(1).            ZBANKULP
011800     88  WS-TWOS-COMP-REQ-YES                VALUE 'Y'.           ZBANKULP
011900     88  WS-TWOS-COMP-REQ-NO                 VALUE 'N'.           ZBANKULP
012000   05  WS-TWOS-COMP-LEN                      PIC S9(4) COMP.      ZBANKULP
012100   05  WS-TWOS-COMP-INPUT                    PIC X(256).          ZBANKULP
012200   05  WS-TWOS-COMP-OUTPUT                   PIC X(256).          ZBANKULP
012300                                                                  ZBANKULP
012400                                                                  ZBANKULP
012500 01  WS-CONSOLE-MESSAGE                      PIC X(65).           ZBANKULP
012600                                                                  ZBANKULP
012700     EXEC SQL                                                     ZBANKULP
012800          BEGIN DECLARE SECTION                                   ZBANKULP
012900     END-EXEC.                                                    ZBANKULP
013000     EXEC SQL                                                     ZBANKULP
013100          INCLUDE CBANKSAC                                        ZBANKULP
013200     END-EXEC.                                                    ZBANKULP
013300     EXEC SQL                                                     ZBANKULP
013400          INCLUDE CBANKSAT                                        ZBANKULP
013500     END-EXEC.                                                    ZBANKULP
013600     EXEC SQL                                                     ZBANKULP
013700          INCLUDE CBANKSCS                                        ZBANKULP
013800     END-EXEC.                                                    ZBANKULP
013900     EXEC SQL                                                     ZBANKULP
014000          INCLUDE CBANKSTX                                        ZBANKULP
014100     END-EXEC.                                                    ZBANKULP
014200*    EXEC SQL                                                     ZBANKULP
014300*         INCLUDE CHELPSQL                                        ZBANKULP
014400*    END-EXEC.                                                    ZBANKULP
014500     EXEC SQL                                                     ZBANKULP
014600          INCLUDE SQLCA                                           ZBANKULP
014700     END-EXEC.                                                    ZBANKULP
014800                                                                  ZBANKULP
014900 PROCEDURE DIVISION.                                              ZBANKULP
015000     MOVE 0 TO WS-BNKACC-REC-COUNT.                               ZBANKULP
015100     MOVE 0 TO WS-BNKATYPE-REC-COUNT.                             ZBANKULP
015200     MOVE 0 TO WS-BNKCUST-REC-COUNT.                              ZBANKULP
015300     MOVE 0 TO WS-BNKTXN-REC-COUNT.                               ZBANKULP
015400*    MOVE 0 TO WS-BNKHELP-REC-COUNT.                              ZBANKULP
015500                                                                  ZBANKULP
015600     MOVE 0 TO WS-BNKACC-FILE-STATUS.                             ZBANKULP
015700     MOVE 0 TO WS-BNKATYPE-FILE-STATUS.                           ZBANKULP
015800     MOVE 0 TO WS-BNKCUST-FILE-STATUS.                            ZBANKULP
015900     MOVE 0 TO WS-BNKTXN-FILE-STATUS.                             ZBANKULP
016000*    MOVE 0 TO WS-BNKHELP-FILE-STATUS.                            ZBANKULP
016100                                                                  ZBANKULP
016200     MOVE 'N' TO WS-BNKACC-SQL-ERROR.                             ZBANKULP
016300     MOVE 'N' TO WS-BNKATYPE-SQL-ERROR.                           ZBANKULP
016400     MOVE 'N' TO WS-BNKCUST-SQL-ERROR.                            ZBANKULP
016500     MOVE 'N' TO WS-BNKTXN-SQL-ERROR.                             ZBANKULP
016600*    MOVE 'N' TO WS-BNKHELP-SQL-ERROR.                            ZBANKULP
016700                                                                  ZBANKULP
016800     MOVE 0 TO WS-RETURN-CODE.                                    ZBANKULP
016900                                                                  ZBANKULP
017000     PERFORM BNKACC-PROCESS THRU                                  ZBANKULP
017100             BNKACC-PROCESS-EXIT.                                 ZBANKULP
017200     PERFORM BNKATYPE-PROCESS THRU                                ZBANKULP
017300             BNKATYPE-PROCESS-EXIT.                               ZBANKULP
017400     PERFORM BNKCUST-PROCESS THRU                                 ZBANKULP
017500             BNKCUST-PROCESS-EXIT.                                ZBANKULP
017600     PERFORM BNKTXN-PROCESS THRU                                  ZBANKULP
017700             BNKTXN-PROCESS-EXIT.                                 ZBANKULP
017800*    PERFORM BNKHELP-PROCESS THRU                                 ZBANKULP
017900*            BNKHELP-PROCESS-EXIT.                                ZBANKULP
018000                                                                  ZBANKULP
018100     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBANKULP
018200     STRING 'BNKACC:   ' DELIMITED BY SIZE                        ZBANKULP
018300            WS-BNKACC-REC-COUNT DELIMITED BY SIZE                 ZBANKULP
018400            ' records unloaded' DELIMITED BY SIZE                 ZBANKULP
018500       INTO WS-CONSOLE-MESSAGE                                    ZBANKULP
018600     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBANKULP
018700     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBANKULP
018800     STRING 'BNKATYPE: ' DELIMITED BY SIZE                        ZBANKULP
018900            WS-BNKATYPE-REC-COUNT DELIMITED BY SIZE               ZBANKULP
019000            ' records unloaded' DELIMITED BY SIZE                 ZBANKULP
019100       INTO WS-CONSOLE-MESSAGE                                    ZBANKULP
019200     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBANKULP
019300     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBANKULP
019400     STRING 'BNKCUST:  ' DELIMITED BY SIZE                        ZBANKULP
019500            WS-BNKCUST-REC-COUNT DELIMITED BY SIZE                ZBANKULP
019600            ' records unloaded' DELIMITED BY SIZE                 ZBANKULP
019700       INTO WS-CONSOLE-MESSAGE                                    ZBANKULP
019800     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBANKULP
019900     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBANKULP
020000     STRING 'BNKTXN:   ' DELIMITED BY SIZE                        ZBANKULP
020100            WS-BNKTXN-REC-COUNT DELIMITED BY SIZE                 ZBANKULP
020200            ' records unloaded' DELIMITED BY SIZE                 ZBANKULP
020300       INTO WS-CONSOLE-MESSAGE                                    ZBANKULP
020400     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBANKULP
020500*    MOVE SPACES TO WS-CONSOLE-MESSAGE.                           ZBANKULP
020600*    STRING 'BNKHELP:  ' DELIMITED BY SIZE                        ZBANKULP
020700*           WS-BNKHELP-REC-COUNT DELIMITED BY SIZE                ZBANKULP
020800*           ' records unloaded' DELIMITED BY SIZE                 ZBANKULP
020900*      INTO WS-CONSOLE-MESSAGE                                    ZBANKULP
021000*    PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBANKULP
021100                                                                  ZBANKULP
021200     IF WS-BNKACC-SQL-ERROR IS NOT EQUAL TO 'N' OR                ZBANKULP
021300        WS-BNKATYPE-SQL-ERROR IS NOT EQUAL TO 'N' OR              ZBANKULP
021400        WS-BNKCUST-SQL-ERROR IS NOT EQUAL TO 'N' OR               ZBANKULP
021500        WS-BNKTXN-SQL-ERROR IS NOT EQUAL TO 'N'
      *       OR                                                        ZBANKULP
021600*       WS-BNKHELP-SQL-ERROR IS NOT EQUAL TO 'N'                  ZBANKULP
021700        MOVE SPACES TO WS-CONSOLE-MESSAGE                         ZBANKULP
021800        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBANKULP
021900        MOVE '***** SQL Errors detected *****'                    ZBANKULP
022000          TO WS-CONSOLE-MESSAGE                                   ZBANKULP
022100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBANKULP
022200        MOVE 8 TO WS-RETURN-CODE                                  ZBANKULP
022300     END-IF.                                                      ZBANKULP
022400     IF WS-BNKACC-FILE-STATUS IS GREATER THAN 4 OR                ZBANKULP
022500        WS-BNKATYPE-FILE-STATUS IS GREATER THAN 4 OR              ZBANKULP
022600        WS-BNKCUST-FILE-STATUS IS GREATER THAN 4 OR               ZBANKULP
022700        WS-BNKTXN-FILE-STATUS IS GREATER THAN 4
      *       OR                                                        ZBANKULP
022800*       WS-BNKHELP-FILE-STATUS IS GREATER THAN 4                  ZBANKULP
022900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBANKULP
023000        MOVE '***** File errors detected *****'                   ZBANKULP
023100          TO WS-CONSOLE-MESSAGE                                   ZBANKULP
023200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBANKULP
023300        MOVE 8 TO WS-RETURN-CODE                                  ZBANKULP
023400     END-IF.                                                      ZBANKULP
023500                                                                  ZBANKULP
023600     MOVE WS-RETURN-CODE TO RETURN-CODE.                          ZBANKULP
023700                                                                  ZBANKULP
023800     GOBACK.                                                      ZBANKULP
023900                                                                  ZBANKULP
024000***************************************************************** ZBANKULP
024100* Process the BNKACC file                                       * ZBANKULP
024200***************************************************************** ZBANKULP
024300 BNKACC-PROCESS.                                                  ZBANKULP
024400     MOVE 'BNKACC' TO WS-MSG-DATA-TABLE.                          ZBANKULP
024500 BNKACC-OPEN-CURSOR.                                              ZBANKULP
024600     OPEN OUTPUT UNLOADED-BNKACC-FILE.                            ZBANKULP
024700     IF WS-UNLOADED-STATUS IS EQUAL TO '00'                       ZBANKULP
024800        ADD 1 TO WS-BNKACC-FILE-STATUS                            ZBANKULP
024900     ELSE                                                         ZBANKULP
025000        ADD 4 TO WS-BNKACC-FILE-STATUS                            ZBANKULP
025100        PERFORM DISPLAY-IO-STATUS                                 ZBANKULP
025200        GO TO BNKACC-PROCESS-EXIT                                 ZBANKULP
025300     END-IF.                                                      ZBANKULP
025400     EXEC SQL                                                     ZBANKULP
025500          DECLARE BNKACC_CSR CURSOR FOR                           ZBANKULP
025600          SELECT BAC_PID,                                         ZBANKULP
025700                 BAC_ACCNO,                                       ZBANKULP
025800                 BAC_ACCTYPE,                                     ZBANKULP
025900                 BAC_BALANCE,                                     ZBANKULP
026000                 BAC_LAST_STMT_DTE,                               ZBANKULP
026100                 BAC_LAST_STMT_BAL,                               ZBANKULP
026200                 BAC_FILLER                                       ZBANKULP
026300          FROM BNKACC                                             ZBANKULP
026400          ORDER BY BAC_ACCNO ASC                                  ZBANKULP
026500          FOR FETCH ONLY                                          ZBANKULP
026600     END-EXEC.                                                    ZBANKULP
026700     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
026800        EXEC SQL                                                  ZBANKULP
026900             OPEN BNKACC_CSR                                      ZBANKULP
027000        END-EXEC                                                  ZBANKULP
027100        IF SQLCODE IS NOT EQUAL TO ZERO                           ZBANKULP
027200           MOVE 'Y' TO WS-BNKACC-SQL-ERROR                        ZBANKULP
027300           GO TO BNKACC-CLOSE-FILE                                ZBANKULP
027400        END-IF                                                    ZBANKULP
027500     ELSE                                                         ZBANKULP
027600        MOVE 'DECLARE_CSR' TO WS-MSG-DATA-FUNC                    ZBANKULP
027700        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
027800        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
027900        MOVE 'Y' TO WS-BNKACC-SQL-ERROR                           ZBANKULP
028000        GO TO BNKACC-CLOSE-FILE                                   ZBANKULP
028100     END-IF.                                                      ZBANKULP
028200                                                                  ZBANKULP
028300***************************************************************** ZBANKULP
028400* Now browse the selected rows and move to our area 1 at a time * ZBANKULP
028500***************************************************************** ZBANKULP
028600 BNKACC-READ-CURSOR.                                              ZBANKULP
028700     EXEC SQL                                                     ZBANKULP
028800          FETCH BNKACC_CSR                                        ZBANKULP
028900          INTO :DCL-BAC-PID,                                      ZBANKULP
029000               :DCL-BAC-ACCNO,                                    ZBANKULP
029100               :DCL-BAC-ACCTYPE,                                  ZBANKULP
029200               :DCL-BAC-BALANCE,                                  ZBANKULP
029300               :DCL-BAC-LAST-STMT-DTE,                            ZBANKULP
029400               :DCL-BAC-LAST-STMT-BAL,                            ZBANKULP
029500               :DCL-BAC-FILLER                                    ZBANKULP
029600     END-EXEC.                                                    ZBANKULP
029700     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
029800        MOVE DCLACC TO UNLOADED-BNKACC-REC                        ZBANKULP
029900        WRITE UNLOADED-BNKACC-REC                                 ZBANKULP
030000        IF WS-UNLOADED-STATUS IS EQUAL TO 00                      ZBANKULP
030100           ADD 1 TO WS-BNKACC-REC-COUNT                           ZBANKULP
030200        ELSE                                                      ZBANKULP
030300           ADD 4 TO WS-BNKACC-FILE-STATUS                         ZBANKULP
030400           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
030500           GO TO BNKACC-CLOSE-CURSOR                              ZBANKULP
030600        END-IF                                                    ZBANKULP
030700     END-IF.                                                      ZBANKULP
030800     IF SQLCODE IS EQUAL TO +100                                  ZBANKULP
030900        GO TO BNKACC-CLOSE-CURSOR                                 ZBANKULP
031000     END-IF.                                                      ZBANKULP
031100     IF SQLCODE IS NOT EQUAL TO ZERO AND                          ZBANKULP
031200        SQLCODE IS NOT EQUAL TO +100                              ZBANKULP
031300        MOVE 'FETCH' TO WS-MSG-DATA-FUNC                          ZBANKULP
031400        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
031500        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
031600        MOVE 'Y' TO WS-BNKACC-SQL-ERROR                           ZBANKULP
031700        GO TO BNKACC-CLOSE-FILE                                   ZBANKULP
031800     END-IF.                                                      ZBANKULP
031900     GO TO BNKACC-READ-CURSOR.                                    ZBANKULP
032000                                                                  ZBANKULP
032100***************************************************************** ZBANKULP
032200* Close the cursor                                              * ZBANKULP
032300***************************************************************** ZBANKULP
032400 BNKACC-CLOSE-CURSOR.                                             ZBANKULP
032500     EXEC SQL                                                     ZBANKULP
032600          CLOSE BNKACC_CSR                                        ZBANKULP
032700     END-EXEC.                                                    ZBANKULP
032800     IF SQLCODE IS NOT EQUAL TO ZERO                              ZBANKULP
032900        MOVE 'CLOSE_CSR' TO WS-MSG-DATA-FUNC                      ZBANKULP
033000        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
033100        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
033200        MOVE 'Y' TO WS-BNKACC-SQL-ERROR                           ZBANKULP
033300        GO TO BNKACC-CLOSE-FILE                                   ZBANKULP
033400     END-IF.                                                      ZBANKULP
033500 BNKACC-CLOSE-FILE.                                               ZBANKULP
033600     IF WS-BNKACC-FILE-STATUS IS GREATER THAN 1                   ZBANKULP
033700        CLOSE UNLOADED-BNKACC-FILE                                ZBANKULP
033800        IF WS-UNLOADED-STATUS IS EQUAL TO '00'                    ZBANKULP
033900           ADD 1 TO WS-BNKACC-FILE-STATUS                         ZBANKULP
034000        ELSE                                                      ZBANKULP
034100           ADD 4 TO WS-BNKACC-FILE-STATUS                         ZBANKULP
034200           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
034300           GO TO BNKACC-PROCESS-EXIT                              ZBANKULP
034400        END-IF                                                    ZBANKULP
034500     END-IF.                                                      ZBANKULP
034600 BNKACC-PROCESS-EXIT.                                             ZBANKULP
034700     EXIT.                                                        ZBANKULP
034800                                                                  ZBANKULP
034900***************************************************************** ZBANKULP
035000* Process the BNKATYPE file                                     * ZBANKULP
035100***************************************************************** ZBANKULP
035200 BNKATYPE-PROCESS.                                                ZBANKULP
035300     MOVE 'BNKATYPE' TO WS-MSG-DATA-TABLE.                        ZBANKULP
035400 BNKATYPE-OPEN-CURSOR.                                            ZBANKULP
035500     OPEN OUTPUT UNLOADED-BNKATYPE-FILE.                          ZBANKULP
035600     IF WS-UNLOADED-STATUS IS EQUAL TO '00'                       ZBANKULP
035700        ADD 1 TO WS-BNKATYPE-FILE-STATUS                          ZBANKULP
035800     ELSE                                                         ZBANKULP
035900        ADD 4 TO WS-BNKATYPE-FILE-STATUS                          ZBANKULP
036000        PERFORM DISPLAY-IO-STATUS                                 ZBANKULP
036100        GO TO BNKATYPE-PROCESS-EXIT                               ZBANKULP
036200     END-IF.                                                      ZBANKULP
036300     EXEC SQL                                                     ZBANKULP
036400          DECLARE BNKATYPE_CSR CURSOR FOR                         ZBANKULP
036500          SELECT BAT_TYPE,                                        ZBANKULP
036600                 BAT_DESC,                                        ZBANKULP
036700                 BAT_FILLER                                       ZBANKULP
036800          FROM BNKATYPE                                           ZBANKULP
036900          ORDER BY BAT_TYPE ASC                                   ZBANKULP
037000          FOR FETCH ONLY                                          ZBANKULP
037100     END-EXEC.                                                    ZBANKULP
037200     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
037300        EXEC SQL                                                  ZBANKULP
037400             OPEN BNKATYPE_CSR                                    ZBANKULP
037500        END-EXEC                                                  ZBANKULP
037600        IF SQLCODE IS NOT EQUAL TO ZERO                           ZBANKULP
037700           MOVE 'Y' TO WS-BNKATYPE-SQL-ERROR                      ZBANKULP
037800           GO TO BNKATYPE-CLOSE-FILE                              ZBANKULP
037900        END-IF                                                    ZBANKULP
038000     ELSE                                                         ZBANKULP
038100        MOVE 'DECLARE_CSR' TO WS-MSG-DATA-FUNC                    ZBANKULP
038200        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
038300        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
038400        MOVE 'Y' TO WS-BNKATYPE-SQL-ERROR                         ZBANKULP
038500        GO TO BNKATYPE-CLOSE-FILE                                 ZBANKULP
038600     END-IF.                                                      ZBANKULP
038700                                                                  ZBANKULP
038800***************************************************************** ZBANKULP
038900* Now browse the selected rows and move to our area 1 at a time * ZBANKULP
039000***************************************************************** ZBANKULP
039100 BNKATYPE-READ-CURSOR.                                            ZBANKULP
039200     EXEC SQL                                                     ZBANKULP
039300          FETCH BNKATYPE_CSR                                      ZBANKULP
039400          INTO :DCL-BAT-TYPE,                                     ZBANKULP
039500               :DCL-BAT-DESC,                                     ZBANKULP
039600               :DCL-BAT-FILLER                                    ZBANKULP
039700     END-EXEC.                                                    ZBANKULP
039800     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
039900        MOVE DCLATYP TO UNLOADED-BNKATYPE-REC                     ZBANKULP
040000        WRITE UNLOADED-BNKATYPE-REC                               ZBANKULP
040100        IF WS-UNLOADED-STATUS IS EQUAL TO 00                      ZBANKULP
040200           ADD 1 TO WS-BNKATYPE-REC-COUNT                         ZBANKULP
040300        ELSE                                                      ZBANKULP
040400           ADD 4 TO WS-BNKATYPE-FILE-STATUS                       ZBANKULP
040500           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
040600           GO TO BNKATYPE-CLOSE-CURSOR                            ZBANKULP
040700        END-IF                                                    ZBANKULP
040800     END-IF.                                                      ZBANKULP
040900     IF SQLCODE IS EQUAL TO +100                                  ZBANKULP
041000        GO TO BNKATYPE-CLOSE-CURSOR                               ZBANKULP
041100     END-IF.                                                      ZBANKULP
041200     IF SQLCODE IS NOT EQUAL TO ZERO AND                          ZBANKULP
041300        SQLCODE IS NOT EQUAL TO +100                              ZBANKULP
041400        MOVE 'FETCH' TO WS-MSG-DATA-FUNC                          ZBANKULP
041500        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
041600        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
041700        MOVE 'Y' TO WS-BNKATYPE-SQL-ERROR                         ZBANKULP
041800        GO TO BNKATYPE-CLOSE-FILE                                 ZBANKULP
041900     END-IF.                                                      ZBANKULP
042000     GO TO BNKATYPE-READ-CURSOR.                                  ZBANKULP
042100                                                                  ZBANKULP
042200***************************************************************** ZBANKULP
042300* Close the cursor                                              * ZBANKULP
042400***************************************************************** ZBANKULP
042500 BNKATYPE-CLOSE-CURSOR.                                           ZBANKULP
042600     EXEC SQL                                                     ZBANKULP
042700          CLOSE BNKATYPE_CSR                                      ZBANKULP
042800     END-EXEC.                                                    ZBANKULP
042900     IF SQLCODE IS NOT EQUAL TO ZERO                              ZBANKULP
043000        MOVE 'CLOSE_CSR' TO WS-MSG-DATA-FUNC                      ZBANKULP
043100        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
043200        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
043300        MOVE 'Y' TO WS-BNKATYPE-SQL-ERROR                         ZBANKULP
043400        GO TO BNKATYPE-CLOSE-FILE                                 ZBANKULP
043500     END-IF.                                                      ZBANKULP
043600 BNKATYPE-CLOSE-FILE.                                             ZBANKULP
043700     IF WS-BNKATYPE-FILE-STATUS IS GREATER THAN 1                 ZBANKULP
043800        CLOSE UNLOADED-BNKATYPE-FILE                              ZBANKULP
043900        IF WS-UNLOADED-STATUS IS EQUAL TO '00'                    ZBANKULP
044000           ADD 1 TO WS-BNKATYPE-FILE-STATUS                       ZBANKULP
044100        ELSE                                                      ZBANKULP
044200           ADD 4 TO WS-BNKATYPE-FILE-STATUS                       ZBANKULP
044300           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
044400           GO TO BNKATYPE-PROCESS-EXIT                            ZBANKULP
044500        END-IF                                                    ZBANKULP
044600     END-IF.                                                      ZBANKULP
044700 BNKATYPE-PROCESS-EXIT.                                           ZBANKULP
044800     EXIT.                                                        ZBANKULP
044900                                                                  ZBANKULP
045000***************************************************************** ZBANKULP
045100* Process the BNKCUST file                                      * ZBANKULP
045200***************************************************************** ZBANKULP
045300 BNKCUST-PROCESS.                                                 ZBANKULP
045400     MOVE 'BNKCUST' TO WS-MSG-DATA-TABLE.                         ZBANKULP
045500 BNKCUST-OPEN-CURSOR.                                             ZBANKULP
045600     OPEN OUTPUT UNLOADED-BNKCUST-FILE.                           ZBANKULP
045700     IF WS-UNLOADED-STATUS IS EQUAL TO '00'                       ZBANKULP
045800        ADD 1 TO WS-BNKCUST-FILE-STATUS                           ZBANKULP
045900     ELSE                                                         ZBANKULP
046000        ADD 4 TO WS-BNKCUST-FILE-STATUS                           ZBANKULP
046100        PERFORM DISPLAY-IO-STATUS                                 ZBANKULP
046200        GO TO BNKCUST-PROCESS-EXIT                                ZBANKULP
046300     END-IF.                                                      ZBANKULP
046400     EXEC SQL                                                     ZBANKULP
046500          DECLARE BNKCUST_CSR CURSOR FOR                          ZBANKULP
046600          SELECT BCS_PID,                                         ZBANKULP
046700                 BCS_NAME,                                        ZBANKULP
046800                 BCS_NAME_FF,                                     ZBANKULP
046900                 BCS_SIN,                                         ZBANKULP
047000                 BCS_ADDR1,                                       ZBANKULP
047100                 BCS_ADDR2,                                       ZBANKULP
047200                 BCS_STATE,                                       ZBANKULP
047300                 BCS_COUNTRY,                                     ZBANKULP
047400                 BCS_POST_CODE,                                   ZBANKULP
047500                 BCS_TEL,                                         ZBANKULP
047600                 BCS_EMAIL,                                       ZBANKULP
047700                 BCS_SEND_MAIL,                                   ZBANKULP
047800                 BCS_SEND_EMAIL,                                  ZBANKULP
047900                 BCS_FILLER                                       ZBANKULP
048000          FROM BNKCUST                                            ZBANKULP
048100          ORDER BY BCS_PID ASC                                    ZBANKULP
048200          FOR FETCH ONLY                                          ZBANKULP
048300     END-EXEC.                                                    ZBANKULP
048400     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
048500        EXEC SQL                                                  ZBANKULP
048600             OPEN BNKCUST_CSR                                     ZBANKULP
048700        END-EXEC                                                  ZBANKULP
048800        IF SQLCODE IS NOT EQUAL TO ZERO                           ZBANKULP
048900           MOVE 'Y' TO WS-BNKCUST-SQL-ERROR                       ZBANKULP
049000           GO TO BNKCUST-CLOSE-FILE                               ZBANKULP
049100        END-IF                                                    ZBANKULP
049200     ELSE                                                         ZBANKULP
049300        MOVE 'DECLARE_CSR' TO WS-MSG-DATA-FUNC                    ZBANKULP
049400        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
049500        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
049600        MOVE 'Y' TO WS-BNKCUST-SQL-ERROR                          ZBANKULP
049700        GO TO BNKCUST-CLOSE-FILE                                  ZBANKULP
049800     END-IF.                                                      ZBANKULP
049900                                                                  ZBANKULP
050000***************************************************************** ZBANKULP
050100* Now browse the selected rows and move to our area 1 at a time * ZBANKULP
050200***************************************************************** ZBANKULP
050300 BNKCUST-READ-CURSOR.                                             ZBANKULP
050400     EXEC SQL                                                     ZBANKULP
050500          FETCH BNKCUST_CSR                                       ZBANKULP
050600          INTO :DCL-BCS-PID,                                      ZBANKULP
050700               :DCL-BCS-NAME,                                     ZBANKULP
050800               :DCL-BCS-NAME-FF,                                  ZBANKULP
050900               :DCL-BCS-SIN,                                      ZBANKULP
051000               :DCL-BCS-ADDR1,                                    ZBANKULP
051100               :DCL-BCS-ADDR2,                                    ZBANKULP
051200               :DCL-BCS-STATE,                                    ZBANKULP
051300               :DCL-BCS-COUNTRY,                                  ZBANKULP
051400               :DCL-BCS-POST-CODE,                                ZBANKULP
051500               :DCL-BCS-TEL,                                      ZBANKULP
051600               :DCL-BCS-EMAIL,                                    ZBANKULP
051700               :DCL-BCS-SEND-MAIL,                                ZBANKULP
051800               :DCL-BCS-SEND-EMAIL,                               ZBANKULP
051900               :DCL-BCS-FILLER                                    ZBANKULP
052000     END-EXEC.                                                    ZBANKULP
052100     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
052200        MOVE DCL-BCS-NAME TO WS-TWOS-COMP-INPUT                   ZBANKULP
052300        MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT                    ZBANKULP
052400        MOVE LENGTH OF DCL-BCS-NAME TO WS-TWOS-COMP-LEN           ZBANKULP
052500        CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                    ZBANKULP
052600                              WS-TWOS-COMP-INPUT                  ZBANKULP
052700                              WS-TWOS-COMP-OUTPUT                 ZBANKULP
052800        MOVE WS-TWOS-COMP-OUTPUT TO DCL-BCS-NAME-FF               ZBANKULP
052900        MOVE DCLCUST TO UNLOADED-BNKCUST-REC                      ZBANKULP
053000        WRITE UNLOADED-BNKCUST-REC                                ZBANKULP
053100        IF WS-UNLOADED-STATUS IS EQUAL TO 00                      ZBANKULP
053200           ADD 1 TO WS-BNKCUST-REC-COUNT                          ZBANKULP
053300        ELSE                                                      ZBANKULP
053400           ADD 4 TO WS-BNKCUST-FILE-STATUS                        ZBANKULP
053500           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
053600           GO TO BNKCUST-CLOSE-CURSOR                             ZBANKULP
053700        END-IF                                                    ZBANKULP
053800     END-IF.                                                      ZBANKULP
053900     IF SQLCODE IS EQUAL TO +100                                  ZBANKULP
054000        GO TO BNKCUST-CLOSE-CURSOR                                ZBANKULP
054100     END-IF.                                                      ZBANKULP
054200     IF SQLCODE IS NOT EQUAL TO ZERO AND                          ZBANKULP
054300        SQLCODE IS NOT EQUAL TO +100                              ZBANKULP
054400        MOVE 'FETCH' TO WS-MSG-DATA-FUNC                          ZBANKULP
054500        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
054600        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
054700        MOVE 'Y' TO WS-BNKCUST-SQL-ERROR                          ZBANKULP
054800        GO TO BNKCUST-CLOSE-FILE                                  ZBANKULP
054900     END-IF.                                                      ZBANKULP
055000     GO TO BNKCUST-READ-CURSOR.                                   ZBANKULP
055100                                                                  ZBANKULP
055200***************************************************************** ZBANKULP
055300* Close the cursor                                              * ZBANKULP
055400***************************************************************** ZBANKULP
055500 BNKCUST-CLOSE-CURSOR.                                            ZBANKULP
055600     EXEC SQL                                                     ZBANKULP
055700          CLOSE BNKCUST_CSR                                       ZBANKULP
055800     END-EXEC.                                                    ZBANKULP
055900     IF SQLCODE IS NOT EQUAL TO ZERO                              ZBANKULP
056000        MOVE 'CLOSE_CSR' TO WS-MSG-DATA-FUNC                      ZBANKULP
056100        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
056200        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
056300        MOVE 'Y' TO WS-BNKCUST-SQL-ERROR                          ZBANKULP
056400        GO TO BNKCUST-CLOSE-FILE                                  ZBANKULP
056500     END-IF.                                                      ZBANKULP
056600 BNKCUST-CLOSE-FILE.                                              ZBANKULP
056700     IF WS-BNKCUST-FILE-STATUS IS GREATER THAN 1                  ZBANKULP
056800        CLOSE UNLOADED-BNKCUST-FILE                               ZBANKULP
056900        IF WS-UNLOADED-STATUS IS EQUAL TO '00'                    ZBANKULP
057000           ADD 1 TO WS-BNKCUST-FILE-STATUS                        ZBANKULP
057100        ELSE                                                      ZBANKULP
057200           ADD 4 TO WS-BNKCUST-FILE-STATUS                        ZBANKULP
057300           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
057400           GO TO BNKCUST-PROCESS-EXIT                             ZBANKULP
057500        END-IF                                                    ZBANKULP
057600     END-IF.                                                      ZBANKULP
057700 BNKCUST-PROCESS-EXIT.                                            ZBANKULP
057800     EXIT.                                                        ZBANKULP
057900                                                                  ZBANKULP
058000***************************************************************** ZBANKULP
058100* Process the BNKTXN file                                       * ZBANKULP
058200***************************************************************** ZBANKULP
058300 BNKTXN-PROCESS.                                                  ZBANKULP
058400     MOVE 'BNKTXN' TO WS-MSG-DATA-TABLE.                          ZBANKULP
058500 BNKTXN-OPEN-CURSOR.                                              ZBANKULP
058600     OPEN OUTPUT UNLOADED-BNKTXN-FILE.                            ZBANKULP
058700     IF WS-UNLOADED-STATUS IS EQUAL TO '00'                       ZBANKULP
058800        ADD 1 TO WS-BNKTXN-FILE-STATUS                            ZBANKULP
058900     ELSE                                                         ZBANKULP
059000        ADD 4 TO WS-BNKTXN-FILE-STATUS                            ZBANKULP
059100        PERFORM DISPLAY-IO-STATUS                                 ZBANKULP
059200        GO TO BNKTXN-PROCESS-EXIT                                 ZBANKULP
059300     END-IF.                                                      ZBANKULP
059400     EXEC SQL                                                     ZBANKULP
059500          DECLARE BNKTXN_CSR CURSOR FOR                           ZBANKULP
059600          SELECT BTX_PID,                                         ZBANKULP
059700                 BTX_TYPE,                                        ZBANKULP
059800                 BTX_SUB_TYPE,                                    ZBANKULP
059900                 BTX_ACCNO,                                       ZBANKULP
060000                 BTX_TIMESTAMP,                                   ZBANKULP
060100                 BTX_TIMESTAMP_FF,                                ZBANKULP
060200                 BTX_AMOUNT,                                      ZBANKULP
060300                 BTX_DATA_OLD,                                    ZBANKULP
060400                 BTX_DATA_NEW,                                    ZBANKULP
060500                 BTX_FILLER                                       ZBANKULP
060600          FROM BNKTXN                                             ZBANKULP
060700          ORDER BY BTX_TIMESTAMP ASC                              ZBANKULP
060800          FOR FETCH ONLY                                          ZBANKULP
060900     END-EXEC.                                                    ZBANKULP
061000     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
061100        EXEC SQL                                                  ZBANKULP
061200             OPEN BNKTXN_CSR                                      ZBANKULP
061300        END-EXEC                                                  ZBANKULP
061400        IF SQLCODE IS NOT EQUAL TO ZERO                           ZBANKULP
061500           MOVE 'Y' TO WS-BNKTXN-SQL-ERROR                        ZBANKULP
061600           GO TO BNKTXN-CLOSE-FILE                                ZBANKULP
061700        END-IF                                                    ZBANKULP
061800     ELSE                                                         ZBANKULP
061900        MOVE 'DECLARE_CSR' TO WS-MSG-DATA-FUNC                    ZBANKULP
062000        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
062100        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
062200        MOVE 'Y' TO WS-BNKTXN-SQL-ERROR                           ZBANKULP
062300        GO TO BNKTXN-CLOSE-FILE                                   ZBANKULP
062400     END-IF.                                                      ZBANKULP
062500                                                                  ZBANKULP
062600***************************************************************** ZBANKULP
062700* Now browse the selected rows and move to our area 1 at a time * ZBANKULP
062800***************************************************************** ZBANKULP
062900 BNKTXN-READ-CURSOR.                                              ZBANKULP
063000     EXEC SQL                                                     ZBANKULP
063100          FETCH BNKTXN_CSR                                        ZBANKULP
063200          INTO :DCL-BTX-PID,                                      ZBANKULP
063300               :DCL-BTX-TYPE,                                     ZBANKULP
063400               :DCL-BTX-SUB-TYPE,                                 ZBANKULP
063500               :DCL-BTX-ACCNO :DCL-BTX-ACCNO-NULL,                ZBANKULP
063600               :DCL-BTX-TIMESTAMP,                                ZBANKULP
063700               :DCL-BTX-TIMESTAMP-FF,                             ZBANKULP
063800               :DCL-BTX-AMOUNT,                                   ZBANKULP
063900               :DCL-BTX-DATA-OLD,                                 ZBANKULP
064000               :DCL-BTX-DATA-NEW,                                 ZBANKULP
064100               :DCL-BTX-FILLER                                    ZBANKULP
064200     END-EXEC.                                                    ZBANKULP
064300     IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
064400        IF DCL-BTX-ACCNO-NULL IS LESS THAN ZERO                   ZBANKULP
064500           MOVE SPACES TO DCL-BTX-ACCNO                           ZBANKULP
064600        END-IF                                                    ZBANKULP
064700        MOVE DCL-BTX-TIMESTAMP TO WS-TWOS-COMP-INPUT              ZBANKULP
064800        MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT                    ZBANKULP
064900        MOVE LENGTH OF DCL-BTX-TIMESTAMP TO WS-TWOS-COMP-LEN      ZBANKULP
065000        CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                    ZBANKULP
065100                              WS-TWOS-COMP-INPUT                  ZBANKULP
065200                              WS-TWOS-COMP-OUTPUT                 ZBANKULP
065300        MOVE WS-TWOS-COMP-OUTPUT TO DCL-BTX-TIMESTAMP-FF          ZBANKULP
065400        MOVE DCLTXN TO UNLOADED-BNKTXN-REC                        ZBANKULP
065500        WRITE UNLOADED-BNKTXN-REC                                 ZBANKULP
065600        IF WS-UNLOADED-STATUS IS EQUAL TO 00                      ZBANKULP
065700           ADD 1 TO WS-BNKTXN-REC-COUNT                           ZBANKULP
065800        ELSE                                                      ZBANKULP
065900           ADD 4 TO WS-BNKTXN-FILE-STATUS                         ZBANKULP
066000           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
066100           GO TO BNKTXN-CLOSE-CURSOR                              ZBANKULP
066200        END-IF                                                    ZBANKULP
066300     END-IF.                                                      ZBANKULP
066400     IF SQLCODE IS EQUAL TO +100                                  ZBANKULP
066500        GO TO BNKTXN-CLOSE-CURSOR                                 ZBANKULP
066600     END-IF.                                                      ZBANKULP
066700     IF SQLCODE IS NOT EQUAL TO ZERO AND                          ZBANKULP
066800        SQLCODE IS NOT EQUAL TO +100                              ZBANKULP
066900        MOVE 'FETCH' TO WS-MSG-DATA-FUNC                          ZBANKULP
067000        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
067100        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
067200        MOVE 'Y' TO WS-BNKTXN-SQL-ERROR                           ZBANKULP
067300        GO TO BNKTXN-CLOSE-FILE                                   ZBANKULP
067400     END-IF.                                                      ZBANKULP
067500     GO TO BNKTXN-READ-CURSOR.                                    ZBANKULP
067600                                                                  ZBANKULP
067700***************************************************************** ZBANKULP
067800* Close the cursor                                              * ZBANKULP
067900***************************************************************** ZBANKULP
068000 BNKTXN-CLOSE-CURSOR.                                             ZBANKULP
068100     EXEC SQL                                                     ZBANKULP
068200          CLOSE BNKTXN_CSR                                        ZBANKULP
068300     END-EXEC.                                                    ZBANKULP
068400     IF SQLCODE IS NOT EQUAL TO ZERO                              ZBANKULP
068500        MOVE 'CLOSE_CSR' TO WS-MSG-DATA-FUNC                      ZBANKULP
068600        MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
068700        PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
068800        MOVE 'Y' TO WS-BNKTXN-SQL-ERROR                           ZBANKULP
068900        GO TO BNKTXN-CLOSE-FILE                                   ZBANKULP
069000     END-IF.                                                      ZBANKULP
069100 BNKTXN-CLOSE-FILE.                                               ZBANKULP
069200     IF WS-BNKTXN-FILE-STATUS IS GREATER THAN 1                   ZBANKULP
069300        CLOSE UNLOADED-BNKTXN-FILE                                ZBANKULP
069400        IF WS-UNLOADED-STATUS IS EQUAL TO '00'                    ZBANKULP
069500           ADD 1 TO WS-BNKTXN-FILE-STATUS                         ZBANKULP
069600        ELSE                                                      ZBANKULP
069700           ADD 4 TO WS-BNKTXN-FILE-STATUS                         ZBANKULP
069800           PERFORM DISPLAY-IO-STATUS                              ZBANKULP
069900           GO TO BNKTXN-PROCESS-EXIT                              ZBANKULP
070000        END-IF                                                    ZBANKULP
070100     END-IF.                                                      ZBANKULP
070200 BNKTXN-PROCESS-EXIT.                                             ZBANKULP
070300     EXIT.                                                        ZBANKULP
070400                                                                  ZBANKULP
070500***************************************************************** ZBANKULP
070600* Process the BNKHELP file                                      * ZBANKULP
070700***************************************************************** ZBANKULP
070800*BNKHELP-PROCESS.                                                 ZBANKULP
070900*    MOVE 'BNKHELP' TO WS-MSG-DATA-TABLE.                         ZBANKULP
071000*BNKHELP-OPEN-CURSOR.                                             ZBANKULP
071100*    OPEN OUTPUT UNLOADED-BNKHELP-FILE.                           ZBANKULP
071200*    IF WS-UNLOADED-STATUS IS EQUAL TO '00'                       ZBANKULP
071300*       ADD 1 TO WS-BNKHELP-FILE-STATUS                           ZBANKULP
071400*    ELSE                                                         ZBANKULP
071500*       ADD 4 TO WS-BNKHELP-FILE-STATUS                           ZBANKULP
071600*       PERFORM DISPLAY-IO-STATUS                                 ZBANKULP
071700*       GO TO BNKHELP-PROCESS-EXIT                                ZBANKULP
071800*    END-IF.                                                      ZBANKULP
071900*    EXEC SQL                                                     ZBANKULP
072000*         DECLARE BNKHELP_CSR CURSOR FOR                          ZBANKULP
072100*         SELECT BHP_SCRN,                                        ZBANKULP
072200*                BHP_LINE,                                        ZBANKULP
072300*                BHP_TEXT                                         ZBANKULP
072400*         FROM BNKHELP                                            ZBANKULP
072500*         ORDER BY BHP_SCRN, BHP_LINE ASC                         ZBANKULP
072600*         FOR FETCH ONLY                                          ZBANKULP
072700*    END-EXEC.                                                    ZBANKULP
072800*    IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
072900*       EXEC SQL                                                  ZBANKULP
073000*            OPEN BNKHELP_CSR                                     ZBANKULP
073100*       END-EXEC                                                  ZBANKULP
073200*       IF SQLCODE IS NOT EQUAL TO ZERO                           ZBANKULP
073300*          MOVE 'Y' TO WS-BNKHELP-SQL-ERROR                       ZBANKULP
073400*          GO TO BNKHELP-CLOSE-FILE                               ZBANKULP
073500*       END-IF                                                    ZBANKULP
073600*    ELSE                                                         ZBANKULP
073700*       MOVE 'DECLARE_CSR' TO WS-MSG-DATA-FUNC                    ZBANKULP
073800*       MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
073900*       PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
074000*       MOVE 'Y' TO WS-BNKHELP-SQL-ERROR                          ZBANKULP
074100*       GO TO BNKHELP-CLOSE-FILE                                  ZBANKULP
074200*    END-IF.                                                      ZBANKULP
074300                                                                  ZBANKULP
074400***************************************************************** ZBANKULP
074500* Now browse the selected rows and move to our area 1 at a time * ZBANKULP
074600***************************************************************** ZBANKULP
074700*BNKHELP-READ-CURSOR.                                             ZBANKULP
074800*    EXEC SQL                                                     ZBANKULP
074900*         FETCH BNKHELP_CSR                                       ZBANKULP
075000*         INTO :DCL-BHP-SCRN,                                     ZBANKULP
075100*              :DCL-BHP-LINE,                                     ZBANKULP
075200*              :DCL-BHP-TEXT                                      ZBANKULP
075300*    END-EXEC.                                                    ZBANKULP
075400*    IF SQLCODE IS EQUAL TO ZERO                                  ZBANKULP
075500*       MOVE DCLHELP TO UNLOADED-BNKHELP-REC                      ZBANKULP
075600*       WRITE UNLOADED-BNKHELP-REC                                ZBANKULP
075700*       IF WS-UNLOADED-STATUS IS EQUAL TO 00                      ZBANKULP
075800*          ADD 1 TO WS-BNKHELP-REC-COUNT                          ZBANKULP
075900*       ELSE                                                      ZBANKULP
076000*          ADD 4 TO WS-BNKHELP-FILE-STATUS                        ZBANKULP
076100*          PERFORM DISPLAY-IO-STATUS                              ZBANKULP
076200*          GO TO BNKHELP-CLOSE-CURSOR                             ZBANKULP
076300*       END-IF                                                    ZBANKULP
076400*    END-IF.                                                      ZBANKULP
076500*    IF SQLCODE IS EQUAL TO +100                                  ZBANKULP
076600*       GO TO BNKHELP-CLOSE-CURSOR                                ZBANKULP
076700*    END-IF.                                                      ZBANKULP
076800*    IF SQLCODE IS NOT EQUAL TO ZERO AND                          ZBANKULP
076900*       SQLCODE IS NOT EQUAL TO +100                              ZBANKULP
077000*       MOVE 'FETCH' TO WS-MSG-DATA-FUNC                          ZBANKULP
077100*       MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
077200*       PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
077300*       MOVE 'Y' TO WS-BNKHELP-SQL-ERROR                          ZBANKULP
077400*       GO TO BNKHELP-CLOSE-FILE                                  ZBANKULP
077500*    END-IF.                                                      ZBANKULP
077600*    GO TO BNKHELP-READ-CURSOR.                                   ZBANKULP
077700                                                                  ZBANKULP
077800***************************************************************** ZBANKULP
077900* Close the cursor                                              * ZBANKULP
078000***************************************************************** ZBANKULP
078100*BNKHELP-CLOSE-CURSOR.                                            ZBANKULP
078200*    EXEC SQL                                                     ZBANKULP
078300*         CLOSE BNKHELP_CSR                                       ZBANKULP
078400*    END-EXEC.                                                    ZBANKULP
078500*    IF SQLCODE IS NOT EQUAL TO ZERO                              ZBANKULP
078600*       MOVE 'CLOSE_CSR' TO WS-MSG-DATA-FUNC                      ZBANKULP
078700*       MOVE SQLCODE TO WS-MSG-DATA-SQLCODE                       ZBANKULP
078800*       PERFORM DISPLAY-SQL-STATUS                                ZBANKULP
078900*       MOVE 'Y' TO WS-BNKHELP-SQL-ERROR                          ZBANKULP
079000*       GO TO BNKHELP-CLOSE-FILE                                  ZBANKULP
079100*    END-IF.                                                      ZBANKULP
079200*BNKHELP-CLOSE-FILE.                                              ZBANKULP
079300*    IF WS-BNKHELP-FILE-STATUS IS GREATER THAN 1                  ZBANKULP
079400*       CLOSE UNLOADED-BNKHELP-FILE                               ZBANKULP
079500*       IF WS-UNLOADED-STATUS IS EQUAL TO '00'                    ZBANKULP
079600*          ADD 1 TO WS-BNKHELP-FILE-STATUS                        ZBANKULP
079700*       ELSE                                                      ZBANKULP
079800*          ADD 4 TO WS-BNKHELP-FILE-STATUS                        ZBANKULP
079900*          PERFORM DISPLAY-IO-STATUS                              ZBANKULP
080000*          GO TO BNKHELP-PROCESS-EXIT                             ZBANKULP
080100*       END-IF                                                    ZBANKULP
080200*    END-IF.                                                      ZBANKULP
080300*BNKHELP-PROCESS-EXIT.                                            ZBANKULP
080400*    EXIT.                                                        ZBANKULP
080500                                                                  ZBANKULP
080600***************************************************************** ZBANKULP
080700* Display the SQL status code for the given table and function  * ZBANKULP
080800***************************************************************** ZBANKULP
080900 DISPLAY-SQL-STATUS.                                              ZBANKULP
081000     MOVE SPACE TO WS-CONSOLE-MESSAGE.                            ZBANKULP
081100     STRING WS-MSG-DATA-TABLE DELIMITED BY SIZE                   ZBANKULP
081200            ' SQL Code: ' DELIMITED BY SIZE                       ZBANKULP
081300            WS-MSG-DATA-SQLCODE DELIMITED BY SIZE                 ZBANKULP
081400            ' on ' DELIMITED BY SIZE                              ZBANKULP
081500            WS-MSG-DATA-FUNC DELIMITED BY SPACE                   ZBANKULP
081600       INTO WS-CONSOLE-MESSAGE.                                   ZBANKULP
081700     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBANKULP
081800                                                                  ZBANKULP
081900***************************************************************** ZBANKULP
082000* Display the file status bytes. This routine will display as   * ZBANKULP
082100* two digits if the full two byte file status is numeric. If    * ZBANKULP
082200* second byte is non-numeric then it will be treated as a       * ZBANKULP
082300* binary number.                                                * ZBANKULP
082400***************************************************************** ZBANKULP
082500 DISPLAY-IO-STATUS.                                               ZBANKULP
082600     IF WS-IO-STATUS NUMERIC                                      ZBANKULP
082700        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBANKULP
082800        STRING WS-MSG-DATA-TABLE DELIMITED BY SIZE                ZBANKULP
082900               ' File status -' DELIMITED BY SIZE                 ZBANKULP
083000               WS-IO-STATUS DELIMITED BY SIZE                     ZBANKULP
083100          INTO WS-CONSOLE-MESSAGE                                 ZBANKULP
083200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBANKULP
083300     ELSE                                                         ZBANKULP
083400        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     ZBANKULP
083500        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    ZBANKULP
083600        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBANKULP
083700        STRING WS-MSG-DATA-TABLE DELIMITED BY SIZE                ZBANKULP
083800               ' File status -' DELIMITED BY SIZE                 ZBANKULP
083900               WS-IO-STAT1 DELIMITED BY SIZE                      ZBANKULP
084000               '/' DELIMITED BY SIZE                              ZBANKULP
084100               WS-TWO-BYTES DELIMITED BY SIZE                     ZBANKULP
084200          INTO WS-CONSOLE-MESSAGE                                 ZBANKULP
084300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBANKULP
084400     END-IF.                                                      ZBANKULP
084500                                                                  ZBANKULP
084600***************************************************************** ZBANKULP
084700* Display CONSOLE messages...                                   * ZBANKULP
084800***************************************************************** ZBANKULP
084900 DISPLAY-CONSOLE-MESSAGE.                                         ZBANKULP
085000     DISPLAY 'ZBANKULP - ' WS-CONSOLE-MESSAGE.                    ZBANKULP
085100     DISPLAY 'ZBANKULP - ' WS-CONSOLE-MESSAGE                     ZBANKULP
085200       UPON CONSOLE.                                              ZBANKULP
085300     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBANKULP
085400                                                                  ZBANKULP
