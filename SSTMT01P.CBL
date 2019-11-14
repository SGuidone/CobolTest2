000100***************************************************************** sstmt01p
000200*                                                               * sstmt01p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sstmt01p
000400*   This demonstration program is provided for use by users     * sstmt01p
000500*   of Micro Focus products and may be used, modified and       * sstmt01p
000600*   distributed as part of your application provided that       * sstmt01p
000700*   you properly acknowledge the copyright of Micro Focus       * sstmt01p
000800*   in this material.                                           * sstmt01p
000900*                                                               * sstmt01p
001000***************************************************************** sstmt01p
001100***************************************************************** sstmt01p
001200* Program:     SSTMT01P.CBL (CICS Version)                      * sstmt01p
001300* Layer:       Transaction manager specific                     * sstmt01p
001400* Function:    Create statement print request                   * sstmt01p
001500***************************************************************** sstmt01p
001600                                                                  sstmt01p
001700 IDENTIFICATION DIVISION.                                         sstmt01p
001800 PROGRAM-ID.                                                      sstmt01p
001900     SSTMT01P.                                                    sstmt01p
002000 DATE-WRITTEN.                                                    sstmt01p
002100     September 2002.                                              sstmt01p
002200 DATE-COMPILED.                                                   sstmt01p
002300     Today.                                                       sstmt01p
002400                                                                  sstmt01p
002500 ENVIRONMENT DIVISION.                                            sstmt01p
002600                                                                  sstmt01p
002700 DATA DIVISION.                                                   sstmt01p
002800                                                                  sstmt01p
002900 WORKING-STORAGE SECTION.                                         sstmt01p
003000 01  WS-MISC-STORAGE.                                             sstmt01p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sstmt01p
003200       VALUE 'SSTMT01P'.                                          sstmt01p
003300   05  WS-COMMAREA-LENGTH                    PIC 9(5).            sstmt01p
003400   05  WS-RESP                               PIC S9(8) COMP.      sstmt01p
003500                                                                  sstmt01p
003600 01  WS-COMMAREA.                                                 sstmt01p
003700 COPY CSTMTD01.                                                   sstmt01p
003800                                                                  sstmt01p
003900 01  WS-PRINT-MSG-AREA.                                           sstmt01p
004000   05  FILLER                                PIC X(28)            sstmt01p
004100       VALUE 'Accepted print request for: '.                      sstmt01p
004200   05  WS-PRINT-MSG-UID                      PIC X(5).            sstmt01p
004300   05  FILLER                                PIC X(10)            sstmt01p
004400       VALUE '. Send by '.                                        sstmt01p
004500   05  WS-PRINT-MSG-METHOD                   PIC X(13).           sstmt01p
004600                                                                  sstmt01p
004700 01  WS-INTRDR-QUEUE                         PIC X(4)             sstmt01p
004800     VALUE 'IRDR'.                                                sstmt01p
004900                                                                  sstmt01p
005000 01  WS-PERFORM-COUNTER                      PIC 9(3).            sstmt01p
005100                                                                  sstmt01p
005200 COPY CSTMTJCL.                                                   sstmt01p
005300                                                                  sstmt01p
005400 COPY CABENDD.                                                    sstmt01p
005500                                                                  sstmt01p
005600 LINKAGE SECTION.                                                 sstmt01p
005700 01  DFHCOMMAREA.                                                 sstmt01p
005800   05  LK-COMMAREA                           PIC X(1)             sstmt01p
005900       OCCURS 1 TO 4096 TIMES                                     sstmt01p
006000         DEPENDING ON WS-COMMAREA-LENGTH.                         sstmt01p
006100                                                                  sstmt01p
006200 COPY CENTRY.                                                     sstmt01p
006300***************************************************************** sstmt01p
006400* Move the passed data to our area                              * sstmt01p
006500***************************************************************** sstmt01p
006600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            sstmt01p
006700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             sstmt01p
006800                                                                  sstmt01p
006900***************************************************************** sstmt01p
007000* Initialize our output area                                    * sstmt01p
007100***************************************************************** sstmt01p
007200     MOVE SPACES TO CSTMTD01O-DATA.                               sstmt01p
007300                                                                  sstmt01p
007400***************************************************************** sstmt01p
007500* Set up message to go to log                                   * sstmt01p
007600***************************************************************** sstmt01p
007700     MOVE CSTMTD01I-CONTACT-ID TO WS-PRINT-MSG-UID.               sstmt01p
007800     IF CSTMTD01I-POST                                            sstmt01p
007900        MOVE Z'regular mail' TO WS-PRINT-MSG-METHOD               sstmt01p
008000     END-IF.                                                      sstmt01p
008100     IF CSTMTD01I-EMAIL                                           sstmt01p
008200        MOVE Z'E-Mail' TO WS-PRINT-MSG-METHOD                     sstmt01p
008300     END-IF.                                                      sstmt01p
008400                                                                  sstmt01p
008500***************************************************************** sstmt01p
008600* Write the log message                                         * sstmt01p
008700***************************************************************** sstmt01p
008800     EXEC CICS WRITE                                              sstmt01p
008900               OPERATOR                                           sstmt01p
009000               TEXT(WS-PRINT-MSG-AREA)                            sstmt01p
009100               TEXTLENGTH(LENGTH OF WS-PRINT-MSG-AREA)            sstmt01p
009200     END-EXEC.                                                    sstmt01p
009300                                                                  sstmt01p
009400***************************************************************** sstmt01p
009500* Set up the JCL to run the job                                 * sstmt01p
009600***************************************************************** sstmt01p
009700     INSPECT WS-JCL-CARD-TABLE                                    sstmt01p
009800       REPLACING ALL '%%%%%' BY CSTMTD01I-CONTACT-ID.             sstmt01p
009900                                                                  sstmt01p
010000     DIVIDE LENGTH OF WS-JCL-CARD(1) INTO                         sstmt01p
010100       LENGTH OF WS-JCL-CARD-TABLE GIVING WS-JCL-CARD-COUNT.      sstmt01p
010200                                                                  sstmt01p
010300***************************************************************** sstmt01p
010400* Write the JCL to the internal reader TD queue                 * sstmt01p
010500***************************************************************** sstmt01p
010600     EXEC CICS ENQ                                                sstmt01p
010700          RESOURCE(WS-INTRDR-QUEUE)                               sstmt01p
010800          RESP(WS-RESP)                                           sstmt01p
010900     END-EXEC.                                                    sstmt01p
011000                                                                  sstmt01p
011100     PERFORM VARYING WS-PERFORM-COUNTER FROM 1 BY 1               sstmt01p
011200       UNTIL WS-PERFORM-COUNTER IS GREATER THAN WS-JCL-CARD-COUNT sstmt01p
011300         EXEC CICS WRITEQ TD                                      sstmt01p
011400              QUEUE(WS-INTRDR-QUEUE)                              sstmt01p
011500              FROM(WS-JCL-CARD(WS-PERFORM-COUNTER))               sstmt01p
011600              RESP(WS-RESP)                                       sstmt01p
011700         END-EXEC                                                 sstmt01p
011800     END-PERFORM.                                                 sstmt01p
011900                                                                  sstmt01p
012000***************************************************************** sstmt01p
012100* Move the result back to the callers area                      * sstmt01p
012200***************************************************************** sstmt01p
012300     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       sstmt01p
012400                                                                  sstmt01p
012500***************************************************************** sstmt01p
012600* Return to our caller                                          * sstmt01p
012700***************************************************************** sstmt01p
012800 COPY CRETURN.                                                    sstmt01p
012900                                                                  sstmt01p
013000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sstmt01p
