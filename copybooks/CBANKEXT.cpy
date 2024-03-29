000100***************************************************************** cbankext
000200*                                                               * cbankext
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankext
000400*                                                               * cbankext
000500***************************************************************** cbankext
000600                                                                  cbankext
000700***************************************************************** cbankext
000800* CBANKEXT.CPY                                                  * cbankext
000900*---------------------------------------------------------------* cbankext
001000***************************************************************** cbankext
001100     10  EXT-DATA                            PIC X(1024).         cbankext
001200                                                                  cbankext
001300     10  EXT-IP-DATA REDEFINES EXT-DATA.                          cbankext
001400       15  EXT-IP-AID                        PIC X(5).            cbankext
001500       15  EXT-IP-AREA                       PIC X(256).          cbankext
001600       15  EXT-IP00-DATA REDEFINES EXT-IP-AREA.                   cbankext
001700         20  EXT-IP00-SEL                    PIC X(1).            cbankext
001800       15  EXT-IP10-DATA REDEFINES EXT-IP-AREA.                   cbankext
001900         20  EXT-IP10-USERID                 PIC X(5).            cbankext
002000         20  EXT-IP10-PSWD                   PIC X(8).            cbankext
002100       15  EXT-IP20-DATA REDEFINES EXT-IP-AREA.                   cbankext
002200         20  EXT-IP20-SEL1ID                 PIC X(1).            cbankext
002300         20  EXT-IP20-SEL1IP                 PIC X(1).            cbankext
002400         20  EXT-IP20-SEL2ID                 PIC X(1).            cbankext
002500         20  EXT-IP20-SEL2IP                 PIC X(1).            cbankext
002600         20  EXT-IP20-SEL3ID                 PIC X(1).            cbankext
002700         20  EXT-IP20-SEL3IP                 PIC X(1).            cbankext
002800         20  EXT-IP20-SEL4ID                 PIC X(1).            cbankext
002900         20  EXT-IP20-SEL4IP                 PIC X(1).            cbankext
003000         20  EXT-IP20-SEL5ID                 PIC X(1).            cbankext
003100         20  EXT-IP20-SEL5IP                 PIC X(1).            cbankext
003200         20  EXT-IP20-SEL6ID                 PIC X(1).            cbankext
003300         20  EXT-IP20-SEL6IP                 PIC X(1).            cbankext
003400         20  EXT-IP20-SEL7ID                 PIC X(1).            cbankext
003500         20  EXT-IP20-SEL7IP                 PIC X(1).            cbankext
003600       15  EXT-IP30-DATA REDEFINES EXT-IP-AREA.                   cbankext
003700         20  EXT-IP30-DET1                   PIC X(1).            cbankext
003800         20  EXT-IP30-DET2                   PIC X(1).            cbankext
003900         20  EXT-IP30-DET3                   PIC X(1).            cbankext
004000         20  EXT-IP30-DET4                   PIC X(1).            cbankext
004100         20  EXT-IP30-DET5                   PIC X(1).            cbankext
004200         20  EXT-IP30-DET6                   PIC X(1).            cbankext
004300       15  EXT-IP35-DATA REDEFINES EXT-IP-AREA.                   cbankext
004400         20  EXT-IP35-DUMMY                  PIC X(1).            cbankext
004500       15  EXT-IP40-DATA REDEFINES EXT-IP-AREA.                   cbankext
004600         20  EXT-IP40-DUMMY                  PIC X(1).            cbankext
004700       15  EXT-IP50-DATA REDEFINES EXT-IP-AREA.                   cbankext
004800         20  EXT-IP50-XFER                   PIC X(8).            cbankext
004900         20  EXT-IP50-FRM1                   PIC X(1).            cbankext
005000         20  EXT-IP50-TO1                    PIC X(1).            cbankext
005100         20  EXT-IP50-FRM2                   PIC X(1).            cbankext
005200         20  EXT-IP50-TO2                    PIC X(1).            cbankext
005300         20  EXT-IP50-FRM3                   PIC X(1).            cbankext
005400         20  EXT-IP50-TO3                    PIC X(1).            cbankext
005500         20  EXT-IP50-FRM4                   PIC X(1).            cbankext
005600         20  EXT-IP50-TO4                    PIC X(1).            cbankext
005700         20  EXT-IP50-FRM5                   PIC X(1).            cbankext
005800         20  EXT-IP50-TO5                    PIC X(1).            cbankext
005900         20  EXT-IP50-FRM6                   PIC X(1).            cbankext
006000         20  EXT-IP50-TO6                    PIC X(1).            cbankext
006100       15  EXT-IP60-DATA REDEFINES EXT-IP-AREA.                   cbankext
006200         20  EXT-IP60-NADDR1                 PIC X(25).           cbankext
006300         20  EXT-IP60-NADDR2                 PIC X(25).           cbankext
006400         20  EXT-IP60-NSTATE                 PIC X(2).            cbankext
006500         20  EXT-IP60-NCNTRY                 PIC X(6).            cbankext
006600         20  EXT-IP60-NPSTCDE                PIC X(6).            cbankext
006700         20  EXT-IP60-NTELNO                 PIC X(12).           cbankext
006800         20  EXT-IP60-NEMAIL                 PIC X(30).           cbankext
006900         20  EXT-IP60-NSMAIL                 PIC X(1).            cbankext
007000         20  EXT-IP60-NSEMAIL                PIC X(1).            cbankext
007100       15  EXT-IP70-DATA REDEFINES EXT-IP-AREA.                   cbankext
007200         20  EXT-IP70-AMOUNT                 PIC X(7).            cbankext
007300         20  EXT-IP70-RATE                   PIC X(5).            cbankext
007400         20  EXT-IP70-TERM                   PIC X(5).            cbankext
007500       15  EXT-IP80-DATA REDEFINES EXT-IP-AREA.                   cbankext
007600         20  EXT-IP80-OPT1                   PIC X(1).            cbankext
007700         20  EXT-IP80-OPT2                   PIC X(1).            cbankext
007800       15  EXT-IPZZ-DATA REDEFINES EXT-IP-AREA.                   cbankext
007900         20  EXT-IPZZ-SEL1ID                 PIC X(1).            cbankext
008000         20  EXT-IPZZ-SEL1IP                 PIC X(1).            cbankext
008100         20  EXT-IPZZ-SEL2ID                 PIC X(1).            cbankext
008200         20  EXT-IPZZ-SEL2IP                 PIC X(1).            cbankext
008300         20  EXT-IPZZ-SEL3ID                 PIC X(1).            cbankext
008400         20  EXT-IPZZ-SEL3IP                 PIC X(1).            cbankext
008500         20  EXT-IPZZ-SEL4ID                 PIC X(1).            cbankext
008600         20  EXT-IPZZ-SEL4IP                 PIC X(1).            cbankext
008700         20  EXT-IPZZ-SEL5ID                 PIC X(1).            cbankext
008800         20  EXT-IPZZ-SEL5IP                 PIC X(1).            cbankext
008900         20  EXT-IPZZ-SEL6ID                 PIC X(1).            cbankext
009000         20  EXT-IPZZ-SEL6IP                 PIC X(1).            cbankext
009100         20  EXT-IPZZ-SEL7ID                 PIC X(1).            cbankext
009200         20  EXT-IPZZ-SEL7IP                 PIC X(1).            cbankext
009300         20  EXT-IPZZ-SEL8ID                 PIC X(1).            cbankext
009400         20  EXT-IPZZ-SEL8IP                 PIC X(1).            cbankext
009500                                                                  cbankext
009600     10  EXT-OP-DATA REDEFINES EXT-DATA.                          cbankext
009700       15  EXT-OP-TRAN                       PIC X(4).            cbankext
009800       15  EXT-OP-SCREEN                     PIC X(8).            cbankext
009900       15  EXT-OP-DATE                       PIC X(11).           cbankext
010000       15  EXT-OP-TIME                       PIC X(8).            cbankext
010100       15  EXT-OP-HEAD1                      PIC X(50).           cbankext
010200       15  EXT-OP-HEAD2                      PIC X(50).           cbankext
010300       15  EXT-OP-VERSION                    PIC X(7).            cbankext
010400       15  EXT-OP-ERR-MSG                    PIC X(75).           cbankext
010500       15  EXT-OP-USERID                     PIC X(5).            cbankext
010600       15  EXT-OP-NAME                       PIC X(25).           cbankext
010700       15  EXT-OP-AREA                       PIC X(768).          cbankext
010800       15  EXT-OP00-DATA REDEFINES EXT-OP-AREA.                   cbankext
010900         20  EXT-OP00-TEXT                   PIC X(8).            cbankext
011000       15  EXT-OP10-DATA REDEFINES EXT-OP-AREA.                   cbankext
011100         20  EXT-OP10-PSWD                   PIC X(8).            cbankext
011200       15  EXT-OP10-DATA REDEFINES EXT-OP-AREA.                   cbankext
011300         20  EXT-OP10-NAME                   PIC X(25).           cbankext
011400       15  EXT-OP20-DATA REDEFINES EXT-OP-AREA.                   cbankext
011500         20  EXT-OP20-SEL1ID                 PIC X(1).            cbankext
011600         20  EXT-OP20-SEL1IP                 PIC X(1).            cbankext
011700         20  EXT-OP20-SEL1TX                 PIC X(40).           cbankext
011800         20  EXT-OP20-SEL2ID                 PIC X(1).            cbankext
011900         20  EXT-OP20-SEL2IP                 PIC X(1).            cbankext
012000         20  EXT-OP20-SEL2TX                 PIC X(40).           cbankext
012100         20  EXT-OP20-SEL3ID                 PIC X(1).            cbankext
012200         20  EXT-OP20-SEL3IP                 PIC X(1).            cbankext
012300         20  EXT-OP20-SEL3TX                 PIC X(40).           cbankext
012400         20  EXT-OP20-SEL4ID                 PIC X(1).            cbankext
012500         20  EXT-OP20-SEL4IP                 PIC X(1).            cbankext
012600         20  EXT-OP20-SEL4TX                 PIC X(40).           cbankext
012700         20  EXT-OP20-SEL5ID                 PIC X(1).            cbankext
012800         20  EXT-OP20-SEL5IP                 PIC X(1).            cbankext
012900         20  EXT-OP20-SEL5TX                 PIC X(40).           cbankext
013000         20  EXT-OP20-SEL6ID                 PIC X(1).            cbankext
013100         20  EXT-OP20-SEL6IP                 PIC X(1).            cbankext
013200         20  EXT-OP20-SEL6TX                 PIC X(40).           cbankext
013300         20  EXT-OP20-SEL7ID                 PIC X(1).            cbankext
013400         20  EXT-OP20-SEL7IP                 PIC X(1).            cbankext
013500         20  EXT-OP20-SEL7TX                 PIC X(40).           cbankext
013600       15  EXT-OP30-DATA REDEFINES EXT-OP-AREA.                   cbankext
013700         20  EXT-OP30-DET1                   PIC X(9).            cbankext
013800         20  EXT-OP30-ACC1                   PIC X(9).            cbankext
013900         20  EXT-OP30-DSC1                   PIC X(15).           cbankext
014000         20  EXT-OP30-BAL1                   PIC X(13).           cbankext
014100         20  EXT-OP30-SRV1                   PIC X(6).            cbankext
014200         20  EXT-OP30-DTE1                   PIC X(11).           cbankext
014300         20  EXT-OP30-TXN1                   PIC X(1).            cbankext
014400         20  EXT-OP30-TXS1                   PIC X(1).            cbankext
014500         20  EXT-OP30-DET2                   PIC X(9).            cbankext
014600         20  EXT-OP30-ACC2                   PIC X(9).            cbankext
014700         20  EXT-OP30-DSC2                   PIC X(15).           cbankext
014800         20  EXT-OP30-BAL2                   PIC X(13).           cbankext
014900         20  EXT-OP30-SRV2                   PIC X(6).            cbankext
015000         20  EXT-OP30-DTE2                   PIC X(11).           cbankext
015100         20  EXT-OP30-TXN2                   PIC X(1).            cbankext
015200         20  EXT-OP30-TXS2                   PIC X(1).            cbankext
015300         20  EXT-OP30-DET3                   PIC X(9).            cbankext
015400         20  EXT-OP30-ACC3                   PIC X(9).            cbankext
015500         20  EXT-OP30-DSC3                   PIC X(15).           cbankext
015600         20  EXT-OP30-BAL3                   PIC X(13).           cbankext
015700         20  EXT-OP30-SRV3                   PIC X(6).            cbankext
015800         20  EXT-OP30-DTE3                   PIC X(11).           cbankext
015900         20  EXT-OP30-TXN3                   PIC X(1).            cbankext
016000         20  EXT-OP30-TXS3                   PIC X(1).            cbankext
016100         20  EXT-OP30-DET4                   PIC X(9).            cbankext
016200         20  EXT-OP30-ACC4                   PIC X(9).            cbankext
016300         20  EXT-OP30-DSC4                   PIC X(15).           cbankext
016400         20  EXT-OP30-BAL4                   PIC X(13).           cbankext
016500         20  EXT-OP30-SRV4                   PIC X(6).            cbankext
016600         20  EXT-OP30-DTE4                   PIC X(11).           cbankext
016700         20  EXT-OP30-TXN4                   PIC X(1).            cbankext
016800         20  EXT-OP30-TXS4                   PIC X(1).            cbankext
016900         20  EXT-OP30-DET5                   PIC X(9).            cbankext
017000         20  EXT-OP30-ACC5                   PIC X(9).            cbankext
017100         20  EXT-OP30-DSC5                   PIC X(15).           cbankext
017200         20  EXT-OP30-BAL5                   PIC X(13).           cbankext
017300         20  EXT-OP30-SRV5                   PIC X(6).            cbankext
017400         20  EXT-OP30-DTE5                   PIC X(11).           cbankext
017500         20  EXT-OP30-TXN5                   PIC X(1).            cbankext
017600         20  EXT-OP30-TXS5                   PIC X(1).            cbankext
017700         20  EXT-OP30-DET6                   PIC X(9).            cbankext
017800         20  EXT-OP30-ACC6                   PIC X(9).            cbankext
017900         20  EXT-OP30-DSC6                   PIC X(15).           cbankext
018000         20  EXT-OP30-BAL6                   PIC X(13).           cbankext
018100         20  EXT-OP30-SRV6                   PIC X(6).            cbankext
018200         20  EXT-OP30-DTE6                   PIC X(11).           cbankext
018300         20  EXT-OP30-TXN6                   PIC X(1).            cbankext
018400         20  EXT-OP30-TXS6                   PIC X(1).            cbankext
018500       15  EXT-OP35-DATA REDEFINES EXT-OP-AREA.                   cbankext
018600         20  EXT-OP35-ACCNO                  PIC X(9).            cbankext
018700         20  EXT-OP35-ACCTYPE                PIC X(15).           cbankext
018800         20  EXT-OP35-BALANCE                PIC X(13).           cbankext
018900         20  EXT-OP35-STMT-DATE              PIC X(11).           cbankext
019000         20  EXT-OP35-ATM-DETAILS.                                cbankext
019100           25  EXT-OP35-ATM-VIS              PIC X(1).            cbankext
019200           25  EXT-OP35-ATM-LIM              PIC X(3).            cbankext
019300           25  EXT-OP35-ATM-LDTE             PIC X(11).           cbankext
019400           25  EXT-OP35-ATM-LAMT             PIC X(3).            cbankext
019500         20  EXT-OP35-RP-DETAILS             OCCURS 3 TIMES.      cbankext
019600           25  EXT-OP35-RP-DAY               PIC X(2).            cbankext
019700           25  EXT-OP35-RP-AMT               PIC X(8).            cbankext
019800           25  EXT-OP35-RP-PID               PIC X(5).            cbankext
019900           25  EXT-OP35-RP-ACC               PIC X(9).            cbankext
020000           25  EXT-OP35-RP-DTE               PIC X(11).           cbankext
020100       15  EXT-OP40-DATA REDEFINES EXT-OP-AREA.                   cbankext
020200         20  EXT-OP40-ACCNO                  PIC X(9).            cbankext
020300         20  EXT-OP40-ACCTYPE                PIC X(25).           cbankext
020400         20  EXT-OP40-PAGING-STATUS          PIC X(1).            cbankext
020500           88  EXT-OP40-PAGING-OFF           VALUE LOW-VALUES.    cbankext
020600           88  EXT-OP40-PAGING-FIRST         VALUE '1'.           cbankext
020700           88  EXT-OP40-PAGING-MIDDLE        VALUE '2'.           cbankext
020800           88  EXT-OP40-PAGING-LAST          VALUE '3'.           cbankext
020900         20  EXT-OP40-DETAILS                OCCURS 8 TIMES.      cbankext
021000           25  EXT-OP40-DATE                 PIC X(11).           cbankext
021100           25  EXT-OP40-TIME                 PIC X(8).            cbankext
021200           25  EXT-OP40-AMNT                 PIC X(13).           cbankext
021300           25  EXT-OP40-DESC                 PIC X(25).           cbankext
021400       15  EXT-OP50-DATA REDEFINES EXT-OP-AREA.                   cbankext
021500         20  EXT-OP50-XFER                   PIC X(9).            cbankext
021600         20  EXT-OP50-FRM1                   PIC X(1).            cbankext
021700         20  EXT-OP50-TO1                    PIC X(1).            cbankext
021800         20  EXT-OP50-ACC1                   PIC X(9).            cbankext
021900         20  EXT-OP50-DSC1                   PIC X(15).           cbankext
022000         20  EXT-OP50-BAL1                   PIC X(13).           cbankext
022100         20  EXT-OP50-FRM2                   PIC X(1).            cbankext
022200         20  EXT-OP50-TO2                    PIC X(1).            cbankext
022300         20  EXT-OP50-ACC2                   PIC X(9).            cbankext
022400         20  EXT-OP50-DSC2                   PIC X(15).           cbankext
022500         20  EXT-OP50-BAL2                   PIC X(13).           cbankext
022600         20  EXT-OP50-FRM3                   PIC X(1).            cbankext
022700         20  EXT-OP50-TO3                    PIC X(1).            cbankext
022800         20  EXT-OP50-ACC3                   PIC X(9).            cbankext
022900         20  EXT-OP50-DSC3                   PIC X(15).           cbankext
023000         20  EXT-OP50-BAL3                   PIC X(13).           cbankext
023100         20  EXT-OP50-FRM4                   PIC X(1).            cbankext
023200         20  EXT-OP50-TO4                    PIC X(1).            cbankext
023300         20  EXT-OP50-ACC4                   PIC X(9).            cbankext
023400         20  EXT-OP50-DSC4                   PIC X(15).           cbankext
023500         20  EXT-OP50-BAL4                   PIC X(13).           cbankext
023600         20  EXT-OP50-FRM5                   PIC X(1).            cbankext
023700         20  EXT-OP50-TO5                    PIC X(1).            cbankext
023800         20  EXT-OP50-ACC5                   PIC X(9).            cbankext
023900         20  EXT-OP50-DSC5                   PIC X(15).           cbankext
024000         20  EXT-OP50-BAL5                   PIC X(13).           cbankext
024100         20  EXT-OP50-FRM6                   PIC X(1).            cbankext
024200         20  EXT-OP50-TO6                    PIC X(1).            cbankext
024300         20  EXT-OP50-ACC6                   PIC X(9).            cbankext
024400         20  EXT-OP50-DSC6                   PIC X(15).           cbankext
024500         20  EXT-OP50-BAL6                   PIC X(13).           cbankext
024600       15  EXT-OP60-DATA REDEFINES EXT-OP-AREA.                   cbankext
024700         20  EXT-OP60-OADDR1                 PIC X(25).           cbankext
024800         20  EXT-OP60-OADDR2                 PIC X(25).           cbankext
024900         20  EXT-OP60-OSTATE                 PIC X(2).            cbankext
025000         20  EXT-OP60-OCNTRY                 PIC X(6).            cbankext
025100         20  EXT-OP60-OPSTCDE                PIC X(6).            cbankext
025200         20  EXT-OP60-OTELNO                 PIC X(12).           cbankext
025300         20  EXT-OP60-NADDR1                 PIC X(25).           cbankext
025400         20  EXT-OP60-NADDR2                 PIC X(25).           cbankext
025500         20  EXT-OP60-NSTATE                 PIC X(2).            cbankext
025600         20  EXT-OP60-NCNTRY                 PIC X(6).            cbankext
025700         20  EXT-OP60-NPSTCDE                PIC X(6).            cbankext
025800         20  EXT-OP60-NTELNO                 PIC X(12).           cbankext
025900         20  EXT-OP60-NEMAIL                 PIC X(30).           cbankext
026000         20  EXT-OP60-NSMAIL                 PIC X(1).            cbankext
026100         20  EXT-OP60-NSEMAIL                PIC X(1).            cbankext
026200       15  EXT-OP70-DATA REDEFINES EXT-OP-AREA.                   cbankext
026300         20  EXT-OP70-AMOUNT                 PIC X(7).            cbankext
026400         20  EXT-OP70-RATE                   PIC X(7).            cbankext
026500         20  EXT-OP70-TERM                   PIC X(5).            cbankext
026600         20  EXT-OP70-PAYMENT                PIC X(9).            cbankext
026700       15  EXT-OP80-DATA REDEFINES EXT-OP-AREA.                   cbankext
026800         20  EXT-OP80-ADDR1                 PIC X(25).            cbankext
026900         20  EXT-OP80-ADDR2                 PIC X(25).            cbankext
027000         20  EXT-OP80-STATE                 PIC X(2).             cbankext
027100         20  EXT-OP80-CNTRY                 PIC X(6).             cbankext
027200         20  EXT-OP80-PSTCDE                PIC X(6).             cbankext
027300         20  EXT-OP80-EMAIL                 PIC X(30).            cbankext
027400         20  EXT-OP80-OPT1                  PIC X(1).             cbankext
027500         20  EXT-OP80-OPT2                  PIC X(1).             cbankext
027600       15  EXT-OPZZ-DATA REDEFINES EXT-OP-AREA.                   cbankext
027700         20  EXT-OPZZ-SEL1ID                 PIC X(1).            cbankext
027800         20  EXT-OPZZ-SEL1IP                 PIC X(1).            cbankext
027900         20  EXT-OPZZ-SEL1TX                 PIC X(40).           cbankext
028000         20  EXT-OPZZ-SEL2ID                 PIC X(1).            cbankext
028100         20  EXT-OPZZ-SEL2IP                 PIC X(1).            cbankext
028200         20  EXT-OPZZ-SEL2TX                 PIC X(40).           cbankext
028300         20  EXT-OPZZ-SEL3ID                 PIC X(1).            cbankext
028400         20  EXT-OPZZ-SEL3IP                 PIC X(1).            cbankext
028500         20  EXT-OPZZ-SEL3TX                 PIC X(40).           cbankext
028600         20  EXT-OPZZ-SEL4ID                 PIC X(1).            cbankext
028700         20  EXT-OPZZ-SEL4IP                 PIC X(1).            cbankext
028800         20  EXT-OPZZ-SEL4TX                 PIC X(40).           cbankext
028900         20  EXT-OPZZ-SEL5ID                 PIC X(1).            cbankext
029000         20  EXT-OPZZ-SEL5IP                 PIC X(1).            cbankext
029100         20  EXT-OPZZ-SEL5TX                 PIC X(40).           cbankext
029200         20  EXT-OPZZ-SEL6ID                 PIC X(1).            cbankext
029300         20  EXT-OPZZ-SEL6IP                 PIC X(1).            cbankext
029400         20  EXT-OPZZ-SEL6TX                 PIC X(40).           cbankext
029500         20  EXT-OPZZ-SEL7ID                 PIC X(1).            cbankext
029600         20  EXT-OPZZ-SEL7IP                 PIC X(1).            cbankext
029700         20  EXT-OPZZ-SEL7TX                 PIC X(40).           cbankext
029800         20  EXT-OPZZ-SEL8ID                 PIC X(1).            cbankext
029900         20  EXT-OPZZ-SEL8IP                 PIC X(1).            cbankext
030000         20  EXT-OPZZ-SEL8TX                 PIC X(40).           cbankext
030100                                                                  cbankext
030200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankext
